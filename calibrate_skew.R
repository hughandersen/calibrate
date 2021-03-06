#calibrate skew curve


library(tidyverse)
library(IBrokers)
library(lubridate)
library(readxl)
library(writexl)
library(RQuantLib)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X,col_types = "guess",na="NA"))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

fn_skew_coeffs <- function(df){
  
  model <- lm(iv ~  k + I(k^2),data=df)
  
  coeffs <- model$coefficients
  names(coeffs) <- c("coeff_b0","coeff_b1","coeff_b2")
  return(coeffs)
}

#old functions------------------------------------
# fn_solve_iv <- function(strike,right,dte_option,price_mid,underlying_last,rf){
#   
#   epsilon <- 0.0025
#   #start with a high iv
#   iv <- 0.25
#   i <- 0
#   repeat{
#     price_solve <- fn_price_current(right,strike,iv,dte_option,underlying_last,rf)
#     vega <- fn_vega(strike,iv,dte_option,underlying_last,rf)
#     
#     i <- i+1
#     error <- abs(price_mid-price_solve)/price_solve
#     
#     #loop until price accurate enough or loops>1000
#     if(max(error)<epsilon | i>1000){
#       break
#     }
#     #get next iv approximation
#     iv <- iv-((price_solve-price_mid)/vega)/100
#     
#   }
#   return(iv)
# }
# 
# fn_vega <- function(strike,iv,dte,spot,rf){
#   d1 <- fn_done(strike,iv,dte,spot)
#   nd1 <- dnorm(-d1,mean=0,sd=1)
#   vega <- (spot * exp(-rf * dte) * dte^0.5*nd1 ) / 100
#   return (vega)
# }
# 
# fn_price_current <- function(right,strike,iv,dte_option,spot,rf){
#   d1 <- fn_done(strike,iv,dte_option,spot)
#   d2 <- fn_dtwo(d1,iv,dte_option)
#   
#   price_current <- ifelse( right == "C",
#                            spot*exp(-rf*dte_option)*pnorm(d1)-strike*exp(-rf*dte_option)*pnorm(d2),
#                            strike*exp(-rf*dte_option)*pnorm(-d2)-spot*exp(-rf*dte_option)*pnorm(-d1)
#   )
#   return (price_current)
# }
# 
# fn_done <- function(strike,iv,dte_option,spot){
#   d1 <- ( log(spot /strike) + dte_option* (0.5*(iv^2))) / (iv * sqrt(dte_option))
#   return (d1)
# }
# 
# fn_dtwo <- function(d1,iv,dte_option){
#   d2 <-  d1 - iv * sqrt(dte_option)
#   return (d2)
# }

#-------------------------------
fn_get_options_last <- function(tws,symbol,security_type,expiry,exchange,currency,multiplier,right,strike){
  
  
  contract <-  twsContract(0,
                           symbol=as.character(symbol),
                           sectype=as.character(security_type),
                           exch=as.character(exchange),
                           primary="",
                           expiry= expiry,
                           strike=strike,
                           currency=as.character(currency),
                           right=right,
                           local="",
                           multiplier = as.character(multiplier),
                           combo_legs_desc = "",
                           comboleg = "",
                           include_expired = "",
                           secIdType = "",
                           secId = "")
 
  data <- reqMktData(tws,contract,snapshot = TRUE)
  
  #indicies don't have bid ask prices, some futures don't always trade
  if(!is.na(data$askPrice) & !is.na(data$bidPrice)){
    last <- (data$askPrice+data$bidPrice)/2
  }
  else{
    last <- data$lastPrice
  }
  
  return(last)
}

fn_get_underlying_last <- function(tws,symbol,security_type,expiry,exchange,currency,multiplier){
  
  contract <-  twsContract(0,
                           symbol=as.character(symbol),
                           sectype=as.character(security_type),
                           exch=as.character(exchange),
                           primary="",
                           # expiry= as.character(as.Date(expiry,format="%d_%b_%y"),"%Y%m%d"),
                           # expiry=expiry,
                           expiry=as.character(dmy(expiry),format="%Y%m%d"),
                           strike="",
                           currency=as.character(currency),
                           right="",
                           local="",
                           multiplier = as.character(multiplier),
                           combo_legs_desc = "",
                           comboleg = "",
                           include_expired = "",
                           secIdType = "",
                           secId = "")
  
  data <- reqMktData(tws,contract,snapshot = TRUE)
  
  #indicies don't have bid ask prices, some futures don't always trade
  if(!is.na(data$askPrice) & !is.na(data$bidPrice)){
    last <- (data$askPrice+data$bidPrice)/2
  }
  else{
    last <- data$lastPrice
  }
  
  return(last)
}

#import datasets------------------------------------------------------------------------------- 
data <- read_excel_allsheets("skew_data.xlsx")
data_old <- data

data$underlying <- data$underlying %>% 
  group_by(symbol,security_type,expiry) %>% 
  filter(row_number()==1) %>% 
  ungroup()

# View(data$underlying)
# View(data$options)


#update underlying-------------------------------------------------

tws <-  twsConnect() 
Sys.sleep(5)
tws_online <- isConnected(twsconn = tws)

Sys.sleep(5)

data$underlying <- data$underlying %>%
  rowwise() %>%
  mutate(
    last=fn_get_underlying_last(tws,symbol,security_type,expiry,exchange,currency,multiplier),
    updated=format(ymd(Sys.Date()),"%d_%b_%Y")
  )

Sys.sleep(15)

data$chain <- data$underlying %>% 
  mutate(udly_dte=(dmy(expiry)-Sys.Date())) %>% 
  filter(udly_dte>7 & udly_dte<100) %>% 
  select(symbol,expiry,expiry,currency,exchange,multiplier,last,rf) %>% 
  rename(last_underlying=last) %>% 
  group_by(symbol,expiry,currency,exchange,multiplier,rf) %>% 
  mutate(
    security_type="OPT",
    last_underlying_round=200*ceiling(last_underlying/200),
    strike=list(seq(from=(last_underlying_round-2000),to=(last_underlying_round+2000),by=400))
  ) %>% 
  unnest(cols=c(strike)) %>% 
  mutate(
    dte=as.numeric(dmy(expiry)-Sys.Date()),
    k=strike/last_underlying,
    right=ifelse(k>1,"C","P"),
    expiry=as.character(dmy(expiry),format="%Y%m%d"),
    last_option=NA
  ) 

#purrr method
# test <- data$chain %>% 
#   mutate(last_option=NA) %>% 
#   rowwise() %>% 
#   mutate(
#     last_option=fn_get_options_last(
#       tws,
#       symbol=data$chain$symbol[k],
#       security_type=data$chain$security_type[k],
#       expiry=data$chain$expiry[k],
#       exchange=data$chain$exchange[k],
#       currency=data$chain$currency[k],
#       multiplier=data$chain$multiplier[k],
#       right=data$chain$right[k],
#       strike=data$chain$strike[k]
#     )
#   )

Sys.sleep(15)

#get prices for options chain-------------------------------
for(k in 1:nrow(data$chain)){
  print(k)
  
  data$chain$last_option[k] <- fn_get_options_last(
    tws,
    symbol=data$chain$symbol[k],
    security_type=data$chain$security_type[k],
    expiry=data$chain$expiry[k],
    exchange=data$chain$exchange[k],
    currency=data$chain$currency[k],
    multiplier=data$chain$multiplier[k],
    right=data$chain$right[k],
    strike=data$chain$strike[k]
  )
}

twsDisconnect(tws)

# Sys.sleep(15)

#solve for IV----------------------------------------------
# data$chain <- data$chain %>% 
#   filter(last_option>5) %>% 
#   mutate(
#     iv=fn_solve_iv(strike=strike,right=right,dte_option = dte/365,
#                    price_mid=last_option,underlying_last = last_underlying,rf = 1/100),
#     updated=format(ymd(Sys.Date()),"%d_%b_%Y")
#   )

data$chain <- data$chain %>% 
  filter(last_option>5,
         !is.na(last_option)
         ) %>% 
  mutate(
    right=str_replace_all(right,"C","call"),
    right=str_replace_all(right,"P","put")
  ) %>% 
  rowwise() %>% 
  mutate(
    iv=as.numeric(EuropeanOptionImpliedVolatility(
      type=right,
      value=last_option,
      underlying=last_underlying,
      strike=strike,
      dividendYield=0,
      riskFreeRate=0.05,
      maturity=dte/365,
      volatility=0.25
    )),
    updated=format(ymd(Sys.Date()),"%d_%b_%Y")
  ) %>% 
  ungroup() %>% 
  select(symbol,security_type,expiry,right,strike,last_option,iv,dte,k,last_underlying,updated) 

#drop unneeded variables, calc skew curve coefficients
data$options <- data$chain %>% 
  select(symbol,security_type,expiry,currency,exchange,multiplier,rf,k,iv) %>% 
  group_by(symbol,security_type,expiry,currency,exchange,multiplier,rf) %>% 
  nest() %>% 
  mutate(
    model = map(data, fn_skew_coeffs)
  ) %>% 
  select(-data) %>% 
  unnest_wider(col=model) %>% 
  mutate(
    expiry=format(ymd(expiry),"%d_%b_%Y"),
    updated=format(ymd(Sys.Date()),"%d_%b_%Y")
  )



data_final <- list()
data_final$options <- bind_rows(data_old$options,data$options) %>% 
  group_by(symbol,security_type,expiry,updated) %>% 
  filter(row_number()==1) %>% 
  ungroup()

data_final$chain <- bind_rows(data_old$chain,data$chain) %>% 
  group_by(symbol,security_type,expiry,strike,k,updated) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  select(symbol,security_type,expiry,right,strike,last_option,iv,dte,k,last_underlying,updated) 

  
data_final$underlying <- bind_rows(data_old$underlying,data$underlying)%>% 
  group_by(symbol,security_type,expiry,updated) %>% 
  filter(row_number()==1) %>% 
  ungroup()

View(data_final$options)
View(data_final$chain)
View(data_final$underlying)

write_xlsx(
  data_final,
  path="skew_data.xlsx",
  col_names = TRUE,
  format_headers = TRUE
)


#------------------------------------------------------------------------
# hsi_idx_contract <- twsIndex(symbol="HSI",exch="HKFE",currency="HKD")
# hsi_fut_contract <- twsFUT(symbol="HSI",exch="HKFE",currency="HKD")
# hsi_options_contract <- twsOption(exch="HKFE",currency="HKD",local="", symbol="HSI",multiplier = "50")
# hsi_options <- reqContractDetails(tws, twsOption(exch="HKFE",currency="HKD",local="", symbol="HSI",multiplier = "50"))

# tws <-  twsConnect() 
# 
# ctc <- hsi_options[[1]]$contract
# last <- reqRealTimeBars(tws,ctc,whatToShow = "BID")
# last <- reqMktData(tws,ctc,snapshot = TRUE)
# 
# hsi_options[[1]]$last <- 999
# hsi_options[[1]]$k <- as.numeric(hsi_options[[1]]$contract$strike)/29554
# hsi_options[[1]]$dte <- as.numeric(dmy(hsi_options[[1]]$contract$expiry)-Sys.Date())

#lapply(hsi_options,)


#check---------------------------

check <- data_final$chain %>% 
  filter(
    right=="call"|right=="put"
  ) %>% 
  # ungroup() %>% 
  select(symbol,expiry,strike,right,k,iv,last_option,dte,last_underlying,updated) %>% 
  mutate(
    expiry=format(ymd(expiry),"%d_%b_%Y")
  ) %>% 
  left_join(data_final$options,
            by=c("symbol","expiry","updated")) %>% 
  rowwise() %>% 
  mutate(
    iv_est=coeff_b0+coeff_b1*k+coeff_b2*k^2,
    last_est=EuropeanOption(type=right,underlying = last_underlying,strike=strike,dividendYield = 0,riskFreeRate = rf,
                                      maturity = dte/365,volatility = 0.25)$value,
    diff_last=last_option-last_est,
    diff_iv=iv-iv_est
  )






