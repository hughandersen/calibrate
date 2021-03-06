
library(shiny)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(DT)
library(alchemy)
library(readxl)
library(writexl)
library(IBrokers)



#init------------------------------------------------------------------------
# setwd("C:\\Hughs files")
data <- read_excel_allsheets("options_trader_data.xlsx")

# chain <- data.frame(
#   symbol=rep(NA,9),
#   expiry=rep(NA,9),
#   dte=rep(NA,9),
#   spot=rep(NA,9),
#   rf=rep(1.5/100,9),
#   delta=c(10,20,30,40,50,40,30,20,10),
#   right=c("C","C","C","C","C","P","P","P","P"),
#   price=rep(NA,9),
#   iv=rep(NA,9)
# )

# udly_symbol <- "sym"

Fn_solve_IV <- function(strike,right,DTE_option,price_mid,underlying_last,risk_free_rate){
  
  epsilon <- 0.0025
  #start with a high IV
  IV <- 0.25
  i <- 0
  repeat{
    price_solve <- Fn_price_current(right,strike,IV,DTE_option,underlying_last,risk_free_rate)
    vega <- Fn_vega(strike,IV,DTE_option,underlying_last,risk_free_rate)
    
    i <- i+1
    error <- abs(price_mid-price_solve)/price_solve
    
    #loop until price accurate enough or loops>1000
    if(max(error)<epsilon | i>1000){
      break
    }
    #get next IV approximation
    IV <- IV-((price_solve-price_mid)/vega)/100
    
  }
  return(IV)
}

# UI------------------------------------------------------------------------------------------------
ui=fluidPage(
    fluidRow(
        br(),
        column(1,
               br(),
               actionButton(inputId = "calibrate", "Calibrate"),
               actionButton(inputId = "update_underling", "Update"),
               actionButton(inputId = "save_dataset", "Save")
               
        ),
        column(6,
               h3("Underlying"),
               DT::dataTableOutput("underlying")
        ),
        column(4,
               plotOutput("price_check_chart",height=300)
        )
        # column(2,
        #        selectInput(inputId="underlying",label="Underlying",choices=c("MHI","AP"),selected = 1),
        #        selectInput(inputId="expiry",label="Expiry",choices=c("25/4/2020","31/9/2020"),selected = 1)
        #        ),
        # column(3,
        #        h3("Manual input"),
        #        DT::dataTableOutput("manual")
        # )
    ),
    fluidRow(
        # br(),
        # br(),
        column(1,
        actionButton(inputId = "insert_option", "Insert"),
        actionButton(inputId = "calibrate", "Update")
    ),
        column(6,
               h3("Options"),
               DT::dataTableOutput("options")
        )
        # column(4,
        #        plotOutput("price_check_chart")
        # )
    ),
    fluidRow(
      br(),
      # br(),
      column(1,
             actionButton(inputId = "insert_option", "Insert"),
             actionButton(inputId = "calibrate", "Update")
      ),
      column(6,
             h3("Chain"),
             DT::dataTableOutput("chain")
      ),
      column(4,
             plotOutput("skew_plot",height=300)
      )
    )
    #set font size of tables
    # useShinyjs(),
    # inlineCSS(list("table" = "font-size: 10px"))
)


# Server ---------------------------------------------------------
server <- function(input, output) {
    
    #reactive values----------------------------------------------------
    rvs <- reactiveValues()
    rvs$underlying <- data$underlying
    rvs$options <- data$options
    rvs$mapping <- data$mapping
    rvs$chain <- data$chain_manual

    previous_row <- NULL
    
    # output$y11 = renderPrint(rvs$underlying[input$underlying_rows_selected,1])
    # output$y11 = renderPrint(input$underlying_rows_selected)
    
    #calibrate event---------------------
    observeEvent(input$calibrate, {
      
      tws <-  twsConnect() 
      tws_online <- isConnected(twsconn = tws)
      
      if(tws_online==TRUE){
        table_list <- fn_calibrate_auto(underlying=data$underlying,
                                        mapping=data$mapping,
                                        tws)
        
        data$chain <- table_list$chain
        data$underlying <- table_list$underlying
        data$options <- table_list$options
      }
      
        # tws <-  twsConnect() 
        # tws_online <- isConnected(twsconn = tws)
        # 
        # #update underlying
        # for(k in 1:nrow(rvs$underlying)){
        #   sym <- rvs$underlying[k,"symbol"]
        #   sec_type <- rvs$underlying[k,"security_type"]
        #   exp <- rvs$underlying[k,"expiry"]
        #   exch <- rvs$underlying[k,"exchange"]
        #   curr <- rvs$underlying[k,"currency"]
        #   mult <- rvs$underlying[k,"multiplier"]
        #   
        #   rvs$underlying[k,"last"] <- fn_get_underlying_last(tws=tws,
        #                                                      symbol=sym,
        #                                                      security_type = sec_type,
        #                                                      expiry=exp,
        #                                                      exchange=exch,
        #                                                      currency=curr,
        #                                                      multiplier=mult
        #                                                      )
        # }
        # 
        # chain <- fn_get_chain(underlying=rvs$underlying,mapping=rvs$mapping)
        # 
        # options <- fn_options_coeffs(chain)
        # options <- options %>%
        #   mutate(
        #     expiry=fn_long_date(expiry),
        #     updated=fn_long_date(Sys.Date())
        #   )
        
    })
    
    
    #underlying render-----------------------------------------------------------------------
    output$underlying <- DT::renderDataTable({
      
      # df <- data.frame(x=seq(1,10),y=rnorm(n=10,0,1))
        
        DT::datatable(
          # data=df,
            data=rvs$underlying,
            editable = TRUE,
            rownames = FALSE,
            class="compact cell-border",
            selection = list(mode = "single",
                             target = "row"
                             # selected = previous_row
            ),
            options = list(
                dom="t",
                autoWidth=TRUE,
                scrollX = TRUE,
                ordering=FALSE,
                pageLength = 16,
                bLengthChange= FALSE,
                # displayStart = previous_page,
                searching=FALSE
            )
        )
        
    })
    
    #underlying edit----
    observeEvent(input$underlying_cell_edit, {
      
      info = input$underlying_cell_edit
      str(info)
      i = info$row
      j = info$col+1
      k = info$value

      # print(names(rvs$underlying)[j])
      
      if (j %in% match(c("last"), names(rvs$underlying))) {
        rvs$underlying[i,j] <<- as.numeric(k)
      }
      
      if (j %in% match(c("expiry","symbol"), names(rvs$underlying))) {
        rvs$underlying[i,j] <<- as.character(k)
      }
      
      previous_row<<- input$underlying_rows_selected
      # udly_symbol <<- rvs$underlying[i,"symbol"]
      print(input$underlying_rows_selected)
      
    })
    
    #underlying delete----
    observeEvent(input$underlying_leg,{
      row_selected = input$underlying_rows_selected
      
      symbol <- rvs$underlying[row_selected,"symbol"]
      expiry <- rvs$underlying[row_selected,"expiry"]
      
      rvs$underlying <- rvs$underlying %>% 
        filter(symbol!=symbol,expiry!=expiry)
      
    })
    
    
    
    #options edit----
    observeEvent(input$options_cell_edit, {
      
      info = input$options_cell_edit
      str(info)
      i = info$row
      j = info$col+1
      k = info$value
      
      # print(names(rvs$underlying)[j])
      
      # if (j %in% match(c("last"), names(rvs$underlying))) {
      #   rvs$underlying[i,j] <<- as.numeric(k)
      # }
      
      if (j %in% match(c("expiry","symbol"), names(rvs$options))) {
        rvs$options[i,j] <<- as.character(k)
      }
      
      previous_row<<- input$options_rows_selected
      udly_symbol <<- rvs$options[row_selected,"symbol"]
      print(udly_symbol)
      
    })
    
    #options render-------
output$options <- DT::renderDataTable({
  
  df_opt <- rvs$options %>% 
    mutate(
      coeff_b0=round(coeff_b0,3),
      coeff_b1=round(coeff_b1,3),
      coeff_b2=round(coeff_b2,3)
    )
    
    DT::datatable(
        data=df_opt,
        editable = TRUE,
        rownames = FALSE,
        class="compact cell-border",
        selection = list(mode = "single",
                         target = "row"
                         # selected = previous_row
        ),
        options = list(
            dom="t",
            autoWidth=TRUE,
            scrollX = TRUE,
            ordering=FALSE,
            pageLength = 16,
            bLengthChange= FALSE,
            # displayStart = previous_page,
            searching=FALSE
        )
    )
    
})
    
    #options skew------------------------------------------------
    
    output$skew_chart <- renderPlot({
        
        data <- data.frame(
            x=seq(from=1,to=25),
            y=rnorm(n=25,mean=0,sd=1)
            )
        
        skew_chart <- data %>% 
            ggplot()+
            aes(x=x,y=y)+
            geom_point()+
            geom_line()+
            labs(title="skew chart")
        
        return(skew_chart)
    })
    
    #options plot price check------------------------------------------------
    
    output$price_check_chart <- renderPlot({
        
        data <- data.frame(
            x=seq(from=1,to=25)
            ) %>% 
            mutate(
            y=x+rnorm(n=25,mean=0,sd=1)
        )
        
        price_check_chart <- data %>% 
            ggplot()+
            aes(x=x,y=y)+
            geom_point()+
            geom_line()+
            labs(title="price check chart")
        
        return(price_check_chart)
    })
    
    
    #chain render table--------------------
    
    output$chain <- DT::renderDataTable({
      
      rvs$chain[,"symbol"] <- as.character(rvs$underlying[input$underlying_rows_selected,"symbol"]) 
      rvs$chain[,"expiry"] <- as.character(rvs$underlying[input$underlying_rows_selected,"expiry"]) 
      rvs$chain[,"rf"] <- 1.5/100
      rvs$chain[,"spot"] <- as.numeric(rvs$underlying[input$underlying_rows_selected,"last"]) 
      exp <- dmy(rvs$underlying[input$underlying_rows_selected,"expiry"])
      dte <- round((exp-Sys.Date())/365,3)
      # print(dte)
      rvs$chain[,"dte"] <- dte 
      
      DT::datatable(
        # data=df,
        data=rvs$chain,
        editable = TRUE,
        rownames = FALSE,
        class="compact cell-border",
        selection = list(mode = "single",
                         target = "row"
                         # selected = previous_row
        ),
        options = list(
          dom="t",
          autoWidth=TRUE,
          scrollX = TRUE,
          ordering=FALSE,
          pageLength = 16,
          bLengthChange= FALSE,
          # displayStart = previous_page,
          searching=FALSE
        )
      )
    })
    
    

    
    #chain edit-----------------
    observeEvent(input$chain_cell_edit, {
      
      info = input$chain_cell_edit
      str(info)
      i = info$row
      j = info$col+1
      k = info$value
      
      # print(names(rvs$chain)[j])
      
      if (j %in% match(c("price","strike"), names(rvs$chain))) {
        rvs$chain[i,j] <<- as.numeric(k)
      }
      
      #calculate iv
      # rvs$chain$iv <<- as.numeric(99)
      dte <- as.numeric(rvs$chain[i,"dte"])
      spot <- as.numeric(rvs$chain[i,"spot"])
      strike <- as.numeric(rvs$chain[i,"strike"])
      right <- rvs$chain[i,"right"]
      price <- rvs$chain[i,"price"]
      
      # print(dte)
      # print(spot)
      # print(strike)
      # print(right)
      # print(price)
      
      if(!is.na(dte) & !is.na(spot) & !is.na(strike) & !is.na(right) & !is.na(price)){
      iv <- Fn_solve_IV(strike=strike,right=right,DTE_option=dte,
                        price_mid=price,underlying_last=spot,risk_free_rate=1.5/100)
      
      rvs$chain[i,"iv"] <<- round(iv,3)
      }
      
      # rvs$chain[i,t] <<- 0.25*as.numeric(k)
      # Fn_solve_IV(strike,right,DTE_option,price_mid,underlying_last,risk_free_rate)
      
    })
    
    #skew plot-----------------------------------------------------------------------
    
    output$skew_plot <- renderPlot({
      
      skew <- data.frame(
        K=seq(from=0.95,to=1.05,by=0.01)
      ) %>% 
        mutate(
          IV=5.0-9.5*K+4.7^2*K
        )

      skew_plot <- skew %>% 
        ggplot()+
        aes(x=K,y=IV)+
        geom_point()+
        geom_line()+
        labs(title="skew plot")
      
      return(skew_plot)
    })
    
    
    #save datasets----
    observeEvent(input$save_data,{
      
      data$underlying <- rvs$underlying
      data$options <- rvs$options 
      
      write_xlsx(
        data,
        path="options_trader_data_test.xlsx",
        col_names = TRUE,
        format_headers = TRUE
      )
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)




