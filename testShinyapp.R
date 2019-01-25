library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(shiny)

setwd("~/Documents/EBG/behavior data/analytical data")
fileNames <- dir()
data <- lapply(fileNames, read.csv, header = TRUE)
numOfData <- length(data)  # = 74 subjects
numOfPair <- numOfData / 2 # = 37 pairs

# try use shiny
{
  stockPrice <- list()
  stockNumber <- list()
  for(i in 1:numOfPair){
    stockPrice[[i]] <- data[[2*1-1]][c("Trials", "StockPrice")]
    sn <- cbind(data[[2*i-1]][c("Trials", "p1Stock")], data[[2*i]]["p2Stock"])
    stockNumber[[i]] <- gather(sn, key = "player", value = "stockNumber", p1Stock, p2Stock)
  }
  
  dataList <- list(stockPrice, stockNumber)
  varName <- c("stock price", "stock number")
  names(dataList) <- varName
  
  
  ui = pageWithSidebar(
    headerPanel(
      "test"
    ),
    sidebarPanel(
      selectInput("pairNumber", "No. pair", 1:numOfPair, selected = 1),
      sliderInput("trialRange", "trial range",
                  min = 1, max = 101, value = c(1, 101))
    ),
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
  
  server = function(input, output){
    spData<- reactive({
      dataList[["stock price"]][[as.integer(input$pairNumber)]] %>% 
        filter(Trials >= input$trialRange[1] & 
                 Trials <= input$trialRange[2])
    })
    snData <- reactive({
      dataList[["stock number"]][[as.integer(input$pairNumber)]] %>% 
        filter(Trials >= input$trialRange[1] & 
                 Trials <= input$trialRange[2])
    })
    output$plot1 <- renderPlot({
      g <- ggplot(spData(), aes(x = Trials, y = StockPrice)) +
        geom_line() +
        theme_bw() +
        labs(title = paste("Pair No.", input$pairNumber, "stock price by trials"),
             x = "trials",
             y = "stock number")
      g
    })
    
    output$plot2 <- renderPlot({
      
      g <- ggplot(snData(), 
                  aes(x = Trials, y = stockNumber, group = player)) +
        geom_line(aes(linetype = player, color = player)) +
        theme_bw() +
        labs(title = paste("Pair No.", input$pairNumber, "stock number by trials"),
             x = "trials",
             y = "stock number") + 
        theme(legend.position = "bottom")
      g
    })      
  }
  
  shinyApp(ui, server)
}

