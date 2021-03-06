library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)

#load in all the data from Camila download
precip_stream_data <- readRDS("D:/Duke/Work(Environ)/Programming/AcidRainStory/DataCleaning/precip_stream_data.rds")

#smalldata <- precip_stream_data[c(1:1000, 99788:101000),]

#make a df of acid rain history dates (CAA, etc.) #https://daattali.com/shiny/timevis-demo/
historyData <- data.frame(
  id = 1:4,
  content = c("Majority of HBEF dataset", "EPA founded", "Clean Air Act", "Today"),
  start = c("1963-06-01", "1970-12-02", "1970-06-01", "2017-06-19"), #FIND THE REAL DATE OF CAA ENACTMENT!
  end = c("2014-05-01", NA, NA, NA)
)

Cadata <- precip_stream_data[precip_stream_data$solute == "Ca",]
Cadata <- Cadata[Cadata$ws == "6",]



shinyServer( function(input, output){
  
  #make a function to create concentration graphs over (reactive) time...
  DatevConcPlotly <- function(compound){
    renderPlot({
      #create df for compound to graph... possibly make this into an lapply?
      functionData <- precip_stream_data[precip_stream_data$solute == compound,]
      functionData <- functionData[functionData$ws == "6",]
      functionData <- as.data.frame(functionData)
      #create ggplot... for some reason ggplotly will not work with this...
      g1 <- ggplot(functionData, aes(x = as.Date(date)))+
        geom_line(aes(y = concentration_ueq, group = source, color = source))+ 
        labs(colour = "Source", x = "Year", y = "(ueq/L)")+
        xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
        ggtitle(as.character(compound))+
        theme(plot.background = element_rect(fill = 'gray', colour = 'gray'))
      return(g1)
    })
  }
  

  #plot of Ca conc over rective time
  output$CaTime <- renderPlotly({
    CaTime <- ggplot(Cadata, aes(x = as.Date(date)))+
      geom_line(aes(y = concentration_ueq, group = source, color = source))+ 
      labs(colour = "Source", x = "Year", y = "Ca (ueq/L)")+
      xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
      ggtitle("Calcium's acid rain peak in 1970 and subsequent decline")+
      theme(plot.background = element_rect(fill = 'gray', colour = 'gray'))
    #      theme(panel.background = element_rect(fill = 'black'))
    
    ggplotly(CaTime)
  })
  #plot of Mg conc over reactive time
  output$MgTime <- DatevConcPlotly("Mg")
  
  #plot of K conc over reactive time... obv make a function for these graphs
  output$KTime <- DatevConcPlotly("K")
  
  output$NaTime <- DatevConcPlotly("Na")
  output$AlTime <- DatevConcPlotly("Al")
  output$NH4Time <- DatevConcPlotly("NH4")
  output$SO4Time <- DatevConcPlotly("SO4")
  output$NO3Time <- DatevConcPlotly("NO3")
  output$ClTime <- DatevConcPlotly("Cl")
  output$HTime <- DatevConcPlotly("H")
  
  #output an interactive timeline for the history of acid rain
  output$CAAetc <- renderTimevis({
    timevis(historyData) #possibly use groups.. policy vs natural occurances? maybe that's extra..
  })
  
})

