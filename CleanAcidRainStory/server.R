library(ggplot2)
library(ggiraph)
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)

#load in all the data from Camila download
precip_stream_data <- readRDS("D:/Duke/Work(Environ)/Programming/AcidRainStory/DataCleaning/precip_stream_data.rds")

smalldata <- precip_stream_data[c(1:1000, 99,788:101,000),]

Cadata <- precip_stream_data[precip_stream_data$solute == "Ca",]
Cadata <- Cadata[Cadata$ws == "6",]

shinyServer( function(input, output){
  output$CaTime <- renderggiraph({
    CaTime <- ggplot(Cadata, aes(x = as.Date(date)))+
      geom_line(aes(y = concentration_ueq, group = source, color = source))+ 
      labs(colour = "Source", x = "Year", y = "Ca (ueq/L)")+
      xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
      ggtitle("Calcium's acid rain peak in 1970 and subsequent decline")+
      theme(plot.background = element_rect(fill = 'gray', colour = 'gray'))+
      theme(panel.background = element_rect(fill = 'black'))
    
    my_ggCa <- CaTime +
      geom_point_interactive(aes(y = concentration_ueq, col = source, tooltip = date, data_id = date), size = 6)+ 
      labs(colour = "Source", x = "Year", y = "Ca (ueq/L)")+
      xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
      theme(axis.title.x =  element_blank(),
            axis.text.x  =  element_text(angle=0, vjust=0.5, size=30), 
            axis.title.y = element_text(face="bold", size=35),
            axis.text.y  = element_text(angle=0.3, vjust=0.5, size=30),
            legend.title = element_text(size=40, face="bold"),
            legend.text = element_text(size = 30),
            plot.title = element_text(face="bold", size=55))
    
    ggiraph(code = print(my_ggCa), width = .8, width_svg = 45,
            height_svg = 15, hover_css = "cursor:pointer;fill:black;stroke:black;")
  })
  
})

