#efforts of making ggiraph - data took too long to load
output$my_gg1 <- renderPlot({
  ggplot(precip_stream_data, aes(x = as.Date(date)))+
    geom_line(aes(y = concentration_ueq, col = source, group = solute))+
    facet_wrap(~solute, scales = "free_y", ncol = 3)
#    my_gg1 <- g1 +
#    geom_point_interactive(aes(y = concentration_ueq, col = solute, tooltip = date, data_id = date), size = 6)+ 
#   labs(colour = "Source", x = "Year", y = "First compound (ueq/L)")+
#  xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))+ #use the date slider to change x axis
# theme(axis.title.x =  element_blank(),
#      axis.text.x  =  element_text(angle=0, vjust=0.5, size=30), 
#     axis.title.y = element_text(face="bold", size=35),
#    axis.text.y  = element_text(angle=0.3, vjust=0.5, size=30),
#   legend.title = element_text(size=40, face="bold"),
#  legend.text = element_text(size = 30),
# plot.title = element_text(face="bold", size=55)) + 
#      ggtitle("P vs Q for first elements")+
#     facet_wrap(~source)

#  ggiraph(code = print(my_gg1), width = .8, width_svg = 45,
#         height_svg = 15, hover_css = "cursor:pointer;fill:black;stroke:black;")

#radioButtons to select compound with clean data... hasn't been working
  
  #radioButtons to select source
  #not                    radioButtons("rbSource", label = "Select a source",
  #needed                                 choices = c("Precipitation")),
  
  #radioButtons to select compounds - try to pair up P&Q on same graph
  radioButtons("rb1", label = "Choose a first compound to graph",
               choices = c("SO4" = "SO4", "Mg" = "Mg", "K" = "K", 
                           "Na" = "Na", "Al" = "Al", "NH4" = "NH4", 
                           "NO3" = "NO3", "Cl" = "Cl", "PO4" = "PO4", 
                           "SiO2" = "SiO2", "Ca" = "Ca", "pH" = "pH"))),
column(4,
       radioButtons("rb2", label = "Choose a second compound",
                    choices = c("SO4" = "solute$SO4", "Mg" = "solute$Mg", "K" = "solute$K", 
                                "Na" = "solute$Na", "Al" = "solute$Al", "NH4" = "solute$NH4", 
                                "NO3" = "solute$NO3", "Cl" = "solute$Cl", "PO4" = "solute$PO4", 
                                "SiO2" = "solute$SiO2", "Ca" = "solute$Ca", "pH" = "solute$pH")))

#functioning ggiraph, but Camila wants to use plotly now
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


#trying to get wrapping to work, using a list thing
#https://stackoverflow.com/questions/33139247/ggplot2-save-individual-facet-wrap-facets-as-separate-plot-objects  
p.list = lapply(sort(unique(precip_stream_data$solute)), function(i) {
  ggplot(precip_stream_data[precip_stream_data$solute==i,], aes(x = as.Date(date))) +
    geom_line(aes(y = concentration_ueq, group = source, color = source))+ 
    facet_wrap(~solute)
})
p.list
ggplotly(p.list[[1]])

