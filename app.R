library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(ggiraph)

w6precip <- read_csv("D:/Duke/Work(Environ)/Data/w6_precip_chem.txt")
w6stream <- read_csv("D:/Duke/Work(Environ)/Data/w6_stream_chem.txt")
public_data <- load("D:/Duke/Work(Environ)/Data/public_data.Rdata")

#rename columns to have ".flow" and ".precip"
#OPTIMIZE
w6stream <- plyr::rename(w6stream, c("Ca" = "Ca.flow",
                                     "Mg" = "Mg.flow",
                                     "K" = "K.flow",
                                     "Na" = "Na.flow",
                                     "Al" = "Al.flow",
                                     "NH4" = "NH4.flow",
                                     "pH" = "pH.flow",
                                     "SO4" = "SO4.flow",
                                     "NO3" = "NO3.flow",
                                     "Cl" = "Cl.flow",
                                     "PO4" = "PO4.flow",
                                     "SiO2" = "SiO2.flow"))
w6precip <- plyr::rename(w6precip, c("Ca" = "Ca.precip",
                                     "Mg" = "Mg.precip",
                                     "K" = "K.precip",
                                     "Na" = "Na.precip",
                                     "Al" = "Al.precip",
                                     "NH4" = "NH4.precip",
                                     "pH" = "pH.precip",
                                     "SO4" = "SO4.precip",
                                     "NO3" = "NO3.precip",
                                     "Cl" = "Cl.precip",
                                     "PO4" = "PO4.precip",
                                     "SiO2" = "SiO2.precip"))

#merge precip and stream df
w6chem <- merge(w6stream, w6precip, by = c("ws", "year", "mo"))

#unite year:mo
w6chem <- unite(w6chem, date, year:mo, sep = '/')
#make into date format
w6chem$date <- as.character(w6chem$date)
w6chem['date'] <- apply(w6chem[, 'date', drop=F], 2, function(x){paste0(x, "/01")})

#make all -3 values NA
w6chem[w6chem == -3] <- NA

#convert compounds to be in ueq/L #########make a function or loop for this?
conc_mg_to_ueqConverter <- function(initial, z, MW){
  ueqValue <- initial*1000*z/MW
}
#calculation: conc_mg * 1000*z/MW
w6chem$Ca.flow <- w6chem$Ca.flow * 1000 * 2 / 40.078
w6chem$Mg.flow <- w6chem$Mg.flow * 1000 * 2 / 24.305
w6chem$K.flow <- w6chem$K.flow * 1000 * 1 / 39.098
w6chem$Na.flow <- w6chem$Na.flow * 1000 * 1 / 22.990
w6chem$Al.flow <- w6chem$Al.flow * 1000 * 3 / 26.982 #figure out Al charge
w6chem$NH4.flow <- w6chem$NH4.flow * 1000 * 1 / 18.040
w6chem$SO4.flow <- w6chem$SO4.flow * 1000 * 2 / 96.060 #negative charge
w6chem$NO3.flow <- w6chem$NO3.flow * 1000 * 1 / 62.004 #negative charge
w6chem$Cl.flow <- w6chem$Cl.flow * 1000 * 1 / 35.450 #negative charge
w6chem$PO4.flow <- w6chem$PO4.flow * 1000 * 3 / 94.971 #negative charge
w6chem$SiO2.flow <- w6chem$SiO2.flow * 1000 * 1 / 60.080 #Net charge?

w6chem$Ca.precip <- w6chem$Ca.precip * 1000 * 2 / 40.078
w6chem$Mg.precip <- w6chem$Mg.precip * 1000 * 2 / 24.305
w6chem$K.precip <- w6chem$K.precip * 1000 * 1 / 39.098
w6chem$Na.precip <- w6chem$Na.precip * 1000 * 1 / 22.990
w6chem$Al.precip <- w6chem$Al.precip * 1000 * 3 / 26.982 #figure out Al charge
w6chem$NH4.precip <- w6chem$NH4.precip * 1000 * 1 / 18.040
w6chem$SO4.precip <- w6chem$SO4.precip * 1000 * 2 / 96.060 #negative charge
w6chem$NO3.precip <- w6chem$NO3.precip * 1000 * 1 / 62.004 #negative charge
w6chem$Cl.precip <- w6chem$Cl.precip * 1000 * 1 / 35.450 #negative charge
w6chem$PO4.precip <- w6chem$PO4.precip * 1000 * 3 / 94.971 #negative charge
w6chem$SiO2.precip <- w6chem$SiO2.precip * 1000 * 1 / 60.080 #Net charge?

#split date 
as.character(w6chem$date)
w6chemsplit <- separate(w6chem, col = date, into = c("year", "month", "day"), sep = "/")
#add year column for later granularity interactive manipulation
w6chem$year <- w6chemsplit$year
#move the pH values to end of the table so they don't get aggregated
w6chem <- w6chem[, c(1:9, 11:22, 24:28, 10, 23, 29)]
#aggregate by year column to get yearly averages
w6chemyear <- group_by(w6chem, year) %>%
  summarise_each(funs(sum), flow:SiO2.precip)#this works great..
#format new year column as date in both w6chemyear AND in w6chem
w6chemyear$year <- as.character(w6chemyear$year)
w6chemyear['year'] <- apply(w6chemyear[, 'year', drop = F], 2, function(x){paste0(x, '/06/01')})
as.Date(w6chemyear$year)

w6chem$year <- as.character(w6chem$year)
w6chem['year'] <- apply(w6chem[, 'year', drop = F], 2, function(x){paste0(x, '/06/01')})
as.Date(w6chem$year)
#rename all vars to have 'yearly' before merging
w6chemyear <- plyr::rename(w6chemyear, c("Ca.precip" = "Ca.precipyear",
                            "Mg.precip" = "Mg.precipyear",
                            "K.precip" = "K.precipyear",
                            "Na.precip" = "Na.precipyear",
                            "Al.precip" = "Al.precipyear",
                            "NH4.precip" = "NH4.precipyear",
                            "SO4.precip" = "SO4.precipyear",
                            "NO3.precip" = "NO3.precipyear",
                            "Cl.precip" = "Cl.precipyear",
                            "PO4.precip" = "PO4.precipyear",
                            "SiO2.precip" = "SiO2.precipyear",
                            "Ca.flow" = "Ca.flowyear",
                            "Mg.flow" = "Mg.flowyear",
                            "K.flow" = "K.flowyear",
                            "Na.flow" = "Na.flowyear",
                            "Al.flow" = "Al.flowyear",
                            "NH4.flow" = "NH4.flowyear",
                            "SO4.flow" = "SO4.flowyear",
                            "NO3.flow" = "NO3.flowyear",
                            "Cl.flow" = "Cl.flowyear",
                            "PO4.flow" = "PO4.flowyear",
                            "SiO2.flow" = "SiO2.flowyear",
                            "flow" = "flowyear",
                            "precip" = "precipyear"))
#now add these columns to w6chem
w6chemDandY <- merge(w6chem, w6chemyear, by = "year")

#make w6chem a date again
w6chem$date <- as.Date(w6chem$date)

#?put precip and flow into one column "source" in order to group.by

#?put each compound into one row
#w6chem <- cbind(w6chem$Ca.flow, w6chem$Ca.precip)


#shiny app interface (ui)
ui <- fluidPage(
  titlePanel(h1(strong("Acid Rain & HBEF"))),
        fluidRow( column(4,
               #make an if-statement to have the radio buttons use yearly vs monthly data!!

        #radioButtons to select compounds - try to pair up P&Q on same graph
      radioButtons("rbP1", label = "Choose a first precipitation compound",
                   choices = c("SO4" = "SO4.precip", "Mg" = "Mg.precip", "K" = "K.precip", 
                               "Na" = "Na.precip", "Al" = "Al.precip", "NH4" = "NH4.precip", 
                               "NO3" = "NO3.precip", "Cl" = "Cl.precip", "PO4" = "PO4.precip", 
                               "SiO2" = "SiO2.precip", "Ca" = "Ca.precip", "pH" = "pH.precip")),
      radioButtons("rbQ1", label = "Choose a first discharge compound",
                   choices = c("SO4" = "SO4.flow", "Mg" = "Mg.flow", "K" = "K.flow", 
                               "Na" = "Na.flow", "Al" = "Al.flow", "NH4" = "NH4.flow",
                               "NO3" = "NO3.flow", "Cl" = "Cl.flow", "PO4" = "PO4.flow", 
                               "SiO2" = "SiO2.flow", "Ca" = "Ca.flow", "pH" = "pH.flow"))),
      column(4,
      radioButtons("rbP2", label = "Choose a second precipitation compound",
                   choices = c("pH" = "pH.precip", "Ca" = "Ca.precip", "Mg" = "Mg.precip", 
                               "K" = "K.precip", 
                               "Na" = "Na.precip", "Al" = "Al.precip", "NH4" = "NH4.precip", 
                               "NO3" = "NO3.precip", "Cl" = "Cl.precip", "PO4" = "PO4.precip", 
                               "SiO2" = "SiO2.precip", "SO4" = "SO4.precip")),
      radioButtons("rbQ2", label = "Choose a second discharge compound",
                   choices = c("pH" = "pH.flow", "Ca" = "Ca.flow", "Mg" = "Mg.flow", 
                               "K" = "K.flow", 
                               "Na" = "Na.flow", "Al" = "Al.flow", "NH4" = "NH4.flow",
                               "NO3" = "NO3.flow", "Cl" = "Cl.flow", "PO4" = "PO4.flow", 
                               "SiO2" = "SiO2.flow", "SO4" = "SO4.flow"))),
      column(4,
        #use slider to view data in specific time ranges
        sliderInput("dateSlide", label = "Input date range",
                    min = as.Date("1963/06/01"), 
                    max = as.Date("2013/06/01"),
                    value = c(as.Date("1963/06/01"), as.Date("2013/06/01")),
                    timeFormat="%b %Y"),
        #switch between monthly and yearly data
        selectInput("selDate", label = "Timescale granularity",
                    choices = c("Monthly" = "date", "Yearly" = "year"))),
fluidRow(
  column(12, plotOutput("cmpd1"))),
fluidRow(
  column(12, plotOutput("cmpd2"))),
fluidRow(
  column(12, ggiraphOutput("my_gg"))
)
)
)

#shiny app commands (server)
server <- function(input, output) {
  #ie if selDate = Monthly use Ca.precip for rbP1, else use Ca.precipyear for rbP1 ##FIX
#  output$ifButtons <- reactiveText({
 # if(input$selDate = "Monthly") {
  #  p("Monthly")
  #}else{
   # p("Yearly")
  #}
  #})
    #plot of P and Q from one compound
    output$cmpd1 <- renderPlot({
    ggplot(w6chem, aes(x = as.Date(date)))+
      geom_line(aes(y = get(input$rbP1), col = "Precip"))+ #maybe make Ca, Na, K, etc. columns&
      geom_line(aes(y = get(input$rbQ1), col = "Discharge"))+ # group.by source within the cmpd?
      labs(colour = "Source", x = "Year", y = "First compound (ueq/L)")+
      xlim(min(input$dateSlide[1]), max(input$dateSlide[2])) #use the date slider to change x axis
  })
    #plot of P and Q from another compound
    output$cmpd2 <- renderPlot({
    ggplot(w6chem, aes(x = as.Date(date)))+
      geom_line(aes(y = get(input$rbP2), col = "Precip"))+
      geom_line(aes(y = get(input$rbQ2), col = "Discharge"))+
      labs(colour = "Source", x = "Year", y = "Second compound (ueq/L)")+
      xlim(min(input$dateSlide[1]), max(input$dateSlide[2]))
  })
    output$my_gg <- renderggiraph({
      g <- ggplot(w6chem, aes(x = as.Date(date)))+geom_line(aes(y = get(input$rbP1), col = "Precip"))+geom_line(aes(y = get(input$rbQ1), col = "Q"))

      my_gg <- g +
        geom_point_interactive(aes(y = get(input$rbP1), col = "Precip", tooltip = date, data_id = date), size = 6)+ 
        geom_point_interactive(aes(y = get(input$rbQ1), col = "Q", tooltip = date, data_id = date), size = 7)+ 
        labs(colour = "Source", x = "Year", y = "First compound (ueq/L)")+
      xlim(min(input$dateSlide[1]), max(input$dateSlide[2])) #use the date slider to change x axis
      
      ggiraph(code = print(my_gg), width = .8, width_svg = 45,
              height_svg = 15, hover_css = "cursor:pointer;fill:black;stroke:black;")
      
    })
}

#call shiny app
shinyApp(ui = ui, server = server)

