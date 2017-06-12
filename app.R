library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)

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

#?put precip and flow into one column "source" in order to group.by

#?put each compound into one row
  #w6chem <- cbind(w6chem$Ca.flow, w6chem$Ca.precip)

#split date then aggregate SO4 by year to simplify/clarify the graphs
as.character(w6chem$date)
w6chemsplit <- separate(w6chem, col = date, into = c("year", "month", "day"), sep = "/")
SO4.precip.year <- group_by(w6chemsplit, year)%>%
  summarise_each(funs(sum), SO4.precip)%>%
  as.data.frame
SO4.flow.year <- group_by(w6chemsplit, year)%>%
  summarise_each(funs(sum), SO4.flow)%>%
  as.data.frame
SO4.year <- merge(SO4.precip.year, SO4.flow.year, by = "year")
#add on /m/d to get line graph
SO4.year$year <- as.character(SO4.year$year)
SO4.year['year'] <- apply(SO4.year[, 'year', drop = F], 2, function(x){paste0(x, '/06/01')})
as.Date(SO4.year$year)
#SO4.year <- SO4.year[-c(1),]

#make w6chem a date again
w6chem$date <- as.Date(w6chem$date)

#shiny app interface (ui)
ui <- fluidPage(
  titlePanel(h1(strong("Acid Rain & HBEF"))),
    sidebarLayout(
      sidebarPanel(
      radioButtons("rbP1", label = "Choose a first precipitation compound",
                   choices = c("SO4" = "SO4.precip", "Mg" = "Mg.precip", "K" = "K.precip", 
                               "Na" = "Na.precip", "Al" = "Al.precip", "NH4" = "NH4.precip", 
                               "NO3" = "NO3.precip", "Cl" = "Cl.precip", "PO4" = "PO4.precip", 
                               "SiO2" = "SiO2.precip", "Ca" = "Ca.precip", "pH" = "pH.precip")),
      radioButtons("rbQ1", label = "Choose a first discharge compound",
                   choices = c("SO4" = "SO4.flow", "Mg" = "Mg.flow", "K" = "K.flow", 
                               "Na" = "Na.flow", "Al" = "Al.flow", "NH4" = "NH4.flow",
                               "NO3" = "NO3.flow", "Cl" = "Cl.flow", "PO4" = "PO4.flow", 
                               "SiO2" = "SiO2.flow", "Ca" = "Ca.flow", "pH" = "pH.flow")),
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
                               "SiO2" = "SiO2.flow", "SO4" = "SO4.flow"))
      ),
      mainPanel(
      plotOutput("cmpd1"), plotOutput("cmpd2"),
      plotOutput("static")
      )
  )
)

#shiny app commands (server)
server <- function(input, output) {
  #?make if statement based on rb input in order to graph P and Q
    #plot of P and Q from one compound
    output$cmpd1 <- renderPlot({
    ggplot(w6chem, aes(x = as.Date(date)))+
      geom_line(aes(y = get(input$rbP1), col = "Precip"))+ #maybe make Ca, Na, K, etc. columns&
      geom_line(aes(y = get(input$rbQ1), col = "Discharge"))+ # group.by source within the cmpd?
      labs(colour = "Source", x = "Year", y = "First compound (ueq/L)")
  })
    #plot of P and Q from another compound
    output$cmpd2 <- renderPlot({
    ggplot(w6chem, aes(x = as.Date(date)))+
      geom_line(aes(y = get(input$rbP2), col = "Precip"))+
      geom_line(aes(y = get(input$rbQ2), col = "Discharge"))+
      labs(colour = "Source", x = "Year", y = "Second compound (ueq/L)")
  })
    output$static <- renderPlot({
      ggplot(SO4.year, aes(x = as.Date(year)))+
        geom_smooth(aes(y = SO4.flow, col = "discharge"), se = F)+
        geom_smooth(aes(y = SO4.precip, col = "precipitation"), se = F)+
        labs(colour = "Source", x = "Year", y = "SO4 (ueq/L)")+
        theme_light()+
        ggtitle("SO4 concentration over time")
    })
}

#call shiny app
shinyApp(ui = ui, server = server)