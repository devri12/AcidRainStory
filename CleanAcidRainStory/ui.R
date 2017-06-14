library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)

shinyUI(
  fluidPage(
  titlePanel(h1(strong("Acid Rain & HBEF"))),
        fluidRow(column(4,
               #make an if-statement to have the radio buttons use yearly vs monthly data!!

        #radioButtons to select compounds - try to pair up P&Q on same graph
      radioButtons("rb1", label = "Choose a first precipitation compound",
                   choices = c("SO4" = "solute$SO4", "Mg" = "solute$Mg", "K" = "solute$K", 
                               "Na" = "solute$Na", "Al" = "solute$Al", "NH4" = "solute$NH4", 
                               "NO3" = "solute$NO3", "Cl" = "solute$Cl", "PO4" = "solute$PO4", 
                               "SiO2" = "solute$SiO2", "Ca" = "solute$Ca", "pH" = "solute$pH"))),
      column(4,
      radioButtons("rb2", label = "Choose a second precipitation compound",
                   choices = c("SO4" = "solute$SO4", "Mg" = "solute$Mg", "K" = "solute$K", 
                               "Na" = "solute$Na", "Al" = "solute$Al", "NH4" = "solute$NH4", 
                               "NO3" = "solute$NO3", "Cl" = "solute$Cl", "PO4" = "solute$PO4", 
                               "SiO2" = "solute$SiO2", "Ca" = "solute$Ca", "pH" = "solute$pH"))),
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
  column(12, plotOutput("cmpd2"))
)
)
)
)