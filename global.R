
library(tidyverse)
library(readxl)
library(lpSolveAPI)
library(shiny)
library(shinydashboard)

library(RCurl)

# the aim of this script is to find the optimal repartition of 
# vegetable in a market gardenning setup, based on different 
# targets. 

### PARAMETERS
# Vegetable data
veg <- read_xlsx("www/vegetable_data.xlsx")
veg_min <- length(unique(veg$vegetable))
# Total surface, in m2
surf_tot <- 500
# min surface for one parcel, in m2
surf_min <- 10
# Max cost
cost_max <- 1000

#CALENDRIER TEST
calendrier <- read_xlsx("www/TestCalendrier.xlsx")