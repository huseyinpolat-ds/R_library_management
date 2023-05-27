# Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(shinyjs)
library(tidyverse)

source("lib_sys.r")

source("ui.r")

source("server.r")

shinyApp(ui = ui, server = server)



