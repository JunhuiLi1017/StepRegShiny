# StepRegShiny Standalone App
# This version works without requiring the StepRegShiny package to be installed

# Load required libraries
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(dplyr)
library(StepReg)
library(cowplot)
library(rmarkdown)
library(summarytools)
library(ggcorrplot)
library(tidyr)
library(stringr)
library(flextable)

# Source the standalone R files
source("ui_standalone.R")
source("server_standalone.R")
source("utils_standalone.R")

# Launch the app
shinyApp(ui = ui(), server = server)
