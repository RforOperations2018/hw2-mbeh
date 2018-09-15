# Homework 2
# By Min Yan BEH (mbeh)

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(RColorBrewer)

# ui configuration
sidebarWidth <- 300
plotlyDefaultFont <- list(
  family = "Arial",
  size = 18,
  color = "#000000"
)

# load dataset (to be filtered by reactive function later)
movies.load <- read.csv('movies.csv', stringsAsFactors=FALSE)
genres <- c('Action', 'Animation', 'Comedy', 'Crime', 'Drama', 'Family', 'Fantasy', 
            'History', 'Mystery', 'Romance', 'Science Fiction', 'Thriller', 'War')
# note: these genres do not represent all genres in the dataset, but are particularly popular ones
pdf(NULL)

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")