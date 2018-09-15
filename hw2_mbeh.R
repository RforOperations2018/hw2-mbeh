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

# Define header, sidebar and body of shinydashboard
header <- dashboardHeader(title = "The Ultimate Movie Collection", titleWidth = sidebarWidth)
sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenu(
    id = "tabs",
    # Sidebar Menu for Plots and DataTable
    menuItem("Visualizations", icon = icon("bar-chart"), tabName = "plots"),
    menuItem("Database", icon = icon("table"), tabName = "table"),
    # Range Slider for Movie Release Year
    sliderInput("yearSelect",
                "Year of Movie Release:",
                width = sidebarWidth - 60,
                min = min(movies.load$release_year, na.rm = T),
                max = max(movies.load$release_year, na.rm = T),
                value = c(min(movies.load$release_year, na.rm = T), max(movies.load$release_year, na.rm = T)),
                sep = '',
                step = 1),
    # Numeric Input for Minimum Movie Revenue
    numericInput("revenueMinimum", 
                 "Minimum Revenue in USD",
                 width = sidebarWidth - 60,
                 min = min(movies.load$revenue, na.rm = T),
                 max = max(movies.load$revenue, na.rm = T),
                 value = min(movies.load$revenue, na.rm = T),
                 step = 100000),
    # Selection for Movie Genre
    selectInput("genreSelect",
                "Genre:",
                width = sidebarWidth - 50,
                choices = sort(unique(genres)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Action", "Animation", "Comedy", "Drama", "Fantasy", "Romance")),
    # Button for selecting all genres
    actionButton("selectAllGenres", "Select All Genres", icon = icon("hand-pointer-o"))
  )
)
body <- dashboardBody(
  # import custom css stylesheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  width = 400,
  tabItems(
    # Plots page, to be displayed when "Plots" is clicked on sidebar
    tabItem("plots",
            fluidRow(
              # Tab Panel for 3 plots
              tabBox(title = "",
                     width = 12,
                     tabPanel("Revenue vs Budget", plotlyOutput("plot_budget_and_revenue")),
                     tabPanel("Ratings by Genre", plotlyOutput("plot_ratings")),
                     tabPanel("Movie Duration", plotlyOutput("plot_durations")))
            )
    ),
    # DataTable page, to be displayed when "Table" is clicked on sidebar
    tabItem("table",
            fluidPage(
              downloadButton("downloadMovieData","Download Movie Data"),
              box(title = "Statistics on Movie Selection", DT::dataTableOutput("moviesTable"), width = 12))
    )
  )
)

# Define ui comprising of header, sidebar and body (defined above)
ui <- dashboardPage(header, sidebar, body, skin = "green")



# Define server logic
server <- function(input, output, session = session) {
  
  # Filtered movie data using reactive method
  movieData <- reactive({
    # Helper function for determining if a movie contains a genre within list of selected genres
    movieMatchesGenreInput <- function(genresString, selectedGenres){
      genresForEachRow = strsplit(genresString, ",")
      outcome = c()
      for(row in genresForEachRow){
        outcome <- c(outcome, length(intersect(row, selectedGenres)) > 0)
      }
      return(outcome)
    }
    # Slider Filter for Release Year of Movie
    movies <- movies.load %>% filter(release_year >= input$yearSelect[1] & release_year <= input$yearSelect[2],
                                     # Numeric Filter for Minimum Revenue
                                     revenue >= input$revenueMinimum)
    
    # Selection Filter for Movie Genre
    if (length(input$genreSelect) > 0){
      movies <- filter(movies, movieMatchesGenreInput(genres, input$genreSelect))
    }
    return(movies)
  })
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")