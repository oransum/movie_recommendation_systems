## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          #dashboardSidebar(disable = FALSE),
          dashboardSidebar(
              #sidebarUserPanel("User Name",
              #  subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
              #  #Image file should be in www/ subdir
              #  image = "userimage.png"
              #),
              #sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
              sidebarMenu(
                # Setting id makes input$tabs give the tabName of currently-selected tab
                id = "tabs",
                #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                #menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
                #         badgeColor = "green"),
                #menuItem("Recommendation Option", icon = icon("bar-chart-o"),
                 # menuSubItem("Recommend by Genre", tabName = "RecByGenre"),
                 # menuSubItem("Recommend by Rating", tabName = "RecByRating")
                #menuItem("Recommendation Option", tabName = "SelectMethod", icon = icon("dashboard")),
                menuItem("Instuction", tabName = "SelectMethod", icon = icon("th")),
                menuItem("Recommedation Type", icon = icon("bar-chart-o"),
                menuItem("Recommend by Genre", tabName = "RecByGenre", icon = icon("dashboard")),
                menuItem("Recommend by Rating", tabName = "RecByRating", icon = icon("dashboard"))
              )
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),              
              tabItems(
                tabItem("SelectMethod",
                  div(p("Please select Recommendation Type on the left."))
                ),
                tabItem("RecByGenre",
                  fluidRow(
                      box(width = 12, title = "Step 1: Select genres", status = "info", solidHeader = TRUE, collapsible = TRUE,
                          selectInput("genre", "Genre:", 
						    c("Action" = "Action", "Adventure" = "Adventure", "Animation" = "Animation", "Children's" = "Children's", "Comedy" = "Comedy", "Crime" = "Crime", "Documentary" = "Documentary", "Drama" = "Drama", "Fantasy" = "Fantasy", "Film-Noir" = "Film-Noir", "Horror" = "Horror", "Musical" = "Musical", "Mystery" = "Mystery", "Romance" = "Romance", "Sci-Fi" = "Sci-Fi", "Thriller" = "Thriller", "War" = "War", "Western" = "Western")),
					               )
                    ),
				          fluidRow(
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        uiOutput("sys1_results")
                      )
                   )
                ),
                tabItem("RecByRating",
                  fluidRow(
                      box(width = 12, title = "Step 1: Rate as many movie as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                          div(class = "rateitems",
                              uiOutput('ratings')
                          )
                      )
                    ),
                  fluidRow(
                      useShinyjs(),
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                          actionButton("btn", "Click here to get your movie recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results")
                      )
                   )
                )                
              )
          )
    )
) 