library(shiny)
library(tidyverse)
library(rsconnect)

#install.packages("DT")
library("DT")

bcl <- read_csv("bcl-data.csv")

ui <- fluidPage(
  ### Feature 1: Added a BC Liquor Store photo in the main panel for aesthetics
  img(src = "bcliquorstore.jpeg", width = 150, height = 100, align = "right"),
  titlePanel("BC Liquor Store Data"),
  h3("Basically everything you can find about the data here!"),
  br(),
  sidebarLayout(
    sidebarPanel(
  ### Original Feature: Slide bar to filter the alcohol within the specified price range
      sliderInput("priceInput", h4("Price"), 0, 100,
                  value = c(25,40), pre = "$"),
  ### Feature 2: Allow users to search for multiple alcohol types at once instead of wine/beer/etc, one at a time,
  ### which improves flexibility where users can see multiple alcohol types at once in both the alcohol histogram
  ### and data table
      checkboxGroupInput("typeInput", label = h4("Type"),
                   choices = c("BEER", "REFRESHMENT",
                               "SPIRITS", "WINE"),
                   selected = "BEER")
    ),
    mainPanel(
  ### Feature 3: Created two separate tabs to place the histogram and data table to improve aesthetics & organization
  ### (layout structure) for app users (user-friendly)
      tabsetPanel(
  ### Feature 4: output to show a interactive plot where the users can find x and y pixels 
  ### on the histogram when hovering their cursor
        tabPanel("Alcohol Content Histogram", plotOutput("alcohol_hist", hover = hoverOpts(id = "plot_hover")),
                 verbatimTextOutput("plot_hoverinfo")),
  ### Feature 5: Turned the static BC Liquor Data Table into an interactive table where users can search &
  ### filter based on key words, sort based on ascending or descending order, highlight entries, etc...
        tabPanel("BC Liquor Data Table", br(), h4(textOutput("no_of_results")), br(), dataTableOutput("data_table"),
                 downloadButton("downloadData", "Download this data!"))
      )
    )
  ),
  ### Original Feature: Website link to the original data
  a(href = "https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv",
    "Link to the original data set")
)

server <- function(input, output) {
  
  filtered_data <-
  ### extract or filter data based on price range and alcohol type
    reactive({
      bcl %>% filter(Price > input$priceInput[1] &
                       Price < input$priceInput[2] &
                       Type %in% input$typeInput)
    })
  
  ### Feature 6: Show the number of results found in the data table whenever the filters change for users
  ### to get a gist of the total number of rows in the table
  output$no_of_results <- renderText({
    paste("We have found", nrow(filtered_data()), "results for you")
  })

  ### A histogram plot of based on alcohol content
  output$alcohol_hist <- renderPlot({
    filtered_data() %>% 
      ggplot(aes(Alcohol_Content)) + geom_histogram() + xlab("Alcohol Content") + ylab("count")
  })
  
  ### A filtered data table
  output$data_table <- renderDataTable({
    filtered_data()
  })
  
  ### Function for feature 4 (hovering to find x and y pixels of the cursor in the histogram)
  output$plot_hoverinfo <- renderPrint({
    cat("Hover: \n")
    x <- round(input$plot_hover$x, 2)
    y <- round(input$plot_hover$y, 2)
    cat("[", "x= ", x, ", ", "y= ", y, "]", sep = "")
  })
  
  ### Feature 7: Allow the user to download the filtered data table as a .csv file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Filtered BC Liquor Table", ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    })
  
}

shinyApp(ui = ui, server = server)

