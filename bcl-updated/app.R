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
  ### Original Feature: Slide bar to filter the alcohol within the specified price range (changed price range & default value range)
      sliderInput("priceInput", h4("Price"), 0, 1000,
                  value = c(0, 300), pre = "$"),
  ### Feature 2: Added a slider for users to custom the number of bins in the alcohol content histogram, which
  ### basically also changes bar/bin width
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
  ### Feature 3: Allow users to search for multiple alcohol types at once instead of wine/beer/etc, one at a time,
  ### which improves flexibility where users can see multiple alcohol types at once in the alcohol histogram, scatterplot,
  ### and data table
      checkboxGroupInput("typeInput", label = h4("Type"),
                   choices = c("BEER", "REFRESHMENT",
                               "SPIRITS", "WINE"),
                   selected = "BEER")
    ),
    mainPanel(
  ### Feature 4: Created three separate tabs to place the histogram, scatterplot, and data table to improve aesthetics & organization
  ### (layout structure) for app users (user-friendly)
      tabsetPanel(
  ### Feature 5: Output to show a interactive histogram where hovering & clicking the cursor helps users locate the bin breaks (the x value range/alcohol content range
  ### for each bin) and find the frequency of each bin (number of alcohols for the specified alcohol content range)
        tabPanel("Alcohol Content Histogram", plotOutput("alcohol_hist", click = hoverOpts(id = "hist_hover")),
                 verbatimTextOutput('check')),
  ### Feature 6: Output to show an interactive scatterplot of the relationship between alcohol content and price where
  ### hovering the cursor tells users which x, y coordinates their cursor is on at the moment in the plot
        tabPanel("Scatterplot of Alcohol Content and Price", plotOutput("scatter", hover = hoverOpts(id = "plot_hover")),
                 verbatimTextOutput("plot_hoverinfo")),
  ### Feature 7: Turned the static BC Liquor Data Table into an interactive table where users can search &
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
  
  ### Feature 8: Show the number of results found in the data table whenever the filters change for users
  ### to get a gist of the total number of rows in the table
  output$no_of_results <- renderText({
    paste("We have found", nrow(filtered_data()), "results for you")
  })

  ### An interactive histogram based on alcohol content where the size (width) & number of bins would
  ### change according to users' specifications (corresponding to features 2 & 5)
  histData<-reactive({
    x    <- filtered_data()[ ,5]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    filtered_data() %>% 
      ggplot(aes(Alcohol_Content)) + 
	  geom_histogram(breaks = bins,fill='forestgreen',colour='gray30') + 
	  xlab("Alcohol Content") + 
	  ylab("Count")
  })
  
  
  ### Render the reactive interactive histogram from the filtered data which is specified by the users (corresponding to features 2 & 5)
  output$alcohol_hist <- renderPlot({
  histPlot <- histData()
  histPlot
  })
 
  ### Render the text message to display the x value range/alcohol content range when hovering & clicking the cursor and
  ### the frequency for each bin and display a default message of "Click green bar to show detail information"
  ### if outside the histogram bins (corresponding to feature 5) 
  output$check<-renderPrint({
   histPlot <- histData()
   binpos <- ggplot_build(histPlot)$data[[1]]
   wherex <- input$hist_hover$x
   wherey <- input$hist_hover$y
   out <- binpos[(binpos$xmin < wherex) & (binpos$xmax > wherex) & (binpos$y >= wherey),]
   out2 <- if(NROW(out)) paste(paste(out$xmin,out$xmax,sep="~"),'Count:',out$count) else('Click green bar to show detail information')
   out2
  })
  
  ### Scatterplot of alcohol content and price where different colors assigned to different alcohol types specified by users
  ### (corresponding to feature 6)
  output$scatter <- renderPlot({
    filtered_data() %>% 
      ggplot(aes(Alcohol_Content, Price)) + geom_point(aes(colour = Type),
                                                       alpha = 0.8,
                                                       size = 3) +
      xlab("Alcohol Content")
  })
  
  ### A filtered data table
  output$data_table <- renderDataTable({
    filtered_data()
  })
  
  ### Hovering to find x and y pixels of the cursor in the scatterplot and display
  ### the default message "Check your logs or contact app author for clarification" if outside
  ### the scatterplot (corresponding to feature 6)
  output$plot_hoverinfo <- renderPrint({

	if(is.null(input$plot_hover)){
	out<-"Check your logs or contact app author for clarification."
	} else {
    x <- round(input$plot_hover$x, 2)
    y <- round(input$plot_hover$y, 2)
    out<-paste0("Hover: \n","[", "x= ", x, ", ", "y= ", y, "]")
	}
	cat(out)
	
  })
  
  ### Feature 9: Allow the user to download the filtered data table as a .csv file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Filtered BC Liquor Table", ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    })
  
}

shinyApp(ui = ui, server = server)

