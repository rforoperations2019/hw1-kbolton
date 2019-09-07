library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(tidyr)
worldHappiness <- read.csv("worldhappinessdata.csv")
wH2 <- worldHappiness[!is.na(worldHappiness$GINI.index..World.Bank.estimate...average.2000.13),]
Hdistribution <- read.csv("happinessdistributions.csv", stringsAsFactors = F)
Hvars <- read.csv("happinessvariables.csv", stringsAsFactors = F)
Hvars[1:10,1] <- colnames(wH2)[c(4:12,19)]
H2015 <- filter(wH2, year==2015)
annualScores <- spread(wH2[,2:4], "country", "Life.Ladder")
colnames(annualScores)[2:136] <- make.names(colnames(annualScores)[2:136])

# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Application title -----------------------------------------------
  titlePanel("World Happiness Statistics"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select region for distribution
      selectInput(inputId = "region",
                  label = "Region",
                  choices = c("World" = "World", 
                              "North America and ANZ" = "Northern.America...ANZ", 
                              "Latin America and Caribbean" = "Latin.America...Caribbean", 
                              "Western Europe" = "Western.Europe", 
                              "Cntral and Eastern Europe" = "Central.and.Eastern.Europe",
                              "East Asia" = "East.Asia",
                              "South Asia" = "South.Asia",
                              "Middle East and North Africa" = "Middle.East...North.Africa",
                              "Sub Saharan Africa" = "Sub.Saharan.Africa"),
                  selected = "World"),
      
      # Select country for happiness over time
      selectInput(inputId = "deltaH",
                  label = "Country",
                  choices = colnames(annualScores)[2:136],
                  selected = "United.States"),
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "Yvar", 
                  label = "Y-axis:",
                  choices = c("Happiness score" = "Life.Ladder", 
                              "Log GDP per capita" = "Log.GDP.per.capita", 
                              "Social support" = "Social.support", 
                              "Healthy life expectancy at birth" = "Healthy.life.expectancy.at.birth", 
                              "Freedom to make life choices" = "Freedom.to.make.life.choices",
                              "Generosity" = "Generosity",
                              "Perceptions of corruption" = "Perceptions.of.corruption",
                              "Positive affect" = "Positive.affect",
                              "Negative affect" = "Negative.affect",
                              "Average GINI index 2000-2013" = "GINI.index..World.Bank.estimate...average.2000.13"), 
                  selected = "Life.Ladder"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "Xvar", 
                  label = "X-axis:",
                  choices = c("Happiness score" = "Life.Ladder", 
                              "Log GDP per capita" = "Log.GDP.per.capita", 
                              "Social support" = "Social.support", 
                              "Healthy life expectancy at birth" = "Healthy.life.expectancy.at.birth", 
                              "Freedom to make life choices" = "Freedom.to.make.life.choices",
                              "Generosity" = "Generosity",
                              "Perceptions of corruption" = "Perceptions.of.corruption",
                              "Positive affect" = "Positive.affect",
                              "Negative affect" = "Negative.affect",
                              "Average GINI index 2000-2013" = "GINI.index..World.Bank.estimate...average.2000.13"), 
                  selected = "Log.GDP.per.capita"),
      
      # Dowload button ---------------------------------------------
      downloadButton('downloadData', 'Download data'),
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE)
    ),
    
    # Output --------------------------------------------------------
    mainPanel(
      
      h2("Distribution of happiness scores by region"),
      
      # Show distribution --------------------------------------------
      plotOutput(outputId = "distribution"),
      
      h2("Happiness over time by country"),
      
      # Show time series
      plotOutput(outputId = "time"),
      
      h2("Scatterplot of relevant national metrics"),
      
      h4("Description of Y axis"),
      
      # Show Y description --------------------------------------------
      textOutput(outputId = "Ydescrip"),
      
      h4("Description of X axis"),
      
      # Show X description --------------------------------------------
      textOutput(outputId = "Xdescrip"),
      
      # Show scatterplot --------------------------------------------
      plotOutput(outputId = "scatterplot"),
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "happinesstable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  # Describe Y variable
  output$Ydescrip <- renderText(print(Hvars[(which(Hvars == input$Yvar)),2]))
  
  # Describe X variable
  output$Xdescrip <- renderText(print(Hvars[(which(Hvars == input$Xvar)),2]))
  
  # Create happiness distribution figure
  output$distribution <- renderPlot({
    ggplot(data = Hdistribution, aes_string(x = "ladder", y = input$region)) +
      geom_col(fill="orange") +
      labs(x = "Happiness Score", y="", title = "World") +
      scale_x_continuous(breaks=0:10, labels=0:10) +
      theme_classic()
  })
  
  # Create line graph
  output$time <- renderPlot({
    ggplot(data=annualScores, aes_string(x="year", y=input$deltaH)) +
      geom_line(linetype = "dashed") +
      geom_point() +
      scale_y_continuous(breaks=1:10, limits = c(1,10)) +
      scale_x_continuous(breaks=2005:2016, limits = c(2005,2016)) +
      labs(y="Happiness score", title=input$deltaH) +
      theme_bw()
  })
  
  # Log countries looked at
  observe({
    if(!is.na(input$deltaH)){
      sink("countrieslog.txt", append=T)
      cat(input$deltaH)
      cat("\n")
      sink()}
  })
  
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = H2015, aes_string(x = input$Xvar, y = input$Yvar)) +
      geom_point() +
      labs(x = str_replace_all(input$Xvar, "\\.", " "),
           y = str_replace_all(input$Yvar, "\\.", " "))
  })
  
  # Print data table if checked -------------------------------------
  output$happinesstable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = H2015[,c(2,4:12,19)], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("2015-happiness-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(H2015, file)
    })
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)