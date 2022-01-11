library(shiny)
library(tidyverse)
library(forcats)
library(dslabs)
data(gapminder)
#define the user interface
ui <- fluidPage(
  #use a theme that matches the theme of the plot
  theme = shinythemes::shinytheme("united"),
  #create a meaningful title
  titlePanel("Shiny App to Explore Worldwide Demographic Trends"),
  #create different panels
  tabsetPanel(
    #put time series plot in one panel
    tabPanel("Life Expectancy",
             #use sidebar layout
             sidebarLayout(
               sidebarPanel(
                 #create a country selector widget to the left of the plot
                 selectInput("country", label = "Select a Country to Compare",
                             choices = as.list(levels(gapminder$country)))
               ),
               #put the line plot in the main panel
               mainPanel(
                 plotOutput(outputId = "line")
               )
             )
    ),
    #put the scatter plot in the second panel
    tabPanel("Fertility and Life Expectancy",
             #use sidebar layout
             sidebarLayout(
               sidebarPanel(
                 #create a slider widget to select a year
                 sliderInput("year", "Year:",
                             value = 1960,
                             min = 1960,
                             max = 2016,
                             step = 1,
                             sep = ""), 
                 br(), 
                 textInput("text", "What country would you like to highlight?")
               ),
               #put the scatter plot in the main panel
               mainPanel(
                 plotOutput(outputId = "scatter"),
                 textOutput("results")
               )
             )
    )
  )
)

#define the server
server <- function(input, output){
  #create a reactive expression to filter data for line plot
  data_line <- reactive({
    filtered <- gapminder %>%
      #only include the U.S. and the country selected
      filter(country %in% c("United States", input$country))
    filtered
  })
  
  #define what the line plot will look like
  output$line <- renderPlot({
    data_line() %>%
      #plot year on the x-axis, life expectancy on the y-axis
      ggplot(aes(x = year, y = life_expectancy)) +
      geom_line(aes(color = country)) + 
      #change where x axis ticks appear 
      scale_x_continuous(breaks = seq(1960, 2020, 10)) +
      #create meaningful labels for axes and title
      xlab("Year") +
      ylab("Life Expectancy") + 
      ggtitle(sprintf("Change in Life Expectancy in the United States and %s Over Time", input$country)) + 
      theme_bw()
  })
  
  #create a reactive expression to filter data for scatter plot
  data_scatter <- reactive({
    filtered <- gapminder %>%
      #use the year that the user selects
      filter(year == input$year)
    filtered
  })
  
  #define what the scatter plot will look like
  output$scatter <- renderPlot({
    #only include the year 2000
    data_scatter() %>%
      #plot fertility on the x-axis and life expectancy on the y-axis
      ggplot(aes(x = fertility, y = life_expectancy)) +
      #color by continent
      geom_point(aes(color = continent, alpha = ifelse (country == input$text, 1, 0.3)), size = 2) + 
      #turn off legend for alpha
      guides(alpha = "none") +
      #create meaningful labels for axes and title
      xlab("Fertility Rate") +
      ylab("Life Expectancy") + 
      labs(color = "Continent") +
      ggtitle("Relationship between Fertility Rate and Life Expectancy by Continent in 2000") +
      theme_bw()
  })
  output$results <- renderText({
    if (!input$text %in% gapminder$country){
      paste0("Sorry, you must have mispelled the country's name")
    }
    else {
      fertility <- gapminder$fertility[gapminder$country == input$text & gapminder$year == input$year]
      life_expectancy <- gapminder$life_expectancy [gapminder$country == input$text & gapminder$year == input$year]
      paste0(input$text, " has life expectancy of ", life_expectancy, " years and
             fertility rate of ", fertility)
    }
  })
}

#embed the Shiny App in the html (it won't be interactive)
shinyApp(ui = ui, server = server)
