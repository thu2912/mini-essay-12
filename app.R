#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


data <- read.csv("~/mini-essay-12/Auschwitz_Death_Certificates_1942-1943 - Auschwitz.csv")

library(dplyr)
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Holocaust data"),
    
    selectInput("Religion", "Religion",
                c("All", "Jew", "Protestant", "Catholic", "andere",
                  "Believes in God", "Greek Catholic", "Atheist", "Greek Orthodox",
                  "Unknown","Eastern Orthodox","Russian Orthodox","Jehovah's Witness",
                  "Czech-Moravian","Buddhist","Hussite","Unaffiliated", "Muslim",
                  "Agnostic")),

    # Bar Chart
    plotOutput("birthplaceBar"), 
    
    #Table
    dataTableOutput("table")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$birthplaceBar <- renderPlot({
    
    
    if (input$Religion != "All") {
      data <- filter(data, Religion == input$Religion)}
    
    birth_place <- group_by(data, Birthplace) %>% 
      summarise(numDeaths = n()) %>% 
      arrange(desc(numDeaths)) %>% 
      top_n(20)
    
    
    # Bar chart
    ggplot(birth_place , aes(reorder(Birthplace,numDeaths))) +
      geom_bar(aes(weight = numDeaths), fill = "tomato3") + 
      coord_flip() +
      #ggtitle("Top 20 Ramen Brands") +
      xlab("Birthplace") +
      ylab("No. Deaths") +
      theme_bw(base_size = 16)
   
  })
  
  output$table <- renderDataTable({
    
    # Filter data based on selected Style
    if (input$Religion != "All") {
      data <- filter(data, Religion == input$Religion)
    }
    data[0:7]
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)



