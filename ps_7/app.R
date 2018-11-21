#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(plotly)


#Reading in rds file shiny_data.
dataset <- read_rds("shiny_data.rds")


# Define UI for application
ui <- fluidPage(theme = shinytheme("superhero"),
   
   # Application title
   titlePanel("How Key Demographics Affect Forecasts of Republican Advantage"),
   
   # Sidebar with a select input for demographic variable.  
   sidebarLayout(
      sidebarPanel(
         selectInput("demographic",
                     "Demographic:",
                     choices = dataset$demographic), 
         
         #Add checkbox for the user to decide whether or not to add linear model. Note: This line of code is from Ms. Gayton's code.
         checkboxInput("line", label = "Add Linear Model") 
         
       
      ),
      
      # Add tabs. Show a plot of the generated scatter plots in the "Plot" tab. Show summary text in "summary" tab. 
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Summary", 
                             h1("Summary"),
                             p("This Shiny App was created  using a collection of 2018 Midterm Election polling data from The Upshot. Additionally, thank you to Mr.
Schroeder for the results dataset. The plots in this shiny app illustrate the relationship between the sample demographic in a polling district and the Upshot's predicted Republican Advantage."), 
                             p("The analysis shows that there is a negative relationship between the percentage of young voters (18 to 29) in the sample and the forecasted Republican Advantage. A higher percentage of young voters in a polling district is 
associated with a smaller forecasted Republican Advantage. Given that a large portion of voters between the ages of 18 and 29 are Democratic, this is not suprising."), 
                             p("There also is a negative relationship between the percentage of college graduates in a polling district sample and the forecasted Republican Advantage. Given political trends, this is also not suprising."),
                             p("There is a positive relationship between the percentage of female respondents in a polling district sample and the predicted republican advantage, meaning that a larger percentage of female 
respondents in a polling district is associated with a higher predicted Republican Advantage."), 
                             p("Interestingly, there is a positive relationship between the percentage of Black voters in a polling district sample and the predicted Republican Advantage. The relationship between the percentage of hispanic voters in the sample 
and the predicted Republican Advantage is near zero.")
                             ),
                    tabPanel("Plot", plotOutput("demographics")),
                    tabPanel("Data Table", dataTableOutput("datatable")))
         
      )
   )
)

# Define server logic
server <- function(input, output, session) {
  
  #Note: Figured out how to add linear model by looking at Ms. Gayton's Code.
   output$demographics <- renderPlot({
     
     #Creates a filtered dataset based on the demographic that is chose in the corresponding selector in the sidepanel. 
     filtered <- reactive({
       df<- dataset %>% filter(demographic == input$demographic) 
     })

     #Creates scatterplot of demographic percentages and the forecasted republican advantage. I use the ifelse commands to show a linear model only if the corresponding checkbox is selected. 
     if (input$line == FALSE) {
       ggplot(filtered(), aes(x = value, y = forecast)) + geom_point() + xlab("Demographic (Percent)") + ylab("Forecasted Republican Advantage") + 
       ggtitle("Respondent Demographics and Forecasted Republican Advantage")

       }
     else {
       ggplot(filtered(), aes(x=value, y=forecast)) + geom_point() + geom_smooth(inherit.aes = FALSE, aes_string(x =filtered()$value, y =filtered()$forecast), method = "lm", se = FALSE) +
       xlab("Demographic (Percent)") + ylab("Forecasted Republican Advantage") + ggtitle("Respondent Demographics and Forecasted Republican Advantage")
     }
     
   })
   
   #Creates datatable to be show in the Data tab. Use filtered(), which is reactive to user input. 
   output$datatable <- renderDataTable({
     filtered <- reactive({
       df<- dataset %>% filter(demographic == input$demographic) 
     })
     filtered()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

