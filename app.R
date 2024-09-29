
# Dean Kolk

library(shiny)
library(readxl)
library(ggplot2)
library(shinythemes)
library(dplyr)
masters <- read_excel('masters.xlsx') 
masters2 <- read_excel('masters2.xlsx')
masters3 <- read_excel('masters3.xlsx')

ui <- fluidPage(
    theme = shinytheme("superhero"), # chooses color scheme


    titlePanel("Total Statisics for the Masters 2021"), # creates title
    h4("Dean Kolk"), # puts name below title


    sidebarLayout(
        sidebarPanel(
            # text input to be on the sidebar of the shiny app
 
            h4("This applet will allow you to explore the cumulative data of the
            Augusta National Golf Course at the Masters 
               tournament this year in 2021. This includes all players, including those who
               only played Tuesday & Thursday and missed the cut for the tournament."),
            h4("Use this as a filter to show only the Par 5's, 3's, or 4's etc."),
            # choose the par of the hole you want to see
            numericInput("par",
                               "Par:",
                         min = 3,
                         max = 5,
                         value = 5),
            # masters 2 data set shows score by hole
            h4("Select to show the total number of Birdies, Pars, or Bogeys on each hole."),
            varSelectInput("score","Choose one:",masters2),
            # masters 3 data set shows handicap by hole (difficulty)
            h4("Select to choose whether the data is shown by the order of the Hole number, 
               or the Rank of difficulty of the Hole."),
            varSelectInput("type","Choose one:",masters3)
   
        ),

     # creates references for outputs in the main panel
        mainPanel(
            dataTableOutput("mydata"),
            textOutput("text"),
           plotOutput("data")
        )
    )
)


server <- function(input, output) {
   # creates data table with statistics sorted by the par chose for the hole (par 3 par 4 par 5)
    output$mydata <- renderDataTable({
        par <- masters %>% filter(Par == input$par)
   
        
    })
    # title for the graph with information calculated based on inputs 
    output$text <- renderText({
        par <- masters %>% filter(Par == input$par)
        total <- par[input$score]
        totalscore <- colSums(total)
        paste("This graph shows that there were",totalscore,"total",input$score,"on all of the holes where
              par was", input$par)
    })
    
    # creates graph based on input
    output$data <- renderPlot({
        par <- masters %>% filter(Par == input$par)
        ggplot(par, aes(x=!!input$type,y=!!input$score)) + 
            geom_col(fill="palegreen4",color="yellow") +
            scale_x_continuous(n.breaks = 18) 
    })

    
 }




 
shinyApp(ui = ui, server = server)
