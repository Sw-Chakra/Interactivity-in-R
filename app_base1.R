library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

sbp <- sidebarPanel(
  h4('Had a long day? This app will help you find the right drink for tonight!', style = 'color:grey'),
  sliderInput(
    "priceInput", "Price", 
    min = 0, max = 100, value = c(25, 40), pre = "$"
  ),
  
  uiOutput(
    'typeOutput'
  ),
  checkboxInput(
    'countryfilter',
    'Filter by country',
    value = TRUE
  ),
  conditionalPanel(
    condition = 'input.countryfilter == true',
    uiOutput(
      "countryOutput"
    )
  ),
  
  fluidRow(
    column(1,downloadButton("my_download_button", label = "Download"))
    
  ),
  
  br(),br()
)

mp <- mainPanel(
  h4(textOutput('text'), style = 'text-align:center'),
  br(),
  plotlyOutput("coolplot"),
  br(), br(),
  dataTableOutput("results")
)

## Generate UI
ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(sbp, mp)
)

#### server

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

bcl <- read.csv("../bcl-data.csv", stringsAsFactors = FALSE)

make_plot <- function(dat, x, fill) {
  ggplot(dat, aes_string(x = x,fill = fill)) +
    geom_histogram(alpha = 0.5, bins = 20) +
    labs(x = 'Alcohol Content (%)', y = 'Count') +
    theme(axis.title = element_text(size = 14),
          legend.title = element_text(size = 14)) + scale_x_continuous(labels = function(x) paste0(x*100,"%"))
}

make_data <- function(price, type, country) {
  filter(bcl,
         Price >= price[1],
         Price <= price[2],
         Type %in% type,
         Country %in% country
  ) %>% mutate(Alcohol_Content = Alcohol_Content/100)
}

server <- function(input, output, session) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country:",
                choices = sort(unique(bcl$Country)),
                multiple = TRUE,
                selected = c('CANADA', 'FRANCE', 'NETHERLANDS')
    )
  })
  
  ## add a 'renderUI` for Country
  
  ## add a 'renderUI` for Type
  output$typeOutput <- renderUI({
    selectInput('typeInput', 'Product type:',
                selected = c('BEER', 'WINE'),
                choices = sort(unique(bcl$Type)), multiple = TRUE)
  })
  ## add a 'reactive({...})` and call is `filtered`
  
  filtered <- reactive({
    req(input$priceInput)
    req(input$typeInput)
    req(input$countryInput)
    if(input$countryfilter == TRUE){
      make_data(input$priceInput, input$typeInput, input$countryInput)
    } else {
      make_data(input$priceInput, input$typeInput, unique(bcl$Country))
    }
  })
  
  output$coolplot <- renderPlotly({
    # make_data(input$priceInput, input$typeInput, input$countryInput) %>%
    filtered() %>% 
      make_plot("Alcohol_Content", fill='Type') %>% 
      ggplotly(tooltip = c("count","Alcohol_Content","fill"))
  })
  
  output$results <- renderDataTable({
    filtered()%>% mutate(Alcohol_Content = paste0(format(round(Alcohol_Content*100, 2), nsmall = 2), "%"))
  },
  filter = 'top',
  rownames = FALSE,
  
  options = list(
    lengthMenu = list(c(5, 10, 25, 50, -1), c('5', '10','25', '50', 'All')),
    pageLength = 10,
    columnDefs = list(list(className = 'dt-center',
                           targets = 0:6))))
  
  ## download:
  output$my_download_button <- downloadHandler(
    filename = function() { "bcl-results.csv" },
    content = function(file) {
      readr::write_csv(filtered(), path = file)
    }
  )
  
  output$text <- renderText({
    paste('We found', nrow(filtered()), 'options for you')
  })
}
shinyApp(ui = ui, server = server)
