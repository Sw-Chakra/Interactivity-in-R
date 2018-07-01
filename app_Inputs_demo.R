library(shiny)
library(dplyr)
library(readr)

radio_value <- c("Option 1", "Option 2", "Option 3")
multiple_select_options <- list("Option A" = "A", "Option B" = "B", "Option C" = "C")
render_select <- list("Option D" = "D","Option E" = "E")
data_sets <- c("None"="","iris"="iris","mtcars"="mtcars")

## inputs to show in the sidebar panel
sbp <- sidebarPanel(
  
  ### slider input
  sliderInput(
    "input_slider", label = "My slider:", 
    min = 100, max = 500, value = 200
  ),
  
  ### double slider input
  sliderInput(
    "dbl_slider", label = "My double slider:", 
    min = -10, max = 50, value = c(1,20)),
  
  ### checkbox input
  checkboxInput("input_checkbox", "My checkbox"),
  
  ### radio button input
  radioButtons("radio_button", "My radio buttons",
               radio_value, inline = TRUE),
  
  ### single select input
  selectInput("single_select", "My select single",
              choices = list("Option A" = "A","Option B" = "B","Option C" = "C"),
              selected = "C"),
  
  ### multiple select input
  uiOutput("multiple_select"),
  
  
  ### render ui select
  uiOutput("render_select"),
  
  
  ### data select
  uiOutput("choose_dataset"),
  
  ### variables select
  conditionalPanel(condition = "input.dataset != ''",
                   uiOutput("choose_columns")),
  
  
  ### action button and download button
  
  fluidRow(
    column(5,actionButton("action_button", "My action button",icon("calendar"))),
    column(1,downloadButton("my_download_button", label = "My download button"))
    
  ),
  
  br(),br(),
  
  ### date range input
  dateRangeInput('dateRange',
                 label = 'My data range',
                 start = "2016-01-08", end = "2017-01-08"),
  
  ### text input
  textInput("text_input", "My text input",placeholder = "Enter a city name"),
  
  
  ###  text input 2
  textAreaInput("text_area", "My text area", placeholder = "Enter a country name", height = "150px"),
  width = 5)

### output to show in the main panel
mp <- mainPanel(
  h3(textOutput("input_slider_value")),
  h3(textOutput("dbl_slider_value")),
  h3(textOutput("input_checkbox_value")),
  h3(textOutput("radio_button_value")),
  h3(textOutput("single_select_value")),
  h3(textOutput("multiple_select_value")),
  h3(textOutput("render_select_value")),
  h3(textOutput("render_options")),
  h3(textOutput("data_select_value")),
  h3(textOutput("variables_select_value")),
  
  conditionalPanel(condition = "input.dataset != ''",
                   h3(textOutput("variable_options"))),
  
  h3(textOutput("my_action_button")),
  h3(textOutput("date_range_values")),
  h3(textOutput("text_input_value")),
  h3(textOutput("text_area_value")),
  ## there is no output in the mainPanel for the download button
  width = 7)

## Define UI for application 
## Note that sbp and mp are defined above

ui <- fluidPage(
  titlePanel("Shiny App Input Assignment"),
  sidebarLayout(sbp, mp)
)

### Define server logic 
server <- function(input, output, session) {
  
  ### slider output
  output$input_slider_value <- renderText({
    paste("The slider value is ", input$input_slider)
  })
  
  ### double slider output
  output$dbl_slider_value <-  renderText({
    paste("The slider values are", 
          paste(input$dbl_slider, collapse = " and "))
  })
  
  ### check box output
  output$input_checkbox_value <- renderText({
    paste("The checkbox value is ", input$input_checkbox)
  })
  
  ### radio button output
  output$radio_button_value <-  renderText({
    colChoice <- match(input$radio_button,radio_value)
    paste("The radio button value is", colChoice)
  })
  
  ### single select output
  output$single_select_value <- renderText({
    paste("The single select value is", input$single_select)
  })
  
  ### multiple select output
  
  output$multiple_select <- renderUI({
    selectInput("multi", "My multiple select", selected = multiple_select_options
                ,choices = multiple_select_options, multiple = TRUE)
  })
  
  output$multiple_select_value <- renderText({
    x <- input$multi
    paste("The select multiple has values", paste(as.character(x), collapse = ", "))
  })
  
  
  ### render ui
  
  output$render_select <- renderUI({
    
    ### Get the multiple selected values
    multi_selected <- input$multi
    render_input <- c(multiple_select_options[!(multiple_select_options %in% multi_selected)], render_select)
    
    ### Create the  render selectInput 
    selectInput("renderUI", "My renderUI select", 
                choices  = render_input, multiple = FALSE)
  })
  
  ### print the render selected values
  output$render_select_value <- renderText({
    paste("The renderUI select has value",paste(input$renderUI,collapse = ", "))
  })
  
  ### print render select options
  output$render_options <- renderText({
    
    if(is.null(input$multi))
      return()
    
    paste("The options available for the renderUI select are",
          paste(as.character(c(multiple_select_options[!(multiple_select_options %in% input$multi)],render_select)),collapse = ", "))
  })
  
  
  ### select data output
  
  output$choose_dataset <- renderUI({
    selectInput("dataset", "My data select", data_sets, multiple = FALSE)
  })
  
  output$data_select_value <- renderText({
    x <- input$dataset
    paste("The data select value is",dQuote(x))
  })
  
  ### select variable output
  
  output$choose_columns <- renderUI({

    # Get dataset
    dat <- get(input$dataset)
    colnames <- names(dat)
    
    selectInput("columns", "My variables select", 
                choices  = colnames, multiple = FALSE)
  })
  
  output$variables_select_value <- renderText({
    
    ifelse(input$dataset == "",
           "The variables select value is ''",
           paste("The variables select value is",paste(dQuote(input$columns))))
  })
  
  output$variable_options <- renderText({
    paste("The options available for the variables select are",
          paste(as.character(colnames(get(input$dataset))),collapse = ", "))
  }) 
  
  ### action button output:
  
  values <- reactiveValues(i = 0)
  
  observeEvent(
    input$action_button, {
      
      values$i <- values$i + 1
      
    })
  
  output$my_action_button <- renderText({
    
    ifelse(input$action_button,
           paste0("My action button value is ", values$i),
           "My action button value is 0")
  })
  
  ## hints:
  
  ## Hint: make sure that the printed text for the textInput 
  ## changes as in the online version when the entered text changes
  
  ## Hint: make sure that the printed text for the textAreaInput 
  ## changes as in the online version when the entered text changes
  
  ## Hint: No error messages should be shown on the screen or in the Rstudio
  ## console when the app is running and the user changes inputs
  
  ## Hint: make sure that the printed text for the selectInput with multiple = TRUE
  ## changes as in the online version when the selections are changed
  
  ## Hint: make sure that the options available in the "My renderUI select" change  
  ## correctly when selected values for "My multiple select" change
  ## also make sure that the vector of options used in both the input and the output
  ## for this renderUI is only generated once when "My multiple select" 
  ## is changed by the user
  
  ## Hint: "My variables select" should not be shown until "My data select" is either
  ## set to "mtcars" or "iris" by the user. The "options available for the variables select" text
  ## should also not be shown until "My data select" is either set to "mtcars" or "iris"
  ## by the user
  ## Your code should use 'colnames' to get the column names
  ## from either the "iris" dataset of the "mtcars" dataset, depending on what the user 
  ## selects
  ## make sure that the vector of options used in both the input and the output
  ## for this renderUI is only generated once when "My data select" 
  ## is changed by the user
  
  
  
  ## download:
  output$my_download_button <- downloadHandler(
    filename = function() { "my_download.csv" },
    content = function(file) {
      readr::write_csv(mtcars, path = file)
    }
  )
  
  
  ## date output
  output$date_range_values  <- renderText({
    paste("The date range values are", 
          paste(as.character(input$dateRange), collapse = " and ")
    )
  })
  
  ## text output
  
  
  output$text_input_value <- renderText({ 
    
    ifelse(input$text_input == "", 
           "No textInput value entered", 
           paste("The textInput value entered by the user is:",dQuote(input$text_input)))
    
  })
  
  
## text output 2
  
  output$text_area_value <- renderText({ 
    
    ifelse(input$text_area == "",
           "No textAreaInput value entered", 
           paste("The textAreaInput value entered by the user is:",dQuote(input$text_area)))
    
  })
  
}

shinyApp(ui = ui, server = server)

