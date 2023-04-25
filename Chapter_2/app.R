library(shiny)

animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")
ui <- fluidPage(
    textInput("name", "What's your name?"),
    textOutput("greeting"),
    passwordInput("password", "What's your password?"),
    textAreaInput("story", "Tell me about yourself", rows = 3),
    numericInput("num", "Number one", value = 0, min = 0, max = 100),
    sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
    sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100),
    dateInput("dob", "When were you born?", format = "yyyy-mm-dd", language = 'pl', weekstart = 1),
    dateRangeInput("holiday", "When do you want to go on vacation next?",
                   format = "yyyy-mm-dd", language = 'pl', weekstart = 1),
    selectInput("state", "What's your favourite state or states?", state.name, multiple = TRUE),
    radioButtons("animal", "What's your favourite animal?", animals),
    checkboxGroupInput("animal", "What animals do you like?", animals),
    radioButtons("rb", "Choose one:",
                 choiceNames = list(
                     icon("angry"),
                     icon("smile"),
                     icon("sad-tear")
                 ),
                 choiceValues = list("angry", "happy", "sad")
    ),
    fileInput("upload", NULL),
    fluidRow(
        actionButton("click", "Click me!", class = "btn-danger"),
        actionButton("drink", "Drink me!", icon = icon("cocktail"), class = "btn-lg btn-success")
    ),
    fluidRow(
        actionButton("eat", "Eat me!", class = "btn-block")
    ),
    checkboxInput("cleanup", "Clean up?", value = TRUE),
    checkboxInput("shutdown", "Shutdown?"),
    textOutput("text"),
    verbatimTextOutput("code"),
    textOutput("text2"),
    verbatimTextOutput("print"),
    tableOutput("static"),
    dataTableOutput("dynamic"),
    plotOutput("plot", width = "400px")
)

server <- function(input, output) {
    output$greeting <- renderText({paste0("Hello, ", input$name,"!")})
    output$text <- renderText("Hello friend!")
    # output$text <- renderText({ 
    #     "Hello friend!"
    # })
    output$code <- renderPrint(summary(1:10))
    # output$code <- renderPrint({ 
    #     summary(1:10) 
    # })
    output$text2 <- renderText("hello!")
    output$print <- renderPrint("hello!")
    output$static <- renderTable(head(mtcars))
    output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
    output$plot <- renderPlot(plot(mtcars$hp), res = 96)
}

shinyApp(ui = ui, server = server)
