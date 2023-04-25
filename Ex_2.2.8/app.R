library(shiny)


ui <- fluidPage(
    textInput("name", "What's your name?", placeholder = "Your name"),
    sliderInput("deliver",
                "When should we deliver?",
                value = lubridate::today()+7,
                min = lubridate::today(),
                max = lubridate::today()+14),
    sliderInput("five", "Choose multiples of five.",
                value = 0,
                min = 0,
                max = 100,
                step = 5,
                animate = TRUE),
    selectizeInput("long",
                   "Dataset",
                   choices = ls("package:datasets"),
                   selected = NULL,
                   multiple = TRUE,
                   options = NULL)
)

server <- function(input, output) {
    
}

shinyApp(ui = ui, server = server)
