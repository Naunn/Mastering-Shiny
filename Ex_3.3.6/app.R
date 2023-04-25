library(shiny)
library(reactlog)

options(shiny.reactlog=TRUE)
# While your Shiny app is running, press the key combination Ctrl+F3 
# to launch the reactlog application.

ui <- fluidPage(
    textInput("name", "What's your name?"),
    textOutput("greeting"),
    
    numericInput("a", "Give me first number", value = 0),
    numericInput("b", "Give me second number", value = 0),
    numericInput("c", "Give me third number", value = 0),
    numericInput("d", "Give me fourth number", value = 0),
    textOutput("f"),
    
    numericInput("x1", "Give me first number", value = 0),
    numericInput("x2", "Give me second number", value = 0),
    numericInput("x3", "Give me third number", value = 0),
    numericInput("y1", "Give me fourth number", value = 0),
    numericInput("y2", "Give me fifth number", value = 0),
    textOutput("z")
)

# Naprawianie ponizszych funkcji server() (naprawione)
server1 <- function(input, output, server) {
    output$greeting <- renderText(paste0("Hello ", input$name))
}

server2 <- function(input, output, server) {
    g <- reactive(paste0("Hello ", input$name))
    output$greeting <- renderText(g())
}

server3 <- function(input, output, server) {
    output$greeting <- renderText(paste0("Hello ", input$name))
}

# Rysowanie grafow dla ponizszych funkcji
server1 <- function(input, output, session) {
    c <- reactive(input$a + input$b)
    e <- reactive(c() + input$d)
    output$f <- renderText(e())
}
server2 <- function(input, output, session) {
    x <- reactive(input$x1 + input$x2 + input$x3)
    y <- reactive(input$y1 + input$y2)
    output$z <- renderText(x() / y())
}
server3 <- function(input, output, session) {
    d <- reactive(c() ^ input$d)
    a <- reactive(input$a * 10)
    c <- reactive(b() / input$c) 
    b <- reactive(a() + input$b)
    # output$f <- renderText(d()+a()+c()+b())
}

shinyApp(ui = ui, server = server)
