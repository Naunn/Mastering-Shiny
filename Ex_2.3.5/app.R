library(shiny)

ui <- fluidPage(
    textOutput("text"),
    verbatimTextOutput("test"),
    verbatimTextOutput("sum"),
    verbatimTextOutput("text2"),
    plotOutput("plot", width = "700px", height = "300px"),
    dataTableOutput("table"),
    reactable::reactableOutput("Retable")
)

server <- function(input, output) {
    output$sum <- renderPrint(summary(mtcars))
    output$text <- renderText("Good morning!")
    output$test <- renderPrint(t.test(1:5, 2:6))
    output$text2 <- renderPrint(str(lm(mpg ~ wt, data = mtcars)))
    output$plot <- renderPlot(plot(1:5), res = 96)
    output$table <- renderDataTable(mtcars,
                                    options = list(pageLength = 5,
                                                   dom = "t"))
    output$Retable <- reactable::renderReactable({reactable::reactable(mtcars)})
}

shinyApp(ui = ui, server = server)
