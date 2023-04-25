library(shiny)
library(reactlog)

options(shiny.reactlog=TRUE)
# While your Shiny app is running, press the key combination Ctrl+F3 
# to launch the reactlog application.

# extracting imperative code out into regular functions
library(ggplot2)

freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
    df <- data.frame(
        x = c(x1, x2),
        g = c(rep("x1", length(x1)), rep("x2", length(x2)))
    )
    
    ggplot(df, aes(x, colour = g)) +
        geom_freqpoly(binwidth = binwidth, size = 1) +
        coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
    test <- t.test(x1, x2)
    
    # use sprintf() to format t.test() results compactly
    sprintf(
        "p value: %0.3f\n[%0.2f, %0.2f]",
        test$p.value, test$conf.int[1], test$conf.int[2]
    )
}

ui <- fluidPage(
    # numericInput("count", "Number of values", value = 100),
    textInput("name", "What's your name?"),
    textOutput("greeting"),
    
    # Apka do porownywania rozkladow Gaussa
    # fluidRow(
    #     column(4, 
    #            "Distribution 1",
    #            numericInput("n1", label = "n", value = 1000, min = 1),
    #            numericInput("mean1", label = "µ", value = 0, step = 0.1),
    #            numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    #     ),
    #     column(4, 
    #            "Distribution 2",
    #            numericInput("n2", label = "n", value = 1000, min = 1),
    #            numericInput("mean2", label = "µ", value = 0, step = 0.1),
    #            numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    #     ),
    #     column(4,
    #            "Frequency polygon",
    #            numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
    #            sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
    #     )
    # ),
    # fluidRow(
    #     column(9, plotOutput("hist")),
    #     column(3, verbatimTextOutput("ttest"))
    # )
    
    # Apka do porownywania rozkladow Poissona
    fluidRow(
        column(3, 
               numericInput("lambda1", label = "lambda1", value = 3),
               numericInput("lambda2", label = "lambda2", value = 5),
               numericInput("n", label = "n", value = 1e4, min = 0),
               actionButton("simulate", "Simulate!")
        ),
        column(9, plotOutput("hist"))
    )
)

server <- function(input, output) {
    # input$count <- 10                                                         # blad!
    # message("The value of input$count is ", input$count)                      # blad!
    # output$greeting <- "Hello human"                                          # blad!
    # message("The greeting is ", output$greeting)                              # blad!
    # output$greeting <- renderText({paste0("Hello human number ",input$count,
    #                                       " named ",input$name,"!")})
    # output$greeting <- renderText(string())
    # string <- reactive(paste0("Hello human number ",input$count,
    #                           " named ",input$name,"!"))
    
    # Apka do porownywania rozkladow Gaussa
    # output$hist <- renderPlot({
    #     x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
    #     x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
    #     
    #     freqpoly(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
    # }, res = 96)
    # 
    # output$ttest <- renderText({
    #     x1 <- rnorm(input$n1, input$mean1, input$sd1)
    #     x2 <- rnorm(input$n2, input$mean2, input$sd2)
    #     
    #     t_test(x1(), x2())
    # })
    
    # Apka do porownywania rozkladow Poissona
    # timer <- reactiveTimer(500)
    
    # x1 <- reactive({
    #     # timer()
    #     input$simulate
    #     rpois(input$n, input$lambda1)
    # })
    
    x1 <- eventReactive(
        input$simulate,
        {rpois(input$n, input$lambda1)
    })
    
    # x2 <- reactive({
    #     # timer()
    #     input$simulate
    #     rpois(input$n, input$lambda2)
    # })
    
    x2 <- eventReactive(
        input$simulate,
        {rpois(input$n, input$lambda2)
    })
    
    output$hist <- renderPlot({
        freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
    }, res = 96)
    
    string <- reactive(paste0("Hello ", input$name, "!"))
    
    output$greeting <- renderText(string())
    observeEvent(input$name, {
        message("Greeting performed")
    })
}

shinyApp(ui = ui, server = server)
