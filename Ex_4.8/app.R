library(shiny)
library(tidyverse)
library(reactlog) # Ctrl + F3

options(shiny.reactlog=TRUE,
        shiny.autoreload = TRUE)

# przekonwertowanie zmiennej na czynnik (factor), uporzadkowanie wedlug
# czestosci poziomow, a nastepnie zrzucenie wszystkich poziomow po 5 najlepszych
count_top <- function(df, var, n = 5) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
    
    fluidRow(
        column(8,
               selectInput("code", "Product",
                           choices = setNames(products$prod_code, products$title),
                           width = "100%"
               )
        ),
        column(2, selectInput("y", "Y axis", c("rate", "count"))),
        column(2, numericInput("nrow", "Number of rows", 5))
    ),
    fluidRow(
        column(4, tableOutput("diag")),
        column(4, tableOutput("body_part")),
        column(4, tableOutput("location"))
    ),
    fluidRow(
        column(12, plotOutput("age_sex"))
    ),
    fluidRow(
        column(2, actionButton("story", "Tell me a story", icon = icon("message"))),
        column(10, textOutput("narrative"))
    ),
    fluidRow(
        column(2, actionButton("Back", "Previous story", icon = icon("arrow-left"))),
        column(2, actionButton("Next", "Next story", icon = icon("arrow-right")))
    )
)

server <- function(input, output) {
    selected <- reactive({
        browser()
        injuries %>% filter(prod_code == input$code)})

    output$diag <- renderTable(count_top(selected(), diag, input$nrow), width = "100%")

    output$body_part <- renderTable(count_top(selected(), body_part, input$nrow), width = "100%")

    output$location <- renderTable(count_top(selected(), location, input$nrow), width = "100%")

    summary <- reactive({
        selected() %>% 
            count(age, sex, wt = weight) %>% 
            left_join(population, by = c("age", "sex")) %>% 
            mutate(rate = n / population * 1e4)
    })
    
    output$age_sex <- renderPlot({
        if (input$y == "count") {
            summary() %>% 
                ggplot(aes(age, n, colour = sex)) +
                geom_line() +
                labs(y = "Estimated number of injuries") 
        } else {
            summary() %>% 
                ggplot(aes(age, rate, colour = sex)) +
                geom_line(na.rm = TRUE) +
                labs(y = "Injuries per 10,000 people")
        }
        
    }, res = 96)
    
    #Narrative
    num_narr <- reactive(length(selected()$narrative))
    
    # a reactive value that can be easily changed later (in events)
    # ref: https://stackoverflow.com/questions/42183161/r-shiny-how-to-change-values-in-a-reactivevalues-object
    i <- reactiveValues(tmp=1)
    
    # reset i to 1 if code is changed by user
    # ref: https://www.collinberke.com/post/shiny-series-implementing-a-next-and-back-button/
    observeEvent(input$code, {
        i$tmp <- 1
    })
    
    output$narrative <- renderText(selected()$narrative[1])
    
    observeEvent(input$Back, {
        i$tmp <- i$tmp - 1
        
        if(i$tmp > 0){
            output$narrative <- renderText(selected()$narrative[i$tmp])
        } else {
            i$tmp <- num_narr()
            output$narrative <- renderText(selected()$narrative[num_narr()])
        }
    })
    
    observeEvent(input$Next, {
        i$tmp <- i$tmp + 1
        
        if(i$tmp <= num_narr()){
            output$narrative <- renderText(selected()$narrative[i$tmp])
        } else {
            i$tmp <- 1
            output$narrative <- renderText(selected()$narrative[1])
        }
    })
    
    observeEvent(input$story, {
        output$narrative <- renderText(selected() %>% pull(narrative) %>% sample(1))
    })
}

shinyApp(ui = ui, server = server)
