library(shiny)
library(vroom)
library(tidyverse)

# prod_codes <- setNames(products$prod_code, products$title)
# this will shows the product name in the UI and returns the product code to the server

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
        column(2, selectInput("y", "Y axis", c("rate", "count")))
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
        column(2, actionButton("story", "Tell me a story")),
        column(10, textOutput("narrative"))
    )
)

server <- function(input, output) {
    selected <- reactive(injuries %>% filter(prod_code == input$code))
    
    # output$diag <- renderTable(
    #     selected() %>% count(diag, wt = weight, sort = TRUE)
    # )
    output$diag <- renderTable(count_top(selected(), diag), width = "100%")
    
    # output$body_part <- renderTable(
    #     selected() %>% count(body_part, wt = weight, sort = TRUE)
    # )
    output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
    
    # output$location <- renderTable(
    #     selected() %>% count(location, wt = weight, sort = TRUE)
    # )
    output$location <- renderTable(count_top(selected(), location), width = "100%")
    
    # it’s good practice to keep computing and plotting separate as it makes the flow of
    # the app easier to understand, and will make it easier to generalise in the future
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
    
    narrative_sample <- eventReactive(
        # Powiazanie "story" z selected() (?)
        eventExpr = list(input$story, selected()),
        # eventExpr - A (quoted or unquoted) expression that represents the event;
        # this can be a simple reactive value like input$click, a call to a reactive
        # expression like dataset(), or even a complex expression inside curly braces
        
        # Okreselenie jakie zadanie ma byc wykonane
        {selected() %>% pull(narrative) %>% sample(1)}
    )
    
    output$narrative <- renderText(narrative_sample())
}

shinyApp(ui = ui, server = server)
