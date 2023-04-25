library(shiny) # Zaladowanie biblioteki

# Zdefiniowanie interfejsu uzytkownika
# (strona internetowa HTML, z którą ludzie wchodzą w interakcję)
# w tym wypadku, strona zawierajaca zwrot "Hello, world!"
# ui <- fluidPage("Hello, world!")

ui <- fluidPage( # layout function that sets up the basic visual structure of the page
    # input control
    selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
    # ls("package:datasets") - zwraca liste dostepnych dataset-ow
    # output control
    verbatimTextOutput("summary"), # displays code
    tableOutput("table") # displays tables
)

# Okreslenie zachowania naszej aplikacji poprzez zdefiniowanie funkcji serwera.
# server <- function(input, output, session){
#     # render function
#     output$summary <- renderPrint({
#         # dataset <- input$dataset # cos takiego przypisze i tyle
#         dataset <- get(input$dataset, pos = "package:datasets") # get() pozwala rowniez na print()
#         # "pos" - where to look for the object
#         summary(dataset)
#     })
#     
#     output$table <- renderTable({
#         dataset <- get(input$dataset, "package:datasets")
#         # dataset <- input$dataset
#         dataset
#     })
# }

server <- function(input, output, session) {
    # Create a reactive expression (tak jakby przemienia w "funkcje")
    dataset <- reactive({
        get(input$dataset, "package:datasets")
    })
    
    output$summary <- renderPrint({
        # Use a reactive expression by calling it like a function
        summary(dataset())
    })
    
    output$table <- renderTable({
        dataset()
    })
}

# Wykonanie shinyApp(ui, serwer), aby skonstruować i uruchomić aplikację Shiny z UI i serwera.
shinyApp(ui, server)
