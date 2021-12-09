library(shiny)
library(magrittr)

ui <- fluidPage(
  actionButton("btn", "Click me")
)

server <- function(input, output, session) {
  observeEvent(input$btn, priority=-100, message("Lowest priority"))
  observeEvent(input$btn, priority=1, message("Higher priority"))
  observeEvent(input$btn, priority=100, message("Highest priority"))
  observeEvent(input$btn, message("Medium priority"))
  output$fake1 <- observe({message("bindEvent 1")}) %>% bindEvent(input$btn)
  output$fake2 <- renderText({message("bindEvent 2")}) %>% bindEvent(input$btn)
}

shinyApp(ui, server)


