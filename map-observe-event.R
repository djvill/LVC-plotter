library(shiny)
library(purrr)

ui <- fluidPage(
  numericInput("max", "Max buttons", value=1, min=1, max=3),
  actionButton("button1", "Button 1"),
  textInput("text1", "Text 1"),
  actionButton("button2", "Button 2"),
  textInput("text2", "Text 2"),
  actionButton("button3", "Button 3"),
  textInput("text3", "Text 3"),
  textOutput("yesno1"),
  textOutput("yesno2"),
  textOutput("yesno3")
)

server <- function(input, output, session) {
  ##outText() starts out as "NULL"
  outTexts <- reactiveValues(button1 = character(0), button2 = character(0), button3 = character(0))
  output$yesno1 <- renderText(outTexts$button1)
  output$yesno2 <- renderText(outTexts$button2)
  output$yesno3 <- renderText(outTexts$button3)

  ##If button1 or button2 pressed, add "YES" to outText()
  # map(1:3, ~ observeEvent(input[[paste0("button",.x)]], {
  map(seq_len(input[["max"]]), ~ observeEvent(input[[paste0("button",.x)]], {
    outTexts[[paste0("button", .x)]] <- c(outTexts[[paste0("button", .x)]], input[[paste0("text", .x)]])
  }))
}

shinyApp(ui, server)
