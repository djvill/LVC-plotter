##This example shows how to create a hideable menu in Shiny, with a button that
##changes between "Hide" and "Show"

##Based on https://shiny.rstudio.com/reference/shiny/1.6.0/updateTabsetPanel.html

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("hideshow", "Show"),
      tabsetPanel(
        id = "hidden_tabs",
        # Hide the tab values.
        # Can only switch tabs by using `updateTabsetPanel()`
        type = "hidden",
        tabPanelBody("panel1"),
        tabPanelBody("panel2", 
                     tagList(
                       h2("Here's the hidden menu!"),
                       p("Here's some content")
                     )
        )
      )
    ),
    mainPanel(
      h1("Placeholder")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$hideshow, {
    if (input$hidden_tabs=="panel1") {
      newTab <- "panel2"
      newLabel <- "Hide"
    } else {
      newTab <- "panel1"
      newLabel <- "Show"
    }
    updateTabsetPanel(session, "hidden_tabs", selected=newTab)
    updateActionButton(session, "hideshow", label=newLabel)
  })
}


shinyApp(ui, server)