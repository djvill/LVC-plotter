## LVC-plotter Shiny app

library(shiny)
library(readr)
library(rlang)
library(dplyr)
library(ggplot2)


# Test data ---------------------------------------------------------------

# testData <- 
#   tidyr::expand_grid(Variety = c("Standard American", 
#                                  "Appalachian working class", 
#                                  "Southern African American working class", 
#                                  "Native American Puebloan"),
#                      Foll_seg = c("Consonant", "Non-consonant"),
#                      Morpho_status = c("Non-past", "Past")) %>% 
#   cbind(Deleted = c(66, 36, 12, 3,
#                     74, 67, 16, 5,
#                     88, 50, 72, 36,
#                     98, 92, 88, 81)) %>% 
#   mutate(Retained = 100 - Deleted) %>% 
#   pivot_longer(Deleted:Retained, names_to="Variant", values_to="n") %>% 
#   uncount(n)
# write.csv(testData, "TestData.csv", row.names=FALSE)

# Parameters ------------------------------------------------------------------

##Debugging
##Show additional UI element(s) at top of main panel for debugging?
showDebug <- FALSE


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "lvc-plotter.css")
  ),
  titlePanel("Plot data"),
  sidebarLayout(
    sidebarPanel(
      p("Your data must be a CSV file that has..."),
      tags$ul(tags$li("one token in every row"), 
              tags$li("one column for your coded variants"), 
              tags$li("one column apiece for each constraint")),
      fileInput("file",
                label="Drag and drop your CSV in the box below",
                buttonLabel="Browse...",
                placeholder="Box outline must turn green",
                accept=".csv",
                multiple = FALSE),
      p() ##Placeholder for various controls, and a "Plot!" button
      ),
    
    # mainPanel(verbatimTextOutput("debug"),
    mainPanel(uiOutput("debug"),
              plotOutput("plot1"),
              plotOutput("plot2"))
  )
)


# Server ------------------------------------------------------------------

##Convenience functions to display/undisplay HTML elements
display <- function(x) {
  if (!("shiny.tag" %in% class(x))) {
    stop("display() only works with shiny.tag objects, not ", 
         paste(class(x), collapse="/"), " objects.")
  }
  
  if (is.null(x$attribs$style)) {
    x$attribs$style <- "display: inherit;"
  } else {
    x$attribs$style <- x$attribs$style %>% 
      gsub("display: \\w+;", "", .) %>%
      paste0("display: inherit;")
  }
  
  x
}
undisplay <- function(x) {
  if (!("shiny.tag" %in% class(x))) {
    stop("undisplay() only works with shiny.tag objects, not ", 
         paste(class(x), collapse="/"), " objects.")
  }
  
  if (is.null(x$attribs$style)) {
    x$attribs$style <- "display: none;"
  } else {
    x$attribs$style <- x$attribs$style %>% 
      gsub("display: \\w+;", "", .) %>%
      paste0("display: none;")
  }
  
  x
}


server <- function(input, output, session) {
  ##Read file
  df <- eventReactive(input$file, {
    read_csv(input$file$datapath[1], na="")
  })
  
  ##Debug wrapper
  output$debug <- renderUI({
    out <- verbatimTextOutput("debugContent")
    
    ##Optionally display or undisplay
    if (showDebug) {
      display(out)
    } else {
      undisplay(out)
    }
  })
  
  ##Verbatim debugging text (works best if a list() of objects with names from
  ##  environment, to 'peek into' environment)
  output$debugContent <- renderPrint({
    list(df = df())
  })
  
  ##Plotting wrapper function
    ##(sym() solution from https://stackoverflow.com/a/49870618)
  plotMe <- function(x, xlab, data=df()) {
    ggplot(data, aes(x=!!sym(x), fill=Variant)) + 
      geom_bar(position="fill") +
      labs(x=xlab, y="Proportion", fill="Variant") +
      theme_bw(base_size=14)
  }
  
  output$plot1 <- renderPlot({
    req(df())
    
    plotMe("Foll_seg", "Following segment")
  })
  
  output$plot2 <- renderPlot({
    req(df())
    
    plotMe("Morpho_status", "Morphological status")
  })
}

shinyApp(ui, server)