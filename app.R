## LVC-plotter Shiny app

library(shiny)
library(readr)
library(rlang)
library(purrr)
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

##Automatically use given csv to expedite testing; set to NULL to force drag-n-drop
testFile <- NULL
# testFile <- "./TestData2.csv"

##Debugging
##Show additional UI element(s) at top of main panel for debugging?
showDebug <- FALSE

##Columns excluded from selectability
exclCols <- c("SearchName", "Number", "Transcript", "Speaker", "Line", "LineEnd", 
              "MatchId", "URL", "Before Match", "Text", "After Match", "Target word", 
              "Target word start", "Target word end", "Target orthography", 
              "Match segment", "Target segment", "Target segment start", "Target segment end")


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "lvc-plotter.css")
  ),
  titlePanel("Plot data"),
  sidebarLayout(
    sidebarPanel(
      ##Instructions
      p("Your data must be a CSV file that has..."),
      tags$ul(tags$li("one token in every row"), 
              tags$li("one column for your coded variants"), 
              tags$li("one column apiece for each constraint")),
      
      ##File upload (drag-n-drop) box
      fileInput("file",
                label="Drag and drop your CSV into the box below",
                buttonLabel="Browse...",
                placeholder="Your file here",
                accept=".csv",
                multiple = FALSE),
      
      ##Configuration settings
      uiOutput("selectCol"),
      uiOutput("genPlotsButton")
    ),
    
    mainPanel(uiOutput("debug"),
              uiOutput("plots"))
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
  if (is.null(testFile) || !file.exists(testFile)) {
    ##If no test file specified, file must be uploaded via drag-n-drop
    df <- eventReactive(input$file, {
      read_csv(input$file$datapath[1], na="", show_col_types=FALSE)
    })
  } else if (file.exists(testFile)) {
    df <- eventReactive(TRUE, {
      read_csv(testFile, na="", show_col_types=FALSE)
    })
  }
  
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
    list()
  })
  
  ##Config options
  ##Column selection
  output$selectCol <- renderUI({
    ##Only show once data has been uploaded
    req(df())
    
    tagList(
      ##Column selection (will create 3 selection inputs)
      h3("Select columns"),
      c(varCol = "Coded variants", const1Col = "Constraint #1", const2Col = "Constraint #2") %>% 
        imap(~ selectInput(.y, .x, choices=setdiff(colnames(df()), exclCols))) %>% 
        map(tagAppendAttributes, class="selectAlign"),
    )
  })
  
  
  ##"Generate plots" button
  output$genPlotsButton <- renderUI({
    ##Only show once data has been uploaded
    req(df())
    
    actionButton("genPlots", "Generate plots")
  })
  
  
  ##Plotting wrapper function
    ##(sym() solution from https://stackoverflow.com/a/49870618)
  plotMe <- function(x, xlab, data=df()) {
    ggplot(data, aes(x=!!sym(x), fill=Variant)) + 
      geom_bar(position="fill") +
      labs(x=xlab, y="Proportion", fill="Variant") +
      theme_bw(base_size=16)
  }
  
  ##Main panel
  output$plots <- renderUI({
    ##Only appear if "Generate plots" has been clicked
    req(input$genPlots)
    tagList(
      plotOutput("plot1"),
      plotOutput("plot2"),
      downloadButton("downloadPlots", "Download plots")
    )
  })
  
  ##Render plots
  output$plot1 <- renderPlot({
    plotMe(input$const1Col, input$const1Col)
  }) %>% 
    ##Only run when "Generate plots" is clicked
    bindEvent(input$genPlots)
  output$plot2 <- renderPlot({
    plotMe(input$const2Col, input$const2Col)
  }) %>% 
    ##Only run when "Generate plots" is clicked
    bindEvent(input$genPlots)
}


shinyApp(ui, server)
