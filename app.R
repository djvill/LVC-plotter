## LVC-plotter Shiny app

library(shiny)
library(readr)
library(rlang)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)
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
testFile <- "./TestData2.csv"

##Debugging
##Show additional UI element(s) at top of main panel for debugging?
showDebug <- T

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
      uiOutput("modifyCol"),
      uiOutput("genPlotsButton")
    ),
      # uiOutput("config")),
    
    # mainPanel(verbatimTextOutput("debug"),
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
    list(`labelMaps$Foll_seg` = labelMaps$Foll_seg,
         `labelsNew$Foll_seg` = labelsNew$Foll_seg,
         `input$modCol` = input$modCol,
         `labelMaps[[input$modCol]]` = labelMaps[[input$modCol]],
         `input[["labelMap_Variant_Deleted"]]` = input[["labelMap_Variant_Deleted"]],
         `input[["labelMap_Foll_seg_Cons"]]` = input[["labelMap_Foll_seg_Cons"]],
         labelsNewIds = labelsNewIds(),
         `labelsNewIds() %>% map(~ input[[.x]])` = labelsNewIds() %>% map_chr(~ input[[.x]]),
         `str_subset(labelsNewIds(), input$modCol)` = str_subset(labelsNewIds(), input$modCol),
         `str_subset(labelsNewIds(), input$modCol) %>% map(~ input[[.x]])` = str_subset(labelsNewIds(), input$modCol) %>% map(~ input[[.x]])
         # labelsNewIds = labelsNewIds
         # `input[[paste("labelMap", input$modCol)]]` = input[[paste("labelMap", input$modCol)]]
         # `updateMaps()` = updateMaps()
         # `names(labelMaps[[input$modCol]])` = names(labelMaps[[input$modCol]]),
         # `map_chr(labelMaps[[input$modCol]], ~ input[[paste("labelMap", input$modCol, .x)]] %||% "")` =
         #   map_chr(labelMaps[[input$modCol]], ~ input[[paste("labelMap", input$modCol, .x)]] %||% "")
         )
  })
  
  ##Config options
  ##Column selection
  # output$config <- renderUI({
  output$selectCol <- renderUI({
    ##Only show once data has been uploaded
    req(df())
    
    tagList(
      h2("Plot configuration"),
      
      ##Column selection (will create 3 selection inputs)
      h3("Specify columns"),
      c(varCol = "Coded variants", const1Col = "Constraint #1", const2Col = "Constraint #2") %>% 
        imap(~ selectInput(.y, .x, choices=setdiff(colnames(df()), exclCols))) %>% 
        map(tagAppendAttributes, class="selectAlign"),
    )
  })
  
  ##Column modification
  labelMaps <- reactiveValues()
  labelsNew <- reactiveValues()
  labelsNew2 <- reactiveValues()
  labelsNewIds <- reactiveVal(character(0))
  # labelsNewIds <- character(0)
  output$modifyCol <- renderUI({
    ##Only show once data has been uploaded
    req(df())
    
    tagList(
      fluidRow(
        column(8, h3("Modify columns", class="margin-top-0")),
        column(4, actionButton("hideshow_modCols", "Show menu")),
      ),
      tabsetPanel(
        id = "modColMenu",
        # Hide the tab values.
        # Can only switch tabs by using `updateTabsetPanel()`
        type = "hidden",
        tabPanelBody("hidden"),
        tabPanelBody("shown", 
                     tagList(
                       ##Start with empty select (filled in by the time user sees it)
                       selectInput("modCol", "Column to modify", ""),
                       textAreaInput("newLabs", 
                                     "New labels (one per line, in preferred order of appearance)", rows=3),
                       uiOutput("labelMap")
                     )
        )
      ),
    )
  })
  
  ##Change modCol options & labelMaps labels when different column selected
  observeEvent(input$varCol, {
    updateSelectInput(session, "modCol", choices=c(input$varCol, input$const1Col, input$const2Col))
    labelMaps[[input$varCol]] <- unique(df()[[input$varCol]])
    labelsNew[[input$varCol]] <- unique(df()[[input$varCol]])
    labelsNew2[[input$varCol]] <- unique(df()[[input$varCol]])
  })
  observeEvent(input$const1Col, {
    updateSelectInput(session, "modCol", choices=c(input$varCol, input$const1Col, input$const2Col))
    labelMaps[[input$const1Col]] <- unique(df()[[input$const1Col]])
    labelsNew[[input$const1Col]] <- unique(df()[[input$const1Col]])
    labelsNew2[[input$const1Col]] <- unique(df()[[input$const1Col]])
  })
  observeEvent(input$const2Col, {
    updateSelectInput(session, "modCol", choices=c(input$varCol, input$const1Col, input$const2Col))
    labelMaps[[input$const2Col]] <- unique(df()[[input$const2Col]])
    labelsNew[[input$const2Col]] <- unique(df()[[input$const2Col]])
    labelsNew2[[input$const2Col]] <- unique(df()[[input$const2Col]])
  })
  
  ##Change textAreaInput value when modCol changes
  observeEvent(input$modCol, {
    req(input$modCol)
    # updateTextAreaInput(session, "newLabs", value=labelMaps[[input$modCol]] %>% paste(collapse="\n"))
    updateTextAreaInput(session, "newLabs", value=labelsNew[[input$modCol]] %>% paste(collapse="\n"))
  })
  
  ##Update stored column labels when newLabs text changes
  observeEvent(input$newLabs, {
    req(input$newLabs)
    # names(labelMaps[[input$modCol]]) <- strsplit(input$newLabs, "\n", fixed=TRUE)[[1]]
    labelsNew[[input$modCol]] <- strsplit(input$newLabs, "\n", fixed=TRUE)[[1]]
  })
  
  ##Dynamically add selectInput()s to end of modifyCol to facilitate recoding
  output$labelMap <- renderUI({
    imap(labelMaps[[input$modCol]], ~ {
      ##Default mapping choice
      defaultMap <-
        if (is.character(.y)) {
          ##If mappings have already been specified, load them
          .y
        } else if (.x %in% labelsNew[[input$modCol]]) {
          ##Otherwise if old label is in list of new labels, default to old label
          .x
        } else {
          ##Otherwise default to first label
          labelsNew[[input$modCol]][1]
        }
      
      ##Add ID to list of select IDs
      newID <- paste("labelMap", input$modCol, .x, sep="_")
      labelsNewIds(unique(c(labelsNewIds(), newID)))
      # labelsNewIds <- unique(c(labelsNewIds, newID))
      
      ##Set up select
      selectInput(newID,
                  paste("New label for", .x),
                  c(labelsNew[[input$modCol]],"(Exclude from plot)"),
                  ##Default mapping choice
                  selected=defaultMap)
    })
  }) %>% 
    ##Only update when newLabs is updated
    bindEvent(input$newLabs)
  
  reactive({
    str_subset(labelsNewIds(), input$modCol) %>%
      map(~ observeEvent(input[[.x]], {
        names(labelMaps[[input$modCol]]) <- input[[.x]]
      }))
  })
  
  ##Store new mapping
  #   map(1:2, ~ observeEvent(input[[paste0("button",.x)]], outText(c(outText(), "YES"))))
  # map(labelsNewIds(), 
  #     ~ observeEvent(input[[.x]], {
  #       names(labelMaps[[input$modCol]]) <-
  #         map_chr(labelMaps[[input$modCol]], 
  #                 ~ input[[paste("labelMap", input$modCol, .x)]] %||% "")
  #     })
  # )
  # observeEvent(input[["labelMap_Variant_Deleted"]], {
  #   names(labelMaps[[input$modCol]]) <-
  #     map_chr(labelMaps[[input$modCol]], 
  #             ~ input[[paste("labelMap", input$modCol, .x)]] %||% "")
  #             # ~ input[[paste("labelMap", input$modCol, .x)]])
  # })
  
  
  
  ##"Generate plots" button
  output$genPlotsButton <- renderUI({
    ##Only show once data has been uploaded
    req(df())
    
    actionButton("genPlots", "Generate plots")
  })
  
  ##Control appearance of "modify columns" menu
  observeEvent(input$hideshow_modCols, {
    if (input$modColMenu=="hidden") {
      newTab <- "shown"
      newLabel <- "Hide menu"
    } else {
      newTab <- "hidden"
      newLabel <- "Show menu"
    }
    updateTabsetPanel(session, "modColMenu", selected=newTab)
    updateActionButton(session, "hideshow_modCols", label=newLabel)
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
      downloadButton("downloadPlots", "Download plots"),
      p("If you're having issues with your plots, please send the configuration data to Dan and describe the issue(s) you're having"),
      downloadButton("downloadConfig", "Download configuration data")
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

# ui <- fluidPage(
#   actionButton("button1", "Button 1"),
#   actionButton("button2", "Button 2"),
#   actionButton("button3", "Button 3"),
#   actionButton("reset", "RESET"),
#   textOutput("yesno")
# )
# 
# server <- function(input, output, session) {
#   outText <- reactiveVal("NULL")
#   output$yesno <- renderText(outText())
# 
#   # observeEvent(input$button1, outText("YES"))
#   # observeEvent(input$button2, outText("YES"))
#   map(1:2, ~ observeEvent(input[[paste0("button",.x)]], outText(c(outText(), "YES"))))
#   observeEvent(input$reset, outText("NULL"))
# }

shinyApp(ui, server)
