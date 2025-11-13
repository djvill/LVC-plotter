## LVC-plotter Shiny app

library(shiny)
library(stringi) ##For guess_encoding()
library(readr)
library(rlang)
library(dplyr)
library(purrr)
library(magrittr)
library(tidyr)
library(ggplot2)

# Parameters ------------------------------------------------------------------

##Automatically use given csv to expedite testing; set to NULL to force drag-n-drop
testFile <- NULL
# testFile <- "./Test-Data-LaBBCAT-Columns.csv"

##Debugging
##Show additional UI element(s) at top of main panel for debugging?
showDebug <- FALSE

##Columns excluded from selectability
exclCols <- c("SearchName", "Number", "Transcript", "Speaker", "Line", "LineEnd", 
              "MatchId", "URL", "Before Match", "Text", "After Match", "Target word", 
              "Target word start", "Target word end",
              "Match segment", "Target segment start", "Target segment end",
              "Title", "DataVersion", "word", "word start", "word end", "segment start", "segment end")

##Special codes from CodeTokens.praat
unsureToken <- "(unsure)"
excludedToken <- "(not a token)"

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "lvc-plotter.css")
  ),
  titlePanel("Plot LVC data"),
  h4("Dan Villarreal (University of Pittsburgh)"), 
  h4("Linguistic Variation & Change (LING 1269)"),
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
      uiOutput("excludeTokens"),
      uiOutput("genPlotsButton")
    ),
    
    mainPanel(uiOutput("debug"),
              uiOutput("plots"))
  ),
  p("App code on ", a("GitHub", href="https://github.com/djvill/LVC-plotter"), class="footer")
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
      filePath <- input$file$datapath[1]
      
      if (endsWith(filePath, ".csv")) {
        enc <- guess_encoding(filePath)$encoding[1]
        read_csv(filePath, na="", show_col_types=FALSE, locale=locale(encoding=enc))
      } else if (grepl("\\.xlsx?$", filePath)) {
        readxl::read_excel(filePath, na="")
      } else {
        stop("File must be a CSV or Excel file")
      }
    })
  } else if (file.exists(testFile)) {
    ##If test file exists, 
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
    list(`df()` = df(),
         `filtered_df()` = filtered_df())
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
        imap(~ selectInput(.y, .x, choices=rev(setdiff(colnames(df()), exclCols)))) %>% 
        map(tagAppendAttributes, class="selectAlign"),
      checkboxInput("exclLCCols", "Exclude common LaBB-CAT columns", value=TRUE)
    )
  })
  
  ##Implement reactivity to "Exclude common LaBB-CAT columns" button
  observeEvent(input$exclLCCols, {
    ##Get current selections
    currSel <- 
      c("varCol", "const1Col", "const2Col") %>% 
      set_names(., .) %>% 
      map(~ input[[.x]])
    
    if (input$exclLCCols) {
      ##New choices
      newChoices <- setdiff(colnames(df()), exclCols)
      currSel %>% 
        imap(~ updateSelectInput(session, .y, 
                                 ##Preserve current selection if it's eligible 
                                 ##(if not, selected=NULL defaults to first choice)
                                 selected=if (.x %in% newChoices) .x,
                                 choices=newChoices))
    } else {
      ##Update select input
      currSel %>% 
        imap(~ updateSelectInput(session, .y, selected=.x, 
                                 choices=colnames(df())))
    }
  })
  
  ##Token exclusions
  output$excludeTokens <- renderUI({
    ##Only show once data has been uploaded
    req(df())
    
    tagList(
      ##Column selection (will create 3 selection inputs)
      h3("Exclude tokens"),
      checkboxInput("exclBlank", "Exclude uncoded tokens", value=TRUE),
      checkboxInput("exclUnsure", tagList(span("Exclude tokens marked"), code(unsureToken)), value=TRUE),
      checkboxInput("exclNotAToken", tagList(span("Exclude tokens marked"), code(excludedToken)), value=TRUE)
    )
  })
  
  ##"Generate plots" button
  output$genPlotsButton <- renderUI({
    ##Only show once data has been uploaded
    req(df())
    
    actionButton("genPlots", "Generate plots")
  })
  
  ##Implement token exclusions
  filtered_df <- eventReactive(input$genPlots, {
    x <- 
      df() |>
      mutate(across(all_of(input$varCol), \(x) replace_na(x, "(blank)")))
    
    if (input$exclBlank) {
      x <- x |>
        filter(if_any(all_of(input$varCol), \(x) x != "(blank)"))
    }
    if (input$exclUnsure) {
      x <- x |>
        filter(if_any(all_of(input$varCol), \(x) x != unsureToken))
    }
    if (input$exclNotAToken) {
      x <- x |>
        filter(if_any(all_of(input$varCol), \(x) x != excludedToken))
    }
    
    x
  })
  
  ##Plotting wrapper function
    ##(sym() solution from https://stackoverflow.com/a/49870618)
  plotMe <- function(constraint, variant, data=filtered_df()) {
    ggplot(data, aes(x=!!sym(constraint), fill=!!sym(variant))) + 
      geom_bar(position="fill") +
      labs(x=constraint, y="Proportion", fill=variant) +
      theme_bw(base_size=20)
  }
  
  ##Main panel
  output$plots <- renderUI({
    ##Only appear if "Generate plots" has been clicked
    req(input$genPlots)
    tagList(
      uiOutput("tokenCount"),
      uiOutput("variantDist"),
      uiOutput("constDists"),
      plotOutput("plot1"),
      plotOutput("plot2"),
      h3("If you right-click on a plot and click 'Copy image', you can paste it into your presentation")
    )
  })
  
  ##Summary stats
  output$tokenCount <- renderUI({
    ##Heading
    tcTags <- list(
      h3(paste("Sample size:",
                sum(!is.na(filtered_df()[[input$varCol]]))),
         class="summary")
    )
    
    if (any(input$exclBlank, input$exclUnsure, input$exclNotAToken)) {
      exclCounts <- list()
      
      if (input$exclBlank) {
        exclCounts <- exclCounts |>
          c(paste("Uncoded tokens: ",
                  df() |>
                    filter(if_any(all_of(input$varCol), is.na)) |>
                    nrow())
          )
      }
      if (input$exclUnsure) {
        exclCounts <- exclCounts |>
          c(list(tagList(span("Tokens marked"), code(unsureToken),
                         span(df() |>
                                filter(if_any(all_of(input$varCol), \(x) x == unsureToken)) |>
                                nrow())))
          )
      }
      if (input$exclNotAToken) {
        exclCounts <- exclCounts |>
          c(list(tagList(span("Tokens marked"), code(excludedToken),
                         span(df() |>
                                filter(if_any(all_of(input$varCol), \(x) x == excludedToken)) |>
                                nrow())))
          )
      }
      
      exclCounts <- exclCounts |>
        map(h5) |>
        map(tags$li)
      
      tcTags <- tagList(tcTags,
                        tags$ul(tags$li(h4(paste("Excluded:", nrow(df()) - nrow(filtered_df()))),
                                        tags$ul(exclCounts))))
    }
    
    tcTags
  }) %>% 
    ##Only run when "Generate plots" is clicked
    bindEvent(input$genPlots)
  
  ##Function to format distributions of categorical variables as bullet-lists with percentages
  distrib <- function(col, data=filtered_df()) {
    tags$ul(
      data %>% 
        filter(!is.na(!!col)) %>% 
        pull(!!col) %>% 
        factor() %>% 
        summary() %>% 
        divide_by(sum(.)) %>% 
        multiply_by(100) %>% 
        round(1) %>% 
        paste(names(.), sep="% ") %>% 
        map(h4) %>% 
        map(tags$li),
      class="summary"
    )
  }
  
  ##Variant distribution
  output$variantDist <- renderUI({
    tagList(
      h3("Variant distribution:", class="summary"),
      distrib(input$varCol)
    )
  }) %>% 
    ##Only run when "Generate plots" is clicked
    bindEvent(input$genPlots)
  
  ##Constraint distributions
  output$constDists <- renderUI({
    fluidRow(
      c(input$const1Col, input$const2Col) %>%
        imap(~ column(6,
                      h3(paste0("Constraint #", .y, " distribution:"), class="summary"),
                      distrib(.x)))
    )
  }) %>% 
    ##Only run when "Generate plots" is clicked
    bindEvent(input$genPlots)
  
  ##Render plots
  output$plot1 <- renderPlot({
    plotMe(input$const1Col, input$varCol)
  }) %>% 
    ##Only run when "Generate plots" is clicked
    bindEvent(input$genPlots)
  output$plot2 <- renderPlot({
    plotMe(input$const2Col, input$varCol)
  }) %>% 
    ##Only run when "Generate plots" is clicked
    bindEvent(input$genPlots)
  
}


shinyApp(ui, server)
