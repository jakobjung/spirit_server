#!/usr/bin/env Rscript
#
# scripts/app_spirit.R
#
# To run this Shiny app from a terminal:
#   Rscript scripts/app_spirit.R
# or in R:
#   shiny::runApp("scripts", launch.browser = TRUE)

library(shiny)
library(shinythemes)
library(shinyjs)    # for hide/show UI elements
library(DT)
library(tidyverse)
library(ggrepel)
library(plotly)     # for interactive plots

# Enable server-side bookmarking
enableBookmarking("server")

###############################################################################
# 1) Define UI as a function (for bookmarking)
###############################################################################
ui <- function(request) {
  fluidPage(
    theme = shinytheme("sandstone"),
    useShinyjs(),
    div(
  style = "background-color: steelblue; padding: 20px; text-align: center; color: white; font-size: 24px; font-weight: bold;",
  "SPIRIT - Swift P-value Integration for sRNA Interaction Targets"
),

    #titlePanel("SPIRIT Web Interface"),

    sidebarLayout(
      sidebarPanel(
        id = "sidebarPanelID",

        # Before pipeline runs: file inputs and "Run SPIRIT" button.
        conditionalPanel(
          condition = "!output.showResults",
          actionButton("exampleDataBtn", "Use Example Data (autofill)"),
          tags$hr(),

          fileInput("fasta", "Upload Organism Fasta File",
                    accept = c(".fasta", ".fa", ".fna")),
          fileInput("gff", "Upload Organism GFF File",
                    accept = c(".gff", ".gff3")),
          fileInput("srna", "Upload sRNA FASTA file",
                    accept = c(".fasta", ".fa", ".fna")),
          fileInput("csv1", "Upload Experiment Table 1 (CSV/Excel)",
                    accept = c(".csv", ".xlsx")),

          checkboxInput("upload_more", "Upload More Tables?", value = FALSE),
          conditionalPanel(
            condition = "input.upload_more == true",
            fileInput("csv2", "Table 2 (CSV/Excel) - Optional",
                      accept = c(".csv", ".xlsx")),
            fileInput("csv3", "Table 3 (CSV/Excel) - Optional",
                      accept = c(".csv", ".xlsx")),
            fileInput("csv4", "Table 4 (CSV/Excel) - Optional",
                      accept = c(".csv", ".xlsx"))
          ),

          textInput("idCol", "Gene identifier column (default: locus_tag)",
                    value = "locus_tag",
                    placeholder = "e.g. gene, locus_tag, etc."),
          textInput("weights", "Fisher Weights (comma-separated)", ""),
          textInput("email", "Email address (optional)", ""),
          tags$hr(),
          actionButton("runBtn", "Run SPIRIT")
        ),

        # After pipeline finishes: show only plot options in sidebar.
        conditionalPanel(
          condition = "output.showResults",
          h4("Plot Options"),
          selectInput("xAxisCol", "X-axis Column (will show -log10):",
                      choices = NULL, selected = NULL),
          selectInput("yAxisCol", "Y-axis Column (will show -log10):",
                      choices = NULL, selected = NULL),
          numericInput("xMax", "X-axis Maximum (optional)", value = NA),
          numericInput("yMax", "Y-axis Maximum (optional)", value = NA),
          selectInput("geneHoverCol", "Gene hover text column:",
                      choices = NULL, selected = NULL)
        )
      ),

      mainPanel(
        # While pipeline is running, show a running message.
        conditionalPanel(
          condition = "output.pipelineRunning",
          tags$div(
            style = "color: red; font-weight: bold; font-size: 150%;",
            "SPIRIT pipeline is running (est. time 2-5 minutes). Please do not refresh."
          )
        ),

        # After pipeline finishes, show final results.
        conditionalPanel(
          condition = "!output.pipelineRunning && output.showResults",
          textOutput("runStatus"),
          tags$hr(),
          h4("Result Plots"),
          plotlyOutput("plotResult"),
          tags$hr(),
          h4("Final Combined Table"),
          dataTableOutput("finalTable")
        )
      )
    )
  )
}

###############################################################################
# 2) Define Server Logic
###############################################################################
server <- function(input, output, session) {

  # Reactive values to track state
  rv <- reactiveValues(
    status          = "",
    finalData       = NULL,
    done            = FALSE,
    useExample      = FALSE,
    pipelineRunning = TRUE,
    spiritFolder    = NULL
  )

  #############################################################################
  # A) "Use Example Data" button: set flag and autofill gene identifier
  #############################################################################
  observeEvent(input$exampleDataBtn, {
    rv$useExample <- TRUE
    updateTextInput(session, "idCol", value = "gene")
    rv$status <- "Example data mode selected. Gene ID set to 'gene'."
    output$runStatus <- renderText(rv$status)
  })

  #############################################################################
  # B) "Run SPIRIT" button: run pipeline (example vs. user data)
  #############################################################################
  observeEvent(input$runBtn, {
    # Hide the entire sidebar so user can't change inputs mid-run.
    shinyjs::hide("sidebarPanelID")

    rv$pipelineRunning <- TRUE
    rv$done <- FALSE
    rv$status <- "Starting SPIRIT pipeline..."
    output$runStatus <- renderText(rv$status)

    if (rv$useExample) {
      # Example data from ./data/default
      defaultDir   <- normalizePath(file.path(".", "data", "default"))
      defaultFasta <- file.path(defaultDir, "FQ312003_wplasmids.fa")
      defaultGff   <- file.path(defaultDir, "FQ312003.1_srnas_plasmids.gff")
      defaultSrna  <- file.path(defaultDir, "pinT.fasta")
      defaultCsv1  <- file.path(defaultDir, "MAPS_test.csv")
      defaultCsv2  <- file.path(defaultDir, "SPI1_Pulse_Tidy.csv")
      csv3         <- ""
      csv4         <- ""

      outputDir <- normalizePath(file.path(".", "data"))
      runSpirit(
        fastaPath = defaultFasta,
        gffPath   = defaultGff,
        srnaPath  = defaultSrna,
        csv1Path  = defaultCsv1,
        csv2Path  = defaultCsv2,
        csv3Path  = csv3,
        csv4Path  = csv4,
        idCol     = input$idCol,
        weights   = input$weights,
        outDir    = outputDir
      )
    } else {
      req(input$fasta, input$gff, input$srna, input$csv1)

      inputDir <- tempdir()
      # Use original file names:
      fastaPath <- file.path(inputDir, input$fasta$name)
      file.copy(input$fasta$datapath, fastaPath, overwrite = TRUE)

      gffPath <- file.path(inputDir, input$gff$name)
      file.copy(input$gff$datapath, gffPath, overwrite = TRUE)

      srnaPath <- file.path(inputDir, input$srna$name)
      file.copy(input$srna$datapath, srnaPath, overwrite = TRUE)

      csv1Path <- file.path(inputDir, input$csv1$name)
      file.copy(input$csv1$datapath, csv1Path, overwrite = TRUE)

      csv2Path <- ""
      if (!is.null(input$csv2)) {
        csv2Path <- file.path(inputDir, input$csv2$name)
        file.copy(input$csv2$datapath, csv2Path, overwrite = TRUE)
      }
      csv3Path <- ""
      if (!is.null(input$csv3)) {
        csv3Path <- file.path(inputDir, input$csv3$name)
        file.copy(input$csv3$datapath, csv3Path, overwrite = TRUE)
      }
      csv4Path <- ""
      if (!is.null(input$csv4)) {
        csv4Path <- file.path(inputDir, input$csv4$name)
        file.copy(input$csv4$datapath, csv4Path, overwrite = TRUE)
      }

      outputDir <- normalizePath(file.path(".", "data"))
      runSpirit(
        fastaPath = fastaPath,
        gffPath   = gffPath,
        srnaPath  = srnaPath,
        csv1Path  = csv1Path,
        csv2Path  = csv2Path,
        csv3Path  = csv3Path,
        csv4Path  = csv4Path,
        idCol     = input$idCol,
        weights   = input$weights,
        outDir    = outputDir
      )
    }

    if (nzchar(input$email) && !is.null(rv$finalData)) {
      message("Would send email to: ", input$email)
      # Email logic goes here
    }
  })

  #############################################################################
  # C) Helper function runSpirit: calls SPIRIT.sh and sets reactive state
  #############################################################################
  runSpirit <- function(fastaPath, gffPath, srnaPath,
                        csv1Path, csv2Path, csv3Path, csv4Path,
                        idCol, weights, outDir) {

    cmd <- paste(
      "bash ./SPIRIT.sh",
      "-f", shQuote(fastaPath),
      "-g", shQuote(gffPath),
      "-s", shQuote(srnaPath),
      "-a", shQuote(csv1Path),
      if (nzchar(csv2Path)) paste("-b", shQuote(csv2Path)) else "",
      if (nzchar(csv3Path)) paste("-c", shQuote(csv3Path)) else "",
      if (nzchar(csv4Path)) paste("-d", shQuote(csv4Path)) else "",
      "-i", shQuote(idCol),
      if (nzchar(weights)) paste("-w", shQuote(weights)) else "",
      "-o", shQuote(outDir)
    )

    rv$status <- paste("Running SPIRIT pipeline with command:", cmd)
    output$runStatus <- renderText(rv$status)
    message(rv$status)

    system(cmd, intern = FALSE)

    # Identify newly created timestamp folder in outDir
    subdirs <- list.dirs(outDir, recursive = FALSE)
    spiritFolder <- subdirs[grepl("\\d{4}_\\d{2}_\\d{2}_T_", basename(subdirs))]
    if (length(spiritFolder) == 0) {
      rv$status <- "SPIRIT pipeline failed or no output folder found."
      rv$finalData <- NULL
      rv$done <- TRUE
      rv$pipelineRunning <- FALSE
      output$runStatus <- renderText(rv$status)
      return(NULL)
    }
    spiritFolder <- spiritFolder[order(spiritFolder, decreasing = TRUE)][1]
    rv$spiritFolder <- spiritFolder

    finalTsv <- file.path(spiritFolder, "data", "combined_tables.tsv")
    if (file.exists(finalTsv)) {
      rv$finalData <- read_tsv(finalTsv)
      rv$status <- paste("SPIRIT pipeline complete! Results in", spiritFolder)
    } else {
      rv$finalData <- NULL
      rv$status <- "SPIRIT pipeline finished but no combined_tables.tsv found."
    }

    rv$done <- TRUE
    rv$pipelineRunning <- FALSE
    output$runStatus <- renderText(rv$status)

    session$doBookmark()  # Bookmark the state
    return(rv$finalData)
  }

  #############################################################################
  # D) Bookmarking callbacks for state restoration
  #############################################################################
  onBookmarked(function(state) {
    state$values <- list(
      spiritFolder = rv$spiritFolder,
      done = rv$done,
      useExample = rv$useExample
    )
  })

  onRestored(function(state) {
    if (!is.null(state$values$spiritFolder)) {
      rv$spiritFolder <- state$values$spiritFolder
      finalTsv <- file.path(rv$spiritFolder, "data", "combined_tables.tsv")
      if (file.exists(finalTsv)) {
        rv$finalData <- read_tsv(finalTsv)
        rv$done <- TRUE
        rv$pipelineRunning <- FALSE
        rv$status <- paste("SPIRIT pipeline complete! Results in", rv$spiritFolder)
        output$runStatus <- renderText(rv$status)
      }
    }
    rv$useExample <- isTRUE(state$values$useExample)
  })

  output$pipelineRunning <- reactive({
    rv$pipelineRunning
  })
  outputOptions(output, "pipelineRunning", suspendWhenHidden = FALSE)

  output$showResults <- reactive({
    rv$done
  })
  outputOptions(output, "showResults", suspendWhenHidden = FALSE)

  #############################################################################
  # E) Populate plot option dropdowns after pipeline finishes
  #############################################################################
  observeEvent(rv$done, {
    req(rv$finalData)
    numericCols <- names(rv$finalData)[sapply(rv$finalData, is.numeric)]

    defaultX <- if ("IntaRNA_p_value" %in% numericCols) "IntaRNA_p_value" else numericCols[1]
    defaultY <- if ("fisher_p_value" %in% numericCols) "fisher_p_value" else numericCols[2]

    updateSelectInput(session, "xAxisCol", choices = numericCols, selected = defaultX)
    updateSelectInput(session, "yAxisCol", choices = numericCols, selected = defaultY)
    updateSelectInput(session, "geneHoverCol", choices = names(rv$finalData), selected = input$idCol)

    shinyjs::show("sidebarPanelID")
  })

  #############################################################################
  # F) Render final table & interactive plot
  #############################################################################
  output$finalTable <- renderDataTable({
    req(rv$done, rv$finalData)
    datatable(rv$finalData, options = list(pageLength = 10))
  })

  output$plotResult <- renderPlotly({
    req(rv$done, rv$finalData, input$xAxisCol, input$yAxisCol, input$geneHoverCol)

    df <- rv$finalData
    validate(
      need(input$xAxisCol %in% names(df), "X-axis col not found in data"),
      need(input$yAxisCol %in% names(df), "Y-axis col not found in data"),
      need(is.numeric(df[[input$xAxisCol]]), "X-axis col must be numeric"),
      need(is.numeric(df[[input$yAxisCol]]), "Y-axis col must be numeric")
    )

    p <- ggplot(df, aes(x = -log10(.data[[input$xAxisCol]]),
                         y = -log10(.data[[input$yAxisCol]]),
                         text = .data[[input$geneHoverCol]])) +
      geom_point(alpha = 0.5, color = "steelblue") +
      theme_minimal() +
      labs(
        title = paste("Scatter of", input$xAxisCol, "vs.", input$yAxisCol, "(both -log10)"),
        x = paste0("-log10(", input$xAxisCol, ")"),
        y = paste0("-log10(", input$yAxisCol, ")")
      )
    if (!is.na(input$xMax))
      p <- p + scale_x_continuous(limits = c(0, input$xMax))
    if (!is.na(input$yMax))
      p <- p + scale_y_continuous(limits = c(0, input$yMax))

    ggplotly(p, tooltip = c("text", "x", "y"))
  })

  #############################################################################
  # G) Bookmarking callbacks for state restoration
  #############################################################################
  onBookmarked(function(state) {
    state$values <- list(
      spiritFolder = rv$spiritFolder,
      done = rv$done,
      useExample = rv$useExample
    )
  })

  onRestored(function(state) {
    if (!is.null(state$values$spiritFolder)) {
      rv$spiritFolder <- state$values$spiritFolder
      finalTsv <- file.path(rv$spiritFolder, "data", "combined_tables.tsv")
      if (file.exists(finalTsv)) {
        rv$finalData <- read_tsv(finalTsv)
        rv$done <- TRUE
        rv$pipelineRunning <- FALSE
        rv$status <- paste("SPIRIT pipeline complete! Results in", rv$spiritFolder)
        output$runStatus <- renderText(rv$status)
      }
    }
    rv$useExample <- isTRUE(state$values$useExample)
  })
}

shinyApp(ui = ui, server = server)
