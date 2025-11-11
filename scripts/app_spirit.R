#!/usr/bin/env Rscript
#
# scripts/app_spirit.R

library(shiny)
library(shinythemes)
library(shinyjs)    # hide/show UI elements
library(DT)
library(tidyverse)
library(ggrepel)
library(plotly)     # interactive plots
library(promises)
library(future)
library(jsonlite)

options(shiny.maxRequestSize = 50 * 1024^2)

# Use background R sessions (works on all platforms)
plan(multisession)

# --- Serve static files when running via Rscript ---
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) normalizePath(dirname(sub("^--file=", "", file_arg))) else normalizePath(".")
}
app_dir <- get_script_dir()
www_dir <- file.path(app_dir, "www")
if (dir.exists(www_dir)) {
  shiny::addResourcePath("spiritres", www_dir)  # maps /spiritres -> scripts/www
}

# URL bookmarking for shareable result links
enableBookmarking("url")

# Helper to build the SPIRIT command
build_spirit_cmd <- function(fastaPath, gffPath, srnaPath,
                             csv1Path, csv2Path, csv3Path, csv4Path,
                             idCol, weights, outDir) {
  paste(
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
}

ui <- function(request) {
  fluidPage(
    theme = shinytheme("sandstone"),
    useShinyjs(),
    tags$head(
      tags$style(HTML("
         /* Footer: white fill + dark-blue top line, stretched logos */
        .footer-bar {
          background-color: #ffffff;
          border-top: 6px solid #002864;
          padding: 12px 24px;
          position: fixed; left: 0; right: 0; bottom: 0;
          z-index: 1000;
          display: flex; flex-wrap: nowrap;
          justify-content: space-between; align-items: center; gap: 24px;
        }
        .footer-bar a { flex: 1 1 0; display: flex; justify-content: center; line-height: 0; }
        .footer-logo { height: 44px; width: auto; max-width: 150%; }
        body { padding-bottom: 72px; }

        /* Buttons in #002864 */
        .btn, .btn-default, .btn-primary {
          background-color: #002864 !important; border-color: #002864 !important; color: #ffffff !important;
        }
        .btn:hover, .btn-default:hover, .btn-primary:hover,
        .btn:focus, .btn-default:focus, .btn-primary:focus {
          background-color: #001e4d !important; border-color: #001e4d !important; color: #ffffff !important;
        }
        .btn:active, .btn-default:active, .btn-primary:active,
        .open > .dropdown-toggle.btn-default,
        .open > .dropdown-toggle.btn-primary {
          background-color: #001b45 !important; border-color: #001b45 !important; color: #ffffff !important;
        }

        /* Intro card */
        .intro-card {
          max-width: 760px; margin: 40px auto; padding: 28px;
          background: #ffffff; border-radius: 14px;
          box-shadow: 0 6px 24px rgba(0,0,0,0.08); text-align: center;
        }
        .intro-logo { max-width: 100%; height: auto; margin-bottom: 16px; }
        .intro-title { font-size: 24px; font-weight: 700; margin-top: 8px; }
        .intro-sub { color:#555; margin-top: 4px; }
        .intro-list { text-align: left; display: inline-block; margin-top: 10px; }
        .intro-credits { margin-top: 14px; font-weight: 600; }

        /* Full-page overlay while running */
        .running-overlay { position: fixed; inset: 0; z-index: 9999; display: flex; align-items: center; justify-content: center; }
        .running-text { color: #c62828; font-weight: 800; text-align: center; font-size: clamp(26px, 4vw, 42px); line-height: 1.25; padding: 0 24px; }
      "))
    ),

    # Title bar
    div(
      style = "background-color: #002864; padding: 20px; text-align: center; color: white; font-size: 24px; font-weight: bold;",
      "SPIRIT - Swift P-value Integration for sRNA Interaction Targets"
    ),

    # Footer logo bar
    div(
      class = "footer-bar",
      tags$a(href = "https://www.bayresq.net", target = "_blank", rel = "noopener",
             tags$img(src = "spiritres/bayresq.png", class = "footer-logo", alt = "BayResQ")),
      tags$a(href = "https://www.helmholtz-hiri.de", target = "_blank", rel = "noopener",
             tags$img(src = "spiritres/hirilogo.svg", class = "footer-logo", alt = "HIRI")),
      tags$a(href = "https://www.uni-wuerzburg.de", target = "_blank", rel = "noopener",
             tags$img(src = "spiritres/uniwue.png", class = "footer-logo", alt = "Universität Würzburg"))
    ),

    sidebarLayout(
      sidebarPanel(
        id = "sidebarPanelID",

        # Before pipeline runs
        conditionalPanel(
          condition = "!output.showResults",
          actionButton("exampleDataBtn", "Use Example Data for PinT (autofill)"),
          tags$hr(),

          # --- Organism choice ---
          selectInput(
            "organismChoice", "Organism",
            choices = c(
              "Bacteroides thetaiotaomicron VPI-5482" = "btheta",
              "Salmonella enterica serovar Typhimurium SL1344" = "sl1344",
              "Own files" = "own"
            ),
            selected = "sl1344"
          ),

          # Default reference downloads (only for preset organisms)
          conditionalPanel(
            condition = "input.organismChoice !== 'own'",
            fluidRow(
              column(6, downloadButton("downloadFasta", "Download FASTA")),
              column(6, downloadButton("downloadGff",   "Download GFF"))
            ),
            tags$hr()
          ),

          # FASTA/GFF uploads (only when "Own files")
          conditionalPanel(
            condition = "input.organismChoice === 'own'",
            fileInput("fasta", "Upload Organism FASTA file", accept = c(".fasta", ".fa", ".fna")),
            fileInput("gff",   "Upload Organism GFF file",   accept = c(".gff", ".gff3")),
            tags$hr()
          ),

          # --- sRNA choice (depends on organism) ---
          uiOutput("srnaChoiceUI"),

          # sRNA upload (only when user selects 'own' sRNA)
          conditionalPanel(
            condition = "input.srnaChoice === 'own'",
            fileInput("srna", "Upload sRNA FASTA file", accept = c(".fasta", ".fa", ".fna")),
            tags$hr()
          ),

          # --- Default dataset selection (only for matching organism + sRNA) ---
          uiOutput("datasetChoiceUI"),

          # User-provided datasets (always available)
          fileInput("csv1", "Upload Experiment Table 1 (CSV/Excel)", accept = c(".csv", ".xlsx")),
          checkboxInput("upload_more", "Upload More Tables?", value = FALSE),
          conditionalPanel(
            condition = "input.upload_more == true",
            fileInput("csv2", "Table 2 (CSV/Excel) - Optional", accept = c(".csv", ".xlsx")),
            fileInput("csv3", "Table 3 (CSV/Excel) - Optional", accept = c(".csv", ".xlsx")),
            fileInput("csv4", "Table 4 (CSV/Excel) - Optional", accept = c(".csv", ".xlsx"))
          ),

          textInput("idCol", "Gene identifier column (default: locus_tag)",
                    value = "locus_tag", placeholder = "e.g. gene, locus_tag, etc."),
          textInput("weights", "Fisher Weights (comma-separated)", ""),
          textInput("email", "Email address (optional)", ""),
          tags$hr(),
          actionButton("runBtn", "Run SPIRIT")
        ),

        # After pipeline finishes: plot options
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
        # Intro card
        conditionalPanel(
          condition = "!output.pipelineRunning && !output.showResults",
          div(class = "intro-card",
              tags$img(src = "spiritres/spirit_logo_2.png", class = "intro-logo", alt = "SPIRIT logo"),
              div(class = "intro-title", "SPIRIT"),
              div(class = "intro-sub", "Swift P-value Integration for sRNA Interaction Targets"),
              tags$div(
                class = "intro-list",
                tags$ul(
                  tags$li("Predicts sRNA–mRNA and sRNA–sRNA interactions."),
                  tags$li("Combines multiple evidence sources via (weighted) Fisher’s method."),
                  tags$li("Parses FASTA/GFF and experiment tables, outputs an integrated result table and plots.")
                )
              ),
              div(class = "intro-credits", "Created by Hoda Kooshapour & Jakob Jung")
          )
        ),

        # Running overlay
        conditionalPanel(
          condition = "output.pipelineRunning",
          div(class = "running-overlay",
              div(class = "running-text",
                  HTML("SPIRIT pipeline is running (est. time 2–10 minutes).<br>Please do not refresh.")
              )
          )
        ),

        # Results area
        conditionalPanel(
          condition = "!output.pipelineRunning && output.showResults",
          textOutput("runStatus"),
          tags$hr(),
          h4("Shareable link to this result"),
          uiOutput("bookmark_ui"),
          tags$hr(),
          h4("Result Plots"),
          plotlyOutput("plotResult"),
          tags$hr(),
          h4("Download results"),
          div(
            style = "display:flex; gap:10px; flex-wrap:wrap;",
            downloadButton("dl_tsv",  "Download TSV (.tsv)"),
            downloadButton("dl_xlsx", "Download Excel (.xlsx)")
          ),
          h4("Final Combined Table"),
          dataTableOutput("finalTable")
        )
      )
    )
  )
}

server <- function(input, output, session) {
  rv <- reactiveValues(
    status           = "",
    finalData        = NULL,
    done             = FALSE,
    useExample       = FALSE,
    pipelineRunning  = FALSE,
    spiritFolder     = NULL,
    runId            = NULL
  )
    output$dl_xlsx <- downloadHandler(
    filename = function() {
      id <- rv$runId %||% format(Sys.time(), "%Y%m%dT%H%M%S")
      paste0("SPIRIT_results_", id, ".xlsx")
    },
    content = function(file) {
      req(rv$spiritFolder)
      xlsx_path <- file.path(rv$spiritFolder, "data", "combined_tables.xlsx")
      if (file.exists(xlsx_path)) {
        file.copy(xlsx_path, file, overwrite = TRUE)
      } else {
        # fallback to on-the-fly export
        if (!requireNamespace("writexl", quietly = TRUE)) {
          stop("Package 'writexl' is not installed. Install with install.packages('writexl').")
        }
        writexl::write_xlsx(as.data.frame(rv$finalData), path = file)
      }
    }
  )

    output$dl_tsv <- downloadHandler(
        filename = function() {
        id <- rv$runId %||% format(Sys.time(), "%Y%m%dT%H%M%S")
        paste0("SPIRIT_results_", id, ".tsv")
        },
        content = function(file) {
        req(rv$spiritFolder)
        tsv_path <- file.path(rv$spiritFolder, "data", "combined_tables.tsv")
        if (file.exists(tsv_path)) {
            file.copy(tsv_path, file, overwrite = TRUE)
        } else {
            # fallback to on-the-fly export
            readr::write_tsv(rv$finalData, file)
        }
        }
    )





  # Define outputs IMMEDIATELY so conditionalPanels can react on refresh
  output$pipelineRunning <- reactive({ rv$pipelineRunning })
  outputOptions(output, "pipelineRunning", suspendWhenHidden = FALSE)
  output$showResults <- reactive({ rv$done })
  outputOptions(output, "showResults", suspendWhenHidden = FALSE)

  jobs_dir <- normalizePath(file.path(".", "data", "jobs"), mustWork = FALSE)
  if (!dir.exists(jobs_dir)) dir.create(jobs_dir, recursive = TRUE)

  read_job <- function(runId) {
    jf <- file.path(jobs_dir, paste0(runId, ".json"))
    if (!file.exists(jf)) return(NULL)
    jsonlite::fromJSON(jf, simplifyVector = TRUE)
  }
  write_job <- function(runId, obj) {
    jf <- file.path(jobs_dir, paste0(runId, ".json"))
    jsonlite::write_json(obj, jf, auto_unbox = TRUE, pretty = FALSE)
  }

  # --- URL recovery AFTER session starts (runs once) ---
  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search %||% "")
    rid <- query$runId
    if (!is.null(rid) && nzchar(rid)) {
      rv$runId <- rid
      # Optimistic: show running overlay immediately; we'll confirm via ticket
      rv$pipelineRunning <- TRUE
      rv$done <- FALSE
      shinyjs::hide("sidebarPanelID")

      job <- read_job(rv$runId)
      if (!is.null(job)) {
        if (identical(job$status, "running")) {
          rv$status <- "SPIRIT pipeline is running (recovered)."
          output$runStatus <- renderText(rv$status)
        } else if (identical(job$status, "done")) {
          rv$pipelineRunning <- FALSE
          rv$done <- TRUE
          if (!is.null(job$folder)) {
            rv$spiritFolder <- job$folder
            finalTsv <- file.path(rv$spiritFolder, "data", "combined_tables.tsv")
            if (file.exists(finalTsv)) {
              rv$finalData <- readr::read_tsv(finalTsv)
            }
          }
          rv$status <- paste("Restored finished run:", rv$spiritFolder %||% "")
          output$runStatus <- renderText(rv$status)
        } else if (identical(job$status, "failed")) {
          rv$pipelineRunning <- FALSE
          rv$done <- TRUE
          rv$status <- "SPIRIT pipeline failed (recovered)."
          output$runStatus <- renderText(rv$status)
        }
      } else {
        # No ticket found yet; keep overlay and let poller resolve it
        rv$status <- "Reconnecting to run..."
        output$runStatus <- renderText(rv$status)
      }
    }
  }, once = TRUE)

  # --- Poll the ticket every 2s while we have a runId ---
  observe({
    if (is.null(rv$runId)) return(invisible())
    invalidateLater(2000, session)

    jf <- file.path(jobs_dir, paste0(rv$runId, ".json"))
    if (!file.exists(jf)) return(invisible())

    job <- read_job(rv$runId)
    if (is.null(job)) return(invisible())

    if (identical(job$status, "done") && !rv$done) {
      rv$pipelineRunning <- FALSE
      rv$done <- TRUE
      if (!is.null(job$folder)) {
        rv$spiritFolder <- job$folder
        finalTsv <- file.path(rv$spiritFolder, "data", "combined_tables.tsv")
        if (file.exists(finalTsv)) {
          rv$finalData <- readr::read_tsv(finalTsv)
        }
      }
      rv$status <- paste("SPIRIT pipeline complete! Results in", rv$spiritFolder %||% "")
      output$runStatus <- renderText(rv$status)
    }

    if (identical(job$status, "failed") && rv$pipelineRunning) {
      rv$pipelineRunning <- FALSE
      rv$done <- TRUE
      rv$status <- "SPIRIT pipeline failed."
      output$runStatus <- renderText(rv$status)
    }
  })

  defaultDir <- normalizePath(file.path(".", "data", "default"))

  # ---- Dynamic sRNA choices based on organism ----
  output$srnaChoiceUI <- renderUI({
    if (identical(input$organismChoice, "sl1344")) {
      selectInput("srnaChoice", "sRNA", choices = c("PinT" = "pint", "Own files" = "own"), selected = "pint")
    } else if (identical(input$organismChoice, "btheta")) {
      selectInput("srnaChoice", "sRNA", choices = c("MasB" = "masb", "Own files" = "own"), selected = "masb")
    } else {
      selectInput("srnaChoice", "sRNA", choices = c("Own files" = "own"), selected = "own")
    }
  })

  # ---- Dataset checkboxes: default-checked by organism+sRNA ----
  output$datasetChoiceUI <- renderUI({
    org <- input$organismChoice
    srn <- input$srnaChoice
    if (identical(org, "sl1344") && identical(srn, "pint")) {
      checkboxGroupInput(
        "defaultDatasets", "Default datasets (Salmonella + PinT):",
        choices  = c("MAPS" = "maps", "SPI1_Pulse" = "pulse"),
        selected = c("maps", "pulse"),
        inline   = FALSE
      )
    } else if (identical(org, "btheta") && identical(srn, "masb")) {
      checkboxGroupInput(
        "defaultDatasets", "Default datasets (B. thetaiotaomicron + MasB):",
        choices  = c("MAPS" = "maps", "RNAseq" = "rnaseq"),
        selected = c("maps", "rnaseq"),
        inline   = FALSE
      )
    } else {
      return(NULL)
    }
  })

  # ---- Default downloads for preset organisms ----
  output$downloadFasta <- downloadHandler(
    filename = function() {
      if (identical(input$organismChoice, "sl1344")) "SL1344_reference.fa"
      else if (identical(input$organismChoice, "btheta")) "B_theta_reference.fa"
      else "reference.fa"
    },
    content = function(file) {
      src <- if (identical(input$organismChoice, "sl1344")) {
        file.path(defaultDir, "FQ312003_wplasmids.fa")
      } else if (identical(input$organismChoice, "btheta")) {
        file.path(defaultDir, "B_theta_genome_and_plasmid.fa")
      } else { "" }
      req(file.exists(src))
      file.copy(src, file, overwrite = TRUE)
    }
  )
  output$downloadGff <- downloadHandler(
    filename = function() {
      if (identical(input$organismChoice, "sl1344")) "SL1344_annotation.gff3"
      else if (identical(input$organismChoice, "btheta")) "B_theta_annotation.gff3"
      else "annotation.gff3"
    },
    content = function(file) {
      src <- if (identical(input$organismChoice, "sl1344")) {
        file.path(defaultDir, "FQ312003.1_srnas_plasmids.gff")
      } else if (identical(input$organismChoice, "btheta")) {
        file.path(defaultDir, "B_theta_annotation_210224.gff")
      } else { "" }
      req(file.exists(src))
      file.copy(src, file, overwrite = TRUE)
    }
  )

  # ---- Bookmarking: save/restore minimal state ----
  onBookmark(function(state) {
    state$values$spiritFolder <- rv$spiritFolder
    state$values$done <- rv$done
    state$values$useExample <- rv$useExample
    state$values$idCol <- input$idCol
    state$values$xAxisCol <- input$xAxisCol
    state$values$yAxisCol <- input$yAxisCol
    state$values$geneHoverCol <- input$geneHoverCol
  })
  onBookmarked(function(url) {
    updateQueryString(gsub("^[^?]*", "", url), mode = "replace")
    output$bookmark_ui <- renderUI({ tags$a(href = url, target = "_blank", url) })
  })
  onRestored(function(state) {
    if (!is.null(state$values$spiritFolder)) {
      rv$spiritFolder <- state$values$spiritFolder
      finalTsv <- file.path(rv$spiritFolder, "data", "combined_tables.tsv")
      if (file.exists(finalTsv)) {
        rv$finalData <- readr::read_tsv(finalTsv)
        rv$done <- TRUE
        rv$pipelineRunning <- FALSE
        rv$status <- paste("Restored result from", rv$spiritFolder)
        output$runStatus <- renderText(rv$status)
      }
    }
    if (!is.null(state$values$xAxisCol) && !is.null(rv$finalData)) {
      numericCols <- names(rv$finalData)[sapply(rv$finalData, is.numeric)]
      updateSelectInput(session, "xAxisCol", choices = numericCols, selected = state$values$xAxisCol)
      updateSelectInput(session, "yAxisCol", choices = numericCols, selected = state$values$yAxisCol)
      updateSelectInput(session, "geneHoverCol", choices = names(rv$finalData), selected = state$values$geneHoverCol)
    }
  })

  # ---- Autofill: SL1344 + PinT + idCol='gene' (datasets auto-default from UI) ----
  observeEvent(input$exampleDataBtn, {
    updateSelectInput(session, "organismChoice", selected = "sl1344")
    updateSelectInput(session, "srnaChoice",     selected = "pint")
    updateTextInput(session, "idCol", value = "gene")
    rv$status <- "Autofill: Salmonella + PinT; Gene ID set to 'gene'."
    output$runStatus <- renderText(rv$status)
  })

    observeEvent(input$runBtn, {
    shinyjs::hide("sidebarPanelID")
    rv$pipelineRunning <- TRUE
    rv$done <- FALSE

    # create a stable run id and ticket
    rv$runId <- format(Sys.time(), "%Y%m%dT%H%M%S")
    outDir <- normalizePath(file.path(".", "data"))
    write_job(rv$runId, list(
      status  = "running",
      started = as.character(Sys.time()),
      outDir  = outDir
    ))
    # put runId in the browser URL so a refresh can recover state
    updateQueryString(paste0("?runId=", rv$runId), mode = "push")

    rv$status <- "Starting SPIRIT pipeline..."
    output$runStatus <- renderText(rv$status)

    defaultDir <- normalizePath(file.path(".", "data", "default"))

    # ---------- FAST-PATH: precomputed Salmonella + PinT + (MAPS & Pulse), no uploads ----------
    is_default_pint <- isTRUE(input$organismChoice == "sl1344") &&
                       isTRUE(input$srnaChoice == "pint") &&
                       !is.null(input$defaultDatasets) &&
                       setequal(input$defaultDatasets, c("maps","pulse")) &&
                       is.null(input$fasta) && is.null(input$gff) && is.null(input$srna) &&
                       is.null(input$csv1) && is.null(input$csv2) &&
                       is.null(input$csv3) && is.null(input$csv4)

    if (is_default_pint) {
      # Point to the precomputed folder
      pre_folder <- normalizePath(file.path(".", "data", "default_result_pinT"), mustWork = FALSE)
      finalTsv   <- file.path(pre_folder, "data", "combined_tables.tsv")

      if (dir.exists(pre_folder) && file.exists(finalTsv)) {
        rv$spiritFolder <- pre_folder
        rv$finalData    <- readr::read_tsv(finalTsv, show_col_types = FALSE)
        rv$status       <- sprintf("Loaded precomputed results: %s", pre_folder)

        # mark ticket as done so refresh works
        write_job(rv$runId, list(
          status   = "done",
          finished = as.character(Sys.time()),
          outDir   = outDir,
          folder   = pre_folder
        ))

        rv$done <- TRUE
        rv$pipelineRunning <- FALSE
        output$runStatus <- renderText(rv$status)
        session$doBookmark()  # generate shareable URL
        return(invisible(NULL))
      } else {
        # fall back to full run if folder/file missing
        message("Precomputed folder not found or incomplete: ", pre_folder, " — falling back to full run.")
      }
    }

    # ------------------ NORMAL PATH: build inputs/paths and run SPIRIT.sh ------------------
    # FASTA/GFF
    if (identical(input$organismChoice, "btheta")) {
      fastaPath <- file.path(defaultDir, "B_theta_genome_and_plasmid.fa")
      gffPath   <- file.path(defaultDir, "B_theta_annotation_210224.gff")
    } else if (identical(input$organismChoice, "sl1344")) {
      fastaPath <- file.path(defaultDir, "FQ312003_wplasmids.fa")
      gffPath   <- file.path(defaultDir, "FQ312003.1_srnas_plasmids.gff")
    } else {
      req(input$fasta, input$gff)
      inputDir <- tempdir()
      fastaPath <- file.path(inputDir, input$fasta$name); file.copy(input$fasta$datapath, fastaPath, overwrite = TRUE)
      gffPath   <- file.path(inputDir, input$gff$name);   file.copy(input$gff$datapath, gffPath, overwrite = TRUE)
    }

    # sRNA
    if (identical(input$srnaChoice, "pint")) {
      srnaPath <- file.path(defaultDir, "pinT.fasta")
    } else if (identical(input$srnaChoice, "masb")) {
      srnaPath <- file.path(defaultDir, "MasB.fasta")
    } else {
      req(input$srna)
      inputDir2 <- if (exists("inputDir")) inputDir else tempdir()
      srnaPath  <- file.path(inputDir2, input$srna$name); file.copy(input$srna$datapath, srnaPath, overwrite = TRUE)
    }

    # Default datasets
    csv1Path <- ""; csv2Path <- ""; csv3Path <- ""; csv4Path <- ""
    if (!is.null(input$defaultDatasets)) {
      if (identical(input$organismChoice, "sl1344") && identical(input$srnaChoice, "pint")) {
        if ("maps"  %in% input$defaultDatasets) csv1Path <- file.path(defaultDir, "spi1_maps_default.csv")
        if ("pulse" %in% input$defaultDatasets) {
          if (nzchar(csv1Path)) csv2Path <- file.path(defaultDir, "spi1_pulse_default.csv") else csv1Path <- file.path(defaultDir, "spi1_pulse_default.csv")
        }
      } else if (identical(input$organismChoice, "btheta") && identical(input$srnaChoice, "masb")) {
        if ("maps"   %in% input$defaultDatasets) csv1Path <- file.path(defaultDir, "MAPS_BTnc201.csv")
        if ("rnaseq" %in% input$defaultDatasets) {
          if (nzchar(csv1Path)) csv2Path <- file.path(defaultDir, "RNASEQ_BT_DE_analysis.xlsx") else csv1Path <- file.path(defaultDir, "RNASEQ_BT_DE_analysis.xlsx")
        }
      }
    }

    # Prefer user uploads if present
    inputDir3 <- if (exists("inputDir")) inputDir else tempdir()
    if (!nzchar(csv1Path) && !is.null(input$csv1)) { csv1Path <- file.path(inputDir3, input$csv1$name); file.copy(input$csv1$datapath, csv1Path, overwrite = TRUE) }
    if (!nzchar(csv2Path) && !is.null(input$csv2)) { csv2Path <- file.path(inputDir3, input$csv2$name); file.copy(input$csv2$datapath, csv2Path, overwrite = TRUE) }
    if (!nzchar(csv3Path) && !is.null(input$csv3)) { csv3Path <- file.path(inputDir3, input$csv3$name); file.copy(input$csv3$datapath, csv3Path, overwrite = TRUE) }
    if (!nzchar(csv4Path) && !is.null(input$csv4)) { csv4Path <- file.path(inputDir3, input$csv4$name); file.copy(input$csv4$datapath, csv4Path, overwrite = TRUE) }

    # Require at least one dataset
    req(nzchar(csv1Path))

    cmd <- build_spirit_cmd(
      fastaPath, gffPath, srnaPath,
      csv1Path, csv2Path, csv3Path, csv4Path,
      idCol = input$idCol, weights = input$weights, outDir = outDir
    )

    rv$status <- paste("Running SPIRIT pipeline with command:", cmd)
    output$runStatus <- renderText(rv$status)
    message(rv$status)

    # ---- Run SPIRIT in background ----
    future({
      exit_code <- system(cmd)
      list(exit_code = exit_code)
    }) %...>% (function(res) {
      # FAILURE
      if (is.null(res) || res$exit_code != 0) {
        write_job(rv$runId, list(
          status   = "failed",
          finished = as.character(Sys.time()),
          outDir   = outDir
        ))
        rv$status <- paste("SPIRIT pipeline failed with exit code", res$exit_code %||% "unknown")
        rv$finalData <- NULL
        rv$done <- TRUE
        rv$pipelineRunning <- FALSE
        output$runStatus <- renderText(rv$status)
        return(NULL)
      }

      # SUCCESS: Find newest timestamped output folder
      subdirs <- list.dirs(outDir, recursive = FALSE)
      spiritFolder <- subdirs[grepl("\\d{4}_\\d{2}_\\d{2}_T_", basename(subdirs))]
      if (length(spiritFolder) == 0) {
        write_job(rv$runId, list(
          status   = "failed",
          finished = as.character(Sys.time()),
          outDir   = outDir
        ))
        rv$status <- "SPIRIT finished but no output folder found."
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
        rv$finalData <- readr::read_tsv(finalTsv, show_col_types = FALSE)
        rv$status <- paste("SPIRIT pipeline complete! Results in", spiritFolder)
      } else {
        rv$finalData <- NULL
        rv$status <- "SPIRIT finished but no combined_tables.tsv found."
      }

      # write 'done' ticket so a refreshed page can flip to results
      write_job(rv$runId, list(
        status   = "done",
        finished = as.character(Sys.time()),
        outDir   = outDir,
        folder   = spiritFolder
      ))

      rv$done <- TRUE
      rv$pipelineRunning <- FALSE
      output$runStatus <- renderText(rv$status)
      session$doBookmark()  # generate shareable URL
      invisible(NULL)
    }) %...!% (function(e) {
      write_job(rv$runId, list(
        status   = "failed",
        finished = as.character(Sys.time()),
        outDir   = outDir
      ))
      rv$status <- paste("Error running SPIRIT:", e$message)
      rv$finalData <- NULL
      rv$done <- TRUE
      rv$pipelineRunning <- FALSE
      output$runStatus <- renderText(rv$status)
    })

    if (nzchar(input$email) && !is.null(rv$finalData)) {
      message("Would send email to: ", input$email)
    }
  })


  # Populate plot option dropdowns after pipeline finishes
  observeEvent(rv$done, {
    req(rv$finalData)
    numericCols <- names(rv$finalData)[sapply(rv$finalData, is.numeric)]
    if (length(numericCols) < 2) return(invisible(NULL))

    defaultX <- if ("IntaRNA_p_value" %in% numericCols) "IntaRNA_p_value" else numericCols[1]
    defaultY <- if ("fisher_p_value" %in% numericCols) "fisher_p_value" else numericCols[2]

    updateSelectInput(session, "xAxisCol", choices = numericCols, selected = defaultX)
    updateSelectInput(session, "yAxisCol", choices = numericCols, selected = defaultY)
    updateSelectInput(session, "geneHoverCol", choices = names(rv$finalData), selected = input$idCol)

    shinyjs::show("sidebarPanelID")
  })

  # Final table & plot
  output$finalTable <- renderDataTable({
    req(rv$done, rv$finalData)
    datatable(rv$finalData, options = list(pageLength = 10))
  })

  output$plotResult <- renderPlotly({
    req(rv$done, rv$finalData, input$xAxisCol, input$yAxisCol, input$geneHoverCol)
    df <- rv$finalData
    shiny::validate(
      shiny::need(input$xAxisCol %in% names(df), "X-axis col not found in data"),
      shiny::need(input$yAxisCol %in% names(df), "Y-axis col not found in data"),
      shiny::need(is.numeric(df[[input$xAxisCol]]), "X-axis col must be numeric"),
      shiny::need(is.numeric(df[[input$yAxisCol]]), "Y-axis col must be numeric")
    )

    p <- ggplot(df, aes(x = -log10(.data[[input$xAxisCol]]),
                        y = -log10(.data[[input$yAxisCol]]),
                        text = .data[[input$geneHoverCol]])) +
      geom_point(alpha = 0.5, colour = "steelblue") +
      theme_bw() +
      labs(
        title = paste("Scatter of", input$xAxisCol, "vs.", input$yAxisCol, "(both -log10)"),
        x = paste0("-log10(", input$xAxisCol, ")"),
        y = paste0("-log10(", input$yAxisCol, ")")
      )
    if (!is.na(input$xMax)) p <- p + scale_x_continuous(limits = c(0, input$xMax))
    if (!is.na(input$yMax)) p <- p + scale_y_continuous(limits = c(0, input$yMax))

    ggplotly(p, tooltip = c("text", "x", "y"))
  })
}

shinyApp(ui = ui, server = server)
