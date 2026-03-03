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
library(htmltools)

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
  navbarPage(
    title = "",
    id = "mainNav",
    theme = shinytheme("sandstone"),
    header = tagList(
      useShinyjs(),
      tags$head(
        tags$style(HTML("
          /* Navbar overrides: dark-blue background, white text */
          .navbar-default {
            background-color: #002864 !important;
            border-color: #002864 !important;
            position: relative;
          }
          /* Hide the empty brand text */
          .navbar-default .navbar-brand { display: none !important; }
          /* SPIRIT tab left, Help/Contact right */
          .navbar-default .navbar-collapse {
            display: flex !important;
            padding: 0 !important;
          }
          .navbar-default .navbar-nav {
            display: flex !important;
            width: 100% !important;
            float: none !important;
            margin: 0 !important;
          }
          .navbar-default .navbar-nav > li:nth-child(2) {
            margin-left: auto !important;
          }
          .navbar-default .navbar-nav > li > a {
            color: #ffffff !important; font-weight: 600;
          }
          .navbar-default .navbar-nav > li > a:hover,
          .navbar-default .navbar-nav > li > a:focus {
            color: #ccdcf0 !important;
            background-color: transparent !important;
          }
          .navbar-default .navbar-nav > .active > a,
          .navbar-default .navbar-nav > .active > a:hover,
          .navbar-default .navbar-nav > .active > a:focus {
            color: #ffffff !important;
            background-color: #001e4d !important;
          }
          .navbar-default .navbar-toggle { border-color: #ffffff; }
          .navbar-default .navbar-toggle .icon-bar { background-color: #ffffff; }
          .navbar-default .navbar-toggle:hover,
          .navbar-default .navbar-toggle:focus {
            background-color: #001e4d !important;
          }

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
          .intro-logo { max-width: 60%; height: auto; margin-bottom: 16px; }
          .intro-title { font-size: 24px; font-weight: 700; margin-top: 8px; }
          .intro-sub { color:#555; margin-top: 4px; }
          .intro-list { text-align: left; display: inline-block; margin-top: 10px; }
          .intro-credits { margin-top: 14px; font-weight: 600; }

          /* Full-page overlay while running */
          .running-overlay { position: fixed; inset: 0; z-index: 9999; display: flex; align-items: center; justify-content: center; }
          .running-text { color: #c62828; font-weight: 800; text-align: center; font-size: clamp(26px, 4vw, 42px); line-height: 1.25; padding: 0 24px; }

          /* Help & Contact page styling */
          .help-container, .contact-container {
            max-width: 860px; margin: 30px auto; padding: 0 20px;
          }
          .help-container h3, .contact-container h3 {
            color: #002864; border-bottom: 2px solid #002864;
            padding-bottom: 6px; margin-top: 28px;
          }
        ")),
        tags$script(HTML("
          window.addEventListener('popstate', function(e) {
            Shiny.setInputValue('browserBack', Math.random());
          });
        "))
      )
    ),
    footer = div(
      class = "footer-bar",
      tags$a(href = "https://www.bayresq.net", target = "_blank", rel = "noopener",
             tags$img(src = "spiritres/bayresq.png", class = "footer-logo", alt = "BayResQ")),
      tags$a(href = "https://www.helmholtz-hiri.de", target = "_blank", rel = "noopener",
             tags$img(src = "spiritres/hirilogo.svg", class = "footer-logo", alt = "HIRI")),
      tags$a(href = "https://www.uni-wuerzburg.de", target = "_blank", rel = "noopener",
             tags$img(src = "spiritres/uniwue.png", class = "footer-logo", alt = "Universität Würzburg"))
    ),

    # ---- SPIRIT tab (main app) ----
    tabPanel("SPIRIT",
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
          selectInput("xAxisCol", "X-axis Column:",
                      choices = NULL, selected = NULL),
          radioButtons("xScale", "X-axis scale:",
                       choices = c("-log10", "log", "raw"),
                       selected = "-log10", inline = TRUE),
          selectInput("yAxisCol", "Y-axis Column:",
                      choices = NULL, selected = NULL),
          radioButtons("yScale", "Y-axis scale:",
                       choices = c("-log10", "log", "raw"),
                       selected = "-log10", inline = TRUE),
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
                  div(style = "font-size: clamp(18px, 2.5vw, 28px);",
                      HTML("SPIRIT pipeline is running (est. time 2&ndash;10 minutes).<br>Please do not refresh")),
                  tags$br(),
                  div(style = "font-size: 15px; font-weight: 400; color: #333; margin-top: 16px;",
                      p("You can safely close this tab and come back later using this link:"),
                      uiOutput("runningLink"),
                      p(style = "margin-top: 8px; font-size: 13px; color: #666;",
                        "Bookmark or copy this URL to check your results later")
                  ),
                  tags$br(),
                  actionButton("cancelBtn", "Cancel Pipeline",
                               style = "font-size: 16px; padding: 10px 28px; background-color: #c62828 !important; border-color: #c62828 !important;")
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
            downloadButton("dl_xlsx", "Download Excel (.xlsx)"),
            downloadButton("dl_html", "Download Interactive Report (.html)")
          ),
          h4("Final Combined Table"),
          dataTableOutput("finalTable")
        )
      )
    )
    ),

    # ---- Help tab ----
    tabPanel("Help",
      div(class = "help-container",
        h2("How to Use SPIRIT"),
        h3("1. Select an Organism"),
        p("Choose a pre-loaded organism (Salmonella SL1344 or B. thetaiotaomicron) from the dropdown,",
          "or select \"Own files\" to upload your own genome FASTA and GFF annotation."),
        h3("2. Select an sRNA"),
        p("For pre-loaded organisms a default sRNA is available (e.g. PinT for Salmonella, MasB for B. theta).",
          "Select \"Own files\" to upload your own sRNA FASTA."),
        h3("3. Upload Experiment Tables"),
        p("Upload one or more CSV or Excel files containing experimental evidence.",
          "Each file must include a gene identifier column (matching the GFF) and a column named",
          tags$code("p_value"), "."),
        p("For pre-loaded organisms, default datasets are available and pre-selected."),
        h3("4. Configure and Run"),
        tags$ul(
          tags$li(tags$b("Gene identifier column:"),
                  " set to the column name used in your experiment tables and GFF (default: locus_tag)."),
          tags$li(tags$b("Fisher weights:"),
                  " optionally provide comma-separated weights for the weighted Fisher's method."),
          tags$li("Click ", tags$b("Run SPIRIT"), " to start the pipeline. Estimated run time: 2\u201310 minutes.")
        ),
        h3("5. View Results"),
        p("After the pipeline completes, an interactive scatter plot and a downloadable results table are shown.",
          "Use the sidebar to change plot axes. Download results as TSV, Excel, or an interactive HTML report."),
        tags$hr(),
        h2("Pipeline Methods"),
        h3("IntaRNA Interaction Prediction"),
        p("SPIRIT uses ",
          tags$a(href = "https://github.com/BackofenLab/IntaRNA", target = "_blank", "IntaRNA"),
          " to predict sRNA\u2013mRNA interactions based on minimum free energy (MFE) of hybridisation.",
          "Statistical significance is assessed by comparing real MFEs against a Gumbel distribution fitted to shuffled-sequence controls."),
        h3("Fisher's Method"),
        p("P-values from each evidence source (IntaRNA + experiment tables) are combined using Fisher's method",
          "(optionally weighted). The test statistic is: ",
          tags$code("-2 * sum(w_i * log(p_i))"), "."),
        h3("Stouffer's Method"),
        p("As an alternative, Stouffer's Z-method converts each p-value to a Z-score and combines them: ",
          tags$code("Z = sum(w_i * qnorm(1 - p_i)) / sqrt(sum(w_i^2))"), "."),
        h3("FDR Correction"),
        p("Both Fisher and Stouffer combined p-values are adjusted for multiple testing",
          "using the Benjamini\u2013Hochberg procedure.")
      )
    ),

    # ---- Contact tab ----
    tabPanel("Contact",
      div(class = "contact-container",
        h2("Contact"),
        h3("Authors"),
        tags$ul(
          tags$li(tags$b("Hoda Kooshapour"),
                  " \u2013 Helmholtz Institute for RNA-based Infection Research (HIRI)"),
          tags$li(tags$b("Jakob J. Jung"),
                  " \u2013 Helmholtz Institute for RNA-based Infection Research (HIRI)")
        ),
        h3("Source Code"),
        p(tags$a(href = "https://github.com/jakobjung/spirit_server", target = "_blank",
                 "https://github.com/jakobjung/spirit_server")),
        h3("Institutions"),
        tags$ul(
          tags$li(tags$a(href = "https://www.helmholtz-hiri.de", target = "_blank",
                         "Helmholtz Institute for RNA-based Infection Research (HIRI)")),
          tags$li(tags$a(href = "https://www.uni-wuerzburg.de", target = "_blank",
                         "Julius-Maximilians-Universit\u00e4t W\u00fcrzburg")),
          tags$li(tags$a(href = "https://www.bayresq.net", target = "_blank",
                         "BayResq \u2013 Bavarian Research Network"))
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
    runId            = NULL,
    pidFile          = NULL,
    cancelled        = FALSE
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

    output$dl_html <- downloadHandler(
        filename = function() {
            id <- rv$runId %||% format(Sys.time(), "%Y%m%dT%H%M%S")
            paste0("SPIRIT_report_", id, ".html")
        },
        content = function(file) {
            req(rv$finalData, input$xAxisCol, input$yAxisCol, input$geneHoverCol)
            df <- rv$finalData
            numericCols <- names(df)[sapply(df, is.numeric)]
            allCols <- names(df)

            # Build initial plotly figure (also bundles plotly.js dependency)
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
            plot_widget <- ggplotly(p, tooltip = c("text", "x", "y"))

            # Build the DT table (same as app)
            table_widget <- DT::datatable(df, options = list(pageLength = 10))

            # Embed full dataset as JSON for JS axis switching
            data_json <- jsonlite::toJSON(
                lapply(df, function(col) {
                    if (is.numeric(col)) col else as.character(col)
                }),
                auto_unbox = FALSE
            )

            # Build dropdown options HTML
            make_options <- function(choices, selected) {
                paste(sapply(choices, function(ch) {
                    sel <- if (identical(ch, selected)) ' selected' else ''
                    sprintf('<option value="%s"%s>%s</option>', ch, sel, ch)
                }), collapse = "\n")
            }

            controls_html <- sprintf('
                <div style="display:flex; gap:16px; flex-wrap:wrap; align-items:center; margin-bottom:12px;">
                    <label>X-axis: <select id="xAxisSelect">%s</select></label>
                    <label>Y-axis: <select id="yAxisSelect">%s</select></label>
                    <label>Hover text: <select id="hoverSelect">%s</select></label>
                </div>',
                make_options(numericCols, input$xAxisCol),
                make_options(numericCols, input$yAxisCol),
                make_options(allCols, input$geneHoverCol)
            )

            # JavaScript to re-plot on dropdown change
            js_code <- '
            <script id="spirit-data" type="application/json">%s</script>
            <script>
            document.addEventListener("DOMContentLoaded", function() {
                var data = JSON.parse(document.getElementById("spirit-data").textContent);
                var plotEl = document.querySelector(".plotly.html-widget");
                if (!plotEl) return;

                var xSel = document.getElementById("xAxisSelect");
                var ySel = document.getElementById("yAxisSelect");
                var hSel = document.getElementById("hoverSelect");

                function updatePlot() {
                    var xCol = xSel.value;
                    var yCol = ySel.value;
                    var hCol = hSel.value;
                    var xRaw = data[xCol];
                    var yRaw = data[yCol];
                    var hRaw = data[hCol];
                    if (!xRaw || !yRaw) return;

                    var x = xRaw.map(function(v) { return v > 0 ? -Math.log10(v) : 0; });
                    var y = yRaw.map(function(v) { return v > 0 ? -Math.log10(v) : 0; });
                    var text = hRaw ? hRaw.map(String) : x.map(function(_, i) { return String(i); });

                    Plotly.react(plotEl, [{
                        x: x, y: y, text: text,
                        mode: "markers",
                        type: "scatter",
                        marker: { color: "steelblue", opacity: 0.5 },
                        hovertemplate: "%%{text}<br>x: %%{x:.3f}<br>y: %%{y:.3f}<extra></extra>"
                    }], {
                        title: "Scatter of " + xCol + " vs. " + yCol + " (both -log10)",
                        xaxis: { title: "-log10(" + xCol + ")" },
                        yaxis: { title: "-log10(" + yCol + ")" }
                    });
                }

                xSel.addEventListener("change", updatePlot);
                ySel.addEventListener("change", updatePlot);
                hSel.addEventListener("change", updatePlot);
            });
            </script>'
            js_code <- sprintf(js_code, data_json)

            # Combine into one page
            html <- htmltools::tagList(
                tags$style(HTML("
                    body { font-family: sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }
                    .header { background-color: #002864; color: white; padding: 20px; text-align: center;
                              font-size: 22px; font-weight: bold; border-radius: 6px; margin-bottom: 24px; }
                    h3 { margin-top: 30px; }
                    label { font-weight: 600; }
                    select { padding: 4px 8px; border-radius: 4px; border: 1px solid #ccc; }
                ")),
                div(class = "header", "SPIRIT - Results Report"),
                h3("Interactive Plot"),
                HTML(controls_html),
                plot_widget,
                HTML(js_code),
                h3("Results Table"),
                table_widget
            )

            # Save to temp dir with dependencies in a lib folder
            tmp_dir <- file.path(tempdir(), paste0("spirit_report_", Sys.getpid()))
            on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
            dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
            tmp_html <- file.path(tmp_dir, "report.html")
            lib_dir <- file.path(tmp_dir, "lib")
            htmltools::save_html(html, file = tmp_html, libdir = lib_dir)

            # Make self-contained (inline all JS/CSS) using pandoc
            if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
                rmarkdown::pandoc_self_contained_html(tmp_html, file)
            } else {
                file.copy(tmp_html, file, overwrite = TRUE)
            }
        }
    )




  # Define outputs IMMEDIATELY so conditionalPanels can react on refresh
  output$pipelineRunning <- reactive({ rv$pipelineRunning })
  outputOptions(output, "pipelineRunning", suspendWhenHidden = FALSE)
  output$showResults <- reactive({ rv$done })
  outputOptions(output, "showResults", suspendWhenHidden = FALSE)

  output$runningLink <- renderUI({
    req(rv$runId, rv$pipelineRunning)
    port <- session$clientData$url_port
    host <- session$clientData$url_hostname
    protocol <- session$clientData$url_protocol
    url <- paste0(protocol, "//", host, ":", port, "/?runId=", rv$runId)
    tags$a(href = url, target = "_blank",
           style = "color: #002864; font-weight: 600; word-break: break-all;",
           url)
  })

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

    # ---------- FAST-PATH: precomputed B. theta + MasB + (MAPS & RNAseq), no uploads ----------
    is_default_masb <- isTRUE(input$organismChoice == "btheta") &&
                       isTRUE(input$srnaChoice == "masb") &&
                       !is.null(input$defaultDatasets) &&
                       setequal(input$defaultDatasets, c("maps","rnaseq")) &&
                       is.null(input$fasta) && is.null(input$gff) && is.null(input$srna) &&
                       is.null(input$csv1) && is.null(input$csv2) &&
                       is.null(input$csv3) && is.null(input$csv4)

    if (is_default_masb) {
      pre_folder <- normalizePath(file.path(".", "data", "default_result_masB"), mustWork = FALSE)
      finalTsv   <- file.path(pre_folder, "data", "combined_tables.tsv")

      if (dir.exists(pre_folder) && file.exists(finalTsv)) {
        rv$spiritFolder <- pre_folder
        rv$finalData    <- readr::read_tsv(finalTsv, show_col_types = FALSE)
        rv$status       <- sprintf("Loaded precomputed results: %s", pre_folder)

        write_job(rv$runId, list(
          status   = "done",
          finished = as.character(Sys.time()),
          outDir   = outDir,
          folder   = pre_folder
        ))

        rv$done <- TRUE
        rv$pipelineRunning <- FALSE
        output$runStatus <- renderText(rv$status)
        session$doBookmark()
        return(invisible(NULL))
      } else {
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

    # Add user uploads into the next available slot(s)
    inputDir3 <- if (exists("inputDir")) inputDir else tempdir()
    csvPaths <- c(csv1Path, csv2Path, csv3Path, csv4Path)
    userInputs <- list(input$csv1, input$csv2, input$csv3, input$csv4)
    for (ui in userInputs) {
      if (!is.null(ui)) {
        idx <- which(!nzchar(csvPaths))[1]
        if (!is.na(idx)) {
          dest <- file.path(inputDir3, ui$name)
          file.copy(ui$datapath, dest, overwrite = TRUE)
          csvPaths[idx] <- dest
        }
      }
    }
    csv1Path <- csvPaths[1]; csv2Path <- csvPaths[2]; csv3Path <- csvPaths[3]; csv4Path <- csvPaths[4]

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
    pid_file <- file.path(tempdir(), paste0(".spirit_pid_", rv$runId))
    rv$pidFile <- pid_file
    rv$cancelled <- FALSE
    wrapped_cmd <- paste0("echo $$ > ", shQuote(pid_file), " && ", cmd)
    future({
      exit_code <- system(wrapped_cmd)
      list(exit_code = exit_code)
    }) %...>% (function(res) {
      if (isTRUE(rv$cancelled)) return(invisible(NULL))
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
      if (isTRUE(rv$cancelled)) return(invisible(NULL))
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

  # ---- Cancel pipeline helper ----
  cancel_pipeline <- function() {
    if (!rv$pipelineRunning) return()
    rv$cancelled <- TRUE

    # Kill the background process
    if (!is.null(rv$pidFile) && file.exists(rv$pidFile)) {
      tryCatch({
        pid <- as.integer(trimws(readLines(rv$pidFile, n = 1, warn = FALSE)))
        if (!is.na(pid)) {
          system(paste("pkill -TERM -P", pid, "2>/dev/null; kill -TERM", pid, "2>/dev/null"),
                 wait = FALSE)
        }
      }, error = function(e) message("Could not kill pipeline process: ", e$message))
      unlink(rv$pidFile)
    }

    # Update job ticket
    if (!is.null(rv$runId)) {
      write_job(rv$runId, list(
        status   = "cancelled",
        finished = as.character(Sys.time())
      ))
    }

    # Reset UI state
    rv$pipelineRunning <- FALSE
    rv$done <- FALSE
    rv$status <- "Pipeline cancelled."
    rv$pidFile <- NULL
    output$runStatus <- renderText(rv$status)
    shinyjs::show("sidebarPanelID")
    updateQueryString("?", mode = "replace")
  }

  observeEvent(input$cancelBtn, { cancel_pipeline() })
  observeEvent(input$browserBack, { cancel_pipeline() })
  observeEvent(input$mainNav, {
    if (identical(input$mainNav, "SPIRIT") && rv$pipelineRunning) {
      cancel_pipeline()
    }
  }, ignoreInit = TRUE)


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
    req(rv$done, rv$finalData, input$xAxisCol, input$yAxisCol, input$geneHoverCol,
        input$xScale, input$yScale)
    df <- rv$finalData
    shiny::validate(
      shiny::need(input$xAxisCol %in% names(df), "X-axis col not found in data"),
      shiny::need(input$yAxisCol %in% names(df), "Y-axis col not found in data"),
      shiny::need(is.numeric(df[[input$xAxisCol]]), "X-axis col must be numeric"),
      shiny::need(is.numeric(df[[input$yAxisCol]]), "Y-axis col must be numeric")
    )

    apply_scale <- function(vals, scale) {
      switch(scale,
        "-log10" = -log10(pmax(vals, .Machine$double.xmin)),
        "log"    = log(pmax(vals, .Machine$double.xmin)),
        "raw"    = vals)
    }
    scale_label <- function(col, scale) {
      switch(scale,
        "-log10" = paste0("-log10(", col, ")"),
        "log"    = paste0("log(", col, ")"),
        "raw"    = col)
    }

    df$x_plot <- apply_scale(df[[input$xAxisCol]], input$xScale)
    df$y_plot <- apply_scale(df[[input$yAxisCol]], input$yScale)

    x_lab <- scale_label(input$xAxisCol, input$xScale)
    y_lab <- scale_label(input$yAxisCol, input$yScale)

    p <- ggplot(df, aes(x = x_plot, y = y_plot,
                        text = .data[[input$geneHoverCol]])) +
      geom_point(alpha = 0.5, colour = "steelblue") +
      theme_bw() +
      labs(
        title = paste("Scatter of", x_lab, "vs.", y_lab),
        x = x_lab,
        y = y_lab
      )
    if (!is.na(input$xMax)) p <- p + scale_x_continuous(limits = c(0, input$xMax))
    if (!is.na(input$yMax)) p <- p + scale_y_continuous(limits = c(0, input$yMax))

    ggplotly(p, tooltip = c("text", "x", "y"))
  })
}

shinyApp(ui = ui, server = server, options = list(port = 3838))
