# ============================================================================
# FACS FILE RENAMING TOOL FOR HUGGING FACE - FINAL VERSION
# Converts plate-based FCS file names to standardized format
# ============================================================================

if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("bslib", quietly = TRUE)) install.packages("bslib")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("zip", quietly = TRUE)) install.packages("zip")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(zip)
  library(dplyr)
})

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly", version = 5),
  
  tags$head(
    tags$style(HTML("
      .gradient-header {
        background: linear-gradient(135deg, #1e3a8a 0%, #0f172a 100%);
        color: white;
        padding: 40px 20px;
        text-align: center;
        margin-bottom: 30px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .gradient-header h1 {
        font-size: 2.5em;
        font-weight: 700;
        margin: 0 0 10px 0;
      }
      .gradient-header p {
        font-size: 1.1em;
        margin: 0;
        opacity: 0.9;
      }
      .info-box {
        background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%);
        padding: 15px;
        border-radius: 8px;
        border-left: 5px solid #2196F3;
        margin: 15px 0;
      }
      .info-box h5 {
        margin: 0 0 8px 0;
        color: #1565c0;
      }
      .info-box code {
        background: rgba(255,255,255,0.95);
        padding: 4px 8px;
        border-radius: 4px;
        color: #d32f2f;
        font-weight: 600;
      }
      .step-box {
        background: #f8f9fa;
        border: 2px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin: 15px 0;
      }
      .step-box h4 {
        margin-top: 0;
        color: #1e3a8a;
      }
      .apply-all-box {
        background: #fff3cd;
        border: 1px solid #ffc107;
        border-radius: 6px;
        padding: 12px;
        margin-bottom: 15px;
      }
      .footer {
        background: linear-gradient(135deg, #1e3a8a 0%, #0f172a 100%);
        color: white;
        padding: 25px 20px;
        text-align: center;
        margin-top: 40px;
      }
      .footer p {
        margin: 5px 0;
        font-size: 0.95em;
      }
      .footer a {
        color: #90caf9;
        text-decoration: none;
      }
      .footer a:hover {
        color: #bbdefb;
        text-decoration: underline;
      }
    "))
  ),
  
  div(class = "gradient-header",
      h1(icon("file-signature"), " FACS File Renaming Tool"),
      p("Convert plate-based FCS files to standardized naming convention")),
  
  div(style = "max-width: 1400px; margin: 0 auto; padding: 0 20px;",
      
      # Step 1: Upload Files
      div(class = "step-box",
          h4(icon("upload"), " Step 1: Upload FCS Files"),
          fileInput("files", "Select .fcs files from your plate experiment:",
                    multiple = TRUE,
                    accept = c(".fcs", ".FCS"),
                    width = "100%"),
          uiOutput("upload_status")
      ),
      
      # Step 2: Define Plate Layout
      conditionalPanel(
        condition = "output.files_uploaded",
        
        div(class = "step-box",
            h4(icon("th"), " Step 2: Define Plate Layout"),
            
            fluidRow(
              column(6,
                     h5("Define Rows (Cell Lines)"),
                     uiOutput("row_inputs")
              ),
              column(6,
                     h5("Define Columns (Treatments & Concentrations)"),
                     
                     # Apply to all option
                     div(class = "apply-all-box",
                         h6(icon("magic"), " Quick Fill Options"),
                         fluidRow(
                           column(6,
                                  textInput("apply_all_treatment", 
                                            "Treatment Name:",
                                            placeholder = "e.g., YF2")),
                           column(6,
                                  actionButton("btn_apply_all_treatment", 
                                               "Apply to All Columns",
                                               class = "btn-warning btn-sm",
                                               icon = icon("copy"),
                                               style = "margin-top: 25px;"))
                         )
                     ),
                     
                     uiOutput("col_inputs")
              )
            )
        ),
        
        # Step 3: Preview & Download
        div(class = "step-box",
            h4(icon("eye"), " Step 3: Preview Renamed Files"),
            actionButton("preview", "Generate Preview", 
                         class = "btn-primary btn-lg",
                         icon = icon("cogs"),
                         style = "margin-bottom: 15px;"),
            br(),
            DTOutput("preview_table"),
            br(),
            uiOutput("download_ui")
        )
      ),
      
      # Info Box
      div(class = "info-box",
          h5(icon("info-circle"), " Target Format"),
          p("Files will be renamed to: ", 
            tags$code("CellLine_Treatment_ConcentrationuM_Replicate.fcs")),
          p(strong("Examples:")),
          tags$ul(
            tags$li(tags$code("Ly18_YF2_0uM_Rep1.fcs")),
            tags$li(tags$code("HT_DMSO_50uM_Rep2.fcs")),
            tags$li(tags$code("SUDHL4_Compound_10.5uM_Rep1.fcs"))
          ))
  ),
  
  # Footer
  div(class = "footer",
      p(strong("Developed by Mahmood Mohammed Ali")),
      p("Université Grenoble Alpes | Institute for Advanced Biosciences"),
      p("mahmood.mohammed-ali@univ-grenoble-alpes.fr"),
      br(),
      # License badge
      tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/", target = "_blank",
             tags$img(src = "https://licensebuttons.net/l/by-nc/4.0/88x31.png", 
                      alt = "CC BY-NC 4.0", 
                      style = "border: 0;")),
      # DOI badge
      tags$a(href = "https://doi.org/10.5281/zenodo.17873024", target = "_blank",
             tags$img(src = "https://zenodo.org/badge/1113365916.svg", 
                      alt = "DOI: 10.5281/zenodo.17873024", 
                      style = "border: 0; margin-left: 10px;")))
)


# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    files = NULL,
    rows = character(0),
    cols = character(0),
    preview_data = NULL
  )
  
  # Upload status
  output$files_uploaded <- reactive({
    !is.null(rv$files) && nrow(rv$files) > 0
  })
  outputOptions(output, "files_uploaded", suspendWhenHidden = FALSE)
  
  observeEvent(input$files, {
    req(input$files)
    
    # 1) Append new files to previously stored ones
    if (is.null(rv$files)) {
      rv$files <- input$files
    } else {
      # Avoid duplicate rows if user re-selects same files
      new <- input$files
      existing_paths <- rv$files$datapath
      
      new <- new[!new$datapath %in% existing_paths, , drop = FALSE]
      
      rv$files <- rbind(rv$files, new)
    }
    
    # 2) Work always from rv$files (cumulative), not input$files
    all_names <- rv$files$name
    
    rows <- unique(gsub(".*_([A-Z])[0-9]+\\.fcs$", "\\1", all_names, ignore.case = TRUE))
    cols <- unique(as.numeric(gsub(".*_[A-Z]([0-9]+)\\.fcs$", "\\1", all_names, ignore.case = TRUE)))
    
    rv$rows <- sort(rows)
    rv$cols <- sort(cols)
    
    output$upload_status <- renderUI({
      div(class = "alert alert-success",
          icon("check-circle"), " ",
          strong(nrow(rv$files), " files uploaded (total across all selections)"),
          p(paste("Detected rows:", paste(rv$rows, collapse = ", "))),
          p(paste("Detected columns:", paste(rv$cols, collapse = ", "))))
    })
  })
  
  # Row inputs (Cell Lines)
  output$row_inputs <- renderUI({
    req(rv$rows)
    
    lapply(rv$rows, function(row) {
      textInput(paste0("row_", row), 
                label = paste("Row", row, ":"),
                value = "",
                placeholder = "e.g., Ly18")
    })
  })
  
  # Column inputs (Treatment_ConcentrationuM)
  output$col_inputs <- renderUI({
    req(rv$cols)
    
    lapply(rv$cols, function(col) {
      fluidRow(
        column(6,
               textInput(paste0("col_treatment_", col), 
                         label = paste("Column", col, "- Treatment:"),
                         value = "",
                         placeholder = "e.g., YF2")),
        column(6,
               numericInput(paste0("col_conc_", col), 
                            label = paste("Column", col, "- Conc. (µM):"),
                            value = 0,
                            min = 0,
                            step = 0.1))
      )
    })
  })
  
  # Apply treatment to all columns
  observeEvent(input$btn_apply_all_treatment, {
    req(rv$cols, input$apply_all_treatment)
    
    if(input$apply_all_treatment == "") {
      showNotification("Please enter a treatment name first", type = "warning")
      return()
    }
    
    for(col in rv$cols) {
      updateTextInput(session, paste0("col_treatment_", col), 
                      value = input$apply_all_treatment)
    }
    
    showNotification(paste("Applied '", input$apply_all_treatment, "' to all columns"), 
                     type = "message", duration = 3)
  })
  
  # Generate Preview
  observeEvent(input$preview, {
    req(rv$files, rv$rows, rv$cols)
    
    # Collect row definitions
    row_map <- setNames(
      sapply(rv$rows, function(r) input[[paste0("row_", r)]]),
      rv$rows
    )
    
    # Collect column definitions
    col_treatment_map <- setNames(
      sapply(rv$cols, function(c) input[[paste0("col_treatment_", c)]]),
      rv$cols
    )
    
    col_conc_map <- setNames(
      sapply(rv$cols, function(c) input[[paste0("col_conc_", c)]]),
      rv$cols
    )
    
    # Validate inputs
    if(any(row_map == "")) {
      showNotification("Please fill in all row (cell line) definitions", 
                       type = "error", duration = 5)
      return()
    }
    
    if(any(col_treatment_map == "")) {
      showNotification("Please fill in all column treatment definitions", 
                       type = "error", duration = 5)
      return()
    }
    
    # Parse files and create new names
    preview_df <- data.frame(
      original_name = rv$files$name,
      row = NA,
      col = NA,
      cell_line = NA,
      treatment = NA,
      concentration_uM = NA,
      replicate = NA,
      new_name = NA,
      stringsAsFactors = FALSE
    )
    
    for(i in 1:nrow(preview_df)) {
      fname <- preview_df$original_name[i]
      
      # Extract row and column
      row <- gsub(".*_([A-Z])[0-9]+\\.fcs$", "\\1", fname, ignore.case = TRUE)
      col <- as.numeric(gsub(".*_[A-Z]([0-9]+)\\.fcs$", "\\1", fname, ignore.case = TRUE))
      
      preview_df$row[i] <- row
      preview_df$col[i] <- col
      
      # Map to cell line and treatment
      preview_df$cell_line[i] <- row_map[row]
      preview_df$treatment[i] <- col_treatment_map[as.character(col)]
      preview_df$concentration_uM[i] <- col_conc_map[as.character(col)]
    }
    
    # Define well ID from row + col (A1, A2, ...)
    preview_df$well_id <- paste0(preview_df$row, preview_df$col)
    
    preview_df <- preview_df %>%
      # Work per biological condition
      group_by(cell_line, treatment, concentration_uM) %>%
      # Keep the original order from the machine
      mutate(
        orig_index = row_number()
      ) %>%
      # Fixed ordering of wells (A1, A2, A3, ...)
      mutate(
        well_order = dense_rank(well_id)  # A1 = 1, A2 = 2, ...
      ) %>%
      arrange(orig_index, .by_group = TRUE) %>%
      # For each well, how many times has it appeared so far in this condition?
      group_by(well_id, .add = TRUE) %>%
      mutate(
        run_index = row_number()  # 1st time A1 appears, 2nd time A1 appears, etc.
      ) %>%
      ungroup() %>%
      group_by(cell_line, treatment, concentration_uM) %>%
      mutate(
        n_wells         = n_distinct(well_id),
        replicate_index = (run_index - 1) * n_wells + well_order,
        replicate       = paste0("Rep", replicate_index)
      ) %>%
      ungroup()
    
    # Create new names
    preview_df$new_name <- paste0(
      preview_df$cell_line, "_",
      preview_df$treatment, "_",
      preview_df$concentration_uM, "uM_",
      preview_df$replicate, ".fcs"
    )
    
    rv$preview_data <- preview_df
    
    output$preview_table <- renderDT({
      datatable(
        preview_df[, c("original_name", "new_name")],
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        colnames = c("Original Filename", "New Filename")
      )
    })
    
    output$download_ui <- renderUI({
      div(
        downloadButton("download_zip", "Download Renamed Files (ZIP)", 
                       class = "btn-success btn-lg",
                       icon = icon("download")),
        p(style = "margin-top: 10px; color: #666;",
          paste("Ready to download", nrow(preview_df), "renamed files"))
      )
    })
    
    showNotification("Preview generated! Review the table and download when ready.", 
                     type = "message", duration = 5)
  })
  
  # Download ZIP
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("renamed_fcs_files_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      req(rv$preview_data, rv$files)
      
      # Create temp directory
      temp_dir <- tempdir()
      renamed_dir <- file.path(temp_dir, "renamed_files")
      dir.create(renamed_dir, showWarnings = FALSE, recursive = TRUE)
      
      withProgress(message = "Creating ZIP file...", value = 0, {
        
        for(i in 1:nrow(rv$preview_data)) {
          # Copy and rename file
          original_path <- rv$files$datapath[i]
          new_name <- rv$preview_data$new_name[i]
          new_path <- file.path(renamed_dir, new_name)
          
          file.copy(original_path, new_path)
          
          incProgress(1/nrow(rv$preview_data))
        }
        
        # Create ZIP
        files_to_zip <- list.files(renamed_dir, full.names = TRUE)
        zip::zip(file, files = basename(files_to_zip), root = renamed_dir)
      })
      
      # Cleanup
      unlink(renamed_dir, recursive = TRUE)
    }
  )
}

shinyApp(ui, server)
