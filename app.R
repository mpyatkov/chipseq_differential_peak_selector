options(shiny.maxRequestSize = 50*1024^2)

library(shiny)
library(readxl)
library(writexl)
library(tools)
library(stringr)

ui <- fluidPage(
  titlePanel("Dynamic Filtering for ChIP-seq Pipeline Output"),
  
  tags$head(
    tags$style(HTML("
      /* Highlight numeric inputs when outside/inside min-max range */
      .form-control[type='number']:out-of-range, input[type='number']:out-of-range {
        border-color: #d9534f;
        box-shadow: 0 0 0 .2rem rgba(217,83,79,.25);
        background-color: #fff5f5;
      }
      .form-control[type='number']:in-range, input[type='number']:in-range {
        border-color: #5cb85c;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      helpText(tags$span("Note: Columns containing '_vs_' will be ignored during file upload.", style = "color: #d9534f;")),
      fileInput("file_input", "Choose Excel File", multiple = FALSE, accept = c(".xlsx")),
      
      textInput("dataset", "Dataset", value = "DatasetID"),
      helpText("Dataset: A prefix text for the output directory and filenames."),
      
      uiOutput("dynamic_filters"),
      
      actionButton("add_btn", "Add Rule"),
      br(), br(),
      actionButton("apply_rules_btn", "Apply Rules and Save Output"),
      br(), br(),
      uiOutput("download_ui")
    ),
    
    mainPanel(
      uiOutput("file_validation_message"),
      uiOutput("dynamic_table"),
      verbatimTextOutput("file_info")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  file_data <- reactiveValues(
    data = NULL,
    dynamic_cols = NULL,
    num_cols = NULL,
    char_cols = NULL,
    num_ranges = NULL,
    is_valid = FALSE,
    validation_msg = ""
  )
  
  table_data <- reactiveValues(data = data.frame(stringsAsFactors = FALSE))
  zip_path <- reactiveVal(NULL)
  
  # Observe file input
  observeEvent(input$file_input, {
    if (is.null(input$file_input)) {
      file_data$is_valid <- FALSE
      file_data$data <- NULL
      file_data$dynamic_cols <- NULL
      file_data$num_cols <- NULL
      file_data$char_cols <- NULL
      file_data$num_ranges <- NULL
      file_data$validation_msg <- "No file uploaded yet."
      return()
    }
    
    tryCatch({
      data <- read_excel(input$file_input$datapath)
      
      if (ncol(data) < 4) {
        file_data$is_valid <- FALSE
        file_data$validation_msg <- paste("Error: File must have at least 4 columns (chr/seqnames, start, end and column for filtering). Current file has", ncol(data), "columns.")
        return()
      }
      
      col_names_lower <- tolower(colnames(data))
      chr_idx <- which(col_names_lower == "chr" | col_names_lower == "seqnames")
      start_idx <- which(col_names_lower == "start")
      end_idx <- which(col_names_lower == "end")
      
      if (length(chr_idx) == 0 || length(start_idx) == 0 || length(end_idx) == 0) {
        file_data$is_valid <- FALSE
        file_data$validation_msg <- "Error: File must contain 'chr' or 'seqnames', 'start', and 'end' columns (case-insensitive)."
        return()
      }
      
      # Rename seqnames to chr if present
      if (col_names_lower[chr_idx] == "seqnames") {
        colnames(data)[chr_idx] <- "chr"
      }
      
      required_indices <- c(chr_idx, start_idx, end_idx)
      dynamic_indices <- setdiff(1:ncol(data), required_indices)
      all_dynamic_names <- colnames(data)[dynamic_indices]
      
      # Filter out columns containing "_vs_"
      dynamic_col_names <- all_dynamic_names[!grepl("_vs_", all_dynamic_names, ignore.case = TRUE)]
      
      if (length(dynamic_col_names) < 1) {
        file_data$is_valid <- FALSE
        file_data$validation_msg <- "Error: File must have at least one dynamic column beyond chr, start, and end (excluding columns with '_vs_')."
        return()
      }
      
      num_cols <- c()
      char_cols <- c()
      num_ranges <- list()
      
      for (col in dynamic_col_names) {
        v <- data[[col]]
        
        if (is.numeric(v)) {
          finite_v <- v[is.finite(v)]
          min_val <- if (length(finite_v)) min(finite_v, na.rm = TRUE) else NA_real_
          max_val <- if (length(finite_v)) max(finite_v, na.rm = TRUE) else NA_real_
          
          if (!is.na(min_val) && !is.na(max_val)) {
            v[v == Inf] <- max_val
            v[v == -Inf] <- min_val
          } else {
            v[is.infinite(v)] <- NA_real_
          }
          
          data[[col]] <- v
          num_cols <- c(num_cols, col)
          num_ranges[[col]] <- c(min_val, max_val)
          
        } else if (is.character(v)) {
          tokens <- trimws(v)
          idx <- !is.na(tokens) & tokens != ""
          t <- tokens[idx]
          is_num <- suppressWarnings(!is.na(as.numeric(t)))
          is_plus_inf <- grepl("^[+]?[Ii][Nn][Ff]$", t)
          is_minus_inf <- grepl("^-[Ii][Nn][Ff]$", t)
          
          if (all(is_num | is_plus_inf | is_minus_inf)) {
            numeric_tokens <- suppressWarnings(as.numeric(t[is_num]))
            finite_tokens <- numeric_tokens[is.finite(numeric_tokens)]
            min_val <- if (length(finite_tokens)) min(finite_tokens, na.rm = TRUE) else NA_real_
            max_val <- if (length(finite_tokens)) max(finite_tokens, na.rm = TRUE) else NA_real_
            
            new_num <- rep(NA_real_, length(v))
            new_num[idx][is_num] <- suppressWarnings(as.numeric(t[is_num]))
            if (!is.na(min_val) && !is.na(max_val)) {
              new_num[idx][is_plus_inf] <- max_val
              new_num[idx][is_minus_inf] <- min_val
            } else {
              new_num[idx][is_plus_inf | is_minus_inf] <- NA_real_
            }
            
            data[[col]] <- new_num
            num_cols <- c(num_cols, col)
            num_ranges[[col]] <- c(min_val, max_val)
          } else {
            char_cols <- c(char_cols, col)
          }
        } else {
          char_cols <- c(char_cols, col)
        }
      }
      
      file_data$data <- data
      file_data$dynamic_cols <- dynamic_col_names
      file_data$num_cols <- num_cols
      file_data$char_cols <- char_cols
      file_data$num_ranges <- num_ranges
      file_data$is_valid <- TRUE
      file_data$validation_msg <- paste("File loaded successfully with", length(dynamic_col_names), "dynamic columns.")
      
      table_data$data <- data.frame(stringsAsFactors = FALSE)
      
    }, error = function(e) {
      file_data$is_valid <- FALSE
      file_data$validation_msg <- paste("Error reading file:", e$message)
    })
  })
  
  # Render validation message
  output$file_validation_message <- renderUI({
    if (is.null(input$file_input)) {
      return(NULL)
    }
    
    if (file_data$is_valid) {
      div(class = "alert alert-success", file_data$validation_msg)
    } else {
      div(class = "alert alert-danger", file_data$validation_msg)
    }
  })
  
  # Render dynamic filters
  output$dynamic_filters <- renderUI({
    if (!file_data$is_valid) {
      return(NULL)
    }
    
    filter_list <- list()
    
    # Chromosome filter first
    filter_list[[length(filter_list) + 1]] <- 
      div(
        style = "margin-bottom: 8px;",
        tags$label("Chromosome:", style = "display: block; margin-bottom: 3px; font-weight: bold; font-size: 13px;"),
        selectInput("chr_filter", NULL, 
                    choices = c("ALL", "noX", "noY", "noXY"),
                    selected = "ALL",
                    width = "100%")
      )
    
    # Numeric filters
    for (col in file_data$num_cols) {
      filter_list[[length(filter_list) + 1]] <- 
        div(
          style = "margin-bottom: 8px;",
          tags$label(col, style = "display: block; margin-bottom: 3px; font-weight: bold; font-size: 13px;"),
          fluidRow(
            column(5, selectInput(paste0("op_", col), NULL,
                                  choices = c("<" = "lt", "<=" = "le", "=" = "eq", 
                                              ">=" = "ge", ">" = "gt"),
                                  selected = "lt",
                                  width = "100%")),
            column(7, tagList(
              numericInput(paste0("val_", col), NULL, value = NA,
                           min = file_data$num_ranges[[col]][1],
                           max = file_data$num_ranges[[col]][2],
                           width = "100%"),
              tags$script(HTML(sprintf(
                "var el=document.getElementById('%s'); if(el){el.setAttribute('placeholder','%s - %s');}",
                session$ns(paste0("val_", col)),
                format(file_data$num_ranges[[col]][1], digits = 5),
                format(file_data$num_ranges[[col]][2], digits = 5)
                
              )))
            ))
          )
        )
    }
    
    # Character filters
    for (col in file_data$char_cols) {
      unique_vals <- unique(na.omit(file_data$data[[col]]))
      filter_list[[length(filter_list) + 1]] <- 
        div(
          style = "margin-bottom: 8px;",
          selectizeInput(paste0("val_", col), col, 
                         choices = unique_vals,
                         multiple = TRUE,
                         options = list(create = FALSE),
                         width = "100%")
        )
    }
    
    filter_list
  })
  
  # Add rule button
  observeEvent(input$add_btn, {
    if (!file_data$is_valid) {
      showNotification("Please upload a valid file first.", type = "error")
      return()
    }
    
    new_row <- data.frame(
      dataset = input$dataset,
      chr = input$chr_filter,
      stringsAsFactors = FALSE
    )
    
    # Add numeric filters
    for (col in file_data$num_cols) {
      op_val <- input[[paste0("op_", col)]]
      num_val <- input[[paste0("val_", col)]]
      
      if (!is.na(num_val)) {
        new_row[[paste0(col, "_op")]] <- op_val
        new_row[[paste0(col, "_val")]] <- num_val
      }
    }
    
    # Add character filters
    for (col in file_data$char_cols) {
      char_vals <- input[[paste0("val_", col)]]
      if (!is.null(char_vals) && length(char_vals) > 0) {
        new_row[[paste0(col, "_vals")]] <- paste(char_vals, collapse = "|")
      }
    }
    
    # Ensure all columns exist
    for (col in file_data$num_cols) {
      if (!(paste0(col, "_op") %in% names(new_row))) {
        new_row[[paste0(col, "_op")]] <- ""
        new_row[[paste0(col, "_val")]] <- NA_real_
      }
    }
    for (col in file_data$char_cols) {
      if (!(paste0(col, "_vals") %in% names(new_row))) {
        new_row[[paste0(col, "_vals")]] <- ""
      }
    }
    
    # Merge with existing data
    if (nrow(table_data$data) == 0) {
      table_data$data <- new_row
    } else {
      all_cols <- unique(c(names(table_data$data), names(new_row)))
      for (col in all_cols) {
        if (!(col %in% names(table_data$data))) table_data$data[[col]] <- NA
        if (!(col %in% names(new_row))) new_row[[col]] <- NA
      }
      table_data$data <- rbind(table_data$data, new_row)
    }
  })
  
  # Render rules table
  output$dynamic_table <- renderUI({
    if (!file_data$is_valid || nrow(table_data$data) == 0) {
      return("No rules added yet.")
    }
    
    header_cols <- c("Dataset", "Chr")
    applied_cols <- c()
    
    for (col in file_data$num_cols) {
      if (any(!is.na(table_data$data[[paste0(col, "_val")]]) & 
              table_data$data[[paste0(col, "_op")]] != "")) {
        applied_cols <- c(applied_cols, col)
        header_cols <- c(header_cols, col)
      }
    }
    for (col in file_data$char_cols) {
      if (any(table_data$data[[paste0(col, "_vals")]] != "")) {
        applied_cols <- c(applied_cols, col)
        header_cols <- c(header_cols, col)
      }
    }
    
    header_cols <- c(header_cols, "Action")
    
    table_rows <- lapply(1:nrow(table_data$data), function(i) {
      row_data <- table_data$data[i, ]
      cells <- list(
        tags$td(row_data$dataset),
        tags$td(row_data$chr)
      )
      
      for (col in applied_cols) {
        if (col %in% file_data$num_cols) {
          op <- row_data[[paste0(col, "_op")]]
          val <- row_data[[paste0(col, "_val")]]
          if (!is.na(val) && op != "") {
            cells[[length(cells) + 1]] <- tags$td(paste0(col, " ", op, " ", val))
          } else {
            cells[[length(cells) + 1]] <- tags$td("-")
          }
        } else {
          vals <- row_data[[paste0(col, "_vals")]]
          if (vals != "") {
            cells[[length(cells) + 1]] <- tags$td(paste0(col, ": ", vals))
          } else {
            cells[[length(cells) + 1]] <- tags$td("-")
          }
        }
      }
      
      cells[[length(cells) + 1]] <- tags$td(
        actionButton(paste0("delete_", i), "Delete", class = "btn-danger btn-sm")
      )
      
      tags$tr(cells)
    })
    
    tags$table(
      class = "table table-striped",
      tags$thead(tags$tr(lapply(header_cols, tags$th))),
      tags$tbody(table_rows)
    )
  })
  
  # Delete buttons
  lapply(1:100, function(i) {
    observeEvent(input[[paste0("delete_", i)]], {
      if (i <= nrow(table_data$data)) {
        table_data$data <- table_data$data[-i, ]
      }
    })
  })
  
  # File info
  output$file_info <- renderPrint({
    if (is.null(input$file_input)) {
      return("No file uploaded yet.")
    } else {
      data.frame(Name = input$file_input$name, Size = input$file_input$size)
    }
  })
  
  # Apply rules button
  observeEvent(input$apply_rules_btn, {
    if (!file_data$is_valid) {
      showNotification("No valid file uploaded!", type = "error")
      return()
    }
    
    if (nrow(table_data$data) == 0) {
      showNotification("No rules added!", type = "error")
      return()
    }
    
    output_dir <- file.path(tempdir(), "output")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    summary_table <- data.frame(stringsAsFactors = FALSE)
    file_name_ix <- 1
    file_name <- tools::file_path_sans_ext(input$file_input$name)
    
    withProgress(message = "Processing rules", value = 0, {
      for (rule_idx in 1:nrow(table_data$data)) {
        rule <- table_data$data[rule_idx, ]
        data <- file_data$data
        
        # Filtering
        filter_conditions <- rep(TRUE, nrow(data))
        
        chr_col <- "chr"
        
        # Chromosome filter
        if (rule$chr != "ALL") {
          fvec <- c()
          if (rule$chr == "noX") fvec <- "chrX"
          if (rule$chr == "noY") fvec <- "chrY"
          if (rule$chr == "noXY") fvec <- c("chrX", "chrY")
          
          filter_conditions <- filter_conditions & !(data[[chr_col]] %in% fvec)
        }
        
        # Numeric filters
        for (col in file_data$num_cols) {
          op <- rule[[paste0(col, "_op")]]
          val <- rule[[paste0(col, "_val")]]
          
          if (!is.na(val) && op != "") {
            col_data <- data[[col]]
            if (op == "lt") filter_conditions <- filter_conditions & (col_data < val)
            else if (op == "le") filter_conditions <- filter_conditions & (col_data <= val)
            else if (op == "eq") filter_conditions <- filter_conditions & (col_data == val)
            else if (op == "ge") filter_conditions <- filter_conditions & (col_data >= val)
            else if (op == "gt") filter_conditions <- filter_conditions & (col_data > val)
          }
        }
        
        # Character filters
        for (col in file_data$char_cols) {
          vals_str <- rule[[paste0(col, "_vals")]]
          if (vals_str != "") {
            vals <- unlist(strsplit(vals_str, "\\|"))
            filter_conditions <- filter_conditions & (data[[col]] %in% vals)
          }
        }
        
        filtered_data <- data[filter_conditions, ]
        
        # Build directory name
        non_default_params <- c()
        non_default_params <- c(non_default_params, paste0("Chr=", rule$chr))
        
        for (col in file_data$num_cols) {
          op <- rule[[paste0(col, "_op")]]
          val <- rule[[paste0(col, "_val")]]
          if (!is.na(val) && op != "") {
            col_name <- gsub(" ", "_", col)
            non_default_params <- c(non_default_params, paste0(col_name, "_", op, "_", val))
          }
        }
        
        for (col in file_data$char_cols) {
          vals_str <- rule[[paste0(col, "_vals")]]
          if (vals_str != "") {
            col_name <- gsub(" ", "_", col)
            vals_clean <- gsub(" ", "_", gsub("\\|", "-", vals_str))
            non_default_params <- c(non_default_params, paste0(col_name, "_", vals_clean))
          }
        }
        
        dir_name <- paste0(rule$dataset, "_", paste(non_default_params, collapse = "_"))
        rule_output_dir <- file.path(output_dir, dir_name)
        if (!dir.exists(rule_output_dir)) {
          dir.create(rule_output_dir, recursive = TRUE)
        }
        
        # Get column references
        col_names_lower <- tolower(colnames(data))
        start_col <- colnames(data)[which(col_names_lower == "start")[1]]
        end_col <- colnames(data)[which(col_names_lower == "end")[1]]
        
        # Prepare output:
        # BED = chr/start/end only; XLSX = original column order excluding "_vs_" columns
        output_data_bed <- filtered_data[, c(chr_col, start_col, end_col)]
        colnames(output_data_bed) <- c("chr", "start", "end")
        
        xlsx_cols <- colnames(filtered_data)
        xlsx_cols <- xlsx_cols[!grepl("_vs_", xlsx_cols, ignore.case = TRUE)]
        output_data_xlsx <- filtered_data[, xlsx_cols, drop = FALSE]
        number_of_rows <- nrow(output_data_bed)
        
        out_prefix <- str_pad(file_name_ix, width = 2, side = "left", pad = "0")
        out_name <- paste0(out_prefix, "_", dir_name, "_", file_name, "_n=", number_of_rows)
        
        # Save files
        bed_file <- file.path(rule_output_dir, paste0(out_name, ".bed"))
        write.table(output_data_bed, bed_file, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
        
        xlsx_file <- file.path(rule_output_dir, paste0(out_name, ".xlsx"))
        write_xlsx(output_data_xlsx, xlsx_file, col_names = TRUE)
        
      # Add to summary
        summary_row <- data.frame(
          filename = out_name,
          inputfile = input$file_input$name,
          dataset = rule$dataset,
          chr = rule$chr,
          n_regions = number_of_rows,
          stringsAsFactors = FALSE
        )
        
        for (col in file_data$num_cols) {
          op <- rule[[paste0(col, "_op")]]
          val <- rule[[paste0(col, "_val")]]
          if (!is.na(val) && op != "") {
            summary_row[[col]] <- paste0(op, val)
          } else {
            summary_row[[col]] <- ""
          }
        }
        
        for (col in file_data$char_cols) {
          vals_str <- rule[[paste0(col, "_vals")]]
          summary_row[[col]] <- vals_str
        }
        
        if (nrow(summary_table) == 0) {
          summary_table <- summary_row
        } else {
          all_cols <- unique(c(names(summary_table), names(summary_row)))
          for (col in all_cols) {
            if (!(col %in% names(summary_table))) summary_table[[col]] <- NA
            if (!(col %in% names(summary_row))) summary_row[[col]] <- NA
          }
          summary_table <- rbind(summary_table, summary_row)
        }
        
        file_name_ix <- file_name_ix + 1
        
        incProgress(1 / nrow(table_data$data), detail = paste("Processing rule", rule_idx, "of", nrow(table_data$data)))
      }
    })
    
    write_xlsx(summary_table, file.path(output_dir, "summary_table.xlsx"), col_names = TRUE)
    
    zip_file <- file.path(tempdir(), "output.zip")
    if (file.exists(zip_file)) file.remove(zip_file)
    
    current_dir <- getwd()
    setwd(output_dir)
    zip(zip_file, files = "./", extras = "-r")
    setwd(current_dir)
    unlink(output_dir, recursive = TRUE)
    
    zip_path(zip_file)
    showNotification("Rules applied successfully! Click 'Download Output' to download.", type = "message")
  })
  
  # Download UI
  output$download_ui <- renderUI({
    if (!is.null(zip_path())) {
      downloadButton("download_btn", "Download Output")
    }
  })
  
  # Download handler
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0("output_", Sys.Date(), ".zip")
    },
    content = function(file) {
      file.copy(zip_path(), file)
    }
  )
}

shinyApp(ui = ui, server = server)