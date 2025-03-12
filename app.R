options(shiny.maxRequestSize = 50*1024^2)

library(shiny)
library(readxl)  # For reading .xlsx files
library(writexl)  # For writing .xlsx files
library(tools)  # For file extensions
library(stringr)
# Define UI
ui <- fluidPage(
  titlePanel("Filtering out overlap files obtained from ChIP-seq pipeline"),
  
  sidebarLayout(
    sidebarPanel(
      
      tags$a(href = "example.xlsx", "Download Example XLSX File", target = "_blank"),
      
      # File input for selecting multiple .xlsx files
      fileInput("file_input", "Choose Excel Files", multiple = TRUE, accept = c(".xlsx")),
      
      # Input fields for the filtering rules
      textInput("dataset", "Dataset", value = "DatasetID"),
      helpText("Dataset: A prefix text to give more detailed explanation of data. This will be used in the output directory and filenames."),
      
      selectInput("chr", "Chromosome", choices = c("ALL", "noX", "noY", "noXY"), selected = "ALL"),
      helpText("Chromosome: Refers to the 'seqnames' column in the input file. Use this to exclude sex chromosomes from analysis."),
      
      numericInput("width", "Width (<= 1000000)", value = 100000, min = 1, max = 1000000),
      helpText("Width: Region size. This parameter allows you to filter regions smaller than or equal to the specified number."),
      
       # Segmented toggle for n_any_quality and n_signif_quality
      radioButtons("quality_toggle", "Quality Type",
                   choices = c("N Any Quality" = "n_any_quality", "N Signif Quality" = "n_signif_quality"),
                   selected = "n_any_quality"),
      uiOutput("quality_input"),  # Dynamic input for quality
      helpText("Quality Type: A comma-separated list (without spaces) of numbers from 1 to 5, representing the overlap across five different counting methods."),
      helpText("n_any_quality: The number of methods overlapping in a given region, regardless of statistical significance."),
      helpText("n_signif_quality: The number of methods overlapping in a region, but only if the overlap is statistically significant. For example, if n_signif_quality = 4,5, it means that only significant regions overlapped by at least four methods are considered."),
      
      numericInput("p_adj", "P.adj", value = 0.05, min = 0),
      helpText("P.adj (less than): Significance of the regions. Use this to filter regions based on their adjusted p-value."),
      
      numericInput("log2FC", "|Log2FC|", value = 0.0),
      helpText("log2FC (greater than): Absolute value of log2 fold change. Use this to filter regions based on their fold change."),
 
      selectInput("regulation", "Regulation", choices = c("ALL", "positive", "negative"), selected = "ALL"),
      helpText("Regulation: Positive/negative (based on the sign of the 'log2FC' column). Use this to extract up/down regulated regions."),
      
      actionButton("add_btn", "Add parameters for analysis"),
      br(), br(),
      actionButton("apply_rules_btn", "Apply Rules and Save Output"),
      br(), br(),
      uiOutput("download_ui")  # Download link for the zip archive
    ),
    
    mainPanel(
      uiOutput("dynamic_table"),
      verbatimTextOutput("file_info")  # Display file information
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the table data (filtering rules)
  table_data <- reactiveValues(data = data.frame(
    dataset = character(),
    chr = character(),
    width = numeric(),
    regulation = character(),
    n_any_quality = character(),
    n_signif_quality = character(),
    p_adj = numeric(),
    log2FC = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Dynamic input for quality based on the segmented toggle
  output$quality_input <- renderUI({
    if (input$quality_toggle == "n_any_quality") {
      textInput("n_any_quality", "N Any Quality (comma-separated)", value = "3,4,5")
    } else {
      textInput("n_signif_quality", "N Signif Quality (comma-separated)", value = "3,4,5")
    }
  })
  
  # Observe the "Add Rule" button click
  observeEvent(input$add_btn, {
    # Add the new rule to the table data
    new_row <- data.frame(
      dataset = input$dataset,  # Include dataset in the rule
      chr = input$chr,
      width = input$width,
      regulation = input$regulation,
      n_any_quality = if (input$quality_toggle == "n_any_quality") input$n_any_quality else "",
      n_signif_quality = if (input$quality_toggle == "n_signif_quality") input$n_signif_quality else "",
      p_adj = input$p_adj,
      log2FC = input$log2FC,
      stringsAsFactors = FALSE
    )
    table_data$data <- rbind(table_data$data, new_row)
  })
  
  # Render the dynamic table with delete buttons
  output$dynamic_table <- renderUI({
    if (nrow(table_data$data) == 0) {
      return("No rules added yet.")
    } else {
      # Create a table with a delete button for each row
      table_rows <- lapply(1:nrow(table_data$data), function(i) {
        tags$tr(
          tags$td(table_data$data[i, "dataset"]),  # Display dataset
          tags$td(table_data$data[i, "chr"]),
          tags$td(table_data$data[i, "width"]),
          tags$td(table_data$data[i, "regulation"]),
          tags$td(table_data$data[i, "n_any_quality"]),
          tags$td(table_data$data[i, "n_signif_quality"]),
          tags$td(table_data$data[i, "p_adj"]),
          tags$td(table_data$data[i, "log2FC"]),
          tags$td(
            actionButton(paste0("delete_", i), "Delete", class = "btn-danger")
          )
        )
      })
      
      # Combine rows into a table
      tags$table(
        class = "table",
        tags$thead(
          tags$tr(
            tags$th("Dataset"),  # Add Dataset column
            tags$th("Chromosome"),
            tags$th("Width"),
            tags$th("Regulation"),
            tags$th("N Any Quality"),
            tags$th("N Signif Quality"),
            tags$th("P.adj"),
            tags$th("|Log2FC|"),
            tags$th("Action")
          )
        ),
        tags$tbody(table_rows)
      )
    }
  })
  
  # Observe delete button clicks
  lapply(1:100, function(i) {  # Create observers for up to 100 rows
    observeEvent(input[[paste0("delete_", i)]], {
      # Remove the corresponding row from the data
      if (i <= nrow(table_data$data)) {
        table_data$data <- table_data$data[-i, ]
      }
    })
  })
  
  # Display information about the uploaded files
  output$file_info <- renderPrint({
    if (is.null(input$file_input)) {
      return("No files uploaded yet.")
    } else {
      # Display file names and sizes
      file_info <- data.frame(
        Name = input$file_input$name
        #Size = input$file_input$size,
        #Type = input$file_input$type
      )
      file_info
    }
  })
  
  # Reactive value to store the path to the zip archive
  zip_path <- reactiveVal(NULL)
  
  # Observe the "Apply Rules and Save Output" button click
  observeEvent(input$apply_rules_btn, {
    if (is.null(input$file_input)) {
      showNotification("No files uploaded!", type = "error")
      return()
    }
    
    if (nrow(table_data$data) == 0) {
      showNotification("No rules added!", type = "error")
      return()
    }
    
    # Create a temporary directory for output
    output_dir <- file.path(tempdir(), "output")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    summary_table <- data.frame(
      filename = character(),
      dataset = character(),
      chr = character(),
      width = numeric(),
      regulation = character(),
      n_any_quality = character(),
      n_signif_quality = character(),
      p_adj = numeric(),
      log2FC = numeric(),
      n_regions = numeric(),
      stringsAsFactors = FALSE
    )
    
    # init the files counter to make prefix
    file_name_ix <- 1
    
    # Start progress bar
    withProgress(message = "Processing files", value = 0, {
      # Loop through each uploaded file
      
      for (rule_idx in 1:nrow(table_data$data)) {
        rule <- table_data$data[rule_idx, ]
        
        files_list <- sort(input$file_input$name)
        input_sorted <- input$file_input[order(input$file_input$name),]
        
        for (file_idx in seq_along(input_sorted$name)) {
          file <- input_sorted$datapath[file_idx]
          file_name <- tools::file_path_sans_ext(input_sorted$name[file_idx])
          
          # Read the Excel file
          data <- read_excel(file)
          data <- data[,1:9]
          colnames(data) <- c("chr", "start", "end", "width", "regulation", "n_any_quality", "n_signif_quality", "p_adj", "log2FC")
          
          # Apply filtering rules using base R
          filter_conditions <- TRUE
          if (rule$chr != "ALL") {
            fvec <- c()
            if (rule$chr == "noX") fvec <- "chrX"
            if (rule$chr == "noY") fvec <- "chrY"
            if (rule$chr == "noXY") fvec <- c("chrX", "chrY")
            
            filter_conditions <- filter_conditions & !(data$chr %in% fvec)
          }
          if (rule$width != 1000000) {
            filter_conditions <- filter_conditions & (data$width <= rule$width)
          }
          if (rule$regulation != "ALL") {
            filter_conditions <- filter_conditions & (data$regulation == rule$regulation)
          }
          if (rule$n_any_quality != "") {
            filter_conditions <- filter_conditions & (data$n_any_quality %in% as.numeric(unlist(strsplit(rule$n_any_quality, ","))))
          }
          if (rule$n_signif_quality != "") {
            filter_conditions <- filter_conditions & (data$n_signif_quality %in% as.numeric(unlist(strsplit(rule$n_signif_quality, ","))))
          }
          if (rule$p_adj != 1.0) {
            filter_conditions <- filter_conditions & (data$p_adj < rule$p_adj)
          }
          if (rule$log2FC != 0.0) {
            filter_conditions <- filter_conditions & (abs(data$log2FC) > rule$log2FC)
          }
          
          # Filter the data
          filtered_data <- data[filter_conditions, ]
          
          # Generate directory name based on non-default parameters
          non_default_params <- c()
          non_default_params <- c(non_default_params, paste0("Chr=", rule$chr))
          # if (rule$chr != "ALL") {
          #   non_default_params <- c(non_default_params, paste0("chr=", rule$chr))
          # }
          
          if (rule$width != 1000000) {
            non_default_params <- c(non_default_params, paste0("Width=", rule$width))
          }
          
          if (rule$regulation != "ALL") {
            regulat <- ifelse(rule$regulation == "positive", "PosReg", "NegReg") 
            non_default_params <- c(non_default_params, regulat)
            
          } else {
            non_default_params <- c(non_default_params, "PosNeg")
          }
          
          if (rule$n_any_quality != "") {
            non_default_params <- c(non_default_params, paste0("NAny=", gsub(",","-",rule$n_any_quality)))
          }
          if (rule$n_signif_quality != "") {
            non_default_params <- c(non_default_params, paste0("NSignif=", gsub(",","-",rule$n_signif_quality)))
          }
          if (rule$p_adj != 1.0) {
            non_default_params <- c(non_default_params, paste0("PAdj=", rule$p_adj))
          }
          
          #if (rule$log2FC != 1.0) {
          non_default_params <- c(non_default_params, paste0("log2FC=", rule$log2FC))
          #}
          
          # Create directory name
          dir_name <- paste0(rule$dataset, "_", paste(non_default_params, collapse = "_"))
          rule_output_dir <- file.path(output_dir, dir_name)
          if (!dir.exists(rule_output_dir)) {
            dir.create(rule_output_dir, recursive = TRUE)
          }
          
          # Calculate number of regions in filtered data
          number_of_rows <- nrow(filtered_data)
          out_name <- gsub("_overlap_manorm2_vs_diffreps", "", file_name)
          out_prefix <- str_pad(file_name_ix, width = 2, side = "left", pad="0")
          out_name <- paste0(out_prefix,"_",dir_name,"_",out_name,"_n=",number_of_rows)
          
          # Save as .bed
          bed_file <- file.path(rule_output_dir, paste0(out_name, ".bed"))
          write.table(filtered_data[,1:3], bed_file, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
          
          # Save as .xlsx using writexl
          xlsx_file <- file.path(rule_output_dir, paste0(out_name, ".xlsx"))
          write_xlsx(filtered_data[,1:3], xlsx_file)
          
          # Create summary table with filenames and number of regions
          tmp_summary <- data.frame(
            filename = out_name,
            dataset = rule$dataset,
            chr = rule$chr,
            width = rule$width,
            regulation = rule$regulation,
            n_any_quality = rule$n_any_quality,
            n_signif_quality = rule$n_signif_quality,
            p_adj = rule$p_adj,
            log2FC = rule$log2FC,
            n_regions = number_of_rows,
            stringsAsFactors = FALSE
          )
          
          summary_table <- rbind(summary_table, tmp_summary)
          file_name_ix <- file_name_ix + 1
        }

        # Update progress bar
        #incProgress(1 / length(input_sorted$name), detail = paste("Processing file", file_idx, "of", length(input_sorted$name)))
        incProgress(1 / nrow(table_data$data), detail = paste("Processing rule", rule_idx, "of", nrow(table_data$data)))
      }
    })
    
    # Save summary table as .xlsx
    write_xlsx(summary_table, file.path(output_dir, "summary_table.xlsx"), col_names = T)
    
    # Create a zip archive of the output directory
    zip_file <- file.path(tempdir(), "output.zip")
    file.remove(zip_file)
    
    ## To pack only the final dir without whole tree 
    current_dir <- getwd()
    setwd(output_dir)
    zip(zip_file, files = "./", extras = "-r")
    setwd(current_dir)
    unlink(output_dir, recursive = TRUE)
    zip_path(zip_file)  # Store the path to the zip file
    
    showNotification("Rules applied and output saved successfully! Click 'Download Output' to download the zip archive.", type = "message")
  })
  
  # Render the download link for the zip archive
  output$download_ui <- renderUI({
    if (!is.null(zip_path())) {
      downloadButton("download_btn", "Download Output")
    }
  })
  
  # Handle the download of the zip archive
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0("output_", Sys.Date(), ".zip")
    },
    content = function(file) {
      file.copy(zip_path(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)