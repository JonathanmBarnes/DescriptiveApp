#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(shinyjs)
library(mice)
library(gt)
library(gtExtras)
library(tidyverse)
library(knitr)
library(svglite)
source("Cleaning.R")
source("gt_summary_table.R")


ui <- navbarPage(theme = "quarto.css",
                 "Data Inspection",
                 
#---------------------------
# Data Importing
#--------------------------- 
  tabPanel("Import",
           h3("Upload Dataset"),
           
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a Data File (30MB)",
                accept = c(".csv", ".xpt", ".xlsx", ".sas7bdat"),
                ),
      
      checkboxInput("header", "Are Headers Column Names", TRUE),
      
      conditionalPanel(
        condition = "output.isXlsx",
        numericInput("sheetNum", "Which Sheet To View", value = 1)
      ),
      
#      conditionalPanel(
#       condition = "output.isSasXPT",
#        checkboxInput("LabelNames", "Replace Variables With Labels", FALSE)
#      ),
      
      conditionalPanel(
        condition = "output.fileCodeAvailable",
        verbatimTextOutput("fileCode")
      ),
    ),
    mainPanel(
      tags$div(style = "overflow-x: auto; max-width: 95%",
               gt_output("importTable")
      )
    )
  )
),

#---------------------------
# Data Cleaning
#---------------------------

tabPanel("Review",
         h3("Review Data"),
         gt_output("CleanTableHead")
),



tabPanel("Change",
         h3("Change Uploaded Data"),
         sidebarLayout(
           sidebarPanel(
             uiOutput("var_select"),
             radioButtons("VarTypeTo", 
                          "Select New Variable Type",
                          choices = c("Character" = "Chr",
                                      "Numeric" = "Num",
                                      "Factor (Group)" = "Fac",
                                      "Date" = "Date"), selected = character(0)),
             conditionalPanel(
               condition = "output.FactorLevShow",
               uiOutput("FactorLevUI")
             ),
             
             conditionalPanel(
               condition = "output.DateFormShow",
               uiOutput("DateFormUI")
             ),
             
             # NA Handle
             radioButtons("NaMethod", 
                          "Select Method for handling NA values",
                          choices = c("Ignore" = "Ignore",
                                      "Complete Cases" = "Complete_Cases",
                                      "Naive Mean" = "NaiveMean",
                                      "Group Mean" = "Group_Mean",
                                      "Multiple Imputation" = "Multiple_Imputation"),
                          selected = "Ignore"),
             conditionalPanel(
               condition = "output.NAGroupShow",
               uiOutput("NaGroupUI")
             ),
            conditionalPanel(
             condition = "output.MIMethodShow",
             uiOutput("MI_MethodUI")
           ),
           conditionalPanel(
             condition = "output.ChangeCodeAvailable",
             verbatimTextOutput("ChangeCode")
           ),
           conditionalPanel(
             condition = "output.NACodeAvailable",
             verbatimTextOutput("NACode")
           ),
           ),
           mainPanel(
             tags$div(style = "overflow-x: auto; max-width: 95%",
                      gt_output("CleanTable")
             )
           )
         )
         
),

#---------------------------
# Descriptives
#---------------------------

# Js variable panel to hide or show var selection in app
tabPanel(
  "Describe",
  useShinyjs(), # Enable shinyjs
  tags$head(
    tags$style(HTML("
        #sidebar {
          position: fixed;
          z-index: 1000;
          height: 90%;
          overflow: auto;
          background-color: #FFFFFF;
          padding: 10px;
          border-right: 2px solid #ddd;
          width: 200px;
          transition: transform 0.3s ease-in-out;
        }
        #sidebar.hidden {
          transform: translateX(-200px); /* Hide sidebar */
        }
        #mainContent {
          margin-left: 210px;
          transition: margin-left 0.3s ease-in-out;
        }
        #mainContent.full {
          margin-left: 5px; /* Full width when sidebar is hidden */
        }
        #toggleButton {
          position: fixed;
          top: 60px; /* Below the navbar */
          left: 10px; /* Position button next to sidebar */
          z-index: 2000; /* Ensure it is above all other elements */
          background-color: transparent;
          border: none;
          color: #137288;
          font-size: 16px;
          cursor: pointer;
        }
      "))
  ),
  fluidRow(
    # Sidebar
    div(
      id = "sidebar",
      uiOutput("variableSelector")
    ),
    # Toggle Button
    div(
      id = "toggleButton",
      "Hide Variables"
    ),
    # Main Content
    div(
      id = "mainContent",
      gt_output("summary")
    )

)),

#---------------------------
# Other Information
#---------------------------

# Potential Other info but likely to be include at github



#---------------------------
# Github Repo Link
#---------------------------


tabPanel(
  title = icon("github"),
  value = "github_tab",
  tags$script(HTML("
      $(document).on('click', 'a[data-value=\"github_tab\"]', function() {
        window.open('https://github.com/JonathanmBarnes/DescriptiveApp', '_blank');
      });
    "))
),
)




server <- function(input, output) {
options(shiny.maxRequestSize = 30*1024^2) # Max file size = 5mb

  #---------------------------
  # Data Importing
  #--------------------------- 

  # If excel file, will create box asking which sheet to view
  output$isXlsx <- reactive({
    req(input$file)  # Ensure a file is uploaded
    tools::file_ext(input$file$name) == "xlsx"
  })
outputOptions(output, "isXlsx", suspendWhenHidden = FALSE) # Condition update so sheet box appears


# If SAS or XPT file format will ask if variable labels (if existing) will replace variable names
output$isSasXPT <- reactive({
  req(input$file)  # Ensure a file is uploaded
  tools::file_ext(input$file$name) %in% c("xpt", "sas7bdat")
})
#outputOptions(output, "isSasXPT", suspendWhenHidden = FALSE) # Condition update so Label box appears

  
fileTypeCode <- reactive({
  req(input$file)  # Ensure a file is uploaded
  ext <- tools::file_ext(input$file$name) # Looks for file extension
  
  switch(ext,
         "csv" = paste0("read_csv(\n", input$file[1], ",\ncol_names = " , input$header,")"),
         "xpt" = paste0("read_xpt(\n", input$file[1],")"),
         "sas7bdat" = paste0("read_sas(\n", input$file[1],")"),
         "xlsx" = paste0("read_xlsx(\n",input$file[1], ",\nsheet = ", input$sheetNum, " ,\ncol_names = " , input$header, ")"),
         "Unsupported file type")
})

# Render the appropriate code snippet
output$fileCode <- renderText({
  fileTypeCode()
})
output$fileCodeAvailable <- reactive({
  !is.null(input$file) && tools::file_ext(input$file$name) %in% c("csv", "xpt", "xlsx", "sas7bdat")
})
outputOptions(output, "fileCodeAvailable", suspendWhenHidden = FALSE)


  
  data <- reactive({
    req(input$file)  # Ensure a file is uploaded before proceeding
    ReadData(input$file$datapath, input$header, input$sheetNum) %>%
      mutate(across(where(~ 
                            (length(unique(.)) == 2 && all(unique(.) %in% c(0, 1))) ||  # Binary indicators
                            (length(unique(.)) < 0.05 * length(.)) ||                  
                            # If num of unqiue levels in a variable is less then 5 of total obs it is assigned to be a group
                            grepl("ID", deparse(substitute(.)), ignore.case = TRUE)   # ID-like variables
      ),
      ~ if (length(unique(.)) == 2 && all(unique(.) %in% c(0, 1))) {
        factor(. + 1, levels = c(1, 2)) # Shift binary 0 to 1 and 1 to 2
      } else {
        as.character(.) # Convert group-like or ID variables to character
      }
      ))
    })
  # Render the data table
  output$importTable <- render_gt({
    req(data())  # Ensure data is available
    # Summarize data
    summary_df <- tibble::tibble(
      Metric = c(
        "Dataset Name",
        "Total Size MB",
        "Number of Variables",
        "Variables > 20% Missing Values",
        "Number of Observations",
        "Observations > 20% Missing Values"
      ),
      Value = c(
        if (!is.null(input$file)) input$file[1] else "Unknown Dataset",
        format(object.size(data()) / 1024^2, digits = 3),
        ncol(data()),
        length(NA_cols(data(), 20)),  # Use length to count columns with >20% missing
        nrow(data()),
        length(NA_rows(data(), 20))  # Use length to count rows with >20% missing
      )
    )
    
    # Render with gt
    summary_df %>%
      gt() %>%
      opt_interactive(use_compact_mode = TRUE)
  })
  
  #---------------------------
  # Data Cleaning
  #---------------------------
  
  output$var_select <- renderUI({
    req(data())  # Ensure data is available
    selectInput(
      inputId = "CleanVar",
      label   = "Select a variable:",
      choices = names(data()))
  })


  
## Change Factor Labels  
  output$FactorLevShow <- reactive({
    req(input$VarTypeTo)
    input$VarTypeTo == "Fac"
  })  
  outputOptions(output, "FactorLevShow", suspendWhenHidden = FALSE)
  
  output$FactorLevUI <- renderUI({
    req(data())  # Ensure data is available
    selectInput(
      inputId = "FactorLevels",
      label   = "Write Levels",
      choices = unique(input$CleanVar))
  })


  output$FactorLevUI <- renderUI({
    req(data())
    req(input$VarTypeTo)
    # Only generate the textInputs if user selected "Fac"
    if (input$VarTypeTo == "Fac") {
      unique_levels <- sort(unique(data()[[input$CleanVar]]))
      if (length(unique_levels) < 11){ # Requires less then 10 levels so it doesn't overwhelm page
      # Build the textInputs in a list
      theInputs <- list()
      for (lev in unique_levels) {
        theInputs[[lev]] <- textInput(
          inputId = paste0("custom_level_", lev),
          label   = paste("Label for:", lev),
          value   = lev  # default is the original
        )
      }
      theInputs
    }}
  })
  
  Factor_Labels <- reactive({
    req(data())
    req(input$VarTypeTo)
    # If not Fac, we might return NA or NULL
    if (input$VarTypeTo != "Fac") {
      return(NA)
    }
    
    unique_levels <- sort(unique(data()[[input$CleanVar]]))
    if (length(unique_levels) < 11){ # Requires less then 10 levels so it doesn't overwhelm page
    
    print(unique_levels)
    
    # Build a character vector by reading each text input
    newLabels <- c()
    for (lev in unique_levels) {
      newLabels <- c(newLabels, input[[paste0("custom_level_", lev)]])
    }
    
    newLabels }
  })
  

## Change Date
  output$DateFormShow <- reactive({
    req(input$VarTypeTo)
    input$VarTypeTo == "Date"
  })  
  outputOptions(output, "DateFormShow", suspendWhenHidden = FALSE)
  
  output$DateFormUI <- renderUI({
    req(data())  # Ensure data is available
    textInput(
      inputId = "DateFormat",
      label   = "Input Date Format", value = "%Y-%m-%d"
    )
  }) 
  

## Change data forms
  dataClean <- reactive({
    req(data())
    if (is.null(input$VarTypeTo)){data()}
    else{
    ChangeVarType(data(),
                  Var = input$CleanVar,
                  Type = input$VarTypeTo,
                  Labels = Factor_Labels(),
                  DateFormat = input$DateFormat)}
  })  

  
## Group Mean
  output$NAGroupShow <- reactive({
    req(input$NaMethod)
    input$NaMethod == "Group_Mean"
  })  
  outputOptions(output, "NAGroupShow", suspendWhenHidden = FALSE)
  
  
  
  output$NaGroupUI <- renderUI({
    req(dataClean())  # Ensure data is available
    selectInput(
      inputId = "NaGroup",
      label   = "Select Group: ",
      choices = names(dataClean() %>% select(where(~ is.character(.) || is.factor(.)))))
  })
  
  
## Impute Method
  output$MIMethodShow <- reactive({
    req(input$NaMethod)
    input$NaMethod == "Multiple_Imputation"
  })  
  outputOptions(output, "MIMethodShow", suspendWhenHidden = FALSE)
  
  output$MI_MethodUI <- renderUI({
    req(dataClean())  # Ensure data is available
    textInput(
      inputId = "ImputeMethod",
      label   = "Provide Multiple Imputation Method", value = "pmm"
    )
  }) 
  
  
  dataNA <- reactive({
    # Performs selected NA method on data; it is switch able so if some selects mean but then
    # does MI it will impute based on the original data (or with updated variable types)
    req(dataClean())
    NAOptions(dataClean(), Method = input$NaMethod, Group = input$NaGroup, impute_method = input$ImputeMethod)
  })

  
  output$CleanTable <- render_gt({
    req(dataNA())  # Ensure data is available
    # Create a summary tibble with variable names and their type
    summary_df <- tibble(
      Variable = names(dataNA()),
      Type     = map_chr(dataNA(), ~ class(.x)[1])  # Take the first class if multiple
    )
    summary_df %>% gt() %>% opt_interactive(use_compact_mode = T)
  })
  

  
  
  
  ChangeVarCode <- reactive({
    req(input$CleanVar)  # Ensure variable name is provided
    req(input$VarTypeTo) # Ensure variable type to convert is selected
    FileName <- tools::file_path_sans_ext(input$file$name) # Generates the file without extension
    
    switch(input$VarTypeTo,
           "Chr" = paste0(
             FileName, " <- ", FileName, " %>%",
             "\n  mutate(", input$CleanVar, " = as.character(", input$CleanVar, "\n))"
           ),
           "Num" = paste0(
             FileName, " <- ", FileName, " %>%",
             "\n  mutate(", input$CleanVar, " = as.numeric(\nas.character(", input$CleanVar, ")\n))"
           ),
           "Fac" = paste0(
             FileName, " <- ", FileName, " %>%",
             "\n  mutate(", input$CleanVar, " = factor(", input$CleanVar, ",\n labels = c(", 
             paste0('"', Factor_Labels(), '"', collapse = ", "), ")\n))"
           ),
           "Date" = paste0(
             FileName, " <- ", FileName, " %>%",
             "\n  mutate(", input$CleanVar, " = as.Date(", input$CleanVar, ",\n format = '", input$DateFormat, "'\n))"
           ),
           "Unsupported file type"
    )
  })
  
  
  # Render the appropriate code snippet
  output$ChangeCode <- renderText({
    ChangeVarCode()
  })
  
  output$ChangeCodeAvailable <- reactive({
    # Checks they selected something. 
    !is.null(input$CleanVar) && input$VarTypeTo %in% c("Chr", "Num", "Fac", "Date")
  })
  outputOptions(output, "ChangeCodeAvailable", suspendWhenHidden = FALSE)
  
  
  
  
  
  NACode <- reactive({
    req(input$NaMethod)  # Ensure a method is selected
    
    # Extract file name without extension
    FileName <- tools::file_path_sans_ext(input$file$name)
    
    # Handle the "Ignore" case to return NULL (no code displayed)
    if (input$NaMethod == "Ignore") {
      return(NULL)
    }
    
    # Generate the appropriate R code based on the selected method
    switch(input$NaMethod,
           "Complete_Cases" = paste0(
             FileName, " <- ", FileName, " %>%\n  filter(complete.cases(.))"
           ),
           "NaiveMean" = paste0(
             FileName, " <- ", FileName, " %>%\n  mutate(across(where(is.numeric), ~ ifelse(is.na(.),\n mean(., na.rm = TRUE), .)))"
           ),
           "Group_Mean" = paste0(
             FileName, " <- ", FileName, " %>%\n  group_by(", input$NaGroup, ") %>%\n  mutate(across(where(is.numeric),\n ~ ifelse(is.na(.),\n mean(., na.rm = TRUE), .))) %>%\n  ungroup()"
           ),
           "Multiple_Imputation" = paste0(
             "library(mice)\n",
             "imputed_data <- mice(", FileName, ",\n m = 1,\n maxit = 1,\n method = '", input$ImputeMethod, "', seed = 91)\n",
             FileName, " <- complete(imputed_data, 1)"
           ),
           "Unsupported method"
    )
  })
  
  output$NACode <- renderText({
    NACode()
  })
  
  output$NACodeAvailable <- reactive({
    # Checks they did something other then ignore NA's
    !is.null(input$NaMethod) && 
      input$NaMethod %in% c("Complete_Cases", "NaiveMean", "Group_Mean", "Multiple_Imputation") 
  })
  outputOptions(output, "NACodeAvailable", suspendWhenHidden = FALSE)
  
  
  
  
  
  
 
  #---------------------------
  # Review Data
  #---------------------------
  
  
  output$CleanTableHead <- render_gt({
    req(dataNA())  # Ensure data is available
    
    # Transpose the data and convert it to a tibble
    CleanTab <- dataNA() %>%
      head(n = 5) %>% # Show only first 5 to reduce total space and overall speed 
      t() %>% # Sets table to be lengthwise to better fit window and is less crammed since variables are rows
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Variable")  # Preserve row names as a column
    
    # Assign new column names instead of V1 V2....
    colnames(CleanTab) <- c("Variable Name", "Obs. 1", "Obs. 2", "Obs. 3", "Obs. 4", "Obs. 5")
    
    # Render with gt
    CleanTab %>%
      gt() %>%
      opt_interactive(use_compact_mode = TRUE)
  })
  
  #---------------------------
  # Descriptive
  #---------------------------
 
  # Dynamically generate variable selection input based on input dataset
  output$variableSelector <- renderUI({
    req(dataNA())
    var_names <- colnames(dataNA())
    # To avoid stressing it only does the first 8 but allows for more to be picked
    # The plot function is a single output so if you ran it on data with 100 variables
    # it would take a long time and produce 100 plots.
    if (length(var_names) > 8) {
      checkboxGroupInput(
        "selectedVars",
        "Select Variables:",
        choices = var_names,
        selected = var_names[1:8] 
      )
    }
  })
  
  # Render the summary table
  output$summary <- render_gt({
    req(dataNA())
    req(input$selectedVars)
    
    if (!is.null(input$selectedVars)) {
      plotData <- dataNA() %>% select(all_of(input$selectedVars))
      print(head(plotData))
    }
    
    gt_plt_summary(plotData, title = "Descriptive Statistics") %>%
      tab_options(
      table.width = pct(99),     # Adjust table width
      table.align = "center",    # Center-align the table
      table.font.size = px(14),  # Adjust font size 
      data_row.padding = px(3)   # Add padding between rows
    
  )})
  
  
  # This exists to hide the variable side bar. Provides JS backend to just transition it. 
  observe({
    runjs("
      let toggleButton = document.getElementById('toggleButton');
      let sidebar = document.getElementById('sidebar');
      let mainContent = document.getElementById('mainContent');

      toggleButton.addEventListener('click', function() {
        if (sidebar.style.transform === 'translateX(-260px)') {
          // Show sidebar
          sidebar.style.transform = 'translateX(0)';
          mainContent.style.marginLeft = '260px';
          toggleButton.innerHTML = 'Hide Variables';
        } else {
          // Hide sidebar
          sidebar.style.transform = 'translateX(-260px)';
          mainContent.style.marginLeft = '10px';
          toggleButton.innerHTML = 'Show Variables';
        }
      });
    ")
    
   })
}





# Run the application 
shinyApp(ui = ui, server = server)
