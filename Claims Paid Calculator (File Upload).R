library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

# Define UI
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims Calculator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel file", accept = c(".xlsx")),
      numericInput("tail_factor", "Tail Factor", value = 1),
      actionButton("calculate_btn", "Calculate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Input Claims Data", tableOutput("claims_table")),
        tabPanel("Cumulative Claims Paid", tableOutput("cumulative_claims_table")),
        tabPanel("Cumulative Claims Graph", plotOutput("claims_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize claims data
  claims_data <- reactiveVal(NULL)
  
  # Initialize cumulative claims data frame with one row
  cumulative_data <- reactiveVal(data.frame(
    `Loss Year` = integer(),
    `Development Year` = integer(),
    `Cumulative Claims` = numeric()
  ))
  
  # Read the uploaded Excel file
  observeEvent(input$file, {
    req(input$file)
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    data <- read_excel(inFile$datapath)
    if ("Loss Year" %in% colnames(data) && "Development Year" %in% colnames(data) && "Claims Amount" %in% colnames(data)) {
      data$`Loss Year` <- as.integer(data$`Loss Year`)
      data$`Development Year` <- as.integer(data$`Development Year`)
      claims_data(data)
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please ensure the uploaded file contains columns: Loss Year, Development Year, Claims Amount."
      ))
    }
  })
  
  # Cumulative claims calculation
  calculate_cumulative_claims <- eventReactive(input$calculate_btn, {
    req(claims_data())
    data <- claims_data()
    
    # Sort data by Loss Year and Development Year
    data <- data[order(data$`Loss Year`, data$`Development Year`), ]
    
    # Initialize a list to store cumulative data frames
    cumulative_data_list <- list()
    
    # Loop through each loss year
    for (i in unique(data$`Loss Year`)) {
      loss_year_data <- data[data$`Loss Year` == i, ]
      
      # Determine the maximum number of development years from user input or data
      max_dev_years <- max(loss_year_data$`Development Year`, na.rm = TRUE)
      
      # Initialize cumulative claims for this loss year
      cumulative_year <- data.frame(
        `Loss Year` = rep(i, max_dev_years),
        `Development Year` = 1:max_dev_years,
        `Cumulative Claims` = NA
      ) 
      
      # Calculate cumulative claims for defined claims amount
      defined_claims <- loss_year_data[!is.na(loss_year_data$`Claims Amount`), ]
      if (nrow(defined_claims) > 0) {
        for (j in 1:max_dev_years) {
          cumulative_year$'Cumulative Claims'[j] <- sum(defined_claims$'Claims Amount'[defined_claims$'Development Year' <= j])
        }
      }
      
      # Calculate cumulative claims for non-defined claims amount using tail factor or previous year's claims amount
      if (nrow(defined_claims) < max_dev_years) {
        current_dev_year <- max(defined_claims$`Development Year`)
        sum_pastcum_same_dev <- sum(cumulative_data()$`Cumulative Claims`[cumulative_data()$`Development Year` == current_dev_year & cumulative_data()$`Loss Year` != i])
        sum_pastcum_prev_dev <- sum(cumulative_data()$`Cumulative Claims`[cumulative_data()$`Development Year` == current_dev_year - 1 & cumulative_data()$`Loss Year` != i])
        prev_loss_dev <- cumulative_data()$`Cumulative Claims`[cumulative_data()$`Loss Year` == i & cumulative_data()$`Development Year` == current_dev_year - 1]
        
        if (any(!is.na(cumulative_data()$`Cumulative Claims`) & cumulative_data()$`Development Year` == current_dev_year)) {
          # Step 1 Calculation 
          cumulative_year$`Cumulative Claims`[1:max_dev_years] <- prev_loss_dev * sum_pastcum_same_dev / sum_pastcum_prev_dev
        } else if (any(!is.na(cumulative_data()$`Cumulative Claims`) & cumulative_data()$`Development Year` < current_dev_year)) {
          # Step 2 Calculation (based on one cumulative claims amount)
          cum_prevloss_samedev <- cumulative_data()$`Cumulative Claims`[cumulative_data()$`Development Year` == current_dev_year]
          cum_prevloss_prevdev <- cumulative_data()$`Cumulative Claims`[cumulative_data()$`Development Year` == max_dev_years - 1]
          cumulative_year$`Cumulative Claims`[1:max_dev_years] <- prev_loss_dev * cum_prevloss_samedev / cum_prevloss_prevdev
        } else {
          # Step 3 Calculation (latest dev year)
          cumulative_year$`Cumulative Claims`[max_dev_years + 1] <- cumulative_year$`Cumulative Claims`[max_dev_years] * input$tail_factor
        }
      }
      
      # Append cumulative_year to the list
      cumulative_data_list[[i]] <- cumulative_year
      
      # Print intermediate result for debugging
      print(paste("Cumulative Data for Loss Year", i))
      print(cumulative_year)
    }
    
    # Combine all cumulative data frames into one
    cumulative_data <- bind_rows(cumulative_data_list)
    
    return(cumulative_data)
  })
  
  # Render input claims table
  output$claims_table <- renderTable({
    req(claims_data())
    claims_data()
  })
  
  # Render cumulative claims table
  output$cumulative_claims_table <- renderTable({
    cumulative_data <- calculate_cumulative_claims()
    
    if (!is.null(cumulative_data)) {
      # Convert Loss Year and Development Year to integers
      cumulative_data$`Loss Year` <- as.integer(cumulative_data$`Loss Year`)
      cumulative_data$`Development Year` <- as.integer(cumulative_data$`Development Year`)
      
      # Sort data by Loss Year and Development Year
      cumulative_data <- cumulative_data[order(cumulative_data$`Loss Year`, cumulative_data$`Development Year`), ]
      
      # Split data into defined and non-defined claims
      defined_claims <- cumulative_data[!is.na(cumulative_data$`Cumulative Claims`), ]
      undefined_claims <- cumulative_data[is.na(cumulative_data$`Cumulative Claims`), ]
      
      # Pivot the defined claims data
      defined_pivot <- defined_claims %>% 
        pivot_wider(names_from = `Development Year`, values_from = `Cumulative Claims`)
      
      # Pivot the non-defined claims data
      undefined_pivot <- undefined_claims %>% 
        pivot_wider(names_from = `Development Year`, values_from = `Cumulative Claims`)
      
      # Merge the two tables
      cumulative_table <- merge(defined_pivot, undefined_pivot, by = "Loss Year", suffixes = c("_Defined", "_Non_Defined"))
      
      # Order columns for better visualization
      cumulative_table <- cumulative_table[, c(1, order(as.integer(colnames(cumulative_table)[2:ncol(cumulative_table)])) + 1)]
      
      return(cumulative_table)
    }
    
    return(NULL)
  })
  
  # Render cumulative claims graph
  output$claims_plot <- renderPlot({
    cumulative_data <- calculate_cumulative_claims()
    if (!is.null(cumulative_data)) {
      ggplot(cumulative_data, aes(x = `Development Year`, y = `Cumulative Claims`, color = factor(`Loss Year`))) +
        geom_line() +
        labs(title = "Cumulative Claims Graph", x = "Development Year", y = "Cumulative Claims", color = "Loss Year")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
