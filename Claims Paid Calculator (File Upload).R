library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(reshape2)

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
    Loss_Year = integer(),
    Development_Year = integer(),
    Cumulative_Claims = numeric()
  ))
  
  # Read the uploaded Excel file
  observeEvent(input$file, {
    req(input$file)
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    data <- read_excel(inFile$datapath)
    if ("Loss_Year" %in% colnames(data) && "Development_Year" %in% colnames(data) && "Claims_Amount" %in% colnames(data)) {
      data$Loss_Year <- as.integer(data$Loss_Year)
      data$Development_Year <- as.integer(data$Development_Year)
      claims_data(data)
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please ensure the uploaded file contains columns: Loss_Year, Development_Year, Claims_Amount."
      ))
    }
  })
  
  # Initialize cumulative claims data frame with one row
  cumulative_data <- reactiveVal(data.frame(
    Loss_Year = integer(),
    Development_Year = integer(),
    Cumulative_Claims = numeric()
  ))
  
  # Cumulative claims calculation
  calculate_cumulative_claims <- eventReactive(input$calculate_btn, {
    req(claims_data())
    data <- claims_data()
    
    # Sort data by Loss Year and Development Year
    data <- data[order(data$Loss_Year, data$Development_Year), ]
    
    # Initialize a list to store cumulative data frames
    cumulative_data_list <- list()
    
    # Calculate the maximum number of development years across all loss years
    max_dev_years <- max(data$Development_Year, na.rm = TRUE)
    
    # Loop through each loss year
    for (i in unique(data$Loss_Year)) {
      loss_year_data <- data[data$Loss_Year == i, ]
      
      # Determine the maximum number of development years for this loss year
      max_dev_years_loss <- max(loss_year_data$Development_Year, na.rm = TRUE)
      
      # Initialize cumulative claims for this loss year
      cumulative_year <- data.frame(
        Loss_Year = rep(i, max_dev_years_loss + 1),  # Include subsequent development year
        Development_Year = 1:(max_dev_years_loss + 1),  # Include subsequent development year
        Cumulative_Claims = NA
      ) 
      
      # Calculate cumulative claims for defined claims amount (type 1 calculation)
      defined_claims <- loss_year_data[!is.na(loss_year_data$Claims_Amount), ]
      if (nrow(defined_claims) > 0) {
        for (j in 1:max_dev_years_loss) {
          cumulative_year$Cumulative_Claims[j] <- sum(defined_claims$Claims_Amount[defined_claims$Development_Year <= j])
        }
      }
      
      # Append cumulative_year to the list
      cumulative_data_list[[i]] <- cumulative_year
      
      # Print intermediate result for debugging
      print(paste("Cumulative Data for Loss Year", i))
      print(cumulative_year)
    }
    
    # Combine all cumulative data frames into one
    cumulative_data <- do.call(rbind, cumulative_data_list)
    return(cumulative_data)
  })
  
  # Render cumulative claims table
  output$cumulative_claims_table <- renderTable({
    cumulative_data <- calculate_cumulative_claims()
    
    if (!is.null(cumulative_data)) {
      # Convert Loss Year and Development Year to integers
      cumulative_data$Loss_Year <- as.integer(cumulative_data$Loss_Year)
      cumulative_data$Development_Year <- as.integer(cumulative_data$Development_Year)
      
      # Sort data by Loss Year and Development Year
      cumulative_data <- cumulative_data[order(cumulative_data$Loss_Year, cumulative_data$Development_Year), ]
      
      # Print cumulative data frame for debugging
      print(cumulative_data)
      
      # Pivot the table
      cumulative_table <- dcast(cumulative_data, Loss_Year ~ Development_Year, value.var = "Cumulative_Claims")
      
      # Order columns for better visualization
      cumulative_table <- cumulative_table[, c(1, order(as.integer(colnames(cumulative_table)[2:ncol(cumulative_table)])) + 1)]
      
      return(cumulative_table)
    }
    
    return(NULL)
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
      cumulative_data$Loss_Year <- as.integer(cumulative_data$Loss_Year)
      cumulative_data$Development_Year <- as.integer(cumulative_data$Development_Year)
      
      # Sort data by Loss Year and Development Year
      cumulative_data <- cumulative_data[order(cumulative_data$Loss_Year, cumulative_data$Development_Year), ]
      
      # Pivot the table
      cumulative_table <- dcast(cumulative_data, Loss_Year ~ Development_Year, value.var = "Cumulative_Claims")
      
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
      ggplot(cumulative_data, aes(x = Development_Year, y = Cumulative_Claims, color = factor(Loss_Year))) +
        geom_line() +
        labs(title = "Cumulative Claims Graph", x = "Development Year", y = "Cumulative Claims", color = "Loss Year")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

