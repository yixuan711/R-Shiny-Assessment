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
    
    # Convert Loss Year and Development Year to integer
    data$`Loss Year` <- as.integer(data$`Loss Year`)
    data$`Development Year` <- as.integer(data$`Development Year`)
    
    # Sort data by Loss Year and Development Year
    data <- data[order(data$`Loss Year`, data$`Development Year`), ]
    
    # Initialize a list to store cumulative data frames
    cumulative_data_list <- list()
    
    # Initialize prev_year_dev3
    prev_year_dev3 <- 0
    
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
      
      # Calculate cumulative claims for defined claims amount only if claims amount is shown
      defined_claims <- loss_year_data[!is.na(loss_year_data$`Claims Amount`), ]
      if (nrow(defined_claims) > 0) {
        for (j in 1:max_dev_years) {
          cumulative_year$'Cumulative Claims'[j] <- sum(defined_claims$'Claims Amount'[defined_claims$'Development Year' <= j])
        }
      }
      
      # Append cumulative_year to the list
      cumulative_data_list[[i]] <- cumulative_year
      
      # Update prev_year_dev3
      prev_year_dev3 <- cumulative_year$`Cumulative Claims`[max_dev_years]
    }
    
    # Combine all cumulative data frames into one
    cumulative_data <- bind_rows(cumulative_data_list)
    
    # Remove .00 from Loss Year and Development Year
    cumulative_data$`Loss Year` <- as.integer(cumulative_data$`Loss Year`)
    cumulative_data$`Development Year` <- as.integer(cumulative_data$`Development Year`)
    
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
      # Check if 'Loss Year' column exists before mutating
      if ('Loss Year' %in% colnames(cumulative_data())) {
        cumulative_data <- cumulative_data() %>%
          mutate(`Loss Year` = as.integer(`Loss Year`))
      }
      
      # Pivot the data
      cumulative_table <- cumulative_data %>% 
        pivot_wider(names_from = `Development Year`, values_from = `Cumulative Claims`)
      
      # Reorder columns to have Development Year in ascending order
      cumulative_table <- cumulative_table %>%
        select(`Loss Year`, order(as.integer(colnames(cumulative_table))[-1]) + 1)
      
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
