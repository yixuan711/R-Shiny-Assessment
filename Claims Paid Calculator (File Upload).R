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
    
    # Convert Loss Year and Development Year to numeric
    data$`Loss Year` <- as.numeric(data$`Loss Year`)
    data$`Development Year` <- as.numeric(data$`Development Year`)
    
    # Sort data by Loss Year and Development Year
    data <- data[order(data$`Loss Year`, data$`Development Year`), ]
    
    # Initialize cumulative claims data frame
    cumulative_data <- data.frame(
      `Loss Year` = numeric(),
      `Development Year` = numeric(),
      `Cumulative Claims` = numeric()
    )
    
    # Initialize prev_loss_year_data and prev_year_dev3
    prev_loss_year_data <- NULL
    prev_year_dev3 <- NULL
    
    # Loop through each loss year
    for (i in unique(data$`Loss Year`)) {
      loss_year_data <- data[data$`Loss Year` == i, ]
      
      # Determine the maximum number of development years from user input or data
      max_dev_years <- max(loss_year_data$`Development Year`)
      
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
      
      # Calculate cumulative claims for non-defined claims amount using tail factor or previous year's claims amount
      if (nrow(defined_claims) < max_dev_years) {
        if (any(!is.na(prev_loss_year_data$`Cumulative Claims`) & prev_loss_year_data$`Development Year` == max_dev_years)) {
          # Step 1 Calculation (based on sum of past years)
          cumulative_year$`Cumulative Claims`[1:max_dev_years] <- prev_year_dev3 * sum(prev_loss_year_data$`Cumulative Claims`[prev_loss_year_data$`Development Year` == max_dev_years]) / sum(prev_loss_year_data$`Cumulative Claims`[prev_loss_year_data$`Development Year` == max_dev_years - 1])
        } else if (any(!is.na(prev_loss_year_data$`Cumulative Claims`) & prev_loss_year_data$`Development Year` < max_dev_years)) {
          # Step 2 Calculation (based on one cumulative claims amount)
          cumulative_year$`Cumulative Claims`[1:max_dev_years] <- prev_year_dev3 * prev_loss_year_data$`Cumulative Claims`[prev_loss_year_data$`Development Year` == max_dev_years] / prev_loss_year_data$`Cumulative Claims`[prev_loss_year_data$`Development Year` == max_dev_years - 1]
        } else {
          # Step 3 Calculation (latest dev year)
          cumulative_year$`Cumulative Claims`[max_dev_years + 1] <- cumulative_year$`Cumulative Claims`[max_dev_years] * input$tail_factor
        }
      }
      
      # Update prev_loss_year_data and prev_year_dev3
      prev_loss_year_data <- cumulative_year
      prev_year_dev3 <- cumulative_year$`Cumulative Claims`[max_dev_years]
      
      cumulative_data <- bind_rows(cumulative_data, cumulative_year)
    }
    
    # Determine the maximum development year with Step 3 calculation
    max_dev_year_step3 <- max(cumulative_data$`Development Year`[!is.na(cumulative_data$`Cumulative Claims`)])
    
    # Filter the cumulative data up to the subsequent of the maximum development year with Step 3 calculation
    cumulative_data <- cumulative_data[cumulative_data$`Development Year` <= max_dev_year_step3 + 1, ]
    
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
      # Pivot the data
      cumulative_table <- cumulative_data %>% 
        pivot_wider(names_from = `Development Year`, values_from = `Cumulative Claims`)
      
      # Reorder columns to have Development Year in ascending order
      cumulative_table <- cumulative_table %>%
        select(`Loss Year`, order(as.numeric(colnames(cumulative_table))[-1]) + 1)
      
      # Remove .00 from Loss Year and Development Year
      cumulative_table$`Loss Year` <- as.integer(cumulative_table$`Loss Year`)
      colnames(cumulative_table)[-1] <- as.integer(colnames(cumulative_table)[-1])
      
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
