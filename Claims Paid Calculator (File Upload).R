library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

# Define UI
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims Calculation"),
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
  
  # Read the uploaded Excel file
  claims_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    claims_data(read_excel(inFile$datapath))
  })
  
  # Calculate cumulative claims
  calculate_cumulative_claims <- eventReactive(input$calculate_btn, {
    req(claims_data())
    
    # Convert Loss_Year and Development_Year to numeric
    claims_data()$Loss_Year <- as.numeric(claims_data()$Loss_Year)
    claims_data()$Development_Year <- as.numeric(claims_data()$Development_Year)
    
    # Sort data by Loss_Year and Development_Year
    claims_data() <- claims_data()[order(claims_data()$Loss_Year, claims_data()$Development_Year), ]
    
    # Initialize cumulative claims data frame
    cumulative_data <- data.frame(
      Loss_Year = numeric(),
      Development_Year = numeric(),
      Cumulative_Claims = numeric()
    )
    
    # Loop through each loss year
    for (i in unique(claims_data()$Loss_Year)) {
      loss_year_data <- claims_data()[claims_data()$Loss_Year == i, ]
      
      # Initialize cumulative claims for this loss year
      cumulative_year <- data.frame(
        Loss_Year = rep(i, 4),
        Development_Year = 1:4,
        Cumulative_Claims = NA
      )
      
      # Calculate cumulative claims for defined claims amount
      defined_claims <- loss_year_data[!is.na(loss_year_data$Claims_Amount), ]
      if (nrow(defined_claims) > 0) {
        for (j in 1:4) {
          cumulative_year$Cumulative_Claims[j] <- sum(defined_claims$Claims_Amount[defined_claims$Development_Year <= j])
        }
      }
      
      # Calculate cumulative claims for non-defined claims amount using tail factor or previous year's claims amount
      if (nrow(defined_claims) < 4) {
        if (i == min(claims_data()$Loss_Year)) {
          # Type 3 Calculation for latest Development Year 
          cumulative_year$Cumulative_Claims[4] <- cumulative_year$Cumulative_Claims[3] * input$tail_factor
        } else {
          prev_loss_year_data <- cumulative_data[cumulative_data$Loss_Year == i - 1, ]
          prev_year_dev3 <- prev_loss_year_data$Cumulative_Claims[prev_loss_year_data$Development_Year == 3]
          
          # Check if there are non-defined claims in past loss years with the same development year
          if (any(is.na(prev_loss_year_data$Cumulative_Claims))) {
            # Type 1 Calculation
            first_loss_year_same_dev <- cumulative_data[cumulative_data$Loss_Year == i & cumulative_data$Development_Year == 3, ]
            cumulative_year$Cumulative_Claims[3] <- prev_year_dev3 * first_loss_year_same_dev$Cumulative_Claims[1] / first_loss_year_same_dev$Cumulative_Claims[3]
          } else {
            # Type 2 Calculation
            cumulative_year$Cumulative_Claims[3] <- prev_year_dev3 * sum(prev_loss_year_data$Cumulative_Claims[prev_loss_year_data$Development_Year == 3]) / sum(prev_loss_year_data$Cumulative_Claims[prev_loss_year_data$Development_Year == 2])
          }
        }
      }
      
      cumulative_data <- bind_rows(cumulative_data, cumulative_year)
    }
    
    # Remove .00 from Loss_Year and Development_Year
    cumulative_data$Loss_Year <- as.integer(cumulative_data$Loss_Year)
    cumulative_data$Development_Year <- as.integer(cumulative_data$Development_Year)
    
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
      cumulative_data <- cumulative_data %>% 
        pivot_wider(names_from = Development_Year, values_from = Cumulative_Claims)
      
      # Remove .00 from Loss_Year and Development_Year
      cumulative_data$Loss_Year <- as.integer(cumulative_data$Loss_Year)
      cumulative_data$Development_Year <- as.integer(cumulative_data$Development_Year)
      
      return(cumulative_data)
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
