library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define UI
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims Calculation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File"),
      numericInput("tail_factor", "Tail Factor", value = 1),
      actionButton("calculate_btn", "Calculate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Input Claims Data Table", tableOutput("claims_table")),  # Include input claims data table tab
        tabPanel("Output Cumulative Claims Paid", tableOutput("cumulative_claims_table")),
        tabPanel("Cumulative Claims Paid Graph", plotOutput("claims_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize claims data
  claims_data <- reactiveVal(NULL)
  
  # Read uploaded file
  observeEvent(input$file, {
    req(input$file)
    inFile <- input$file
    if (grepl("\\.xlsx$", inFile$name)) {
      claims_data(read_excel(inFile$datapath))
    } else {
      # Handle error for unsupported file format
      showModal(modalDialog(
        title = "Error",
        "Please upload an Excel file (.xlsx)."
      ))
    }
  })
  
  # Calculate cumulative claims
  calculate_cumulative_claims <- eventReactive(input$calculate_btn, {
    if (!is.null(claims_data())) {
      claims_data_sorted <- claims_data()[order(claims_data()$Loss_Year, claims_data()$Development_Year), ]
      cumulative_data <- data.frame(
        Loss_Year = numeric(),
        Development_Year = numeric(),
        Cumulative_Claims = numeric()
      )
      for (i in unique(claims_data_sorted$Loss_Year)) {
        loss_year_data <- claims_data_sorted[claims_data_sorted$Loss_Year == i, ]
        cumulative_year <- data.frame(
          Loss_Year = rep(i, 4),
          Development_Year = 1:4,
          Cumulative_Claims = NA
        )
        defined_claims <- loss_year_data[!is.na(loss_year_data$Claims_Amount), ]
        if (nrow(defined_claims) > 0) {
          for (j in 1:4) {
            cumulative_year$Cumulative_Claims[j] <- sum(defined_claims$Claims_Amount[defined_claims$Development_Year <= j])
          }
        }
        if (nrow(defined_claims) < 4) {
          if (i == min(claims_data_sorted$Loss_Year)) {
            cumulative_year$Cumulative_Claims[4] <- cumulative_year$Cumulative_Claims[3] * input$tail_factor
          } else {
            prev_loss_year_data <- cumulative_data[cumulative_data$Loss_Year == i - 1, ]
            prev_year_dev3 <- prev_loss_year_data$Cumulative_Claims[prev_loss_year_data$Development_Year == 3]
            if (any(is.na(prev_loss_year_data$Cumulative_Claims))) {
              first_loss_year_same_dev <- cumulative_data[cumulative_data$Loss_Year == i & cumulative_data$Development_Year == 3, ]
              cumulative_year$Cumulative_Claims[3] <- prev_year_dev3 * first_loss_year_same_dev$Cumulative_Claims[1] / first_loss_year_same_dev$Cumulative_Claims[3]
            } else {
              cumulative_year$Cumulative_Claims[3] <- prev_year_dev3 * sum(prev_loss_year_data$Cumulative_Claims[prev_loss_year_data$Development_Year == 3]) / sum(prev_loss_year_data$Cumulative_Claims[prev_loss_year_data$Development_Year == 2])
            }
          }
        }
        cumulative_data <- bind_rows(cumulative_data, cumulative_year)
      }
      cumulative_data$Loss_Year <- as.integer(cumulative_data$Loss_Year)
      cumulative_data$Development_Year <- as.integer(cumulative_data$Development_Year)
      return(cumulative_data)
    } else {
      return(NULL)
    }
  })
  
  # Render input claims table
  output$claims_table <- renderTable({
    if (!is.null(claims_data())) {
      claims_data()
    }
  })
  
  # Render cumulative claims table
  output$cumulative_claims_table <- renderTable({
    cumulative_data <- calculate_cumulative_claims()
    if (!is.null(cumulative_data)) {
      pivot_wider(cumulative_data, names_from = Development_Year, values_from = Cumulative_Claims) %>%
        arrange(Loss_Year)
    }
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
