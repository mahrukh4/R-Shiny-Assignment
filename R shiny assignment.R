library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

ui <- fluidPage(
  titlePanel("R Shiny Training"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Upload an Excel or csv File", accept = c(".xlsx", ".csv")),  
      textOutput("file_format"),
      sliderInput("tail_param", "Tail Parameter", min = 0, max = 10, value = 1.1, step=0.1),
      textOutput("tail_value"),
    ),
    
    mainPanel(
      textOutput("error"),
      tabsetPanel(
        tabPanel("Claim Data", dataTableOutput("dynamic"),textOutput("definitions")),
        tabPanel("Cumulative Table", tableOutput("cumulative_table")),
        tabPanel("Plot", plotOutput("cumulative_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Validate file extension
  output$error <- renderText({
    req(input$upload) # Ensure a file has been uploaded
    ext <- tools::file_ext(input$upload$name) # Get the file extension
    if (!ext %in% c("xlsx", "csv")) {
      return("Error: Please upload a valid Excel file (.xlsx or .csv).")
    }
    NULL # Return NULL if there is no error
  })
  
  output$file_format <- renderText({
    paste0("Please ensure that the uploaded file has the following columns in this order:Loss year, Development year, Amount of claims paid")
  })
  
  output$dynamic <- renderDataTable({
    req(input$upload) # Ensure a file has been uploaded
    ext <- tools::file_ext(input$upload$name) # Get the file extension
    if (ext %in% c("xlsx", "csv")) {
      read_excel(input$upload$datapath, skip=3) # Read and display the Excel file contents
    }
  })
  
  output$tail_value <- renderText({
    paste0("Selected Tail Parameter:", input$tail_param)
  })
  
  
  output$definitions <- renderText({
    paste(
      "Loss Year: The year a claim occurred.\n",
      "Development Year: The years since the loss occurred.\n",
      "Amount of Claims Paid: Total paid for claims."
    )
  })
  
  
  cumulative_table_reactive <- reactive({
    # Compute cumulative claims table dynamically
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    
    if (ext %in% c("xlsx", "csv")) {
      
      # Read claims paid data
      claims_data <- read_excel(input$upload$datapath, skip = 1)
      
      # Ensure column names are valid
      names(claims_data) <- make.names(names(claims_data), unique = TRUE)
      
      # Rename columns
      if (ncol(claims_data) >= 3) {
        colnames(claims_data) <- c("Loss_Year", "Development_Year", "Claims_Paid")
      } else {
        return(data.frame(Error = "Invalid file format. Ensure at least 3 columns: Loss Year, Development Year, Claims Paid."))
      }
      
      # Convert to numeric and replace NA with 0
      claims_data <- claims_data %>%
        mutate(across(everything(), ~ as.numeric(.), .names = "clean_{col}")) %>%
        replace(is.na(.), 0)  
      
      # Compute cumulative claims
      cumulative_data <- claims_data %>%
        arrange(Loss_Year, Development_Year) %>%
        group_by(Loss_Year) %>%
        mutate(Cumulative_Paid_Claims = cumsum(Claims_Paid)) %>%
        ungroup()
      
      # Pivot table
      cumulative_table_wide <- cumulative_data %>%
        select(Loss_Year, Development_Year, Cumulative_Paid_Claims) %>%
        pivot_wider(
          names_from = Development_Year, 
          values_from = Cumulative_Paid_Claims,
          names_prefix = "Year_"
        ) %>%
        arrange(Loss_Year) %>%
        replace(is.na(.), 0)
      
      # Ensure proper column names
      colnames(cumulative_table_wide) <- c("Loss Year", "1", "2", "3", "4")
      
      # Remove the last "Loss Year" row
      cumulative_table_wide <- cumulative_table_wide[-nrow(cumulative_table_wide), ]
      
      # Apply formulas for missing values
      cumulative_table_wide <- cumulative_table_wide %>%
        mutate(
          `2` = ifelse(`Loss Year` == 2019, sum(`2`[1:2]) / sum(`1`[1:2]) * `1`, `2`),
          `3` = case_when(
            `Loss Year` == 2018 ~ `2` * cumulative_table_wide$`3`[1] / cumulative_table_wide$`2`[1], 
            `Loss Year` == 2019 ~ `2` * cumulative_table_wide$`3`[1] / cumulative_table_wide$`2`[1],
            TRUE ~ `3`
          ),
          `4` = `3` * input$tail_param
        )
      
      # Format values with commas
      cumulative_table_wide <- cumulative_table_wide %>%
        mutate(across(where(is.numeric), ~ format(., big.mark = ",", scientific = FALSE)))
      
      return(cumulative_table_wide)
    }
  })
  
  output$cumulative_table <- renderTable({
    req(cumulative_table_reactive())  # Ensure data exists
    cumulative_table_reactive()
  })
  
  output$cumulative_plot <- renderPlot({
    req(cumulative_table_reactive())  # Ensure data exists
    
    # Get the table data
    cumulative_table <- cumulative_table_reactive()
    
    # Ensure numeric format for plotting
    cumulative_table <- cumulative_table %>%
      mutate(across(-`Loss Year`, ~ as.numeric(gsub(",", "", .))))
    
    # Convert to long format for ggplot
    cumulative_long <- pivot_longer(cumulative_table, 
                                    cols = -`Loss Year`, 
                                    names_to = "Development Year", 
                                    values_to = "Cumulative Paid Claims") %>%
      mutate(`Development Year` = as.numeric(`Development Year`))
    
    # Plot using ggplot
    ggplot(cumulative_long, aes(x = `Development Year`, 
                                y = `Cumulative Paid Claims`, 
                                color = factor(`Loss Year`))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_text(aes(label = format(`Cumulative Paid Claims`, big.mark = ",")), 
                vjust = -0.5, size = 4, fontface = "bold") +  
      labs(title = "Cumulative Paid Claims ($)", 
           x = "Development Year", y = "Claims Paid", color = "Loss Year") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  
}

shinyApp(ui, server)

