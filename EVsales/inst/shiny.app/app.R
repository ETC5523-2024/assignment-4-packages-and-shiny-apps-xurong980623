# Load the necessary packages
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(EVsales)

# Define UI for the Shiny App
ui <- fluidPage(

  # App title
  titlePanel("Global EV Sales Analysis"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(

      # Input: Select Region
      selectInput("region",
                  "Select Region:",
                  choices = c("All", unique(ev_data$Grouped_Region))),

      # Input: Year range slider
      sliderInput("year",
                  "Select Year Range:",
                  min = min(ev_data$year),
                  max = max(ev_data$year),
                  value = c(min(ev_data$year), max(ev_data$year))),

      # Input: Select Parameter
      selectInput("parameter",
                  "Select Parameter:",
                  choices = c("EV sales", "EV sales share", "EV stock", "EV stock share")),

      # Input: Select Powertrain type
      selectInput("powertrain",
                  "Select Powertrain:",
                  choices = c("All", unique(ev_data$powertrain))),

      # Add description for users
      p("Use the filters above to explore different regions, years, and types of electric vehicles (EVs)."),
      p("Sales parameters: Choose between 'EV sales' (number of sales), 'EV sales share' (percentage share of total vehicle sales), 'EV stock' (total number of EVs), or 'EV stock share'."),
      p("Powertrain: Select the type of EV power (BEV, PHEV, or FCEV).")
    ),

    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Sales Over Time", plotlyOutput("sales_plot")),
        tabPanel("Worldwide Distribution", plotlyOutput("dist_plot")),
        tabPanel("Sales Share by Region", plotlyOutput("share_plot")),
        tabPanel("Data Table", DTOutput("table"))
      )
    )
  )
)

# Define server logic for the Shiny App
server <- function(input, output) {

  # Reactive expression: Filtered data based on user input
  filtered_data <- reactive({
    data <- ev_data

    if (input$region != "All") {
      data <- data %>% filter(Grouped_Region == input$region)
    }

    if (input$powertrain != "All") {
      data <- data %>% filter(powertrain == input$powertrain)
    }

    data <- data %>% filter(year >= input$year[1] & year <= input$year[2])

    return(data)
  })

  # Plot 1: Sales over time (Plotly)
  output$sales_plot <- renderPlotly({
    data <- filtered_data() %>% filter(parameter == input$parameter)

    p <- ggplot(data, aes(x = year, y = value, fill = Grouped_Region)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "EV Sales Over Time by Region", x = "Year", y = "Sales Value") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")

    ggplotly(p)
  })

  # Plot 2: Worldwide distribution (Plotly)
  output$dist_plot <- renderPlotly({
    data <- filtered_data() %>% filter(parameter == "EV sales")

    p <- ggplot(data, aes(x = powertrain, y = value, fill = powertrain)) +
      geom_bar(stat = "identity") +
      labs(title = "Worldwide Distribution of EV Sales by Powertrain Type",
           x = "Powertrain", y = "Total Sales") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")

    ggplotly(p)
  })

  # Plot 3: Sales share by region (Plotly)
  output$share_plot <- renderPlotly({
    data <- filtered_data() %>% filter(parameter == "EV sales")

    data <- data %>%
      group_by(Grouped_Region, powertrain) %>%
      summarize(total_sales = sum(value, na.rm = TRUE)) %>%
      ungroup()

    p <- ggplot(data, aes(x = Grouped_Region, y = total_sales, fill = powertrain)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "EV Sales Share by Region and Powertrain", x = "Region", y = "Sales Share") +
      theme_minimal()

    ggplotly(p)
  })

  # Data Table: Interactive table
  output$table <- renderDT({
    data <- filtered_data() %>%
      select(year, Grouped_Region, powertrain, parameter, value)

    datatable(data, options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
