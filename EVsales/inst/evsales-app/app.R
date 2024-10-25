# Load the necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# Define UI for the Shiny App with shinydashboard
ui <- dashboardPage(

  # Dashboard Header
  dashboardHeader(title = "Global EV Sales Analysis"),

  # Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sales Over Time", tabName = "sales_time", icon = icon("chart-bar")),
      menuItem("Worldwide Distribution", tabName = "worldwide_dist", icon = icon("globe")),
      menuItem("Sales Share by Region", tabName = "sales_share", icon = icon("map-marked-alt")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem("Instructions", tabName = "instructions", icon = icon("info-circle"))
    ),

    # Sidebar Inputs
    selectInput("region", "Select Region:", choices = c("All", unique(ev_data$Grouped_Region)), selected = "All"),
    sliderInput("year", "Select Year Range:", min = min(ev_data$year), max = max(ev_data$year),
                value = c(min(ev_data$year), max(ev_data$year))),
    selectInput("parameter", "Select Parameter:", choices = c("EV sales", "EV sales share", "EV stock", "EV stock share")),
    selectInput("powertrain", "Select Powertrain:", choices = c("All", unique(ev_data$powertrain))),

    # Description Section
    h5("Field Descriptions:"),
    p("1. Region: Select a region or 'All' to see global data."),
    p("2. Year Range: Use the slider to select the time period of interest."),
    p("3. Parameter: Choose between 'EV sales', 'EV sales share', 'EV stock', or 'EV stock share'."),
    p("4. Powertrain: Filter the data based on the type of EV (BEV, PHEV, or FCEV).")
  ),

  # Dashboard Body
  dashboardBody(
    # CSS for hover effects on box titles
    tags$style(HTML("
      /* Title hover effect */
      .box-title {
        color: #2874A6;
        transition: color 0.3s ease;
      }
      .box-title:hover {
        color: #1ABC9C; /* Change title color on hover */
      }
      .box {
        border-top: 3px solid #2874A6;
      }
    ")),

    tabItems(

      # Sales Over Time Tab
      tabItem(tabName = "sales_time",
              fluidRow(
                box(title = span("EV Sales Over Time by Region", class = "box-title"), status = "primary", solidHeader = TRUE,
                    width = 12, plotlyOutput("sales_plot"))
              )),

      # Worldwide Distribution Tab
      tabItem(tabName = "worldwide_dist",
              fluidRow(
                box(title = span("Worldwide Distribution of EV Sales by Powertrain Type", class = "box-title"), status = "primary",
                    solidHeader = TRUE, width = 12, plotlyOutput("dist_plot"))
              )),

      # Sales Share by Region Tab
      tabItem(tabName = "sales_share",
              fluidRow(
                box(title = span("EV Sales Share by Region and Powertrain", class = "box-title"), status = "primary", solidHeader = TRUE,
                    width = 12, plotlyOutput("share_plot"))
              )),

      # Data Table Tab
      tabItem(tabName = "data_table",
              fluidRow(
                box(title = span("Detailed EV Sales Data", class = "box-title"), status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("table"))
              )),

      # Instructions Tab with Full Text
      tabItem(tabName = "instructions",
              h4("How to Use the App:"),
              p("Use the selectors on the left sidebar to filter and explore the data. The app is designed to help users investigate trends in EV sales across different regions, time periods, and powertrain types."),

              h4("How to Interpret the Outputs:"),
              p("1. **Sales Over Time**: This bar chart shows how EV sales have changed over the selected time period. Each bar represents the total sales for a given year, with different regions stacked on top of each other for comparison. Use this chart to identify growth patterns, leading regions, and how the global market has evolved."),

              p("2. **Worldwide Distribution**: This chart shows the distribution of EV sales based on powertrain type (e.g., BEV, PHEV, FCEV). The taller the bar, the higher the sales for that powertrain. This is useful for understanding the market share of different powertrain technologies."),

              p("3. **Sales Share by Region**: This stacked bar chart shows how EV sales are distributed across regions. The size of each bar reflects the total sales in that region, and the stacked segments represent the sales split by powertrain type. Use this to identify which regions lead in EV sales and what types of powertrains are popular."),

              p("4. **Data Table**: The table displays detailed data based on your filters. It includes the year, region, powertrain type, selected parameter, and corresponding values. You can use this table for in-depth analysis, sorting, and exporting filtered data.")
      )
    )
  )
)

# Define server logic for the Shiny App
server <- function(input, output) {

  # Reactive expression: Filtered data based on user input
  filtered_data <- reactive({
    data <- ev_data
    if (input$region != "All") data <- data |> filter(Grouped_Region == input$region)
    if (input$powertrain != "All") data <- data |> filter(powertrain == input$powertrain)
    data |> filter(year >= input$year[1] & year <= input$year[2])
  })

  # Plot 1: Sales over time (Plotly)
  output$sales_plot <- renderPlotly({
    data <- filtered_data() |> filter(parameter == input$parameter)
    p <- ggplot(data, aes(x = year, y = value, fill = Grouped_Region)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Year", y = "Sales Value") +
      theme_minimal() + scale_fill_brewer(palette = "Set1")
    ggplotly(p)
  })

  # Plot 2: Worldwide distribution (Plotly)
  output$dist_plot <- renderPlotly({
    data <- filtered_data() |> filter(parameter == "EV sales")
    p <- ggplot(data, aes(x = powertrain, y = value, fill = powertrain)) +
      geom_bar(stat = "identity") +
      labs(x = "Powertrain", y = "Total Sales") +
      theme_minimal() + scale_fill_brewer(palette = "Set3")
    ggplotly(p)
  })

  # Plot 3: Sales share by region (Plotly)
  output$share_plot <- renderPlotly({
    data <- filtered_data() |> filter(parameter == "EV sales") |>
      group_by(Grouped_Region, powertrain) |>
      summarize(total_sales = sum(value, na.rm = TRUE)) |>
      ungroup()
    p <- ggplot(data, aes(x = Grouped_Region, y = total_sales, fill = powertrain)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Region", y = "Sales Share") + theme_minimal()
    ggplotly(p)
  })

  # Data Table: Interactive table
  output$table <- renderDT({
    filtered_data() |> select(year, Grouped_Region, powertrain, parameter, value) |>
      datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
