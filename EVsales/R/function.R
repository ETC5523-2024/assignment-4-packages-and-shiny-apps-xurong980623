#' Plot EV Sales Over Time by Region and Powertrain (Bar Plot)
#'
#' This function generates a bar plot showing EV sales over time for a specific region and powertrain type.
#'
#' @param data The EV sales data (e.g., ev_data). Default is ev_data.
#' @param region A character string specifying the region to filter by. Default is "All".
#' @param powertrain A character string specifying the powertrain type (e.g., "BEV", "PHEV"). Default is "All".
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @return A ggplot2 object showing the EV sales over time as a bar plot.
#' @examples
#' plot_ev_sales_over_time()
#' plot_ev_sales_over_time(ev_data)
#' @export
plot_ev_sales_over_time <- function(data = ev_data, region = "All", powertrain = "All") {

  # Filter data by region, if specified
  if (region != "All") {
    data <- data |> dplyr::filter(Grouped_Region == region)
  }

  # Filter data by powertrain, if specified
  if (powertrain != "All") {
    data <- data |> dplyr::filter(powertrain == powertrain)
  }

  # Create the bar plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = as.factor(year), y = value, fill = Grouped_Region)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = "EV Sales Over Time", x = "Year", y = "Sales") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold")
    )

  return(p)
}



#' Plot Powertrain Distribution (Bar Plot)
#'
#' This function generates a bar plot showing the distribution of EV sales by powertrain type.
#'
#' @param data The EV sales data (default is ev_data).
#'
#' @return A ggplot2 object showing the distribution of powertrain types.
#' @examples
#' plot_powertrain_distribution()
#' plot_powertrain_distribution(ev_data)
#' @export
plot_powertrain_distribution <- function(data = ev_data) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = powertrain, y = value, fill = powertrain)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = "EV Powertrain Distribution", x = "Powertrain", y = "Sales") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::scale_fill_brewer(palette = "Set2")

  return(p)
}



#' View EV Sales Data
#'
#' This function renders an interactive data table for exploring the EV sales data.
#'
#' @param data The EV sales data (default is ev_data).
#'
#' @return A datatable object showing the EV sales data.
#' @examples
#' view_ev_sales_data()
#' view_ev_sales_data(ev_data)
#' @export
view_ev_sales_data <- function(data = ev_data) {
  DT::datatable(data, options = list(pageLength = 10, autoWidth = TRUE))
}
