#' IEA Global EV Data
#'
#' A subset of data from the IEA Global EV Data Report summarizing electric vehicle sales and trends across various regions and years.
#' Report ...
#'
#' @format ## `ev_data`
#' A data frame with 10042 rows and 6 columns:
#' \describe{
#'   \item{year}{Year of EV sales (numeric).}
#'   \item{category}{Whether the data is historical or predicted.}
#'   \item{parameter}{Include 4 categories: EV sales,EV sales share,EV stock,EV stock share.}
#'   \item{Grouped_Region}{Regions grouped into major categories (North America, Europe, etc.).}
#'   \item{powertrain}{Types of electric vehicle powertrain: BEV (Battery Electric Vehicle), PHEV (Plug-in Hybrid Electric Vehicle), FCEV (Fuel Cell Electric Vehicle).}
#'   \item{value}{The number of EVs sold, stock count, or percentage, depending on the `parameter`.}
#'   ...
#' }
#' @source <https://www.kaggle.com/datasets/patricklford/global-ev-sales-2010-2024/data>
"ev_data"
