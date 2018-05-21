# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

#' Percent Rank
#'
#' This is a function taken from /href{https://stats.stackexchange.com/questions/11924/computing-percentile-rank-in-r}{Stack Overflow} that creates a percentile rank of a quantitative field.  This is useful for creating cumulative and power law distributions.
#'
#' @param x The quantitative field from which to create the percentile rank.
#'
#' @return Returns a decimal from 0 to 1 of the percentile rank of a given
#'
#' @examples
#' \dontrun{percRank(mtcars$mpg)}
#' @export

percRank <- function(x) trunc(rank(x))/length(x)

#' Density Chart with Meank
#'
#' This is a function that creates a density chart based on raw values in a population.
#'
#' @param yValue The quantitative field from which to create the density chart.
#'
#' @param title The title of the chart
#'
#' @param subTitle The subtitle of the chart
#'
#' @param xAxisTitle The x Axis title
#'
#' @return Returns a highchart object
#'
#' @examples
#' \dontrun{
#' densityChartWMean(mtcars$cyl,"Density of Cylinders in MT Cars Dataset","Example of density chart","Cylinders")
#' }
#' @export

densityChartWMean <- function(yValue,title,subTitle,xAxisTitle){
  require(dplyr)
  require(highcharter)

  highchart() %>%
    # hc_yAxis(title = list(text = "Density")) %>%
    hc_yAxis_multiples(
      list(lineWidth = 3, title = list(text = "Density"), labels = list(enabled = FALSE)),
      list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Count"))
    ) %>%
    hc_add_series(type = "areaspline",data = density(yValue, kernel = "gaussian"), name = "Density Distribution") %>%
    # hc_add_series(data = data.frame(x = hist(yValue)$mids, y = hist(yValue)$counts), type = "spline", yAxis = 1, name = "Population Count") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subTitle) %>%
    hc_xAxis(list(title = list(text = xAxisTitle),
                  plotLines = list(list(value = as.numeric(quantile(yValue,0.02)), color = "#12939A", width = 3, zIndex = 4,
                                        label = list(text = paste0("P2: ",round(as.numeric(quantile(yValue,0.02)),2)),
                                                     style = list(color = "#12939A", fontWeight = "bold"))),
                                   list(value = as.numeric(mean(yValue)), color = "#FF991F", width = 3, zIndex = 4,
                                        label = list(text = paste0("Mean: ",round(as.numeric(mean(yValue)),2)),
                                                     verticalAlign = "middle",
                                                     style = list(color = "#FF991F", fontWeight = "bold"))),
                                   list(value = as.numeric(quantile(yValue,0.05)), color = "#12939A", width = 3, zIndex = 4,
                                        label = list(text = paste0("P5: ",round(as.numeric(quantile(yValue,0.05)),2)),
                                                     style = list(color = "#12939A", fontWeight = "bold"))),
                                   list(value = as.numeric(quantile(yValue,0.50)), color = "#12939A", width = 3, zIndex = 4,
                                        label = list(text = paste0("P50: ",round(as.numeric(quantile(yValue,0.50)),2)),
                                                     style = list(color = "#12939A", fontWeight = "bold"))),
                                   list(value = as.numeric(quantile(yValue,0.95)), color = "#12939A", width = 3, zIndex = 4,
                                        label = list(text = paste0("P95: ",round(as.numeric(quantile(yValue,0.95)),2)),
                                                     style = list(color = "#12939A", fontWeight = "bold"))),
                                   list(value = as.numeric(quantile(yValue,0.98)), color = "#12939A", width = 3, zIndex = 4,
                                        label = list(text = paste0("P98: ",round(as.numeric(quantile(yValue,0.98)),2)),
                                                     style = list(color = "#12939A", fontWeight = "bold")))
                  )
    ))
}
