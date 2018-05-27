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

#' Shifted Beta Gamma Model Parameter Estimates
#'
#' This function estimates parameters for the shifed beta gamma distribution (often used in survival curves)
#' and used by the \code{\link{survivalCurve}} function.
#'
#' @param active.vec Numeric vector representing active members
#'
#' @param lost.vec Numeric vector representing lost members
#'
#' @return Returns a vector of parameters (alpha and beta)
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(titanic)
#' titanic::titanic_train %>%
#' mutate(Deceased = ifelse(Survived == 1,0,1)) %>%
#' group_by(Age = round(Age,0)) %>%
#' summarise(Survived = sum(Survived), Deceased = sum(Deceased)) %>% {
#' sBG.est(
#' .$Survived[2:nrow(.)],
#' .$Deceased[2:nrow(.)]
#' )$par}
#' }
#' @export

sBG.est <- function(active.vec, lost.vec) {
  require(dplyr)
  require(purrr)

  sbg.loglik <- function(params) {

    # Set parameters
    a      <- params[1]
    b      <- params[2]

    # Forward recursion
    loglik <-
      1:length(lost.vec) %>%
      purrr::reduce(
        function(.x, .y) {
          .x + lost.vec[.y] * log(beta(a + 1, b + .y - 1)/beta(a, b))
        },
        .init = 0
      )

    # Final line (for active)
    loglik <- loglik + active.vec[length(lost.vec)] * log(beta(a, b + length(lost.vec))/beta(a, b))

    # Maximize log likelihood
    return(-loglik)
  }

  return(
    stats::optim(
      par = c(1,1),
      sbg.loglik
    )
  )
}

#' Survival Curve
#'
#' This function creates a survival curve based on an \code{\link{sBG.est}} model parameters.
#'
#' @param alpha Alpha parameter
#'
#' @param beta Beta parameter
#'
#' @param t iterations
#'
#' @return Returns a vector of survival percent estimates
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(titanic)
#' titanic::titanic_train %>%
#' mutate(Deceased = ifelse(Survived == 1,0,1)) %>%
#' group_by(Age = round(Age,0)) %>%
#' summarise(Survived = sum(Survived), Deceased = sum(Deceased)) %>% {
#' sBG.est(
#' .$Survived[2:nrow(.)],
#' .$Deceased[2:nrow(.)]
#' )$par} %>% {
#' survivalCurve(.[1], .[2], 50)
#' }
#' }
#' @export

survivalCurve <- function (alpha, beta, t = 96) {
  require(purrr)
  # Produce a vector describing the survival curve
  # Inputs:
  #   alpha, beta: parameters estimated from sBG.est
  #   t          : number of iterations
  #   single     : if only last value is needed, FALSE by default
  # Output:
  #   vector: vector of survival percentages starting at 1 up to t

  seq(1:t) %>%
    map_dbl(
      function(time) {
        (beta + time - 1)/(alpha + beta + time - 1)
      }
    ) %>%
    purrr::accumulate(
      function(.x, .y) {
        .x * .y
      },
      .init = 1
    )
}

#' Percent Rank
#'
#' This is a function taken from \href{https://stats.stackexchange.com/questions/11924/computing-percentile-rank-in-r}{Stack Overflow} that creates a percentile rank of a quantitative field.  This is useful for creating cumulative and power law distributions.
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
#' @param colorType Specify the theme to use for the chart (either Verasite or Uber)
#'
#' @return Returns a highchart object
#'
#' @examples
#' \dontrun{
#' densityChartWMean(mtcars$cyl,"Density of Cylinders in MT Cars Dataset","Example of density chart","Cylinders")
#' }
#' @export

densityChartWMean <- function(yValue,title,subTitle,xAxisTitle,colorType){
  require(dplyr)
  require(highcharter)

  colors = colorSchemes %>%
    filter(theme == colorType) %>%
    collect %>%
    .[["colors"]]

  highchart() %>%
    # hc_yAxis(title = list(text = "Density")) %>%
    hc_yAxis_multiples(
      list(lineWidth = 3, title = list(text = "Density"), labels = list(enabled = FALSE)),
      list(showLastLabel = FALSE, opposite = TRUE, title = list(text = ""))
    ) %>%
    hc_add_series(type = "areaspline",data = density(yValue, kernel = "gaussian"), name = "Density Distribution", color = colors[3]) %>%
    # hc_add_series(data = data.frame(x = hist(yValue)$mids, y = hist(yValue)$counts), type = "spline", yAxis = 1, name = "Population Count") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subTitle) %>%
    hc_xAxis(list(title = list(text = xAxisTitle),
                  plotLines = list(list(value = as.numeric(quantile(yValue,0.02)), color = colors[1], width = 3, zIndex = 4,
                                        label = list(text = paste0("P2: ",round(as.numeric(quantile(yValue,0.02)),2)),
                                                     style = list(color = colors[1], fontWeight = "bold"))),
                                   list(value = as.numeric(mean(yValue)), color = colors[2], width = 3, zIndex = 4,
                                        label = list(text = paste0("Mean: ",round(as.numeric(mean(yValue)),2)),
                                                     verticalAlign = "middle",
                                                     style = list(color = colors[2], fontWeight = "bold"))),
                                   list(value = as.numeric(quantile(yValue,0.05)), color = colors[1], width = 3, zIndex = 4,
                                        label = list(text = paste0("P5: ",round(as.numeric(quantile(yValue,0.05)),2)),
                                                     style = list(color = colors[1], fontWeight = "bold"))),
                                   list(value = as.numeric(quantile(yValue,0.50)), color = colors[1], width = 3, zIndex = 4,
                                        label = list(text = paste0("P50: ",round(as.numeric(quantile(yValue,0.50)),2)),
                                                     style = list(color = colors[1], fontWeight = "bold"))),
                                   list(value = as.numeric(quantile(yValue,0.95)), color = colors[1], width = 3, zIndex = 4,
                                        label = list(text = paste0("P95: ",round(as.numeric(quantile(yValue,0.95)),2)),
                                                     style = list(color = colors[1], fontWeight = "bold"))),
                                   list(value = as.numeric(quantile(yValue,0.98)), color = colors[1], width = 3, zIndex = 4,
                                        label = list(text = paste0("P98: ",round(as.numeric(quantile(yValue,0.98)),2)),
                                                     style = list(color = colors[1], fontWeight = "bold")))
                  )
    )) %>%
    hc_add_theme(hc_theme_elementary())
}

#' Color Schemes
#'
#' Color schemes for use with graphs
#'
#' @docType data
#'
#' @usage data(colorSchemes)
#'
#' @format An object of class \code{"data.frame"}; see \code{\link[shinyStats]{densityChartWMean}}.
#'
#' @keywords datasets
#'
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{http://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source \href{http://www.jordandeherrera.com}{Verasite Template}
#'
#' @examples
#' data(colorSchemes)
#' \donttest{densityChart(mtcars$mpg,"MPG Test","Sample Density Chart","MPG","Uber")}
"colorSchemes"
