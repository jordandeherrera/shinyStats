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

#' Harmonic Mean
#'
#' This is a function taken from the blog \href{https://statistic-on-air.blogspot.com/2009/07/geometric-and-harmonic-means-in-r.html}{Statistics on aiR} that calculates the harmonic mean of a set of numbers.  Along with the \code{\link[shinyStats]{gm_mean}} and \code{\link[base]{mean}} it provides the three most basic functions of "average." Typically, the arithmetic mean should be used for linear relationships, the geometric mean should be used for exponential or multiplicative relationships, and the harmonic mean should be used for reciprocal relationships.
#'
#' @param x The quantitative field from which to calculate the harmonic mean.
#'
#' @return Returns a numeric which is the harmonic mean
#'
#' @source \href{https://towardsdatascience.com/on-average-youre-using-the-wrong-average-geometric-harmonic-means-in-data-analysis-2a703e21ea0}{592862722639. “On Average, You're Using the Wrong Average: Geometric & Harmonic Means in Data Analysis.” Towards Data Science, Towards Data Science, 28 Jan. 2018, towardsdatascience.com/on-average-youre-using-the-wrong-average-geometric-harmonic-means-in-data-analysis-2a703e21ea0.}
#'
#' @examples
#' \dontrun{h_mean(mtcars$mpg)}
#' @export

h_mean <- function(x) 1 / mean(1/x,na.rm = TRUE)

#' Geometric Mean
#'
#' This is a function taken from the blog \href{https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in}{Stack Overflow} that calculates the geometric mean of a set of numbers.  Along with the \code{\link[shinyStats]{h_mean}} and \code{\link[base]{mean}} functions, they provide the three most basic functions of "average." Typically, the arithmetic mean should be used for linear relationships, the geometric mean should be used for exponential or multiplicative relationships, and the harmonic mean should be used for reciprocal relationships.
#'
#' @param x The quantitative field from which to calculate the geometric mean.
#'
#' @return Returns a numeric which is the geometric mean
#'
#' @source \href{https://towardsdatascience.com/on-average-youre-using-the-wrong-average-geometric-harmonic-means-in-data-analysis-2a703e21ea0}{592862722639. “On Average, You're Using the Wrong Average: Geometric & Harmonic Means in Data Analysis.” Towards Data Science, Towards Data Science, 28 Jan. 2018, towardsdatascience.com/on-average-youre-using-the-wrong-average-geometric-harmonic-means-in-data-analysis-2a703e21ea0.}
#'
#' @examples
#' \dontrun{gm_mean(mtcars$mpg)}
#' @export

gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

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

#' Comparison Density Chart with Mean and Median
#'
#' This is a function that creates a density chart based on raw values in a population.
#'
#' @param yValue1 The quantitative field for series 1 from which to create the density chart.
#'
#' @param yValue1 The quantitative field for series 2 from which to create the density chart.
#'
#' @param yValue1Name The label for the distribution of the density chart for the first series.
#'
#' @param yValue2Name The label for the distribution of the density chart for the second series.
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

compareDensityChartWMean <- function(yValue1,yValue2,yValue1Name,yValue2Name,title,subTitle,xAxisTitle,colorType){
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
    hc_add_series(type = "areaspline",data = density(yValue1, kernel = "gaussian"), name = yValue1Name, color = colors[3]) %>%
    hc_add_series(type = "areaspline",data = density(yValue2, kernel = "gaussian"), name = yValue2Name, color = colors[3]) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subTitle) %>%
    hc_xAxis(list(title = list(text = xAxisTitle),
                  plotLines = list(list(value = as.numeric(mean(yValue1)), color = colors[1], width = 3, zIndex = 4,
                                        label = list(text = paste0("Mean of ",yValue1Name,": ",round(as.numeric(mean(yValue1)),2)),
                                                     verticalAlign = "middle",
                                                     style = list(color = colors[1], fontWeight = "bold"))
                                        ),
                                   list(value = as.numeric(mean(yValue2)), color = colors[1], width = 3, zIndex = 4,
                                        label = list(text = paste0("Mean of ",yValue2Name,": ",round(as.numeric(mean(yValue2)),2)),
                                                     verticalAlign = "middle",
                                                     style = list(color = colors[1], fontWeight = "bold"))
                                        )
                                   )
                  )
    ) %>%
    hc_add_theme(hc_theme_elementary())
}
