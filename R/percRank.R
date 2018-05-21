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
