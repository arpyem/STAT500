# Module 5 - Confidence Intervals



#' Get confidence interval of a mean
#'
#' Calculates a confidence interval for a mean, given a confidence level between 0 and 1, a sample size, a sample mean, and a sample standard
#' error (estimate of population standard deviation). Takes one value for each argument and returns a vector containing the margin of error and
#' confidence interval. This function assumes the sampling distribution is normal.
#'
#' @param x \code{numeric} Sample mean
#' @param s \code{numeric} Sample standard deviation
#' @param n \code{integer} Sample size - will be coerced to an integer
#' @param level \code{numeric} Confidence level - must be between 0 and 1
#'
#' @return a \code{numeric} vector
#'
#' @importFrom stats qt
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#' stat500_ci_mean(x = 24, s = 1.7, n = 41)
#' stat500_ci_mean(x = 11, s = 19, n = 36, level = 0.99)
#' stat500_ci_mean(x = 273, s = 56.2, n = 25, level = 0.99)
#' }

stat500_ci_mean <- function(x, s, n, level = 0.95) {

    level <- as.numeric(level[1])
    n <- as.integer(n[1])
    x <- as.numeric(x[1])
    s <- as.numeric(s[1])

    t <- qt(p = (1 - level) / 2, df = n - 1, lower.tail = FALSE)
    se <- s / sqrt(n)
    me <- t * se
    lower <- x - me
    upper <- x + me

    result <- c(me, lower, upper)
    result_names <- c("ME", paste0("lower ", level * 100, "%"), paste0("upper ", level * 100, "%"))
    result <- setNames(result, result_names)

    return(result)
}



# TODO exact method for proportions that do not approximate a normal sampling distribution

#' Get confidence interval of a proportion
#'
#' @param p \code{numeric} Sample proportion
#' @param n \code{integer} Sample size - will be coerced to an integer
#' @param level \code{numeric} Confidence level - must be between 0 and 1
#' @param approximate_cutoff \code{numeric} Cutoff for using the Z-method when \code{p >= approximate_cutoff && (1 - p) >= approximate_cutoff}
#'
#' @return
#'
#' @importFrom stats qnorm
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#' stat500_ci_prop(p = 0.55, n = 600, level = 0.98)
#' }

stat500_ci_prop <- function(p, n, level = 0.95, approximate_cutoff = 5) {

    p <- as.numeric(p[1])
    n <- as.integer(n[1])
    level <- as.numeric(level[1])
    approximate_cutoff <- as.numeric(approximate_cutoff)

    if (n * p < approximate_cutoff || n * (1 - p) < approximate_cutoff) {
        stop("conditions for z-method are not met - exact method not implemented yet")
    }

    z <- qnorm(p = (1 - level) / 2, lower.tail = FALSE)
    se <- sqrt(p * (1 - p) / n)
    me <- z * se
    lower <- p - me
    upper <- p + me

    result <- c(me, lower, upper)
    result_names <- c("ME", paste0("lower ", level * 100, "%"), paste0("upper ", level * 100, "%"))
    result <- setNames(result, result_names)

    return(result)
}




#' Get the sample size required for a desired margin of error of a mean
#'
#' @param E \code{numeric} Desired effect size/margin of error
#' @param sigma Standard error of the sample mean
#' @param level Confidence level
#'
#' @return \code{integer}
#'
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' \dontrun{
#' stat500_n_mean(E = 0.5, sigma = 2)
#' }

stat500_n_mean <- function(E, sigma, level = 0.95) {

    E <- as.numeric(E[1])
    sigma <- as.numeric(sigma[1])
    level <- as.numeric(level[1])

    z <- qnorm(p = (1 - level) / 2, lower.tail = FALSE)
    n <- (z * sigma / E)^2
    result <- ceiling(n)

    return(result)
}




#' Get the sample size required for a desired margin of error of a proportion
#'
#' @param E \code{numeric} Desired effect size/margin of error
#' @param p \code{numeric} Sample proportion
#' @param level \code{numeric} Confidence level
#'
#' @return \code{integer}
#'
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' \dontrun{
#' stat500_n_prop(E = 0.01, p = 0.72)
#' stat500_n_prop(E = 0.01)
#' }

stat500_n_prop <- function(E, p = 0.5, level = 0.95) {

    E <- as.numeric(E[1])
    p <- as.numeric(p[1])
    level <- as.numeric(level[1])

    if (p == 0.5) message("using conservative method")

    z <- qnorm(p = (1 - level) / 2, lower.tail = FALSE)
    n <- ((z^2) * p * (1 - p)) / E^2
    result <- ceiling(n)

    return(result)
}




