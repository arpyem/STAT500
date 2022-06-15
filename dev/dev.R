


# CI for the mean

stat_500_ci_mean <- function(level = 0.95, n, x, s) {

    t <- qt(p = (1 - level) / 2, df = n - 1, lower.tail = FALSE)
    se <- s / sqrt(n)
    M <- t * se
    CI <- c(x - M, x + M)

    result <- c(M, CI)
    setNames(result, c("ME", paste0("lower ", level * 100, "%"), paste0("upper ", level * 100, "%")))
}

stat_500_ci_mean(n = 41, x = 24, s = 1.7)
stat_500_ci_mean(level = 0.99, n = 36, x = 11, s = 19)
stat_500_ci_mean(level = 0.99, n = 25, x = 273, s = 56.2)


stat_500_ci_prop <- function(level = 0.95, n, p) {

    if (n * p >= 5 && n * (1 - p) >= 5) {

        z <- qnorm(p = (1 - level) / 2, lower.tail = FALSE)
        se <- sqrt(p * (1 - p) / n)
        M <- z * se
        CI <- c(p - M, p + M)

        CI

    } else {
        stop("conditions for z-method are not met")
    }

}

stat_500_ci_prop(.98,600,.55)



stat_500_n_mean <- function(level = 0.95, E, sigma) {

    z <- qnorm(p = (1 - level) / 2, lower.tail = FALSE)
    n <- (z * sigma / E)^2

    ceiling(n)
}

stat_500_n_mean(E = 0.5, sigma = 2)



stat_500_n_prop <- function(level = 0.95, p = 0.5, E) {

    z <- qnorm(p = (1 - level) / 2, lower.tail = FALSE)
    n <- ((z^2) * p * (1 - p)) / E^2

    ceiling(n)
}

stat_500_n_prop(p = 0.72, E = .01)
stat_500_n_prop(E = .01)
















