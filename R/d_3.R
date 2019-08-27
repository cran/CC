"d_3" <-
function (n) 
{
    lower <- -3 + (n - 1)/8
    upper <- 5 - (n - 1)/120
    f <- function(x1) {
        (xn - x1)^2 * dnorm(x1) * (pnorm(xn) - pnorm(x1))^(n - 
            2)
    }
    fn <- numeric(150)
    x.inner <- seq(lower, upper, length = length(fn))
    for (i in 1:length(fn)) {
        xn <- x.inner[i]
        fn[i] <- integrate(f, -Inf, xn)$value
    }
    f1 <- approxfun(x.inner, n * (n - 1) * dnorm(x.inner) * fn)
    round(sqrt(integrate(f1, lower, upper)$value - d_2(n)^2), 
        3)
}
