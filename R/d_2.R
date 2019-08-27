"d_2" <-
function (n) 
{
    if ((length(n) > 1) || (mode(n) != "numeric")) {
        stop("Argument must be a scalar integer.")
    }
    if (n < 2) {
        approx <- NA
    }
    else {
        x <- 6 - ((seq(1, (6^(50/49)), length = 51)))^(49/50)
        x <- c(-x, rev(x[-1]))
        x.diff <- diff(x)
        x.101 <- x[-101] + diff(x)/2
        approx <- 2 * n * sum(dnorm(x.101) * x.101 * (pnorm(x.101)^(n - 
            1)) * x.diff)
    }
    approx
}
