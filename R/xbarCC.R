"xbarCC" <-
function (xbar, n, sigma, k, mu) 
{
    if (!missing(mu)) {
        UCL <- mu + k * sigma/sqrt(n)
        LCL <- mu - k * sigma/sqrt(n)
    }
    else {
    number.ooc <- 1
    out.of.control <- rep(FALSE, length(xbar))
    while (number.ooc > 0) {
        mu <- mean(xbar[!out.of.control])
        UCL <- mu + k * sigma/sqrt(n)
        LCL <- mu - k * sigma/sqrt(n)
        number.ooc <- sum(((xbar > UCL) | (xbar < LCL))) - sum(out.of.control)
        out.of.control <- ((xbar > UCL) | (xbar < LCL))   
       }
    }
list(CL = mu, UCL = UCL, LCL = LCL, mu = mu)
}
