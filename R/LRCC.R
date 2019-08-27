LRCC <- function (x, sigma, plotit = FALSE) {
    mu <- lowess(x)$y
    LR <- abs(x - mu)
    if (!missing(sigma)) {
        UCL <- sigma*(sqrt(2/pi) + 3*sqrt(1-2/pi))
        LCL <- 0
        out.of.control <- (LR > UCL)
        LRbar <- mean(LR[!out.of.control])
    } else {        
        number.ooc <- 1
        out.of.control <- rep(FALSE, length(LR))
        while (number.ooc > 0) {
            LRbar <- mean(LR[!out.of.control])
            sigma <- LRbar * sqrt(pi/2)
            UCL <- sigma*(sqrt(2/pi) + 3*sqrt(1-2/pi))
            LCL <- 0
            number.ooc <- sum(LR > UCL) - sum(out.of.control)
            out.of.control <- (LR > UCL)
        }
    }
    if (plotit) {
        plot(LR, type = "l", ylim = range(c(UCL, LR, 0)), xlab = "t")
        abline(UCL, 0, col = "red")
        abline(LCL, 0, col = "red")
    }
    list(CL = LRbar, UCL = UCL, LCL = LCL, mu = mu, sigma 
         = sigma, LR = LR, ooc = which(LR>UCL))
}
