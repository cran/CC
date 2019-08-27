xCC <- function (x, sigma, k = 3, mu, newdata) {
    if (!missing(mu)) { # control limits are revised
        UCL <- mu + k * sigma
        LCL <- mu - k * sigma
        if (!missing(newdata)) {
             out.of.control <- ((newdata > UCL) | (newdata < LCL))       
        } else {
            out.of.control <- ((x > UCL) | (x < LCL))
        } 
    }
    else {  # revise control limits 
        number.ooc <- 1
        out.of.control <- rep(FALSE, length(x))
        while (number.ooc > 0) {
            mu <- mean(x[!out.of.control])
            UCL <- mu + k * sigma
            LCL <- mu - k * sigma
            number.ooc <- sum(((x > UCL) | (x < LCL))) - 
                sum(out.of.control)
            out.of.control <- ((x > UCL) | (x < LCL))
        }
    }
    list(CL = mu, UCL = UCL, LCL = LCL, ooc = which(out.of.control))
}

