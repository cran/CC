xLRCC <-
function (qc.obj, k = 3, sigma, mu, revise = TRUE, newdata) 
{
    if (revise) {
        if (!inherits(qc.obj, "CC")) {
            x <- qc.obj
            LR <- LRCC(x)
            qc.obj <- list(R = LR$LR, xbar = x, k = k, n = 1, 
                R.chart.label = "LR-chart", x.chart.label = "x-chart", 
                R.ylabel = "LR", x.ylabel = "x")
            class(qc.obj) <- c("CC")
        }
        if (!missing(sigma)) {
            R.par <- LRCC(qc.obj$xbar, sigma=sigma)
        } else {
            R.par <- LRCC(qc.obj$xbar)
        }
        if (!missing(mu)) {
            if (length(R.par$ooc) > 0) {        
                xbar.par <- xCC(qc.obj$xbar[-R.par$ooc], 
                    R.par$sigma, qc.obj$k, mu)
            } else {
                xbar.par <- xCC(qc.obj$xbar, 
                    R.par$sigma, qc.obj$k, mu)
            }
        }
        else {
            if (length(R.par$ooc) > 0) {        
                xbar.par <- xCC(qc.obj$xbar[-R.par$ooc],  
                    R.par$sigma, qc.obj$k)
            } else {
                xbar.par <- xCC(qc.obj$xbar, 
                    R.par$sigma, qc.obj$k, mu)
            }
        }
        qc.obj$R.par <- R.par
        qc.obj$xbar.par <- xbar.par
    }
    mu <- mean(qc.obj$R.par$mu)
    qc.obj$xbar.par$mu <- mu
    if (!missing(newdata)) {
        LR.new <- abs(newdata - mu)
        xbar.new <- newdata
        qc.obj$R <- c(qc.obj$R, LR.new)
        qc.obj$xbar <- c(qc.obj$xbar, xbar.new)
    }
    qc.obj
}
