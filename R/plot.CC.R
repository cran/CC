"plot.CC" <- function (x, start = 1, ...) {
    chart.obj <- x
    R <- chart.obj$R
    m <- length(R)
    time <- seq(start, m)
    R <- R[time]
    xbar <- chart.obj$xbar[time]
    oldpar <- par(mfrow = c(2, 1), mar=c(4, 3, 2.6, 1))
    on.exit(par(oldpar))
    plot(R ~ time, ylim = range(
        c(chart.obj$R.par$LCL, chart.obj$R.par$UCL, R)), 
          ylab = chart.obj$R.ylabel, 
          pch = 16)
    mtext(chart.obj$R.chart.label, side=3, line = 0.5) 
    abline(chart.obj$R.par$LCL, 0, col = "red", lwd = 2)
    abline(chart.obj$R.par$CL, 0, col = "blue")
    abline(chart.obj$R.par$UCL, 0, col = "red", lwd = 2)
    lines(time, R)
    plot(xbar ~ time, ylim = range(c(chart.obj$xbar.par$LCL, 
        chart.obj$xbar.par$UCL, xbar)),  
        ylab = chart.obj$x.ylabel, pch = 16)
    mtext(chart.obj$x.chart.label, side=3, line = 0.5)
    abline(chart.obj$xbar.par$LCL, 0, col = "red", lwd = 2)
    abline(chart.obj$xbar.par$mu, 0, col = "blue")
    abline(chart.obj$xbar.par$UCL, 0, col = "red", lwd = 2)
    lines(time, xbar)
}
