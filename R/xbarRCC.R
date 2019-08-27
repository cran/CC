"xbarRCC" <-
function (qc.obj, k = 3, sigma, mu, revise = TRUE, newdata) 
{
if (revise) {
   if (!inherits(qc.obj, "CC")) {
      x <- qc.obj
      R <- apply(x, 1, diffrange)
      n <- length(x[1, ])
      xbar <- apply(x, 1, mean)
      qc.obj <- list(R = R, xbar = xbar, k = k, n = n,  
          R.chart.label = "R-chart", x.chart.label = "xbar-chart", 
          R.ylabel = "R", x.ylabel = "xbar")
      class(qc.obj) <- c("CC")
   }
   if (!missing(sigma)) {
      R.par <- RCC(qc.obj$R, qc.obj$n, qc.obj$k, sigma)
      }
   else {
      R.par <- RCC(qc.obj$R, qc.obj$n, qc.obj$k) 
   }
   if (!missing(mu)) {
          xbar.par <- xbarCC(qc.obj$xbar[!R.par$ooc], qc.obj$n, 
          R.par$sigma, qc.obj$k, mu)
      }
   else {
       xbar.par <- xbarCC(qc.obj$xbar[!R.par$ooc], qc.obj$n, 
           R.par$sigma, qc.obj$k) 
   }
   qc.obj$R.par <- R.par
   qc.obj$xbar.par <- xbar.par
}
if (!missing(newdata)) {
   if (is.vector(newdata)) {
   R.new <- diffrange(newdata)
   xbar.new <- mean(newdata)
   }
   else {
   R.new <- apply(newdata, 1, diffrange)
   xbar.new <- apply(newdata, 1, mean)
   }
   qc.obj$R <- c(qc.obj$R, R.new)
   qc.obj$xbar <- c(qc.obj$xbar, xbar.new)
}
qc.obj
}
