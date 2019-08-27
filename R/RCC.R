"RCC" <-
function (R, n, k=3, sigma) 
{
     d2n <- d_2(n)
     d3n <- d_3(n)
     if (!missing(sigma)) {
         Rbar <- d2n*sigma
         UCL <- (d2n + d3n * k) * sigma
         LCL <- max(0, (d2n - d3n * k) * sigma)
         out.of.control <- (R > UCL)
     }
     else {
         number.ooc <- 1
         out.of.control <- rep(FALSE, length(R))
         while (number.ooc > 0) {
             Rbar <- mean(R[!out.of.control])
             sigma <- Rbar/d2n
             UCL <- (d2n + d3n * k) * sigma
             LCL <- max(0, (d2n - d3n * k) * sigma)
             number.ooc <- sum(R > UCL) - sum(out.of.control)
             out.of.control <- (R > UCL)
         }
     }
     list(CL = Rbar, UCL = UCL, LCL = LCL, sigma = sigma, ooc = out.of.control)
}
