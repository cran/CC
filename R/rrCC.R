rrCC <-
function (RR, k = 3, revise = TRUE, newdata) {
    averages <- RR[,1]
    STDs <- RR[,2]
    if (!missing(newdata)) {
        newx <- newdata[, 1]
        newSTD <- newdata[, 2]
        avg.xLR <- xLRCC(averages, k = k, revise = revise)
        STD.xLR <- xLRCC(1/STDs, k = k, revise =  revise)
    } else {
        avg.xLR <- xLRCC(averages, k = k, revise = revise)
        STD.xLR <- xLRCC(1/STDs, k = k, revise = revise)
        newx <- NULL
        newSTD <- NULL
    }
    avg.xLR$xbar <- c(avg.xLR$xbar, newx)
    avg.xLR$R <- 1/c(STDs, newSTD)
    avg.xLR$R.par <- STD.xLR$xbar.par
    avg.xLR$R.chart.label <- "Daily RR-Variability"
    avg.xLR$R.ylabel <- "precision"
    avg.xLR$x.ylabel <- "avg. rr"
    avg.xLR$x.chart.label <- "Daily Baseline RR"
    avg.xLR
}

