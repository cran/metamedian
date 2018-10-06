qe.group.level <- function(min.val, q1.val, med.val, q3.val, max.val, n,
                           mean.val, sd.val, qe.fit.control = list(),
                           scenario) {
  if (scenario != "S4") {
    output <- qe.fit(min.val = min.val, q1.val = q1.val, med.val = med.val,
                     q3.val = q3.val, max.val = max.val, n = n,
                     qe.fit.control = qe.fit.control)
  } else {
    values <- c(0, rep(NA, 3))
    names(values) <- c("normal", "log-normal", "gamma", "weibull")
    output <- list(values = values,
                   norm.par = c(mean.val, sqrt((n - 1) / n)) * sd.val,
                   lnorm.par = NA, gamma.par = NA, weibull.par = NA)
    class(output) <- "qe.fit"
  }
  return(output)
}
