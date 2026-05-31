library(comorosmaps)
m <- view_map(island='anjouan', commune=TRUE, city=TRUE)
cat("Number of calls:", length(m$x$calls), "\n")
for (i in seq_along(m$x$calls)) {
  call <- m$x$calls[[i]]
  cat(i, ":", call$method, "\n")
  if (!is.null(call$args)) {
    for (nm in names(call$args)) {
      if (nm == "group") cat("   group =", call$args[[nm]], "\n")
    }
  }
}
