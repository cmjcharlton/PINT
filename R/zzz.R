.onAttach <- function(...) {
  packageStartupMessage("")
  packageStartupMessage("This programs performs calculations corresponding to the paper")
  packageStartupMessage("\"Standard errors and Sample Sizes for Two-Level Research\"")
  packageStartupMessage("")
  packageStartupMessage("by Tom A.B. Snijders and Roel J. Bosker,")
  packageStartupMessage("Journal of Educational Statistics, Vol. 18, 1993, p. 237-259.")
  packageStartupMessage(paste(format(utils::citation(package="PINT")), collapse="\n"))
}
