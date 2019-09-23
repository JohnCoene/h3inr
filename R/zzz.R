h3 <- NULL

.onLoad <- function(libname, pkgname) {
  h3 <<- V8::new_context()
  file <- system.file("h3/h3.min.js", package = "h3inr")
  h3$source(file)
}