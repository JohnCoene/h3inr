has_it <- function(x) {
  !missing(x)
}

on_failure(has_it) <- function(call, env) {
  paste0(
    "`", crayon::blue(deparse(call$x)),
    "` is missing."
  )
}

is_resolution_valid <- function(x) {
  x > 0 && x < 16
}

on_failure(is_resolution_valid) <- function(call, env) {
  paste0(
    "`", crayon::blue(deparse(call$x)),
    "` must be between 0 and 15."
  )
}