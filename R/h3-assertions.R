#' Check Hexagon
#' 
#' Check whether hexagons are valid.
#' 
#' @export
h3_is_valid <- function(data, ...) UseMethod("h3_is_valid")

#' @export
h3_is_valid.default <- function(data, ...) {
  
}

#' @export
#' @method h3_is_valid data.frame
h3_is_valid.data.frame <- function(data, ..., hex) {
  assert_that(has_it(hex))
}