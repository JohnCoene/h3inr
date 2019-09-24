#' Check Hexagon
#' 
#' Check whether hexagons are valid.
#' 
#' @inheritParams h3_to_geo
#' 
#' @examples
#' h3_is_valid("invalid")
#' 
#' @return A vector or original data.frame as 
#' \link[tibble]{tibble} with \code{is_valid}
#' column.
#' 
#' @export
h3_is_valid <- function(data, ...) UseMethod("h3_is_valid")

#' @export
h3_is_valid.default <- function(data, ...){
  map(data, function(x){
    h3$call("h3.h3IsValid", x)
  }) %>% 
    unlist()
}

#' @export
#' @method h3_is_valid data.frame 
h3_is_valid.data.frame <- function(data, ..., hex){
  # check inputs
  assert_that(has_it(data))

  hex_enquo <- enquo(hex)
  hexes <- pull(data, !!hex_enquo)

  is_valid <- map(hexes, function(x){
    h3$call("h3.h3IsValid", x)
  }) %>% 
    map_dfr(function(x){
      tibble::tibble(
        is_valid = TRUE
      )
    })

  bind_cols(data, is_valid)
}