#' Base Cell
#' 
#' Get the number of the base cell for a given H3 index.
#' 
#' @inheritParams h3_to_geo
#' 
#' @return A vector or original data.frame as 
#' \link[tibble]{tibble} with an additional column 
#' as specified by \code{.name} which defaults to 
#' \code{base_cell}.
#' 
#' @export
get_base_cell <- function(data, ...) UseMethod("get_base_cell")

#' @export
get_base_cell.default <- function(data, ...) {
  map(data, function(x){
      h3$call("h3.h3GetBaseCell", x)
    }) %>% 
    unlist()
}

#' @export
#' @method get_base_cell data.frame
get_base_cell.data.frame <- function(data, ..., hex, .name = "base_cell") {
  assert_that(has_it(data))

  hex_enquo <- enquo(hex)
  base_cell <- pull(data, !!hex_enquo) %>% 
    map(get_base_cell)

  data[[.name]] <- base_cell
  return(data)
}
