#' Check Hexagon
#' 
#' Check whether hexagons are valid.
#' 
#' @inheritParams h3_to_geo
#' 
#' @examples
#' h3_is_hex("invalid")
#' h3_is_hex("809bfffffffffff")
#' 
#' geo_to_h3(quakes, lat, long, resolution = 3L) %>% 
#'   h3_is_hex(hex = hex, .name = "is_it_a_valid_hex")
#' 
#' @return A vector or original data.frame as 
#' \link[tibble]{tibble} with an additional column 
#' as specified by \code{.name} which defaults to 
#' \code{is_hex}.
#' 
#' @export
h3_is_hex <- function(data, ...) UseMethod("h3_is_hex")

#' @export
h3_is_hex.default <- function(data, ...){
  map(data, function(x){
    h3$call("h3.h3IsValid", x)
  }) %>% 
    unlist()
}

#' @export
#' @method h3_is_hex data.frame 
h3_is_hex.data.frame <- function(data, ..., hex, .name = "is_hex"){
  # check inputs
  assert_that(has_it(hex))

  hex_enquo <- enquo(hex)
  hexes <- pull(data, !!hex_enquo)

  data[[.name]] <- h3_is_hex(hexes)
  return(data)
}

#' Check Pentagon
#' 
#' Check whether pentagons are valid.
#' 
#' @inheritParams h3_to_geo
#' 
#' @examples
#' h3_is_pentagon("invalid")
#' 
#' @return A vector or original data.frame as 
#' \link[tibble]{tibble} with an additional column 
#' as specified by \code{.name} which defaults to 
#' \code{is_pentagon}.
#' 
#' @export
h3_is_pentagon <- function(data, ...) UseMethod("h3_is_pentagon")

#' @export
h3_is_pentagon.default <- function(data, ...){
  map(data, function(x){
    h3$call("h3.h3IsPentagon", x)
  }) %>% 
    unlist()
}

#' @export
#' @method h3_is_pentagon data.frame 
h3_is_pentagon.data.frame <- function(data, ..., hex, .name = "is_pentagon"){
  # check inputs
  assert_that(has_it(data))

  hex_enquo <- enquo(hex)
  hexes <- pull(data, !!hex_enquo)

  data[[.name]] <- h3_is_pentagon(hexes)
  return(data)
}

#' Check Class III Resolution
#' 
#' Whether the given H3 index is in a Class III resolution 
#' (rotated versus the icosahedron and subject to shape 
#' distortion adding extra points on icosahedron edges, 
#' making them not true hexagons).
#' 
#' @inheritParams h3_to_geo
#' 
#' @examples
#' h3_is_class_res("invalid")
#' 
#' @return A vector or original data.frame as 
#' \link[tibble]{tibble} with an additional column 
#' as specified by \code{.name} which defaults to 
#' \code{is_pentagon}.
#' 
#' @export
h3_is_class_res <- function(data, ...) UseMethod("h3_is_class_res")

#' @export
h3_is_class_res.default <- function(data, ...){
  map(data, function(x){
    h3$call("h3.h3IsResClassIII", x)
  }) %>% 
    unlist()
}

#' @export
#' @method h3_is_class_res data.frame 
h3_is_class_res.data.frame <- function(data, ..., hex, .name = "is_pentagon"){
  # check inputs
  assert_that(has_it(data))

  hex_enquo <- enquo(hex)
  hexes <- pull(data, !!hex_enquo)

  data[[.name]] <- h3_is_class_res(hexes)
  return(data)
}