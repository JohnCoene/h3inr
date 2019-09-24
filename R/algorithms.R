#' Get neighbours
#' 
#' Get neighbouring hexagons.
#' 
#' @inheritParams geo_to_h3
#' @inheritParams h3_to_geo_boundary
#' @param size Radius of the ring ($k$).
#' 
#' @return A named list of neighbours where the names are the hex index
#' of the which the neighbours have been computed.
#' 
#' @examples
#' hexagons <- geo_to_h3(quakes, lat, long)
#' k_ring(hexagons, hex)
#' 
#' @export
k_ring <- function(data, hex, size = 1L) {
  # check inputs
  assert_that(size > 0)
  assert_that(has_it(data))
  assert_that(has_it(hex))

  hex_enquo <- enquo(hex)
  data <- pull(data, !!hex_enquo)
  neighbours <- map(data, function(x, s){
    h3$call("h3.kRing", x, s)
  }, s = 1L)

  names(neighbours) <- data
  return(neighbours)
}

#' Polyfill
#' 
#' Get the set of hexagons within a polygon.
#' 
#' @inheritParams geo_to_h3
#' @param polygon A list defining coordiantes of polygon.
#' 
#' @examples
#' # define a polygon
#' polygon <- list(
#'  list(37.813318999983238, -122.4089866999972145),
#'  list(37.7198061999978478, -122.3544736999993603),
#'  list(37.8151571999998453, -122.4798767000009008)
#' )
#' 
#' polyfill(polygon, 7L)
#' 
#' @export
polyfill <- function(polygon, resolution = 4L) {
  h3$call("h3.polyfill", polygon, resolution)
}