#' Get Neighbouring Hexagons
#' 
#' Get all hexagons in a k-ring around a given center. 
#' The order of the hexagons is undefined.
#' 
#' @inheritParams h3_to_geo
#' @param size Radius of the ring ($k$).
#' 
#' @return A named list of neighbours where the names are the hex index
#' of the which the neighbours have been computed.
#' 
#' @examples
#' \dontrun{
#' hexagons <- geo_to_h3(quakes, lat, long) %>% 
#'   k_ring(hex = hex)
#' }
#' 
#' @export
k_ring <- function(data, size = 1L, ...) UseMethod("k_ring")

#' @export
k_ring.default <- function(data, size = 1L, ...) {
  # check inputs
  assert_that(size > 0)
  assert_that(has_it(data))
  
  neighbours <- map(data, function(x, s){
    h3$call("h3.kRing", x, s)
  }, s = 1L)

  names(neighbours) <- data
  return(neighbours)
}

#' @export
#' @method k_ring data.frame
k_ring.data.frame <- function(data, size = 1L, ..., hex) {
  # check inputs
  assert_that(size > 0)
  assert_that(has_it(hex))

  hex_enquo <- enquo(hex)
  
  pull(data, !!hex_enquo) %>% 
    k_ring()
}

#' Polyfill
#' 
#' Get the set of hexagons within a polygon.
#' 
#' @inheritParams geo_to_h3
#' @param polygon A list defining coordiantes of polygon.
#' 
#' @examples
#' \dontrun{
#' # define a polygon
#' polygon <- list(
#'  list(37.813318999983238, -122.4089866999972145),
#'  list(37.7198061999978478, -122.3544736999993603),
#'  list(37.8151571999998453, -122.4798767000009008)
#' )
#' 
#' polyfill(polygon, 7L)
#' }
#' 
#' @export
polyfill <- function(polygon, resolution = 4L) {
  h3$call("h3.polyfill", polygon, resolution)
}

#' Get Neighbouring Hexagons Ordered by Distance
#' 
#' Get all hexagons in a k-ring around a given center, 
#' in an array of arrays ordered by distance from the origin. 
#' The order of the hexagons within each ring is undefined.
#' 
#' @inheritParams h3_to_geo
#' @param size Radius of the ring ($k$).
#' 
#' @return A named list of neighbours where the names are the hex index
#' of the which the neighbours have been computed.
#' 
#' @examples
#' \dontrun{
#' hexagons <- geo_to_h3(quakes, lat, long) %>% 
#'   k_ring_dist(hex = hex)
#' }
#' 
#' @export
k_ring_dist <- function(data, size = 1L, ...) UseMethod("k_ring_dist")

#' @export
k_ring_dist.default <- function(data, size = 1L, ...) {
  # check inputs
  assert_that(size > 0)
  assert_that(has_it(data))
  
  neighbours <- map(data, function(x, s){
    h3$call("h3.kRingDistances", x, s)
  }, s = 1L)

  names(neighbours) <- data
  return(neighbours)
}

#' @export
#' @method k_ring_dist data.frame
k_ring_dist.data.frame <- function(data, size = 1L, ..., hex) {
  # check inputs
  assert_that(size > 0)
  assert_that(has_it(hex))

  hex_enquo <- enquo(hex)
  
  pull(data, !!hex_enquo) %>% 
    k_ring_dist()
}

#' Get Line Between Hexagons
#' 
#' Given two H3 indexes, return the line of indexes between them (inclusive).
#' 
#' @param origin,destination H3indices to draw line.
#' 
#' @details This function may fail to find the line between two indexes,
#' for example if they are very far apart. It may also fail when finding 
#' distances for indexes on opposite sides of a pentagon.
#' 
#' @note Lines are drawn in grid space, and may not correspond exactly to 
#' either Cartesian lines or great arcs.
#' 
#' @return A list of h3indices drawing line.
#' 
#' @examples
#' \dontrun{
#' hexagons <- geo_to_h3(quakes, lat, long) 
#' h3_line(sample(hexagons$hex, 1), sample(hexagons$hex, 1))
#' }
#' 
#' @export
h3_line <- function(origin, destination) {
  # check inputs
  assert_that(has_it(origin))
  assert_that(has_it(destination))
  
  map2(origin, destination, function(x, y){
    h3$call("h3.h3Line", x, y)
  })
}

#' Get Hexagons Information
#' 
#' Average hexagon area, or edge length at a given resolution.
#' 
#' @param resolution Hexagon resolution.
#' @param unit Metric, either meters (\code{m2})
#' or kilometers (/code{km}, \code{km2}).
#' 
#' @examples
#' h3_area(7, "km2")
#' h3_edge_length(7, "km")
#' 
#' @rdname hex_info
#' @export
h3_area <- function(resolution = 4L, unit = c("m2", "km2")) {
  # check inputs
  unit <- match.arg(unit)
  unit <- paste0("h3.UNITS.", unit)
  
  h3$call("h3.hexArea", resolution, V8::JS(unit))
}

#' @rdname hex_info
#' @export
h3_edge_length <- function(resolution = 4L, unit = c("m", "km")) {
  # check inputs
  unit <- match.arg(unit)
  unit <- paste0("h3.UNITS.", unit)
  
  h3$call("h3.edgeLength", resolution, V8::JS(unit))
}

#' Get Number of Hexagons
#' 
#' The total count of hexagons in the world at a given resolution. 
#' Note that above resolution 8 the exact count cannot be represented 
#' in a JavaScript 32-bit number, so consumers should use caution when 
#' applying further operations to the output.
#' 
#' @inheritParams hex_info
#' 
#' @examples
#' h3_num_hexagons(2)
#' 
#' @rdname hex_info
#' @export
h3_num_hexagons <- function(resolution = 4L) {
  h3$call("h3.numHexagons", resolution)
}