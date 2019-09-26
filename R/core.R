#' Coords to H3
#' 
#' Turn coordinates into hexagon indices.
#' 
#' @param data A data.frame, \link[tibble]{tibble} containing
#' \code{lat} and \code{lon}.
#' @param lat,lon Coordinates to bin.
#' @param resolution Hexbin resolution, see 
#' \href{https://uber.github.io/h3/#/documentation/core-library/resolution-table}{official documentation}.
#' 
#' @return The \code{data} as \link[tibble]{tibble} with their corresponding
#' hexagon index as \code{hex} column.
#' 
#' @examples
#' geo_to_h3(quakes, lat, long)
#' 
#' @export
geo_to_h3 <- function(data, lat, lon, resolution = 7L) {
  # check inputs
  assert_that(has_it(data))
  assert_that(has_it(lat))
  assert_that(has_it(lon))
  assert_that(is_resolution_valid(resolution))

  lat_enquo <- enquo(lat)
  lon_enquo <- enquo(lon)
  
  indices <- select(data, lat = !!lat_enquo, lon = !!lon_enquo) %>% 
    pmap(function(lat, lon, res){
      h3$call("h3.geoToH3", lat, lon, res)
    }, res = resolution) %>% 
    map_dfr(function(x){
      tibble::tibble(hex = x)
    })

  bind_cols(data, indices)
}

#' Get Hexagon Center Coordinates
#' 
#' @param data A data.frame or character vector,
#' if the former is specified then the \code{hex}
#' argument \emph{must} be passed, specifying the
#' bare column name containing hexagon indices.
#' @param ... Other arguments passed to methods.
#' 
#' @return The \code{data} as \link[tibble]{tibble} with their corresponding
#' hexagon centers as \code{hex_center_lat} and \code{hex_center_lon} columns,
#' \code{data} is a data.frame then data is bound to the latter.
#' 
#' @examples
#' geo_to_h3(quakes, lat, long) %>% 
#'   h3_to_geo(hex = hex)
#' 
#' @export
h3_to_geo <- function(data, ...) UseMethod("h3_to_geo")

#' @export
h3_to_geo.default <- function(data, ...) {
  map(data, function(x){
      tryCatch(
        h3$call("h3.h3ToGeo", x),
        error = function(e) list(NA, NA)
      )
    }) %>% 
    map_dfr(function(x){
      tibble::tibble(
        hex_center_lat = x[[1]],
        hex_center_lon = x[[2]]
      )
    })
}

#' @export
#' @method h3_to_geo data.frame
h3_to_geo.data.frame <- function(data, ..., hex) {
  # check inputs
  assert_that(has_it(hex))

  hex <- enquo(hex)

  centers <- pull(data, !!hex) %>% 
    h3_to_geo()

  bind_cols(data, centers)
}

#' Get Hexagon Boundaries
#' 
#' @inheritParams h3_to_geo
#' 
#' @examples
#' \dontrun{
#' geo_to_h3(quakes, lat, long) %>% 
#'   h3_to_geo_boundary(hex = hex)
#' }
#' 
#' @return A named list of hexagon boundaries where the names are the hex index.
#' 
#' @export
h3_to_geo_boundary <- function(data, ...) UseMethod("h3_to_geo_boundary")

#' @export
h3_to_geo_boundary.default <- function(data, ...) {
  boundaries <- data %>% 
    map(function(x){
      h3$call("h3.h3ToGeoBoundary", x)
    }) 

  names(boundaries) <- data
  return(boundaries)
}

#' @export
#' @method h3_to_geo_boundary data.frame 
h3_to_geo_boundary.data.frame <- function(data, ..., hex) {
  assert_that(has_it(hex))

  hex_enquo <- enquo(hex)

  pull(data, !!hex_enquo) %>% 
    h3_to_geo_boundary()
}