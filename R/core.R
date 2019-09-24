#' Coords to H3
#' 
#' @param data A data.frame or tibble containing \code{lat} and \code{lon}.
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
geo_to_h3 <- function(data, lat, lon, resolution = 4L) {
  # check inputs
  assert_that(has_it(data))
  assert_that(has_it(lat))
  assert_that(has_it(lon))
  assert_that(is_resolution_valid(resolution))

  lat <- enquo(lat)
  lon <- enquo(lon)
  
  indices <- select(data, lat = !!lat, lon = !!lon) %>% 
    pmap(function(lat, lon){
      h3$call("h3.geoToH3", lat, lon)
    }) %>% 
    map_dfr(function(x){
      tibble::tibble(hex = x)
    })

  bind_cols(data, indices)
}

#' Get Hexagon Center Coordinates
#' 
#' @inheritParams geo_to_h3
#' @param hex Column containing hexagon indices.
#' 
#' @return The \code{data} as \link[tibble]{tibble} with their corresponding
#' hexagon centers as \code{hex_center_lat} and \code{hex_center_lon} columns.
#' 
#' @examples
#' geo_to_h3(quakes, lat, long) %>% 
#'   h3_to_geo(hex)
#' 
#' @export
h3_to_geo <- function(data, hex) {
  # check inputs
  assert_that(has_it(data))
  assert_that(has_it(hex))

  hex <- enquo(hex)

  centers <- select(data, hex = !!hex) %>% 
    pmap(function(hex){
      h3$call("h3.h3ToGeo", hex)
    }) %>% 
    map_dfr(function(x){
      tibble::tibble(
        hex_center_lat = x[[1]],
        hex_center_lon = x[[2]]
      )
    })

  bind_cols(data, centers)
}

#' Get Hexagon Boundaries
#' 
#' @inheritParams geo_to_h3
#' @param hex Column containing hexagon indices.
#' 
#' @examples
#' geo_to_h3(quakes, lat, long) %>% 
#'   h3_to_geo_boundary(hex)
#' 
#' @return A named list of hexagon boundaries where the names are the hex index.
#' 
#' @export
h3_to_geo_boundary <- function(data, hex) {
  # check inputs
  assert_that(has_it(data))
  assert_that(has_it(hex))

  hex <- enquo(hex)

  boundaries <- select(data, hex = !!hex) %>% 
    pmap(function(hex){
      h3$call("h3.h3ToGeoBoundary", hex)
    }) 

  names(boundaries) <- pull(data, !!hex)
  return(boundaries)
}