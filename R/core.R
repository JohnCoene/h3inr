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

  lat <- dplyr::enquo(lat)
  lon <- dplyr::enquo(lon)
  
  indices <- dplyr::select(data, lat = !!lat, lon = !!lon) %>% 
    purrr::pmap(function(lat, lon){
      h3$call("h3.geoToH3", lat, lon)
    }) %>% 
    purrr::map_dfr(function(x){
      tibble::tibble(hex = x)
    })

  dplyr::bind_cols(data, indices)
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

  hex <- dplyr::enquo(hex)

  centers <- dplyr::select(data, hex = !!hex) %>% 
    purrr::pmap(function(hex){
      h3$call("h3.h3ToGeo", hex)
    }) %>% 
    purrr::map_dfr(function(x){
      tibble::tibble(
        hex_center_lat = x[[1]],
        hex_center_lon = x[[2]]
      )
    })

  dplyr::bind_cols(data, centers)
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
#' @return A named list of hexagon boundaries.
#' 
#' @export
h3_to_geo_boundary <- function(data, hex) {
  # check inputs
  assert_that(has_it(data))
  assert_that(has_it(hex))

  hex <- dplyr::enquo(hex)

  boundaries <- dplyr::select(data, hex = !!hex) %>% 
    purrr::pmap(function(hex){
      h3$call("h3.h3ToGeoBoundary", hex)
    }) 

  names(boundaries) <- dplyr::pull(data, hex = !!hex)
  return(boundaries)
}