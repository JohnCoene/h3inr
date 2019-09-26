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
#' @examples
#' h3_to_base_cell("809bfffffffffff")
#' 
#' @export
h3_to_base_cell <- function(data, ...) UseMethod("h3_to_base_cell")

#' @export
h3_to_base_cell.default <- function(data, ...) {
  map(data, function(x){
      h3$call("h3.h3GetBaseCell", x)
    }) %>% 
    unlist()
}

#' @export
#' @method h3_to_base_cell data.frame
h3_to_base_cell.data.frame <- function(data, ..., hex, .name = "base_cell") {
  assert_that(has_it(data))

  hex_enquo <- enquo(hex)
  base_cell <- pull(data, !!hex_enquo) %>% 
    map(h3_to_base_cell)

  data[[.name]] <- base_cell
  return(data)
}

#' Base Resolution 
#'
#' Get the resolution of an H3 index.
#'
#' @inheritParams h3_to_geo
#'
#' @return A vector or original data.frame as
#' \link[tibble]{tibble} with an additional column
#' as specified by \code{.name} which defaults to
#' \code{resolution}.
#' 
#' @examples
#' h3_to_resolution("809bfffffffffff")
#'
#' @export
h3_to_resolution <- function(data, ...) UseMethod("h3_to_resolution")

#' @export
h3_to_resolution.default <- function(data, ...) {
  map(data, function(x){
      h3$call("h3.h3GetResolution", x)
    }) %>%
    unlist()
}

#' @export
#' @method h3_to_resolution data.frame
h3_to_resolution.data.frame <- function(data, ..., hex, .name = "resolution") {
  assert_that(has_it(data))

  hex_enquo <- enquo(hex)
  base_cell <- pull(data, !!hex_enquo) %>%
    map(h3_to_resolution)

  data[[.name]] <- base_cell
  return(data)
}

#' Get Parent
#'
#' Get the parent of the given hexagon at a particular resolution.
#'
#' @inheritParams h3_to_geo
#'
#' @return A vector or original data.frame as
#' \link[tibble]{tibble} with an additional column
#' as specified by \code{.name} which defaults to
#' \code{parent}.
#' 
#' @examples
#' h3_to_parent("809bfffffffffff")
#'
#' @export
h3_to_parent <- function(data, ...) UseMethod("h3_to_parent")

#' @export
h3_to_parent.default <- function(data, ...) {
  map(data, function(x){
      h3$call("h3.h3ToParent", x)
    }) %>%
    unlist()
}

#' @export
#' @method h3_to_parent data.frame
h3_to_parent.data.frame <- function(data, ..., hex, .name = "parent") {
  assert_that(has_it(data))

  hex_enquo <- enquo(hex)
  base_cell <- pull(data, !!hex_enquo) %>%
    map(h3_to_parent)

  data[[.name]] <- base_cell
  return(data)
}

#' Get Children
#'
#' Get the children/descendents of the given hexagon at a particular resolution.
#'
#' @inheritParams h3_to_geo
#'
#' @return A named list of children where the name of the list item
#' is the corresponding h3index.
#' 
#' @examples
#' \dontrun{h3_to_children("809bfffffffffff")}
#'
#' @export
h3_to_children <- function(data, ...) UseMethod("h3_to_children")

#' @export
h3_to_children.default <- function(data, ...) {
  children <- map(data, function(x){
      h3$call("h3.h3ToChildren", x)
    })
  
  names(children) <- data
  return(children)
}

#' @export
#' @method h3_to_children data.frame
h3_to_children.data.frame <- function(data, ..., hex, .name = "child") {
  assert_that(has_it(data))

  hex_enquo <- enquo(hex)
  pull(data, !!hex_enquo) %>%
    map(h3_to_children)
}

#' Get Center Child
#'
#' Get the center child of the given hexagon at a particular resolution.
#'
#' @inheritParams h3_to_geo
#'
#' @return A vector or original data.frame as
#' \link[tibble]{tibble} with an additional column
#' as specified by \code{.name} which defaults to
#' \code{center_child}.
#' 
#' @examples
#' h3_to_center_child("809bfffffffffff")
#'
#' @export
h3_to_center_child <- function(data, ...) UseMethod("h3_to_center_child")

#' @export
h3_to_center_child.default <- function(data, ...) {
  map(data, function(x){
      h3$call("h3.h3ToCenterChild", x)
    }) %>%
    unlist()
}

#' @export
#' @method h3_to_center_child data.frame
h3_to_center_child.data.frame <- function(data, ..., hex, .name = "center_child") {
  assert_that(has_it(data))

  hex_enquo <- enquo(hex)
  base_cell <- pull(data, !!hex_enquo) %>%
    map(h3_to_center_child)

  data[[.name]] <- base_cell
  return(data)
}
