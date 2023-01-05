utils::globalVariables(c("x", "y", "id"))

#' Imports
#' @name trace_image
#' @importFrom grid grid.locator
#' @importFrom stringr str_extract
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes theme_void geom_polygon geom_point geom_text
#' @importFrom ggimage geom_image
#' @importFrom crayon underline
#' @importFrom ggforce geom_bspline_closed0
NULL

#' Scale coords
#'
#' Scales recorded coordinates to (0, 1)
#'
#' @param x Vector of values to be scaled
#'
#' @return Vector of numeric values
#' @export
#'
#' @examples
#' x <- 1:10
#' scale_coords(x)
scale_coords <- function(x) {
  (x - min(x))/(max(x) - min(x))
}

#' Inspect trace
#'
#' Takes as input the traced coordinates and plots them as a polygon.
#'
#' @param data Data frame of traced coordinates i.e. the output of `trace_image`. Must have values
#' `x` and `y`
#'
#' @return ggplot image
#' @export
#'
#' @examples
#' if(interactive()) {
#'   library(ggimage)
#'
#'   # plot an image
#'   img <- paste0(system.file(package = "traceR"), "/images/star.png")
#'   ggplot() +
#'     geom_image(aes(0, 0, image = img), size = 1)
#'
#'   # trace the image
#'   df <- trace_image()
#'
#'   # inspect the traced image
#'   inspect_trace(df)
#' }
inspect_trace <- function(data) {
  data$id <- 1:nrow(data)
  if(!"layer" %in% colnames(data)) {
    data$layer <- "null"
  }
  print(
    ggplot(data, aes(x, y, group = layer)) +
      geom_polygon(fill = NA, colour = "black") +
      geom_bspline_closed0(fill = NA, colour = "blue") +
      geom_point(size = 6) +
      geom_text(aes(label = id), colour = "white", size = 2)
  )
}
