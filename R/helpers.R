#' Title
#'
#' @param x Vector of values to be scaled
#'
#' @return
#' @export
#'
#' @examples
scale_coords <- function(x) {
  (x - min(x))/(max(x) - min(x))
}
