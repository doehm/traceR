#' Records coordinates of the traced plot
#'
#' Click on the image to record the coordinates. When finished click on the
#' right side of the window. A data frame is returned with the (x, y) coordinates
#' scaled to (0, 1).
#'
#' @param stop_window The number of pixels to create the kill switch.
#'
#' @details To use \code{trace_image}:
#' \enumerate{
#' \item{}{Plot an image or any \code{ggplot}}
#' \item{}{Run \code{df <- trace_image()}}
#' \item{}{Click on the desired coordinates in the required sequence}
#' \item{}{When finished click on the left side of the plot window between 0
#' and \code{stop_window} pixels on the x-axis}
#' }
#'
#' The coordinates are scaled to (0, 1) and returned as a \code{tibble}
#'
#' @return A tibble
#' @export
#'
#' @examples
#' if(interactive()) {
#'   df <- trace_image()
#'   inspect_trace(df)
#' }
trace_image <- function(
  stop_window = 20
  ) {

  # initialise
  df <- NULL
  k <- 0
  x <- stop_window + 1

  # record coords
  cat(glue("
  {underline('Trace image')}
  \n  Recording started. Click on the image to record the coordinates.
  To stop the recording click on the left side of the plot window
  less than {stop_window} pixels from the border.
  \n  Coordinates:\n\n\n"))
  while(x > stop_window) {
    k <- k + 1
    coords <- grid.locator()
    x <- as.numeric(str_extract(coords$x, "[:digit:]+"))
    y <- as.numeric(str_extract(coords$y, "[:digit:]+"))
    cat(glue("  {k} ({x}, {y})\n\n"))
    df <- bind_rows(df, tibble(x = x, y = y))
  }

  cat(glue("\n\nTrace killed / {k-1} points recorded\n\n"))
  df <- df[-nrow(df),]
  df$x <- scale_coords(df$x)
  df$y <- scale_coords(df$y)

  df
}
