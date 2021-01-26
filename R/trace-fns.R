#' Records coordinates of the traced plot
#'
#' @param name Name of the traced segment. This will add the name as a column to the
#' resulting data frame. This is so you can `bind_rows()` multiple data frames and
#' name references the segment.
#' @param scale Logical. Scale the coords as the final step.
#'
#' @return
#' @export
#'
#' @examples \dontrun{}
record_coords <- function(
  name,
  scale = FALSE
  ) {
  df <- NULL
  x <- 10
  y <- 10
  k <- 0
  while(x > 9 & y > 9) {
    k = k + 1
    coords <- grid.locator()
    x <- as.numeric(str_extract(coords$x, "[:digit:]+"))
    y <- as.numeric(str_extract(coords$y, "[:digit:]+"))
    cat(green(glue("counter : {k}\r")))
    df <- df %>%
      bind_rows(tibble(x = x, y = -y))
  }
  cat(green(glue("trace killed / {k} traced points\n")))
  df %>%
    filter(x > 9 & y < -9) %>%
    mutate(name = name) %>%
    if(scale) {
      mutate(.,
             x = scale_01(x),
             y = scale_01(y)
      )
    } else .
}
