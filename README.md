
# traceR <img src='dev/images/hex.png' align="right" height="240" />

`traceR` allows you to click on an image in the plot window (kind of
like a join the dots picture) and record the coordinates into a data
frame. They can then be plotted independently in a ggplot.

This enables the user to plot custom shapes in R without having to
manually enter the coordinates into a data frame.

## Installation

Install from Github:

``` r
devtools::install_github("doehm/traceR")
```

## Usage

### TL;DR

To use `trace_image`:

1.  Plot an image or any `ggplot`
2.  Run `df <- trace_image()`
3.  Click on the desired coordinates in the required sequence
4.  When finished click on the left side of the plot window between 0
    and `stop_window` pixels on the x-axis

The coordinates are scaled to (0, 1) by default and returned as a
`tibble`. There is an option to return the raw pixel coordinates.

## Example

To demonstrate

TBA
