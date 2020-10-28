
#' Function to extract darkpeak colors as hex codes
#'
#' @param ... Character names of darkpeak_colors
#'
darkpeak_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (darkpeak_colours)

  darkpeak_colours[cols]
}

