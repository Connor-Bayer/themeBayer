
bayer_colors_vector <- function() {
  #'Function to return a list of named colors and their hex colors
  #'
  #'
  #'#' @export
  c(
    dark_blue = "#10384f", mid_blue = "#00617f", blue = "#0091df",
    light_blue = "#00bcff", dark_green = "#004422", mid_green = "#2b6636",
    green = "#66b512", light_green = "#89d329", dark_purple = "#443247",
    mid_purple = "#624963", raspberry = "#d30f4b", fuchsia = "#ff3162",
    white = "#ffffff"
  )
}

dark_blue <- "#10384f"
mid_blue <- "#00617f"
blue <- "#0091df"
light_blue <- "#00bcff"
dark_green <- "#004422"
mid_green <- "#2b6636"
green <- "#66b512"
light_green <- "#89d329"
dark_purple <- "#443247"
mid_purple <- "#624963"
raspberry <- "#d30f4b"
fuchsia <- "#ff3162"
white <- "#ffffff"

bayer_colors <- list(
  blue_green = c(
    "#10384f", "#00617f",
    "#0091df", "#00bcff",
    "#004422", "#2b6636",
    "#66b512", "#89d329"
  ),
  purple_green = c(dark_purple, mid_purple, raspberry, fuchsia, light_green, green, mid_green, dark_green),
  lights = c(light_blue, light_green, fuchsia),
  darks = c(dark_blue, dark_green, dark_purple),
  mids = c(mid_blue, mid_green, mid_purple),
  all = c(dark_blue, dark_purple, dark_green, light_blue, fuchsia, light_green, blue, green, mid_purple, mid_green, mid_blue, raspberry),
  greens = c(dark_green, mid_green, light_green),
  purples = c(dark_purple, mid_purple, raspberry, fuchsia),
  blues = c(dark_blue, mid_blue, light_blue)
)

bayer_palettes <- function(name, n, all_palettes = bayer_colors, type = c("discrete", "continuous")) {
  #' Returns a ggplot-compatible palette object
  #'
  #' Provides palettes that correspond to official Bayer color schema
  #' @param name Name of palette that you want to use
  #' @param n Number of values you want from the palette
  #' @param all_palettes List of colors - generally autofilled (don't use)
  #' @param type string, type of scale either "discrete" or "continuous"
  #' 
  #' @export
  palette <- all_palettes[[name]]
  if (missing(n)) {
    n <- length(palette)
  }
  else if(n > length(palette)){
    type = 'continuous'
  }
  type <- match.arg(type)
  out <- switch(type,
    continuous = grDevices::colorRampPalette(palette)(n),
    discrete = grDevices::colorRampPalette(palette)(n)
  )
  structure(out, name = name, class = "palette")
}

scale_color_bayer_c <- function(name, direction = 1) {
  #' Returns a ggplot-compatible palette object for continuous colors
  #'
  #' Provides a continuous color ggplot theme object that corresponds to official Bayer color schema
  #' @param name Name of palette that you want to use
  #' @param direction integer: if negative, use reverse order of colors.
  #'
  #' @return scale
  #' @export
  if(direction < 0){
    ggplot2::scale_colour_gradientn(colors = rev(bayer_palettes(name,
                                                         type = "continuous"
    )))
  }
  else{
    ggplot2::scale_colour_gradientn(colors = bayer_palettes(name,
    type = "continuous"
  ))
  }
}

scale_color_bayer_d <- function(name, direction = 1) {
  #' Returns a ggplot-compatible palette object for continuous colors
  #'
  #' Provides a discrete color ggplot theme object that corresponds to official Bayer color schema
  #' @param name Name of palette that you want to use
  #' @param direction integer: if negative, use reverse order of colors.
  #'
  #' @return scale
  #' @export

  if(direction < 0){
    ggplot2::scale_colour_manual(values = rev(bayer_palettes(name,
                                                         type = "discrete"
    )))
  }
  else{
  ggplot2::scale_colour_manual(values = bayer_palettes(name,
    type = "discrete"
  ))
  }
}

scale_fill_bayer_c <- function(name, direction = 1) {
  #' Returns a ggplot-compatible palette object for continuous colors
  #'
  #' Provides a continuous fill ggplot theme object that corresponds to official Bayer color schema
  #' @param name Name of palette that you want to use
  #' @param direction integer: if negative, use reverse order of colors.
  #'
  #' @return scale
  #' @export

  if(direction < 0){
    ggplot2::scale_fill_gradientn(colors = rev(bayer_palettes(name,
                                                       type = "continuous"
    )))
  }
  else{
  ggplot2::scale_fill_gradientn(colors = bayer_palettes(name,
    type = "continuous"
  ))
  }
}

scale_fill_bayer_d <- function(name, direction = 1) {
  #' Returns a ggplot-compatible palette object for continuous colors
  #'
  #' Provides a discrete fill ggplot theme object that corresponds to official Bayer color schema
  #' @param name Name of palette that you want to use
  #' @param direction integer: if negative, use reverse order of colors.
  #'
  #' @return scale
  #' @export

  if(direction < 0){
    ggplot2::scale_fill_manual(values = rev(bayer_palettes(name,
                                                       type = "discrete"
    )))
  }
  else{
    ggplot2::scale_fill_manual(values = bayer_palettes(name,
    type = "discrete"
  ))
  }
}

.onLoad <- function(libname, pkgname){
  dark_blue <- "#10384f"
  mid_blue <- "#00617f"
  blue <- "#0091df"
  light_blue <- "#00bcff"
  dark_green <- "#004422"
  mid_green <- "#2b6636"
  green <- "#66b512"
  light_green <- "#89d329"
  dark_purple <- "#443247"
  mid_purple <- "#624963"
  raspberry <- "#d30f4b"
  fuchsia <- "#ff3162"
  white <- "#ffffff"
}
