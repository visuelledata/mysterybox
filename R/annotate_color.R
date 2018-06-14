#' Annotate plots with each word being colored
#'
#' Creates a list of annotate functions by going along 'labels' and makes multiple strings
#' with all but one word being invisible. Assigns a color to each string and overlays them all
#' on a ggplot.
#'
#' @inheritParams annotate
#' @param labels The string that you want to annotate the plot with.
#' @param colors A character vector of colors
#' @param default_color A string with a default color of your choosing
#'
#' @seealso [annotate()]
#'
#' @examples
#' p <- qplot(1,1)
#'
#' p + annotate_color(x = 1, y = 1, size = 6,
#'                    labels = 'Assign different colors for each word',
#'                    colors = c('blue', 'green', 'purple', 'orange', 'black', 'yellow'))
#'
#' p + annotate_color(x = 1, y = 1,
#'                    default_color = 'black',
#'                    labels = 'Assign different color for one word',
#'                    colors = c('', '', 'red'))
#'
#' p + annotate_color(x = 1, y = 1,
#'                    default_color = 'Grey30',
#'                    labels = 'Assign different color for first word',
#'                    colors = c('red'))
#'

annotate_color <- function(geom = 'text', x, y, xmin = NULL, xmax = NULL,
                           ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, ..., na.rm = FALSE,
                           labels, colors, default_color = 'black') {
  labels <- strsplit(labels, " ")[[1]]
  n <- length(labels)

  # Assigns any empty values in 'colors' to the 'default_color'
  if (length(colors) < length(labels)){
    colors <- map_chr(seq_len(length(labels)),
                      function(i){
                        if (is.na(colors[i]) | colors[i] == ''){
                          colors[i] <- default_color
                        } else colors[i] <- colors [i]
                      })
  }

  # Shortens the length of 'colors' to match the length of 'labels'
  if (length(colors) > length(labels)){
    colors = colors[1:length(labels)]
    warning("'colors' exceeds words in 'labels'. Extra colors will be ignored.")
  }

  # Creates multiple labels by wrapping each word in phantom()
  labels <- map_chr(seq_len(n),
                    function(i) {
                      start0 <- labels[seq_along(labels) < i]
                      mid0 <- labels[i]
                      end0 <- labels[seq_along(labels) > i]
                      start <- paste0('phantom("', paste(start0, collapse = " "), ' ")')
                      end <- paste0('phantom("', paste(end0, collapse = " "), ' ")')
                      if(length(start0) > 0 && length(end0) > 0) {
                        paste(start, paste0('"', paste(mid0, collapse = " "), '"'), end, sep = ' * ')
                      } else if (length(end0) > 0) {
                        paste(paste0('"', paste(mid0, collapse = " "), '"'), end, sep = ' * ')
                      } else if (length(start0) > 0) {
                        paste(start, paste0('"', paste(mid0, collapse = " "), '"'), sep = ' * ')
                      } else {
                        stop("couldn't finish ...")
                      } # Anonymous function above created with the assistance of
                    })  # https://stackoverflow.com/users/3521006/docendo-discimus

  # Creates a list of annotate() functions by plugging arguments into them
  annofuncs <- list()
  annofuncs <- map2(labels, colors,
                    function(annolabel, annocolor){
                      annofuncs[seq_along(annolabel)] <-
                                  list(annotate(geom, x, y, xmin, xmax,
                                                ymin, ymax, xend, yend, ..., na.rm,
                                                parse = T, label = annolabel, color = annocolor))
                     })
  return(annofuncs)
}
