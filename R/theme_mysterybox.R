#' A theme for use with ggtitle_color()
#'
#' A theme for this package.
#'
theme_mysterybox <- function() {
  theme(axis.title = element_text(color = 'grey40', size = 14),
        axis.title.y = element_text(hjust = .94),
        axis.line.x = element_line(color = 'grey35'),
        axis.line.y = element_line(color = 'grey35'),
        axis.text = element_text(color = 'grey40'),
        axis.ticks = element_line(color = 'grey35'))
}

