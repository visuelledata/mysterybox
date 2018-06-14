#' Creates a movable title with colored words at the top-left side of the plot
#'
#' You can't add this to a 'ggplot chain', you need to make the plot before and execute
#' this on a separate line. This title can be moved around and each segment of it can be
#' colored. It suffers from formatting issues, so the size of the plot, must be limited.
#' I recommend you only use it if you only absolutely need a colored title, as it can be
#' a lot of trouble to format unless you screenshot it and add it into a document later.
#'
#' @inheritParams cowplot::draw_text()
#' @param plot A ggplot
#' @param title_segments A title separated into a character vector.
#' @param colors A character vector of colors.
#' @param nudge_x A number, if positive the title moves right. Use {n; abs(n) < .5}.
#' @param nudge_y A number, if positive the title moves up. Use {n; abs(n) < .5}.
#'
#' @return ggplot
#'
#' @example
#' iris %>%
#' ggplot(aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   geom_smooth(se = FALSE) +
#'   xlab("\n Sepal length") +
#'   ylab("Sepal width \n") +
#'   theme_mysterybox()
#'
#' ggtitle_color(title_segments =  c('I ', 'love ', 'you'),
#'               colors = c('grey30', 'red', 'grey30'))
#'

ggtitle_color <- function(plot = last_plot(), title_segments, colors,
                          nudge_x = 0, nudge_y = 0, size = 14,
                          hjust = 0, vjust = 0, ...,
                          plot.margin = unit(c(.9, 1, 1, 1.2), "cm")) {

  assertthat::assert_that(is.character(title_segments), is.character(colors))

  plot.new()
  # Preformat the graph for the title
  plot <- plot +
    ggtitle(label = ' \n') +
    theme(plot.margin = plot.margin)

  # Build a gtable, then turn off clipping
  plot <- ggplot_build(plot)
  plot <- ggplot_gtable(plot)
  plot$layout$clip[plot$layout$name=="panel"] <- "off"

  # Modifying width between x_segments, then shifting them
  x <- .08 + nudge_x
  y <- .94 + nudge_y
  x_segments <- c(x, x + cumsum(strwidth(title_segments, font = 2)[-length((title_segments))]))

  # Draw plot with title
  cowplot::ggdraw(plot) +
    cowplot::draw_text(text = title_segments, colour = colors,
                       x = x_segments, y = y, hjust = hjust, vjust = vjust,
                       size = size, ..., fontface = 'bold')

}
