#'


require(ggplot2)

ggtitle_color <- function(plot = last_plot(), title_segments, colors,
                          nudge_x = 0, nudge_y = 0, size = 14,
                          hjust = 0, vjust = 0, ...,
                          plot.margin = unit(c(.9, 1, 1, 1.2), "cm")) {
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
  x <- .065 + nudge_x
  y <- .94 + nudge_y
  x_segments <- c(x, x + cumsum(strwidth(title_segments, font = 2)[-length((title_segments))]))

  # Draw plot with title
  cowplot::ggdraw(plot) +
    cowplot::draw_text(text = title_segments, colour = colors,
                       x = x_segments, y = y, hjust = hjust, vjust = vjust,
                       size = size, ..., fontface = 'bold')

}

# iris %>%
#   ggplot(aes(Sepal.Length, Sepal.Width, color = Species)) +
#   geom_point() +
#   geom_smooth(se = FALSE) +
#   xlab('\n Sepal length') +
#   ylab('Sepal width \n') +
#   theme_mysterybox() +
#   no_legend()
#
#
# ggtitle_color(title_segments =  c('I ', 'love ', 'you'),
#               colors = c('grey30', 'red', 'grey30'))
