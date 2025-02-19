#' @title plot_volcano
#'
#' @description Create volcano plot.
#'
#' @param A data frame used for plotting.
#' @param filename Output file name.
#' @param title Title of the plot.
#' @param p.cut.off p value cut off.
#' @param max.overlaps Maximum number of overlapping labels.
#' @param color.manual Colors used for different groups.
#'
#' @return A volcano plot.
#' @export
#' @import ggplot2 ggrepel

plot_volcano <- function(datatable = NULL,
                         filename = NULL,
                         title = NULL,
                         p.cut.off = 0.05,
                         max.overlaps = 10,
                         color.manual = c("#B4464B", "#4682B4", "grey50")){


    p.x.position <- max(datatable$log2FC) - 0.5

    p <-
      ggplot( data = datatable,
              aes( x = log2FC, y = log10p)) +
      geom_point( aes( color = Presence_in_media, text = variable),
                  alpha = 0.8, show.legend = TRUE ) +
      geom_text_repel( aes( label = label),
                                max.overlaps = max.overlaps, size = 2.5 ) +
      scale_color_manual( values = color.manual, drop = FALSE ) +
      geom_text( aes( p.x.position, -log10(p.cut.off), label = paste0("p=", p.cut.off), vjust = 1.5),
                 size = 3, col = "black") +
      geom_hline( yintercept = -log10(p.cut.off), col = "black") +
      theme_bw() +
      #ylim(0, max(-log10(data.plot.f$adj.p.value))+0.5) +
      #xlim(min(data.plot.f$estimate)-0.5, max(data.plot.f$estimate)+0.5) +
      labs(x = "log2(FC)",
           y = "-log10(p value)",
           title = title) +
      guides(color = guide_legend(title="Presence in media"))

    ggsave(filename = filename, p, width = 7, height = 5, dpi = 300)

    return(p)
}
