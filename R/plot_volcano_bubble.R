#' @title plot_volcano_bubble
#'
#' @description Create volcano plot with different bubble size.
#'
#' @param A data frame used for plotting.
#' @param filename Output file name.
#' @param title Title of the plot.
#' @param p.cut.off p value cut off.
#' @param max.overlaps Maximum number of overlapping labels.
#' @param color.manual Colors used for different groups.
#' @param ratio_range Range of cell/media ratio, e.g. c(-10, 2)
#' @param bubble_size_range Range of bubble size, e.g. c(-1, 6)
#' @param breaks Breaks for cell/media ratio
#' @param bubble_name Legend name for bubbles
#'
#' @return A volcano plot with different bubble size.
#' @export
#' @import ggplot2 ggrepel

plot_volcano_bubble <- function(datatable = NULL,
                         filename = NULL,
                         title = NULL,
                         p.cut.off = 0.05,
                         max.overlaps = 10,
                         color.manual = c("#B4464B", "#4682B4", "grey50"),
                         ratio_range = c(-10, 2),
                         bubble_size_range = c(-1, 6),
                         breaks = c(-10, -7.5, -5, -2.5, 0, 2.5),
                         bubble_name = "Cell/media ratio"){


    p.x.position <- max(datatable$log2FC) - 0.5

    data_plot <-
      datatable %>%
      mutate(place_holder = FALSE) %>%
      add_row(log2FC = 0,
              log10p = 1.3,
              ratio_cell_media_log2 = ratio_range[1] - 0.5,
              place_holder = TRUE) %>%
      add_row(log2FC = 0,
              log10p = 1.3,
              ratio_cell_media_log2 = ratio_range[2] + 0.5,
              place_holder = TRUE)


    p <-
      ggplot( data = data_plot,
              aes( x = log2FC, y = log10p)) +
      geom_point( data = data_plot[data_plot$place_holder, ],
                  aes( color = Presence_in_media, text = variable,
                       size = ratio_cell_media_log2),
                  alpha = 0, show.legend = FALSE ) +
      geom_point( data = data_plot[data_plot$Presence_in_media == "Present", ],
                    aes( color = Presence_in_media, text = variable,
                         size = ratio_cell_media_log2),
                  alpha = 0.8, show.legend = TRUE ) +
      geom_point( data = data_plot[data_plot$Presence_in_media != "Present" & data_plot$place_holder == FALSE, ],
                  aes( color = Presence_in_media, text = variable),
                  alpha = 0.8, show.legend = TRUE ) +
      geom_text_repel( aes( label = label),
                                max.overlaps = max.overlaps, size = 2.5 ) +
      scale_color_manual( values = color.manual, drop = FALSE ) +
      scale_size(breaks = breaks,
                 range = bubble_size_range,
                 name = bubble_name) +
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
