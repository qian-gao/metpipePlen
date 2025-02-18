#' @title plot_volcano_bubble
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param datatable
#' @param path.result
#' @param title description
#' @param FC_thres
#' @param p.cut.off
#' @param max.overlaps
#' @param color.manual
#'
#' @return A data frame
#' @examples
#' @export
#' @import ggplot2 ggrepel openxlsx

plot_volcano_bubble <- function(datatable = NULL,
                         filename = NULL,
                         title = NULL,
                         FC_thres = NULL,
                         p.cut.off = NULL,
                         max.overlaps = NULL,
                         color.manual = c("#B4464B", "#4682B4", "grey50")){


    p.x.position <- max(datatable$log2FC) - 0.5

    p <-
      ggplot( data = datatable,
              aes( x = log2FC, y = log10p)) +
      geom_point( data = datatable[datatable$Presence_in_media == "Present", ],
                    aes( color = Presence_in_media, text = variable,
                         size = ratio_cell_media_log2),
                  alpha = 0.8, show.legend = TRUE ) +
      geom_point( data = datatable[datatable$Presence_in_media != "Present", ],
                  aes( color = Presence_in_media, text = variable),
                  alpha = 0.8, show.legend = TRUE ) +
      ggrepel::geom_text_repel( aes( label = label),
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
