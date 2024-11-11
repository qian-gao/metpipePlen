#' @title run_analysis
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' @export
#' @import dplyr openxlsx

run_analysis <- function(data = data,
                         media.thres = 500,
                         FC_thres = 2,
                         path.result = getwd(),
                         p.cut.off = 0.05,
                         max.overlaps = 10,
                         map.names = NULL,
                         feature.info = NULL,
                         color.manual = c("#B4464B", "#4682B4", "grey50")){

  genes <- unique(data$Group)
  genes <- genes[!genes %in% c("MOCK", "MEDIA")]

  m_names <- colnames(data)
  m_names <- m_names[!m_names %in% c("Sample.name", "Group")]

  names(color.manual) <- c("Present", "Absent", "Non-significant")

  for (i in genes){
    subset <-
      data$Group %in% c(i, "MOCK")

    data.i <- data[subset, ]

    unique(data.i$Group)
    data.i$Group <- factor(data.i$Group, levels = c("MOCK", i))

    result <-
      compute_t( data = data.i,
                 formula = "~ Group",
                 dv = m_names,
                 map.names = map.names)

    result.table <-
      result$coef.tbl.all %>%
      dplyr::rename(log2FC = estimate,
                    Gene = coefficient) %>%
      mutate(Gene = gsub("Group", "", Gene),
             FC = 2^log2FC,
             log10p = -log10(adj.p.value)) %>%
      left_join(feature.info[, c("Identity_mode","mean.MEDIA")],
                by = c("variable" = "Identity_mode")) %>%
      mutate(In_media = mean.MEDIA > media.thres,
             FC_over_thres = FC > FC_thres,
             Presence_in_media = case_when( In_media & FC_over_thres & adj.p.value < p.cut.off ~ "Present",
                                            !In_media & FC_over_thres & adj.p.value < p.cut.off ~ "Absent",
                                            TRUE ~ "Non-significant"
             ),
             label = if_else(FC_over_thres & adj.p.value < p.cut.off, variable, "")) %>%
      select(-c(AveExpr, B))

    result.table$Presence_in_media <- factor(result.table$Presence_in_media, levels = c("Present", "Absent", "Non-significant"))

    p.x.position <- max(result.table$log2FC) - 0.5

    p <-
      ggplot( data = result.table,
              aes( x = log2FC, y = log10p)) +
      geom_point( aes( color = Presence_in_media, text = variable),
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
           title = paste0("Volcano plot for ", i )) +
      guides(color = guide_legend(title="Presence in media"))

    ggsave(filename = paste0(path.result, "Volcano_plot_", i, ".png"), p, width = 7, height = 5, dpi = 300)

    result.table <-
      result.table %>%
      select(-c(FC_over_thres, Presence_in_media, label))

    openxlsx::write.xlsx(result.table, file = paste0(path.result, "result_table_", i, ".xlsx"))

  }
}
