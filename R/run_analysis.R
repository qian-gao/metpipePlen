#' @title run_analysis
#'
#' @description A wrapper function to perform t test to data and then generate
#'              volcano plot.
#'
#' @param A data frame for analysis, sample x feature.
#' @param media.thres A threshold to label if feature exist in the media.
#' @param FC_thres A fold change threshold for highlighting only feature with
#'                 FC >= FC_thres.
#' @param path.result Path of the result folder.
#' @param p.cut.off p value cut off.
#' @param max.overlaps Maximum number of overlapping labels.
#' @param map.names A vector for mapping between original and formatted variable
#'                  names.
#' @param feature.info Feature information.
#' @param color.manual Colors used for different groups.
#' @param fig.format Output figure format
#'
#' @return t test results and volcano plot for each gene.
#' @export
#' @import dplyr openxlsx

run_analysis <- function(data = NULL,
                         media.thres = 500,
                         FC_thres = 2,
                         path.result = getwd(),
                         p.cut.off = 0.05,
                         max.overlaps = 10,
                         map.names = NULL,
                         feature.info = NULL,
                         color.manual = c("#B4464B", "#4682B4", "grey50"),
                         fig.format = ".svg"){

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

    filename <- paste0(path.result, "Volcano_plot_", i, fig.format)
    plot_title <- paste0("Volcano plot for ", i )

    plot_volcano(datatable = result.table,
                 filename = filename,
                 title = plot_title,
                 p.cut.off = p.cut.off,
                 max.overlaps = max.overlaps,
                 color.manual = color.manual)

    result.table <-
      result.table %>%
      select(-c(FC_over_thres, Presence_in_media, label))

    openxlsx::write.xlsx(result.table, file = paste0(path.result, "result_table_", i, ".xlsx"))

  }
}
