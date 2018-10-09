#Chunks modified:
### CO-EXPRESSION HEATMAP ALL CLUSTERS ---- heatmap (h) chunk
### CO EXPRESSION TAB SELECTED HEATMAP ---- heatmap (h) chunk
## Heatmap chuncks--------------------
heat_map <- function(input, output, session,
		     data = expression,
		     cluster_cols = FALSE,
		     annotation_col = NA) {
  h <- pheatmap(
    as.matrix(data),
    cluster_rows = TRUE,
    cluster_cols = cluster_cols,
    scale = 'row',
    fontsize_row = 10,
    labels_col = colnames(data),
    labels_row = dataTables$featuredata[rownames(data), 'Associated.Gene.Name'],
    show_rownames = TRUE,
    annotation_col = annotation_col,
    show_colnames = FALSE,
    annotation_legend = TRUE,
    breaks = seq(-6, 6, by = .12),
    colorRampPalette(rev(brewer.pal(
      n = 6, name = "RdBu")))(100)
  )
  h
}

