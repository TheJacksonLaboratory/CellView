#Chunks modified:
### CO-EXPRESSION HEATMAP ALL CLUSTERS ---- heatmap (h) chunk
### CO EXPRESSION TAB SELECTED HEATMAP ---- heatmap (h) chunk
## Heatmap chuncks--------------------
heat_map <- function(input, output, session, cluster_cols=FALSE,annotation_col=NA){
  pheatmap(
  as.matrix(expression),
  cluster_rows = TRUE,
  cluster_cols = FALSE,
  scale = 'row',
  fontsize_row = 10,
  labels_col = colnames(expression),
  labels_row = dataTables$featuredata[rownames(expression), 'Associated.Gene.Name'],
  show_rownames = TRUE,
  annotation_col = NA,
  show_colnames = FALSE,
  annotation_legend = TRUE,
  breaks = seq(-6, 6, by = .12),
  colorRampPalette(rev(brewer.pal(
    n = 6, name = "RdBu")))(100)
  )
}

