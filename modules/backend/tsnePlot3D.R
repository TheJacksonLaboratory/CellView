## 3D tsne plots layout-----------------------------------
tsne_3D_layout <- function(input, output, session){
plot_ly(
  tsne.data,
  x = ~ V1,
  x = ~ V2,
  z = ~ V3,
  type = "scatter3d",
  color =  ~ dbCluster,
  hoverinfo = "text",
  text = paste('Cluster:', tsne.data$dbCluster),
  mode = 'markers',
  marker = 
   list(
    line = list(width = 0),
    size = rep(10, nrow(tsne.data)),
    sizeref = 3
   )
 )
}  

