## 3D tsne plots layout-----------------------------------
tsne_3d_layout <- function(input, output, session, data, title, color, size){
  p <-   
    plot_ly(data,
            x = ~ V1,
            x = ~ V2,
            z = ~ V3,
            type = "scatter3d",
            color = color,
            hoverinfo = "text",
            text = paste('Cluster:', data$dbCluster),
            mode = 'markers',
            marker = list(
                          line = list(width = 0),
                          size = size,
                          sizeref = 3)
  )
  layout(p, title)
}  

