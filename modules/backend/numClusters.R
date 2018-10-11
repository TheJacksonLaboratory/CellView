# RENDER UI section of server.R-----------------------------------------------------

render_noOfClusters <- function(input, output, session, data, cluster, choices){
  output$clusters <- renderUI({
    noOfClusters <- max(data)
    selectInput(cluster,
                label = "Cluster",
                choices = choices,
                selected = 1)
  })
}

