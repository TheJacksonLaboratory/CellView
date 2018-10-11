# EXPLORE TAB 3D PLOT ---------------------------------
# EXPLORE TAB PANEL PLOT--------------------------------
# CO-EXPRESSION HEATMAP ALL CLUSTERS -------------------
# CO EXPRESSION TAB SELECTED HEATMAP -------------------
# CO EXPRESSION TAB ON/OFF PLOT ------------------------
#server function ---------------------------------------
get_genesin <- function(input, output, session, data, data_name){
  if (data_name %in% c("mclustids", "heatmap_geneids", "heatmap_geneids2")) {
    genesin <- input$data
    genesin <- toupper(genesin)
    genesin <- strsplit(genesin, ',')
  } else if (data_name == "genesList") {
    genesin <- input$data
    genesin <- strsplit(genesin, ',')
    return(genesin)
  } else if(data_name == "panelplotids") {
    genesin <- input$data
    genesin <- toupper(genesin)
    genesin <- strsplit(genesin, ',')
    genesin<-genesin[[1]]
  } else 
    cat("Not in the list of possible options. \n Try again!")

