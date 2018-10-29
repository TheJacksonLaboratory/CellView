###Sections where module will be added######## 
##EXPLORE TAB 3D PLOT --------------------------------
##EXPLORE TAB CLUSTER PLOT ---------------------------
##EXPLORE TAB VIOLIN PLOT ----------------------------
##EXPLORE TABL DOWNLOAD SELECTED WITH BRUSH ----------
##CO EXPRESSION TAB CLUSTER PLOT ---------------------

#Module grabs geneid, expression, tsne.data, subsetData for the gene/s input
subsetdata <- function(input, output, session, dataTables, tsne){
  geneid <- rownames(dataTables$featuredata[which(dataTables$featuredat
a$Associated.Gene.Name == toupper(input$gene_id)), ])[1]
  expression <- dataTables$log2cpm[geneid, ]
  cat(file = stderr(), rownames(expression))
  validate(need(is.na(sum(expression)) != TRUE, 'Gene symbol incorrect or gene not expressed'))
  tsne.data <- cbind(dataTables$tsne.data, t(expression))
  names(tsne.data)[names(tsne.data) == geneid] <- 'values'
  subsetData <- subset(tsne.data, dbCluster == input$cluster)
  if (tsne == TRUE){
    return(tsne.data)    
  } else {
    return(subsetData)
  }
}

