# LIBRARY -----------------------------------------------------------------

init_server <- function() {
  set.seed(1)
  options(shiny.maxRequestSize = 2000 * 1024 ^ 2)
}

load_rds <- function(input) {
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)


    load(inFile$datapath)

    cat(stderr(), 'Loaded')

    dataTables$log2cpm <- log2cpm
    dataTables$tsne.data <- tsne.data
    featuredata <-
      featuredata[which(featuredata$Chromosome.Name %in% c(unlist(lapply(
        seq(1, 22, 1), toString
      )), c("X", "Y", "MT"))), ]
    featuredata$Associated.Gene.Name <-
      toupper(featuredata$Associated.Gene.Name)
    featuredata<-featuredata[rownames(log2cpm),]
    dataTables$featuredata <- featuredata
    dataTables$positiveCells <- NULL
    dataTables$positiveCellsAll <- NULL
}

shinyServer(function(input, output) {
  init_server()

  dataTables <- reactiveValues(
    log2cpm = NULL,
    tsne.data = NULL,
    featuredata = NULL,
    selectedDge = NULL
  )

  data <- reactive({
    load_rds(input)
  })


  # RENDER UI  ------------------------------------------------------------------
  
  callModule(render_noOfClusters,
             data = dataTables$tsne.data$dbCluster,
             cluster = "cluster",
             choices = c(0:noOfClusters))

  callModule(render_noOfClusters,
             data = dataTables$tsne.data$dbCluster, 
             cluster = "cluster1",
             choices = c(0:noOfClusters))

  callModule(render_noOfClusters,
             data = dataTables$tsne.data$dbCluster, 
             cluster = "cluster2", choices = c(0:noOfClusters))

  callModule(render_noOfClusters,
             data = dataTables$tsne.data$dbCluster,
             cluster = "cluster3", choices = c(0:noOfClusters))

  callModule(render_noOfClusters, 
             data = dataTables$tsne.data$dbCluster,
             cluster = "cluster4",
             choices = (c("All"), c(0:noOfClusters)))
  
  # MAIN 3D PLOT ------------------------------------------------------------------

  output$tsne_main <- renderPlotly({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    data()
    tsne.data <- as.data.frame(dataTables$tsne.data)
    #cat(stderr(),colnames(tsne.data)[1:5])
    tsne.data$dbCluster <- as.factor(tsne.data$dbCluster)

    callModule(tsne_3d_plot, 
               data = tsne.data,
               title = NULL,
               color = ~ dbCluster,
               size = rep(10, nrow(tsne.data)))
    
  })

  # SUMMARY STATS ----------------------------------------------------------------

  output$summaryStats<-renderUI({

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    line1<-paste('No. of cells:', dim(dataTables$log2cpm)[2],sep='\t')
    line2<-paste('Median UMIs:', median(t(dataTables$log2cpm['ENSGUMI',])),sep='\t')
    line3<-paste('Median Genes:', median(t(dataTables$log2cpm['ENSGGENES',])),sep='\t')
    line4<-paste('No. of clusters:', max(dataTables$tsne.data$dbCluster),sep='\t')
    HTML(
      paste0("Summary statistics of this dataset:", '<br/>','<br/>',
        line1, '<br/>',
             line2, '<br/>',
             line3, '<br/>',
             line4)
    )
  })

  # EXPLORE TAB 3D PLOT ------------------------------------------------------------------
  genes <- eventReactive(input$goButton, {
    callModule(get_genesin, data = genesList, list_genesin = "genesList")
  })

  v <- reactiveValues(doPlot = FALSE)
  observeEvent(input$goButton, {
    v$doPlot <- input$goButton
  })

  output$tsne_plt <- renderPlotly({
    if (v$doPlot == FALSE)
      return()

    isolate({
      callModule(subsetdata, dataTables = dataTables, tsne = TRUE)
      callModule(tsne_3d_layout,
                 data = tsne.data,
                 title = toupper(input$gene_id),
                 color = ~ values,
                 size = 2)
    })
  })
  # EXPLORE TAB CLUSTER PLOT ------------------------------------------------------------------

  output$clusterPlot <- renderPlot({
    if (v$doPlot == FALSE)
      return()

    isolate({
      callModule(subsetdata, dataTables = dataTables, tsne = FALSE)
      
      p1 <-
        ggplot(subsetData,
               aes_string(x = input$dimension_x, y = input$dimension_y)) +
        geom_point(aes_string(size = 2, color = 'values')) +
        geom_point(shape = 1,
                   size = 4,
                   colour = "black") +
        callModule(plot_theme) +
        ggtitle(paste(toupper(input$gene_id), input$cluster, sep = '-Cluster')) +
        scale_colour_gradient2(low = 'grey50', high = "red")
      p1
    })
  })
  # EXPLORE TAB VIOLIN PLOT ------------------------------------------------------------------

  output$gene_vio_plot <- renderPlot({
    if (v$doPlot == FALSE)
      return()

    isolate({
      callModule(subsetdata, dataTables = dataTables, tsne = TRUE)

      p1 <-
        ggplot(tsne.data, aes(factor(dbCluster), values, fill = factor(dbCluster))) +
        geom_violin(scale = "width") +
        stat_summary(
          fun.y = median,
          geom = "point",
          size = 5,
          color = 'black'
        ) +
        stat_summary(fun.data = n_fun, geom = "text") +
	callModule(plot_theme) +        
        xlab('Cluster') +
        ylab('Expression') +
        ggtitle(toupper(input$gene_id))
      p1
    })
  })
  # EXPLORE TABL DOWNLOAD SELECTED WITH BRUSH ------------------------------------------------------------------

  output$downloadExpression <- downloadHandler(
    filename = function() {
      paste(input$cluster, "Selected_Expression_table.csv", sep = '_')
    },
    content = function(file) {
      callModule(subsetdata, dataTables = dataTables, tsne = FALSE)

      cells.names <- brushedPoints(subsetData, input$b1, allRows = T)
      cells <-
        rownames(subsetData[which(cells.names$selected_ == TRUE), ])

      if (length(cells) == 1) {
        subsetExpression <- dataTables$log2cpm[, cells]
        subsetExpression <-
          as.data.frame(subsetExpression, row.names = rownames(dataTables$log2cpm))
        colnames(subsetExpression) <- cells
        subsetExpression$Associated.Gene.Name <-
          dataTables$featuredata[rownames(subsetExpression), 'Associated.Gene.Name']
        write.csv(subsetExpression, file)
      }
      else{
        subsetExpression <- dataTables$log2cpm[, cells]

        subsetExpression$Associated.Gene.Name <-
          dataTables$featuredata[rownames(subsetExpression), 'Associated.Gene.Name']
        write.csv(subsetExpression, file)
      }
    }
  )
  # EXPLORE TAB PANEL PLOT------------------------------------------------------------------
  vvvvvvvv <- reactiveValues(doPlot = FALSE)
  observeEvent(input$goButton8, {
    vvvvvvvv$doPlot <- input$goButton8
  })


  output$panelPlot <- renderPlot({
    if (vvvvvvvv$doPlot == FALSE)
      return()

    isolate({
      callModule(get_genesin, 
                 data = panelplotids,
                 data_name = "panelplotids" )

      cat(file=stderr(),length(genesin))
      par(mfrow=c(ceiling(length(genesin)/4),4), mai = c(0, 0., 0., 0.))
      rbPal <- colorRampPalette(c('#f0f0f0','red'))
      cat(file=stderr(),input$clusters4)

      if (input$clusters4 == 'All')
        {
        for (i in 1:length(genesin)){
          Col <- rbPal(10)[
            as.numeric(
              cut(
                as.numeric(
                  dataTables$log2cpm[
                    rownames(dataTables$featuredata[which(dataTables$featuredata$Associated.Gene.Name==genesin[i]),])
                    ,]
                ),breaks = 10))]
          plot(dataTables$tsne.data[,input$dimension_x4],dataTables$tsne.data[,input$dimension_y4],col=Col,pch=16,axes = FALSE,frame.plot = TRUE, ann=FALSE)
          title(genesin[i],line=-1.2,adj = 0.05,cex.main=2)
          cat(file=stderr(),genesin[i])
        }
      }
        else{
          for (i in 1:length(genesin)){

            subsetTSNE <- subset(dataTables$tsne.data, dbCluster == input$clusters4)

            Col <- rbPal(10)[
              as.numeric(
                cut(
                  as.numeric(
                    dataTables$log2cpm[
                      rownames(dataTables$featuredata[which(dataTables$featuredata$Associated.Gene.Name==genesin[i]),])
                      ,]
                  ),breaks = 10))]

            names(Col)<-rownames(dataTables$tsne.data)
            plotCol<-Col[rownames(subsetTSNE)]
            plot(subsetTSNE[,input$dimension_x4],subsetTSNE[,input$dimension_y4],col=plotCol,pch=16,axes = FALSE,frame.plot = TRUE, ann=FALSE)
            title(genesin[i],line=-1.2,adj = 0.05,cex.main=2)
            cat(file=stderr(),input$clusters4)
        }
      }
    })
  })
  # CO-EXPRESSION HEATMAP ALL CLUSTERS ------------------------------------------------------------------
  vv <- reactiveValues(doPlot = FALSE)
  observeEvent(input$goButton1, {
    vv$doPlot <- input$goButton1
  })


  output$heatmap <- renderPlot({
    if (vv$doPlot == FALSE)
      return()

    isolate({
      callModule(get_genesin, 
                 data = heatmap_geneids,
                 data_name = "heatmap_geneids")

      map <- rownames(dataTables$featuredata[which(dataTables$featuredata$Associated.Gene.Name %in% genesin[[1]]), ])
      cat(file = stderr(), length(map))

      expression <- dataTables$log2cpm[map, ]

      validate(need(
        is.na(sum(expression)) != TRUE,
        'Gene symbol incorrect or genes not expressed'
      ))

      tsne.data <- dataTables$tsne.data
      tsne.data <- tsne.data[order(tsne.data$dbCluster), ]

      expression <- expression[, rownames(tsne.data)]
      expression <- expression[complete.cases(expression), ]

      annotation <- data.frame(factor(tsne.data$dbCluster))
      rownames(annotation) <- colnames(expression)
      colnames(annotation) <- c('Cluster')
        
      callModule(heat_map, data = expression, cluster_cols = FALSE, annotation_col = annotation)

  # CO EXPRESSION TAB CLUSTER PLOT ------------------------------------------------------------------

  vvvvv <- reactiveValues(doPlot = FALSE)
  observeEvent(input$goButton5, {
    vvvvv$doPlot <- input$goButton5
  })

  output$clusterPlot2 <- renderPlot({
    if (vvvvv$doPlot == FALSE)
      return()

    isolate({
      callModule(subsetdata, dataTables = dataTables, tsne = FALSE)

      p1 <-
        ggplot(subsetData,
               aes_string(x = input$dimension_x2, y = input$dimension_y2)) +
        geom_point(aes_string(size = 2, color = 'values')) +
        geom_point(shape = 1,
                   size = 4,
                   colour = "black") +
	callModule(plot_theme) +        
        ggtitle(paste(toupper(input$gene_id_sch), input$clusters2, sep =
                        '-Cluster')) +
        scale_colour_gradient2(low = 'grey50', high = "red")
      p1
    })
  })

  # CO EXPRESSION TAB SELECTED HEATMAP ------------------------------------------------------------------
  vvvvvv <- reactiveValues(doPlot = FALSE)
  observeEvent(input$goButton6, {
    vvvvvv$doPlot <- input$goButton6
  })

  output$selectedHeatmap <- renderPlot({
    if (vvvvvv$doPlot == FALSE)
      return()

    isolate({
      callModule(get_genesin,
                 data = heatmap_geneids2,
                 data_name = "heatmap_geneids2")

      subsetData <-
        subset(dataTables$tsne.data, dbCluster == input$clusters2)
      cells.1 <- rownames(brushedPoints(subsetData, input$scb1))

      map <- rownames(dataTables$featuredata[which(dataTables$featuredata$Associated.Gene.Name %in% genesin[[1]]), ])

      expression <- dataTables$log2cpm[map, cells.1]
      cat(file = stderr(), rownames(expression))

      expression <- expression[complete.cases(expression), ]
      cat(file = stderr(), rownames(expression))
      mColor <- max(expression)

      validate(need(
        is.na(sum(expression)) != TRUE,
        'Gene symbol incorrect or genes not expressed'
      ))
      
      callModule(heat_map, data = expression, cluster_cols = TRUE)
    })
  })

  # CO EXPRESSION TAB ON/OFF PLOT ------------------------------------------------------------------

  vvvvvvv <- reactiveValues(doPlot = FALSE)
  observeEvent(input$goButton7, {
    vvvvvvv$doPlot <- input$goButton7
  })

  output$plotCoExpression <- renderPlot({
    if (vvvvvvv$doPlot == FALSE)
      return()

    isolate({
      callModule(get_genesin, 
                 data = mclustids, 
                 data_name = "mclustids")

      subsetData <-
        subset(dataTables$tsne.data, dbCluster == input$clusters3)
      cells.1 <- rownames(subsetData)

      map <- rownames(dataTables$featuredata[which(dataTables$featuredata$Associated.Gene.Name %in% genesin[[1]]), ])

      expression <- dataTables$log2cpm[map, ]

      validate(need(
        is.na(sum(expression)) != TRUE,
        'Gene symbol incorrect or genes not expressed'
      ))

      bin <- expression
      bin[] <- 0

      for (i in 1:nrow(expression))
      {
        x <- Mclust(expression[i, ], G = 2)
        bin[i, ] <- x$classification
      }
      bin <- bin - 1
      allexprs <- apply(bin, 2, sum)
      plotexprs <- allexprs
      plotexprs[] <- 0
      plotexprs[allexprs >= length(rownames(bin))] <- 1
      dataTables$positiveCells <- allexprs >= length(rownames(bin))
      dataTables$positiveCellsAll <- plotexprs

      mergeExprs <- plotexprs[rownames(subsetData)]

      subsetData$CoExpression <- mergeExprs

      p1 <-
        ggplot(subsetData,
               aes_string(x = input$dimension_x3, y = input$dimension_y3)) +
        geom_point(aes_string(size = 2, color = 'CoExpression')) +
        geom_point(shape = 1,
                   size = 4,
                   colour = "black") +
        callModule(plot_theme) +
        scale_colour_gradient2(low = 'grey50', high = "red")
      p1
    })
  })

  # ONOFF TAB DOWNLOAD POSITIVECELLS ------------------------------------------------------------------

  output$downloadExpressionOnOff <- downloadHandler(
    filename = function() {
      paste(input$clusters3, "PositiveCells.csv", sep = '_')
    },
    content = function(file) {
      cells <- dataTables$positiveCells

      if (length(cells) == 1) {
        subsetExpression <- dataTables$log2cpm[, cells]
        subsetExpression <-
          as.data.frame(subsetExpression, row.names = rownames(dataTables$log2cpm))
        colnames(subsetExpression) <- cells
        subsetExpression$Associated.Gene.Name <-
          dataTables$featuredata[rownames(subsetExpression), 'Associated.Gene.Name']
        write.csv(subsetExpression, file)
      }
      else{
        subsetExpression <- dataTables$log2cpm[, cells]
        subsetExpression$Associated.Gene.Name <-
          dataTables$featuredata[rownames(subsetExpression), 'Associated.Gene.Name']
        write.csv(subsetExpression, file)
      }
    }
  )

  # ONOFF TAB RENDER TABLE ALL CELLS ------------------------------------------------------------------

  output$onOffTable <- DT::renderDataTable({
    if (vvvvvvv$doPlot == FALSE)
      return()

    isolate({
      merge <- dataTables$tsne.data
      merge$CoExpression <- dataTables$positiveCellsAll
      df <-
        as.data.frame(table(merge[, c('dbCluster', 'CoExpression')]))
      dfOut <- cast(df, dbCluster ~ CoExpression)
      colnames(dfOut) <- c("Cluster", 'OFF', 'ON')
      rownames(dfOut) <- dfOut$Cluster
      dfOut['Sum', ] <- c('', sum(dfOut$OFF), sum(dfOut$ON))
      DT::datatable(dfOut)

    })
  })

  # SUBCLUSTER DGE PLOT1 ------------------------------------------------------------------

  vvv <- reactiveValues(doPlot = FALSE)
  observeEvent(input$goButton2, {
    vvv$doPlot <- input$goButton2
  })

  output$dge_plot1 <- renderPlot({
    if (vvv$doPlot == FALSE)
      return()

    isolate({
      tsne.data <- dataTables$tsne.data

      subsetData <- subset(tsne.data, dbCluster == input$clusters1)
      
      callModule(tsne_2d_plot, data = subsetData)     
    })
  })

  # SUBCLUSTER DGE PLOT2 ------------------------------------------------------------------

  output$dge_plot2 <- renderPlot({
    if (vvv$doPlot == FALSE)
      return()

    isolate({
      tsne.data <- dataTables$tsne.data

      subsetData <- subset(tsne.data, dbCluster == input$clusters1)
    
      callModule(tsne_2d_plot, data = subsetData)
    })
  })

  # SUBCLUSTER DGE ANALYSIS ------------------------------------------------------------------

  vvvv <- reactiveValues(doPlot = FALSE)
  observeEvent(input$goButton3, {
    vvvv$doPlot <- input$goButton3
  })

  dge <- reactive({
    if (vvvv$doPlot == FALSE)
      return()

    isolate({
      subsetData <- subset(dataTables$tsne.data, dbCluster == input$clusters1)
      cells.1 <- rownames(brushedPoints(subsetData, input$db1))

      cat(file = stderr(), cells.1[1:5])

      cells.2 <- rownames(brushedPoints(subsetData, input$db2))
      cat(file = stderr(), cells.2[1:5])

      subsetExpression <- dataTables$log2cpm[, union(cells.1, cells.2)]

      genes.use <- rownames(subsetExpression)
      data.1 = apply(subsetExpression[genes.use, cells.1], 1, expMean)
      data.2 = apply(subsetExpression[genes.use, cells.2], 1, expMean)
      total.diff = (data.1 - data.2)

      genes.diff = names(which(abs(total.diff) > .2))
      genes.use = ainb(genes.use, genes.diff)

      toReturn <-
        DiffExpTest(subsetExpression, cells.1, cells.2, genes.use = genes.use)
      toReturn[, "avg_diff"] = total.diff[rownames(toReturn)]
      toReturn$Associated.Gene.Name <-
        dataTables$featuredata[rownames(toReturn), 'Associated.Gene.Name']
      dataTables$selectedDge <- toReturn
      return(toReturn)
      cat(stderr(), rownames(toReturn)[1:5])

    })
  })

  # SUBCLUSTER DGE OUTPUT TABLE ------------------------------------------------------------------

  output$dge <- DT::renderDataTable({
    if (vvvv$doPlot == FALSE)
      return()

    isolate({
      top.genes <- dge()
      top.genes$Associated.Gene.Name <-
        dataTables$featuredata[rownames(top.genes), 'Associated.Gene.Name']
      if (dim(top.genes)[1] > 1) {
        DT::datatable(top.genes,
                      options = list(
                        orderClasses = TRUE,
                        lengthMenu = c(10, 30, 50),
                        pageLength = 10
                      ))
      }
    })
  })

  # SUBCLUSTER DGE DOWNLOADS ------------------------------------------------------------------

  output$download_dge_table <- downloadHandler(
    filename = function() {
      paste("SubCluster", "DGE_table.csv", sep = '_')
    },
    content = function(file) {
      write.csv(dataTables$selectedDge, file)
    }
  )
})
