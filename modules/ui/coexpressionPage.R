coexpressionPageUI <- function(id) {
  ns <- NS(id)

  navbarMenu(
    "Co-expression",
    allClustersPanelUI(ns("allClusters")),
    selectionCellsPanelUI(ns("selectedCells")),
    binarizePanelUI(ns("binarize"))
  )
}

allClustersPanelUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "All clusters",

    fluidRow(
      column(
        2,
        tagAppendAttributes(
          textInput(
            ns('heatmap_geneids'),
            'Comma seperated gene names',
            value = 'Il6,Cd3d,Genes,Umi'
          ),
          `data-proxy-click` = ns("goButton1")
        ),
        actionEnterButton(ns('goButton1'), 'Run')
      )
    ),

    fluidRow(
      column(
        10,
        offset = 1,
        plotOutput(ns('heatmap'))
      )
    )
  )
}

selectionCellsPanelUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Selected cells",
    tags$ul(
      tags$li(
        strong('Subclustering'),
        ':Select a group of cells in plot1 based based on a single gene expression. Enter multiple gene ids to assess the co-expression of genes in these cells'
      )
    ),

    fluidRow(
      column(
        2,
        tagAppendAttributes(
          textInput(ns('gene_id_sch'), 'Enter gene', value = 'Il6'),
          `data-proxy-click` = ns("goButton5")
        ),
        actionEnterButton(ns('goButton5'), 'Run')
      ),

      column(
        2,
        uiOutput(ns("clusters2"))
      ),

      dimensionSelection(ns('dimension_x2'), 'X', default='V1'),
      dimensionSelection(ns('dimension_y2'), 'Y', default='V2')
    ),

    fluidRow(
      column(
        5,
        offset = 1,
        plotOutput(
          ns('clusterPlot2'),
          brush = brushOpts(id = ns('scb1'))
        )
      )
    ),

    fluidRow(
      column(
        2,
        tagAppendAttributes(
          textInput(ns('heatmap_geneids2'), 'Comma seperated gene names', value = 'Il6,Cd3d'),
          `data-proxy-click` = ns("goButton6")
        ),
        actionEnterButton(ns('goButton6'), 'Run')
      )
    ),

    fluidRow(
      column(
        10,
        offset = 1,
        plotOutput(ns('selectedHeatmap'))
      )
    )
  )
}


binarizePanelUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Binarize",
    tags$ul(
      tags$li(
        strong('Binary Expression'),
        ':Select a cluster. Enter',
        strong('ONE'),
        'or',
        strong('MULTIPLE'),
        'gene ids to assess the co-expression of genes in these cells. Highlighted cells have all genes expressed as determined by a GMM'
      )
    ),

    fluidRow(
      column(
        2,
        uiOutput(ns("clusters3"))
      ),

      dimensionSelection(ns('dimension_x3'), 'X', default='V1'),
      dimensionSelection(ns('dimension_y3'), 'Y', default='V2'),

      column(
        2,
        tagAppendAttributes(
          textInput(
            ns('mclustids'),
            'Comma seperated gene names',
            value = 'Il6'
          ),
          `data-proxy-click` = ns("goButton7")
        ),
        actionEnterButton(ns('goButton7'), 'Run')
      )
    ),

    fluidRow(
      column(
        10,
        offset = 1,
        plotOutput(ns('plotCoExpression'))
      )
    ),

    fluidRow(
      div(
        align = "center",
        style = "margin-center:50px; margin-top:25px",
        downloadButton(
          ns("downloadExpressionOnOff"),
          "Download Expression +ve Cells in cluster"
        )
      )
    ),

    br(), br(), br(),

    fluidRow(
      h4(
        'Positive Cells in all clusters',
        align = "center"
      ),
      column(
        6,
        offset = 3,
        DT::dataTableOutput(ns('onOffTable'))
      )
    )
  )
}
