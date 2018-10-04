subclusterPageUI <- function(id) {
  ns <- NS(id)

  navbarMenu(
    "Subcluster analysis",

    dgeAnalysisTabUI(ns("dgeAnalysis"))
  )
}

dgeAnalysisTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "DGE analysis",
    tags$ul(
      tags$li(
        strong('Subclustering'),
        ':Select a group of cells in plot1 and a different group of cells in plot2 for identifying differential features between these subclusters'
      )
    ),

    fluidRow(
      column(
        2,
        uiOutput(ns("clusters1"))
      ),

      dimensionSelection(ns('dimension_x1'), 'X', default='V1'),
      dimensionSelection(ns('dimension_y1'), 'Y', default='V2'),

      column(
        2,
        actionEnterButton(ns('goButton2'), 'Plot')
      )
    ),

    fluidRow(
      column(
        4,
        plotOutput(
          ns('dge_plot1'),
          brush = brushOpts(id = ns("db1"))
        )
      ),

      column(
        4,
        plotOutput(
          ns('dge_plot2'),
          brush = brushOpts(id = ns('db2'))
        )
      ),

      column(
        2,
        actionEnterButton(
          ns('goButton3'),
          'Differential'
        )
      )
    ),

    fluidRow(
      h4('Top Differentially Expressed Genes', offset = 1),
      DT::dataTableOutput(ns('dge'))
    ),

    fluidRow(
      div(
        align = "right",
        style = "margin-right:15px; margin-bottom:10px",
        downloadButton(ns("download_dge_table"), "Download DGE Table")
      )
    )
  )
}
