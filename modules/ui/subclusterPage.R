subclusterPageUI <- function(id) {
  ns <- NS(id)

  navbarMenu(
    id,

    dgeAnalysisTabUI("DGE Analysis")
  )
}

dgeAnalysisTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    id,
    tags$ul(
      tags$li(
        strong('Subclustering'),
        ':Select a group of cells in plot1 and a different group of cells in plot2 for identifying differential features between these subclusters'
      )
    ),

    fluidRow(
      column(
        2,
        uiOutput("clusters1")
      ),

      dimensionSelection('dimension_x1', 'X', default='V1'),
      dimensionSelection('dimension_y1', 'Y', default='V2'),

      column(
        2,
        actionEnterButton('goButton2', 'Plot')
      )
    ),

    fluidRow(
      column(
        4,
        plotOutput(
          'dge_plot1',
          brush = brushOpts(id = "db1")
        )
      ),

      column(
        4,
        plotOutput(
          'dge_plot2',
          brush = brushOpts(id = 'db2')
        )
      ),

      column(
        2,
        actionEnterButton(
          'goButton3',
          'Differential'
        )
      )
    ),

    fluidRow(
      h4('Top Differentially Expressed Genes', offset = 1),
      DT::dataTableOutput('dge')
    ),

    fluidRow(
      div(
        align = "right",
        style = "margin-right:15px; margin-bottom:10px",
        downloadButton("download_dge_table", "Download DGE Table")
      )
    )
  )
}
