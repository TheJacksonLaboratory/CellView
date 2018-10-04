explorePageUI <- function(id) {
  ns <- NS(id)

  navbarMenu(
    ns(id),
    expressionTabUI(ns("Expression")),
    panelTabUI(ns("Panel plot"))
  )
}

expressionTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    ns(id),
    fluidRow(
      div(
        p(strong('\tInformation:')),
        tags$ul(
          tags$li(
            strong('Clustering'),
            ':Clustering was performed with t-SNE followed by identification using DBSCAN'
          ),
          tags$li(
            strong('Cluster 0'),
            ':Cells that cannot be assigned to any cluster'
          ),
          tags$li(
            strong('3D Plot'),
            ':Enter gene name to visualize expression in a single cell'
          ),
          tags$li(
            strong('2D Plot'),
            ':Pick a cluster, highlight cells of interest to download gene expression matrix'
          )
        )
      )
    ),

    br(),br(),

    fluidRow(
      column(
        2,
        tagAppendAttributes(
          textInput(ns('gene_id'), 'Enter gene', value = 'Il6'),
          `data-proxy-click` = ns("goButton")
        ),
        actionEnterButton(ns('goButton'), 'Run')
      ),

      column(
        2,
        uiOutput(ns("clusters"))
      ),

      dimensionSelection(ns('dimension_x'), 'X', default='V1'),
      dimensionSelection(ns('dimension_y'), 'Y', default='V2'),

      column(
        2,
        div(
          align = "center",
          style = "margin-center:50px; margin-top:25px",
          downloadButton(ns("downloadExpression"), "Download Expression")
        )
      )
    ),

    br(),br(),br(),

    fluidRow(
      column(
        5,
        offset = 1,
        plotlyOutput(ns('tsne_plt'))
      ),

      column(
        5,
        offset = 0,
        plotOutput(ns('clusterPlot'), brush = brushOpts(id = ns('b1')))
      )
    ),

    fluidRow(
      column(
        10,
        offset = 1,
        plotOutput(ns('gene_vio_plot'))
      )
    )
  )
}

panelTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    'Panel plot',
    tags$ul(
      tags$li(
        strong('Panel plot'),
        ':Select a cluster. Enter',
        strong('ONE'),
        'or',
        strong('MULTIPLE'),
        'gene ids to visualize expression in all clusters'
      )
    ),

    fluidRow(
      column(
        2,
        uiOutput(ns("clusters4"))
      ),

      dimensionSelection(ns('dimension_x4'), 'X', default='V1'),
      dimensionSelection(ns('dimension_y4'), 'Y', default='V2'),

      column(
        2,
        tagAppendAttributes(
          textInput(ns('panelplotids'), 'Comma seperated gene names', value = 'Il6'),
          `data-proxy-click` = ns("goButton8")
        ),
        actionEnterButton(ns('goButton8'), 'Run')
      )
    ),

    fluidRow(
      column(
        10,
        offset = 1,
        plotOutput(ns('panelPlot'))
      )
    )
  )
}
