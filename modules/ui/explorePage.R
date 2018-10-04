explorePageUI <- function(id) {
  ns <- NS(id)

  navbarMenu(
    id,
    expressionTabUI("Expression"),
    panelTabUI("Panel plot")
  )
}

expressionTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    id,
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
    br(),
    br(),
    fluidRow(
      column(
        2,
        tagAppendAttributes(
          textInput('gene_id', 'Enter gene', value = 'Il6'),
          `data-proxy-click` = "goButton"
        ),
        actionEnterButton('goButton', 'Run')
      ),
      column(
        2,
        uiOutput("clusters")
      ),
      column(
        2,
        selectInput(
          'dimension_x',
          label = 'X',
          choice = c('V1', 'V2', 'V3'),
          selected = 'V1'
        )
      ),
      column(
        2,
        selectInput(
          'dimension_y',
          label = 'Y',
          choice = c('V1', 'V2', 'V3'),
          selected = 'V2'
        )
      ),
      column(
        2,
        div(
          align = "center",
          style = "margin-center:50px; margin-top:25px",
          downloadButton("downloadExpression", "Download Expression")
        )
      )
    ),
    br(),
    br(),
    br(),
    fluidRow(
      column(
        5,
        offset = 1,
        plotlyOutput('tsne_plt')
      ),
      column(
        5,
        offset = 0,
        plotOutput('clusterPlot', brush = brushOpts(id = 'b1'))
      )
    ),
    fluidRow(
      column(
        10,
        offset = 1,
        plotOutput('gene_vio_plot')
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
        uiOutput("clusters4")
      ),

      dimensionSelection('dimension_x4', 'X', default='V1'),

      dimensionSelection('dimension_y4', 'Y', default='V2'),

      column(
        2,
        tagAppendAttributes(
          textInput('panelplotids', 'Comma seperated gene names', value = 'Il6'),
          `data-proxy-click` = "goButton8"
        ),
        actionEnterButton('goButton8', 'Run')
      )
    ),

    fluidRow(
      column(
        10,
        offset = 1,
        plotOutput('panelPlot')
      )
    )
  )
}
