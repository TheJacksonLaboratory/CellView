mainPageUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    id,
    fluidRow(
      div(
        h3("CellView"),
        align = "center"
      )
    ),
    br(),
    fluidRow(
      div(
        h5(
          "This app is designed for exploratory data analysis of
          processed RNA-Seq data of single cell experiments."
        ),
        align = "center"
      )
    ),
    br(),
    br(),
    fluidRow(
      column(
        5,
        offset = 4,
        fileInput(
          ns("file1"),
          "Choose .Rds file to upload",
          accept = c(
            ".Rds",
            "text/comma-separated-values",
            "text/tab-separated-values",
            "text/plain",
            ".csv",
            ".tsv"
          )
        )
      )
    ),

    fluidRow(
      column(
        5,
        offset="4",
        htmlOutput(ns("summaryStats"))
      )
    ),
    br(),

    fluidRow(
      column(
        5,
        offset = "3",
        plotlyOutput(ns("tsne_main"))
      )
    )
  )
}
