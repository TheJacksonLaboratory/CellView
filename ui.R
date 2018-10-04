shinyUI(
  navbarPage(
    theme = shinytheme("flatly"),
    a('SingleCellBiology',
      href = 'https://www.jax.org/research-and-faculty/tools/scientific-research-services/genome-tech-single-cell-biology/single-cell-biology',
      target = '_blank'),
    windowTitle = 'Single Cell Biology',

    mainPageUI("overview"),
    explorePageUI("explore"),
    coexpressionPageUI("coexpression"),
    subclusterPageUI("subclusterAnalysis")
  )
)

