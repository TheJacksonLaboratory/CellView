## 2D tsne plot 
tsne_2D_plot <- function(input, output, session){
  ggplot(subsetData,
    aes_string(x = input$dimension_x1, y = input$dimension_y1)) +
  geom_point() +
  geom_point(shape = 1,
    size = 4,
    color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 12, 
    vjust = 0.5),
    axis.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 14),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    legend.position = "none") +
  ggtitle(input$clusters1)
}

## plot theme -----------------------
plot_theme <- function(input, output, session(){
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      size = 12,
      vjust = 0.5
      ),
    axis.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 14),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    legend.position = "none"
    )
} 
