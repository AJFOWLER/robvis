library(tidymeta)
library(ggplot2)
library(dplyr)
library(broom)
library(metafor)
library(patchwork)
library(grid)


rob_forest <- function(meta, rob.data) {


  if (class(rob.data) != "data.frame"){
    dataframe.indcator <- TRUE
  }



}

meta4 <- iud_cxca %>%
  meta_analysis(yi = lnes, sei = selnes, slab = study_name)


meta4 <- meta4 %>%
  dplyr::mutate(studycol := lock_order(study))


dot.intercpt <- meta4$estimate[which(meta4$study=="Overall")]


(p1 <- meta4 %>%
  ggplot2::ggplot(ggplot2::aes(x = estimate, y = studycol),
                               shape = 1, col = "black") +

  ggplot2::geom_vline(xintercept = 0) +

  ggplot2::geom_vline(xintercept = dot.intercpt,linetype = "dashed") +

  ggplot2::geom_point() +

  ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low,
                                       xmax = conf.high)) +

  theme_forest())







df <- as.data.frame(meta4$study, stringsAsFactors = FALSE)

colnames(df)[1] <- "Study"

df$R <- rep("h",17)
df$R[1] <- "l"
df$D <- rep("h",17)
df$Mi <- rep("h",17)
df$Me <- rep("h",17)
df$S <- rep("h",17)
df$O <- rep("h",17)



rob.tidy <- suppressWarnings(tidyr::gather(df,
                                           domain, judgement, -Study))


low_colour <- "#fed98e"
concerns_colour <- "#fe9929"
high_colour <- "#d95f0e"
critical_colour <- "#993404"
ni_colour <- "#ffffff"

p4 <- rob.tidy %>%
  ggplot2::ggplot(ggplot2::aes(y = Study, x = domain, col = judgement), shape = 1) +
  ggplot2::geom_point(size = 6) +
  ggplot2::geom_point(ggplot2::aes(shape = judgement), col = "black") +
  scale_x_discrete(position = "top") +
  ggplot2::scale_shape_manual(values = c(h = 120,
                                           s = 45, l = 43, n = 63), labels = c(h = "High", s = "Some concerns",
                                                                               l = "Low", n= "No information")) +
  ggplot2::scale_colour_manual(values = c(h = high_colour,
                                          s = concerns_colour, l = low_colour, n= ni_colour)) +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.y = ggplot2::element_text(angle = 180),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        strip.background = ggplot2::element_rect(color = "grey86", fill = "grey92"),
        axis.text.x = ggplot2::element_text(size = 12),
        axis.title.y = ggplot2::element_blank(),
        legend.position = "none")

meat3 <- df[,c(1,7)]

rob.tidy3 <- suppressWarnings(tidyr::gather(meat3,
                                           domain, judgement, -Study))

p3 <- rob.tidy3 %>%
  ggplot2::ggplot(ggplot2::aes(y = Study, x = domain, col = judgement), shape = 1) +
  ggplot2::geom_point(size = 6) +
  ggplot2::geom_point(ggplot2::aes(shape = judgement), col = "black") +
  scale_x_discrete(position = "top") +
  ggplot2::scale_shape_manual(values = c(h = 120,
                                         s = 45, l = 43, n = 63), labels = c(h = "High", s = "Some concerns",
                                                                             l = "Low", n= "No information")) +
  ggplot2::scale_colour_manual(values = c(h = high_colour,
                                          s = concerns_colour, l = low_colour, n= ni_colour)) +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.y = ggplot2::element_text(angle = 180),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        strip.background = ggplot2::element_rect(color = "grey86", fill = "grey92"),
        axis.text.x = ggplot2::element_text(size = 12),
        axis.title.y = ggplot2::element_blank(),
        legend.position = "none")

aligned_plots <- align_plots(p1,p4,p3)

p1 + p4 + p3 + plot_layout(widths = c(2,1,0.2)) + plot_annotation(
  title = "patchwork",
  caption = "Risk of Bias Domains:
D1: Bias due to randomisation.
D2: Bias due to deviations from intended intervention.
D3: Bias due to missing data.
D4: Bias due to outcome measurement.
D5: Bias due to selection of reported result.",
  theme = theme(
    plot.title = element_text(size = 10),

    plot.caption = element_text(size = 10,hjust = 0, vjust = 1),

  )
)







