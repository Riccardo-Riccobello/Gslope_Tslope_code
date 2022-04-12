#####BoxPlots in a single plots for combinations of MR and N - First Design#####

tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 70), rep("MR = 1.43", 70)),
  "N"  = rep("N = 250", 140),
  rbind(tstudent_250_100_MR0_cl$data_F1, tstudent_250_100_MR143_cl$data_F1)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 60), rep("MR = 1.43", 60)),
  "N"  = rep("N = 50", 120),
  rbind(tstudent_50_100_MR0_cl$data_F1, tstudent_50_100_MR143_cl$data_F1)
)

data_tstudent = rbind(tstudent_cl_250, tstudent_cl_50)

F1_plot_tstudent <- data_tstudent %>%
  ggplot(aes(
    x = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.45) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  #scale_y_log10 +
  theme(legend.position = "none",
        plot.title = element_text(size = 10)) +
  ylim(c(0, 1)) +
  ggtitle("Cluster - T-Student") +
  xlab("Methods") +
  ylab("F1 score")

F1_plot_tstudent + facet_grid(rows = vars(MR), cols = vars(N))

####BoxPlots in a single plots for combinations of MR and N - Second Design#####
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 70), rep("MR = 1.43", 70)),
  "N"  = rep(250, 140),
  rbind(tstudent_250_100_MR0_cl$data_F1, tstudent_250_100_MR143_cl$data_F1)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 60), rep("MR = 1.43", 60)),
  "N"  = rep(50, 120),
  rbind(tstudent_50_100_MR0_cl$data_F1, tstudent_50_100_MR143_cl$data_F1)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 70), rep("MR = 1.43", 70)),
  "N"  = rep(250, 140),
  rbind(normal_250_100_MR0_cl$data_F1, normal_250_100_MR143_cl$data_F1)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 60), rep("MR = 1.43", 60)),
  "N"  = rep(50, 120),
  rbind(normal_50_100_MR0_cl$data_F1, normal_50_100_MR143_cl$data_F1)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 70), rep("MR = 1.43", 70)),
  "N"  = rep(250, 140),
  rbind(mix_250_100_MR0_cl$data_F1, mix_250_100_MR143_cl$data_F1)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 60), rep("MR = 1.43", 60)),
  "N"  = rep(50, 120),
  rbind(mix_50_100_MR0_cl$data_F1, mix_50_100_MR143_cl$data_F1)
)

data_tstudent = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal = rbind(normal_cl_250, normal_cl_50)
data_mix = rbind(mix_cl_250, mix_cl_50)

#Tstudent#
F1_plot_tstudent <- data_tstudent %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Cluster - T-Student",
       x = "Sample Size",
       y = "F1 score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50"))

F1_plot_tstudent + facet_grid(cols = vars(MR))

#Normal#
F1_plot_normal <- data_normal %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Cluster - Normal",
       x = "Sample Size",
       y = "F1 score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("50", "250"))

F1_plot_normal + facet_grid(cols = vars(MR))

#Mix#
F1_plot_mix <- data_mix %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Cluster - Mix",
       x = "Sample Size",
       y = "F1 score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("50", "250"))

F1_plot_mix + facet_grid(cols = vars(MR))


####BoxPlots in a single plots for combinations of MR and N - Third Design######

#####################################oracle#########################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_cl$data_oracle, tstudent_250_100_MR143_cl$data_oracle)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_cl$data_oracle, tstudent_50_100_MR143_cl$data_oracle)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_cl$data_oracle, normal_250_100_MR143_cl$data_oracle)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_cl$data_oracle, normal_50_100_MR143_cl$data_oracle)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_cl$data_oracle, mix_250_100_MR143_cl$data_oracle)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_cl$data_oracle, mix_50_100_MR143_cl$data_oracle)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

oracle_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "oracle score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

oracle_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("oracle_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_ra$data_oracle, tstudent_250_100_MR143_ra$data_oracle)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_ra$data_oracle, tstudent_50_100_MR143_ra$data_oracle)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_ra$data_oracle, normal_250_100_MR143_ra$data_oracle)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_ra$data_oracle, normal_50_100_MR143_ra$data_oracle)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_ra$data_oracle, mix_250_100_MR143_ra$data_oracle)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_ra$data_oracle, mix_50_100_MR143_ra$data_oracle)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

oracle_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Random",
       x = "Sample Size",
       y = "oracle score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

oracle_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("oracle_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_hub$data_oracle, tstudent_250_100_MR143_hub$data_oracle)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_hub$data_oracle, tstudent_50_100_MR143_hub$data_oracle)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_hub$data_oracle, normal_250_100_MR143_hub$data_oracle)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_hub$data_oracle, normal_50_100_MR143_hub$data_oracle)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_hub$data_oracle, mix_250_100_MR143_hub$data_oracle)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_hub$data_oracle, mix_50_100_MR143_hub$data_oracle)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

oracle_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "oracle score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

oracle_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("oracle_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_band$data_oracle, tstudent_250_100_MR143_band$data_oracle)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_band$data_oracle, tstudent_50_100_MR143_band$data_oracle)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_band$data_oracle, normal_250_100_MR143_band$data_oracle)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_band$data_oracle, normal_50_100_MR143_band$data_oracle)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_band$data_oracle, mix_250_100_MR143_band$data_oracle)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_band$data_oracle, mix_50_100_MR143_band$data_oracle)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

oracle_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Band",
       x = "Sample Size",
       y = "oracle score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

oracle_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("oracle_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_sf$data_oracle, tstudent_250_100_MR143_sf$data_oracle)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_sf$data_oracle, tstudent_50_100_MR143_sf$data_oracle)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_sf$data_oracle, normal_250_100_MR143_sf$data_oracle)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_sf$data_oracle, normal_50_100_MR143_sf$data_oracle)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_sf$data_oracle, mix_250_100_MR143_sf$data_oracle)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_sf$data_oracle, mix_50_100_MR143_sf$data_oracle)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

oracle_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "oracle score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

oracle_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("oracle_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#####################################empirical#########################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_cl$data_empirical, tstudent_250_100_MR143_cl$data_empirical)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_cl$data_empirical, tstudent_50_100_MR143_cl$data_empirical)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_cl$data_empirical, normal_250_100_MR143_cl$data_empirical)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_cl$data_empirical, normal_50_100_MR143_cl$data_empirical)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_cl$data_empirical, mix_250_100_MR143_cl$data_empirical)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_cl$data_empirical, mix_50_100_MR143_cl$data_empirical)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

empirical_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "empirical score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

empirical_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("empirical_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_ra$data_empirical, tstudent_250_100_MR143_ra$data_empirical)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_ra$data_empirical, tstudent_50_100_MR143_ra$data_empirical)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_ra$data_empirical, normal_250_100_MR143_ra$data_empirical)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_ra$data_empirical, normal_50_100_MR143_ra$data_empirical)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_ra$data_empirical, mix_250_100_MR143_ra$data_empirical)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_ra$data_empirical, mix_50_100_MR143_ra$data_empirical)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

empirical_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Random",
       x = "Sample Size",
       y = "empirical score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

empirical_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("empirical_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_hub$data_empirical, tstudent_250_100_MR143_hub$data_empirical)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_hub$data_empirical, tstudent_50_100_MR143_hub$data_empirical)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_hub$data_empirical, normal_250_100_MR143_hub$data_empirical)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_hub$data_empirical, normal_50_100_MR143_hub$data_empirical)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_hub$data_empirical, mix_250_100_MR143_hub$data_empirical)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_hub$data_empirical, mix_50_100_MR143_hub$data_empirical)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

empirical_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "empirical score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

empirical_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("empirical_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_band$data_empirical, tstudent_250_100_MR143_band$data_empirical)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_band$data_empirical, tstudent_50_100_MR143_band$data_empirical)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_band$data_empirical, normal_250_100_MR143_band$data_empirical)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_band$data_empirical, normal_50_100_MR143_band$data_empirical)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_band$data_empirical, mix_250_100_MR143_band$data_empirical)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_band$data_empirical, mix_50_100_MR143_band$data_empirical)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

empirical_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Band",
       x = "Sample Size",
       y = "empirical score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

empirical_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("empirical_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_sf$data_empirical, tstudent_250_100_MR143_sf$data_empirical)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_sf$data_empirical, tstudent_50_100_MR143_sf$data_empirical)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_sf$data_empirical, normal_250_100_MR143_sf$data_empirical)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_sf$data_empirical, normal_50_100_MR143_sf$data_empirical)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_sf$data_empirical, mix_250_100_MR143_sf$data_empirical)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_sf$data_empirical, mix_50_100_MR143_sf$data_empirical)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

empirical_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "empirical score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

empirical_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("empirical_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#####################################actual#########################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_cl$data_actual, tstudent_250_100_MR143_cl$data_actual)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_cl$data_actual, tstudent_50_100_MR143_cl$data_actual)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_cl$data_actual, normal_250_100_MR143_cl$data_actual)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_cl$data_actual, normal_50_100_MR143_cl$data_actual)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_cl$data_actual, mix_250_100_MR143_cl$data_actual)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_cl$data_actual, mix_50_100_MR143_cl$data_actual)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

actual_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "actual score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

actual_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("actual_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_ra$data_actual, tstudent_250_100_MR143_ra$data_actual)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_ra$data_actual, tstudent_50_100_MR143_ra$data_actual)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_ra$data_actual, normal_250_100_MR143_ra$data_actual)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_ra$data_actual, normal_50_100_MR143_ra$data_actual)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_ra$data_actual, mix_250_100_MR143_ra$data_actual)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_ra$data_actual, mix_50_100_MR143_ra$data_actual)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

actual_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Random",
       x = "Sample Size",
       y = "actual score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

actual_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("actual_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_hub$data_actual, tstudent_250_100_MR143_hub$data_actual)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_hub$data_actual, tstudent_50_100_MR143_hub$data_actual)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_hub$data_actual, normal_250_100_MR143_hub$data_actual)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_hub$data_actual, normal_50_100_MR143_hub$data_actual)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_hub$data_actual, mix_250_100_MR143_hub$data_actual)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_hub$data_actual, mix_50_100_MR143_hub$data_actual)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

actual_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "actual score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

actual_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("actual_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_band$data_actual, tstudent_250_100_MR143_band$data_actual)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_band$data_actual, tstudent_50_100_MR143_band$data_actual)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_band$data_actual, normal_250_100_MR143_band$data_actual)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_band$data_actual, normal_50_100_MR143_band$data_actual)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_band$data_actual, mix_250_100_MR143_band$data_actual)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_band$data_actual, mix_50_100_MR143_band$data_actual)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

actual_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Band",
       x = "Sample Size",
       y = "actual score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

actual_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("actual_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_sf$data_actual, tstudent_250_100_MR143_sf$data_actual)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_sf$data_actual, tstudent_50_100_MR143_sf$data_actual)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_sf$data_actual, normal_250_100_MR143_sf$data_actual)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_sf$data_actual, normal_50_100_MR143_sf$data_actual)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_sf$data_actual, mix_250_100_MR143_sf$data_actual)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_sf$data_actual, mix_50_100_MR143_sf$data_actual)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

actual_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "actual score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

actual_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("actual_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#####################################means#########################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_cl$data_means, tstudent_250_100_MR143_cl$data_means)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_cl$data_means, tstudent_50_100_MR143_cl$data_means)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_cl$data_means, normal_250_100_MR143_cl$data_means)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_cl$data_means, normal_50_100_MR143_cl$data_means)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_cl$data_means, mix_250_100_MR143_cl$data_means)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_cl$data_means, mix_50_100_MR143_cl$data_means)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

means_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "means score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

means_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("means_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_ra$data_means, tstudent_250_100_MR143_ra$data_means)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_ra$data_means, tstudent_50_100_MR143_ra$data_means)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_ra$data_means, normal_250_100_MR143_ra$data_means)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_ra$data_means, normal_50_100_MR143_ra$data_means)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_ra$data_means, mix_250_100_MR143_ra$data_means)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_ra$data_means, mix_50_100_MR143_ra$data_means)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

means_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Random",
       x = "Sample Size",
       y = "means score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

means_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("means_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_hub$data_means, tstudent_250_100_MR143_hub$data_means)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_hub$data_means, tstudent_50_100_MR143_hub$data_means)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_hub$data_means, normal_250_100_MR143_hub$data_means)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_hub$data_means, normal_50_100_MR143_hub$data_means)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_hub$data_means, mix_250_100_MR143_hub$data_means)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_hub$data_means, mix_50_100_MR143_hub$data_means)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

means_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "means score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

means_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("means_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_band$data_means, tstudent_250_100_MR143_band$data_means)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_band$data_means, tstudent_50_100_MR143_band$data_means)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_band$data_means, normal_250_100_MR143_band$data_means)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_band$data_means, normal_50_100_MR143_band$data_means)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_band$data_means, mix_250_100_MR143_band$data_means)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_band$data_means, mix_50_100_MR143_band$data_means)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

means_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Band",
       x = "Sample Size",
       y = "means score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

means_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("means_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_sf$data_means, tstudent_250_100_MR143_sf$data_means)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_sf$data_means, tstudent_50_100_MR143_sf$data_means)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_sf$data_means, normal_250_100_MR143_sf$data_means)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_sf$data_means, normal_50_100_MR143_sf$data_means)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_sf$data_means, mix_250_100_MR143_sf$data_means)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_sf$data_means, mix_50_100_MR143_sf$data_means)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

means_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "means score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

means_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("means_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#####################################SR#########################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_cl$data_SR, tstudent_250_100_MR143_cl$data_SR)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_cl$data_SR, tstudent_50_100_MR143_cl$data_SR)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_cl$data_SR, normal_250_100_MR143_cl$data_SR)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_cl$data_SR, normal_50_100_MR143_cl$data_SR)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_cl$data_SR, mix_250_100_MR143_cl$data_SR)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_cl$data_SR, mix_50_100_MR143_cl$data_SR)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

SR_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "SR score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

SR_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("SR_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_ra$data_SR, tstudent_250_100_MR143_ra$data_SR)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_ra$data_SR, tstudent_50_100_MR143_ra$data_SR)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_ra$data_SR, normal_250_100_MR143_ra$data_SR)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_ra$data_SR, normal_50_100_MR143_ra$data_SR)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_ra$data_SR, mix_250_100_MR143_ra$data_SR)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_ra$data_SR, mix_50_100_MR143_ra$data_SR)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

SR_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Random",
       x = "Sample Size",
       y = "SR score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

SR_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("SR_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_hub$data_SR, tstudent_250_100_MR143_hub$data_SR)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_hub$data_SR, tstudent_50_100_MR143_hub$data_SR)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_hub$data_SR, normal_250_100_MR143_hub$data_SR)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_hub$data_SR, normal_50_100_MR143_hub$data_SR)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_hub$data_SR, mix_250_100_MR143_hub$data_SR)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_hub$data_SR, mix_50_100_MR143_hub$data_SR)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

SR_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "SR score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

SR_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("SR_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_band$data_SR, tstudent_250_100_MR143_band$data_SR)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_band$data_SR, tstudent_50_100_MR143_band$data_SR)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_band$data_SR, normal_250_100_MR143_band$data_SR)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_band$data_SR, normal_50_100_MR143_band$data_SR)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_band$data_SR, mix_250_100_MR143_band$data_SR)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_band$data_SR, mix_50_100_MR143_band$data_SR)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

SR_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Band",
       x = "Sample Size",
       y = "SR score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

SR_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("SR_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Tstudent", 1600),
  rbind(tstudent_250_100_MR0_sf$data_SR, tstudent_250_100_MR143_sf$data_SR)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_50_100_MR0_sf$data_SR, tstudent_50_100_MR143_sf$data_SR)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Normal", 1600),
  rbind(normal_250_100_MR0_sf$data_SR, normal_250_100_MR143_sf$data_SR)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_50_100_MR0_sf$data_SR, normal_50_100_MR143_sf$data_SR)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 800), rep("MR = 1.43", 800)),
  "N"  = rep(250, 1600),
  "Distr" = rep("Mixture", 1600),
  rbind(mix_250_100_MR0_sf$data_SR, mix_250_100_MR143_sf$data_SR)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(50, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_50_100_MR0_sf$data_SR, mix_50_100_MR143_sf$data_SR)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

SR_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Ledoit",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, max(data_cl$value))) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "SR score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

SR_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("SR_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#####################################F1#########################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_cl$data_F1, tstudent_250_100_MR143_cl$data_F1)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_cl$data_F1, tstudent_50_100_MR143_cl$data_F1)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_cl$data_F1, normal_250_100_MR143_cl$data_F1)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_cl$data_F1, normal_50_100_MR143_cl$data_F1)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_cl$data_F1, mix_250_100_MR143_cl$data_F1)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_cl$data_F1, mix_50_100_MR143_cl$data_F1)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                           'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

F1_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "F1 score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

F1_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("F1_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_ra$data_F1, tstudent_250_100_MR143_ra$data_F1)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_ra$data_F1, tstudent_50_100_MR143_ra$data_F1)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_ra$data_F1, normal_250_100_MR143_ra$data_F1)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_ra$data_F1, normal_50_100_MR143_ra$data_F1)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_ra$data_F1, mix_250_100_MR143_ra$data_F1)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_ra$data_F1, mix_50_100_MR143_ra$data_F1)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

F1_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Random",
       x = "Sample Size",
       y = "F1 score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

F1_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("F1_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_hub$data_F1, tstudent_250_100_MR143_hub$data_F1)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_hub$data_F1, tstudent_50_100_MR143_hub$data_F1)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_hub$data_F1, normal_250_100_MR143_hub$data_F1)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_hub$data_F1, normal_50_100_MR143_hub$data_F1)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_hub$data_F1, mix_250_100_MR143_hub$data_F1)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_hub$data_F1, mix_50_100_MR143_hub$data_F1)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

F1_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "F1 score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

F1_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("F1_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_band$data_F1, tstudent_250_100_MR143_band$data_F1)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_band$data_F1, tstudent_50_100_MR143_band$data_F1)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_band$data_F1, normal_250_100_MR143_band$data_F1)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_band$data_F1, normal_50_100_MR143_band$data_F1)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_band$data_F1, mix_250_100_MR143_band$data_F1)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_band$data_F1, mix_50_100_MR143_band$data_F1)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

F1_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Band",
       x = "Sample Size",
       y = "F1 score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

F1_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("F1_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_sf$data_F1, tstudent_250_100_MR143_sf$data_F1)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_sf$data_F1, tstudent_50_100_MR143_sf$data_F1)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_sf$data_F1, normal_250_100_MR143_sf$data_F1)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_sf$data_F1, normal_50_100_MR143_sf$data_F1)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_sf$data_F1, mix_250_100_MR143_sf$data_F1)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_sf$data_F1, mix_50_100_MR143_sf$data_F1)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

F1_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "F1 score") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

F1_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("F1_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#################################FROBENIUS######################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_cl$data_frob, tstudent_250_100_MR143_cl$data_frob)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_cl$data_frob, tstudent_50_100_MR143_cl$data_frob)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_cl$data_frob, normal_250_100_MR143_cl$data_frob)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_cl$data_frob, normal_50_100_MR143_cl$data_frob)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_cl$data_frob, mix_250_100_MR143_cl$data_frob)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_cl$data_frob, mix_50_100_MR143_cl$data_frob)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Frob_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "Frobenius Distance") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Frob_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("frob_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_ra$data_frob, tstudent_250_100_MR143_ra$data_frob)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_ra$data_frob, tstudent_50_100_MR143_ra$data_frob)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_ra$data_frob, normal_250_100_MR143_ra$data_frob)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_ra$data_frob, normal_50_100_MR143_ra$data_frob)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_ra$data_frob, mix_250_100_MR143_ra$data_frob)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_ra$data_frob, mix_50_100_MR143_ra$data_frob)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Frob_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Random",
       x = "Sample Size",
       y = "Frobenius Distance") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Frob_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("frob_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_hub$data_frob, tstudent_250_100_MR143_hub$data_frob)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_hub$data_frob, tstudent_50_100_MR143_hub$data_frob)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_hub$data_frob, normal_250_100_MR143_hub$data_frob)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_hub$data_frob, normal_50_100_MR143_hub$data_frob)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_hub$data_frob, mix_250_100_MR143_hub$data_frob)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_hub$data_frob, mix_50_100_MR143_hub$data_frob)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Frob_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "Frobenius Distance") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Frob_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("frob_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_band$data_frob, tstudent_250_100_MR143_band$data_frob)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_band$data_frob, tstudent_50_100_MR143_band$data_frob)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_band$data_frob, normal_250_100_MR143_band$data_frob)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_band$data_frob, normal_50_100_MR143_band$data_frob)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_band$data_frob, mix_250_100_MR143_band$data_frob)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_band$data_frob, mix_50_100_MR143_band$data_frob)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Frob_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Band",
       x = "Sample Size",
       y = "Frobenius Distance") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Frob_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("frob_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_sf$data_frob, tstudent_250_100_MR143_sf$data_frob)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_sf$data_frob, tstudent_50_100_MR143_sf$data_frob)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_sf$data_frob, normal_250_100_MR143_sf$data_frob)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_sf$data_frob, normal_50_100_MR143_sf$data_frob)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_sf$data_frob, mix_250_100_MR143_sf$data_frob)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_sf$data_frob, mix_50_100_MR143_sf$data_frob)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Frob_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "Frobenius Distance") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Frob_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("frob_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#################################COND_NUM#######################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_cl$data_cond, tstudent_250_100_MR143_cl$data_cond)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_cl$data_cond, tstudent_50_100_MR143_cl$data_cond)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_cl$data_cond, normal_250_100_MR143_cl$data_cond)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_cl$data_cond, normal_50_100_MR143_cl$data_cond)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_cl$data_cond, mix_250_100_MR143_cl$data_cond)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_cl$data_cond, mix_50_100_MR143_cl$data_cond)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Cond_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 150)) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "Condition Number") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Cond_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("cond_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_ra$data_cond, tstudent_250_100_MR143_ra$data_cond)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_ra$data_cond, tstudent_50_100_MR143_ra$data_cond)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_ra$data_cond, normal_250_100_MR143_ra$data_cond)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_ra$data_cond, normal_50_100_MR143_ra$data_cond)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_ra$data_cond, mix_250_100_MR143_ra$data_cond)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_ra$data_cond, mix_50_100_MR143_ra$data_cond)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Cond_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 150)) +
  labs(title = "Random",
       x = "Sample Size",
       y = "Condition Number") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Cond_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("cond_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_hub$data_cond, tstudent_250_100_MR143_hub$data_cond)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_hub$data_cond, tstudent_50_100_MR143_hub$data_cond)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_hub$data_cond, normal_250_100_MR143_hub$data_cond)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_hub$data_cond, normal_50_100_MR143_hub$data_cond)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_hub$data_cond, mix_250_100_MR143_hub$data_cond)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_hub$data_cond, mix_50_100_MR143_hub$data_cond)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Cond_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 150)) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "Condition Number") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Cond_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("cond_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_band$data_cond, tstudent_250_100_MR143_band$data_cond)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_band$data_cond, tstudent_50_100_MR143_band$data_cond)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_band$data_cond, normal_250_100_MR143_band$data_cond)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_band$data_cond, normal_50_100_MR143_band$data_cond)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_band$data_cond, mix_250_100_MR143_band$data_cond)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_band$data_cond, mix_50_100_MR143_band$data_cond)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Cond_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 150)) +
  labs(title = "Band",
       x = "Sample Size",
       y = "Condition Number") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Cond_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("cond_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_sf$data_cond, tstudent_250_100_MR143_sf$data_cond)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_sf$data_cond, tstudent_50_100_MR143_sf$data_cond)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_sf$data_cond, normal_250_100_MR143_sf$data_cond)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_sf$data_cond, normal_50_100_MR143_sf$data_cond)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_sf$data_cond, mix_250_100_MR143_sf$data_cond)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_sf$data_cond, mix_50_100_MR143_sf$data_cond)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

Cond_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 150)) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "Condition Number") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

Cond_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("cond_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#################################ACCURACY#######################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_cl$data_ACC, tstudent_250_100_MR143_cl$data_ACC)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_cl$data_ACC, tstudent_50_100_MR143_cl$data_ACC)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_cl$data_ACC, normal_250_100_MR143_cl$data_ACC)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_cl$data_ACC, normal_50_100_MR143_cl$data_ACC)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_cl$data_ACC, mix_250_100_MR143_cl$data_ACC)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_cl$data_ACC, mix_50_100_MR143_cl$data_ACC)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

ACC_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "Accuracy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

ACC_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("acc_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_ra$data_ACC, tstudent_250_100_MR143_ra$data_ACC)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_ra$data_ACC, tstudent_50_100_MR143_ra$data_ACC)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_ra$data_ACC, normal_250_100_MR143_ra$data_ACC)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_ra$data_ACC, normal_50_100_MR143_ra$data_ACC)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_ra$data_ACC, mix_250_100_MR143_ra$data_ACC)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_ra$data_ACC, mix_50_100_MR143_ra$data_ACC)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

ACC_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Random",
       x = "Sample Size",
       y = "Accuracy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

ACC_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("acc_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_hub$data_ACC, tstudent_250_100_MR143_hub$data_ACC)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_hub$data_ACC, tstudent_50_100_MR143_hub$data_ACC)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_hub$data_ACC, normal_250_100_MR143_hub$data_ACC)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_hub$data_ACC, normal_50_100_MR143_hub$data_ACC)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_hub$data_ACC, mix_250_100_MR143_hub$data_ACC)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_hub$data_ACC, mix_50_100_MR143_hub$data_ACC)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

ACC_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "Accuracy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

ACC_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("acc_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_band$data_ACC, tstudent_250_100_MR143_band$data_ACC)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_band$data_ACC, tstudent_50_100_MR143_band$data_ACC)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_band$data_ACC, normal_250_100_MR143_band$data_ACC)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_band$data_ACC, normal_50_100_MR143_band$data_ACC)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_band$data_ACC, mix_250_100_MR143_band$data_ACC)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_band$data_ACC, mix_50_100_MR143_band$data_ACC)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

ACC_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Band",
       x = "Sample Size",
       y = "Accuracy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

ACC_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("acc_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_sf$data_ACC, tstudent_250_100_MR143_sf$data_ACC)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_sf$data_ACC, tstudent_50_100_MR143_sf$data_ACC)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_sf$data_ACC, normal_250_100_MR143_sf$data_ACC)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_sf$data_ACC, normal_50_100_MR143_sf$data_ACC)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_sf$data_ACC, mix_250_100_MR143_sf$data_ACC)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_sf$data_ACC, mix_50_100_MR143_sf$data_ACC)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

ACC_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "Accuracy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

ACC_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("acc_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)


#################################ENTROPY########################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_cl$data_entropy, tstudent_250_100_MR143_cl$data_entropy)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_cl$data_entropy, tstudent_50_100_MR143_cl$data_entropy)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_cl$data_entropy, normal_250_100_MR143_cl$data_entropy)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_cl$data_entropy, normal_50_100_MR143_cl$data_entropy)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_cl$data_entropy, mix_250_100_MR143_cl$data_entropy)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_cl$data_entropy, mix_50_100_MR143_cl$data_entropy)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "Entropy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("entropy_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_ra$data_entropy, tstudent_250_100_MR143_ra$data_entropy)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_ra$data_entropy, tstudent_50_100_MR143_ra$data_entropy)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_ra$data_entropy, normal_250_100_MR143_ra$data_entropy)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_ra$data_entropy, normal_50_100_MR143_ra$data_entropy)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_ra$data_entropy, mix_250_100_MR143_ra$data_entropy)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_ra$data_entropy, mix_50_100_MR143_ra$data_entropy)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Random",
       x = "Sample Size",
       y = "Entropy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("entropy_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_hub$data_entropy, tstudent_250_100_MR143_hub$data_entropy)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_hub$data_entropy, tstudent_50_100_MR143_hub$data_entropy)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_hub$data_entropy, normal_250_100_MR143_hub$data_entropy)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_hub$data_entropy, normal_50_100_MR143_hub$data_entropy)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_hub$data_entropy, mix_250_100_MR143_hub$data_entropy)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_hub$data_entropy, mix_50_100_MR143_hub$data_entropy)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "Entropy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("entropy_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_band$data_entropy, tstudent_250_100_MR143_band$data_entropy)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_band$data_entropy, tstudent_50_100_MR143_band$data_entropy)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_band$data_entropy, normal_250_100_MR143_band$data_entropy)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_band$data_entropy, normal_50_100_MR143_band$data_entropy)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_band$data_entropy, mix_250_100_MR143_band$data_entropy)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_band$data_entropy, mix_50_100_MR143_band$data_entropy)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Band",
       x = "Sample Size",
       y = "Entropy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("entropy_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_sf$data_entropy, tstudent_250_100_MR143_sf$data_entropy)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_sf$data_entropy, tstudent_50_100_MR143_sf$data_entropy)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_sf$data_entropy, normal_250_100_MR143_sf$data_entropy)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_sf$data_entropy, normal_50_100_MR143_sf$data_entropy)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_sf$data_entropy, mix_250_100_MR143_sf$data_entropy)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_sf$data_entropy, mix_50_100_MR143_sf$data_entropy)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  #ylim(c(0, 1)) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "Entropy") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("entropy_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#################################LocFDR#########################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_cl$data_localFDR, tstudent_250_100_MR143_cl$data_localFDR)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_cl$data_localFDR, tstudent_50_100_MR143_cl$data_localFDR)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_cl$data_localFDR, normal_250_100_MR143_cl$data_localFDR)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_cl$data_localFDR, normal_50_100_MR143_cl$data_localFDR)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_cl$data_localFDR, mix_250_100_MR143_cl$data_localFDR)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_cl$data_localFDR, mix_50_100_MR143_cl$data_localFDR)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "Local FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("locFDR_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_ra$data_localFDR, tstudent_250_100_MR143_ra$data_localFDR)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_ra$data_localFDR, tstudent_50_100_MR143_ra$data_localFDR)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_ra$data_localFDR, normal_250_100_MR143_ra$data_localFDR)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_ra$data_localFDR, normal_50_100_MR143_ra$data_localFDR)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_ra$data_localFDR, mix_250_100_MR143_ra$data_localFDR)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_ra$data_localFDR, mix_50_100_MR143_ra$data_localFDR)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Random",
       x = "Sample Size",
       y = "Local FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("locFDR_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_hub$data_localFDR, tstudent_250_100_MR143_hub$data_localFDR)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_hub$data_localFDR, tstudent_50_100_MR143_hub$data_localFDR)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_hub$data_localFDR, normal_250_100_MR143_hub$data_localFDR)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_hub$data_localFDR, normal_50_100_MR143_hub$data_localFDR)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_hub$data_localFDR, mix_250_100_MR143_hub$data_localFDR)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_hub$data_localFDR, mix_50_100_MR143_hub$data_localFDR)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "Local FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("locFDR_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_band$data_localFDR, tstudent_250_100_MR143_band$data_localFDR)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_band$data_localFDR, tstudent_50_100_MR143_band$data_localFDR)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_band$data_localFDR, normal_250_100_MR143_band$data_localFDR)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_band$data_localFDR, normal_50_100_MR143_band$data_localFDR)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_band$data_localFDR, mix_250_100_MR143_band$data_localFDR)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_band$data_localFDR, mix_50_100_MR143_band$data_localFDR)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Band",
       x = "Sample Size",
       y = "Local FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("locFDR_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_sf$data_localFDR, tstudent_250_100_MR143_sf$data_localFDR)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_sf$data_localFDR, tstudent_50_100_MR143_sf$data_localFDR)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_sf$data_localFDR, normal_250_100_MR143_sf$data_localFDR)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_sf$data_localFDR, normal_50_100_MR143_sf$data_localFDR)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_sf$data_localFDR, mix_250_100_MR143_sf$data_localFDR)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_sf$data_localFDR, mix_50_100_MR143_sf$data_localFDR)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

entropy_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "Local FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

entropy_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("locFDR_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

#################################FDR############################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_cl$data_FDR, tstudent_250_100_MR143_cl$data_FDR)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_cl$data_FDR, tstudent_50_100_MR143_cl$data_FDR)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_cl$data_FDR, normal_250_100_MR143_cl$data_FDR)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_cl$data_FDR, normal_50_100_MR143_cl$data_FDR)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_cl$data_FDR, mix_250_100_MR143_cl$data_FDR)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_cl$data_FDR, mix_50_100_MR143_cl$data_FDR)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FDR_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FDR_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FDR_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_ra$data_FDR, tstudent_250_100_MR143_ra$data_FDR)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_ra$data_FDR, tstudent_50_100_MR143_ra$data_FDR)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_ra$data_FDR, normal_250_100_MR143_ra$data_FDR)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_ra$data_FDR, normal_50_100_MR143_ra$data_FDR)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_ra$data_FDR, mix_250_100_MR143_ra$data_FDR)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_ra$data_FDR, mix_50_100_MR143_ra$data_FDR)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FDR_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Random",
       x = "Sample Size",
       y = "FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FDR_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FDR_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_hub$data_FDR, tstudent_250_100_MR143_hub$data_FDR)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_hub$data_FDR, tstudent_50_100_MR143_hub$data_FDR)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_hub$data_FDR, normal_250_100_MR143_hub$data_FDR)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_hub$data_FDR, normal_50_100_MR143_hub$data_FDR)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_hub$data_FDR, mix_250_100_MR143_hub$data_FDR)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_hub$data_FDR, mix_50_100_MR143_hub$data_FDR)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FDR_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FDR_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FDR_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_band$data_FDR, tstudent_250_100_MR143_band$data_FDR)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_band$data_FDR, tstudent_50_100_MR143_band$data_FDR)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_band$data_FDR, normal_250_100_MR143_band$data_FDR)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_band$data_FDR, normal_50_100_MR143_band$data_FDR)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_band$data_FDR, mix_250_100_MR143_band$data_FDR)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_band$data_FDR, mix_50_100_MR143_band$data_FDR)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FDR_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Band",
       x = "Sample Size",
       y = "FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FDR_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FDR_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_sf$data_FDR, tstudent_250_100_MR143_sf$data_FDR)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_sf$data_FDR, tstudent_50_100_MR143_sf$data_FDR)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_sf$data_FDR, normal_250_100_MR143_sf$data_FDR)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_sf$data_FDR, normal_50_100_MR143_sf$data_FDR)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_sf$data_FDR, mix_250_100_MR143_sf$data_FDR)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_sf$data_FDR, mix_50_100_MR143_sf$data_FDR)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FDR_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "FDR") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FDR_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FDR_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################TP############################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_cl$data_TP, tstudent_250_100_MR143_cl$data_TP)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_cl$data_TP, tstudent_50_100_MR143_cl$data_TP)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_cl$data_TP, normal_250_100_MR143_cl$data_TP)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_cl$data_TP, normal_50_100_MR143_cl$data_TP)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_cl$data_TP, mix_250_100_MR143_cl$data_TP)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_cl$data_TP, mix_50_100_MR143_cl$data_TP)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

TP_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "TP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

TP_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("TP_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_ra$data_TP, tstudent_250_100_MR143_ra$data_TP)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_ra$data_TP, tstudent_50_100_MR143_ra$data_TP)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_ra$data_TP, normal_250_100_MR143_ra$data_TP)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_ra$data_TP, normal_50_100_MR143_ra$data_TP)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_ra$data_TP, mix_250_100_MR143_ra$data_TP)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_ra$data_TP, mix_50_100_MR143_ra$data_TP)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

TP_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Random",
       x = "Sample Size",
       y = "TP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

TP_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("TP_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_hub$data_TP, tstudent_250_100_MR143_hub$data_TP)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_hub$data_TP, tstudent_50_100_MR143_hub$data_TP)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_hub$data_TP, normal_250_100_MR143_hub$data_TP)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_hub$data_TP, normal_50_100_MR143_hub$data_TP)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_hub$data_TP, mix_250_100_MR143_hub$data_TP)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_hub$data_TP, mix_50_100_MR143_hub$data_TP)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

TP_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "TP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

TP_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("TP_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_band$data_TP, tstudent_250_100_MR143_band$data_TP)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_band$data_TP, tstudent_50_100_MR143_band$data_TP)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_band$data_TP, normal_250_100_MR143_band$data_TP)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_band$data_TP, normal_50_100_MR143_band$data_TP)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_band$data_TP, mix_250_100_MR143_band$data_TP)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_band$data_TP, mix_50_100_MR143_band$data_TP)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

TP_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Band",
       x = "Sample Size",
       y = "TP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

TP_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("TP_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_sf$data_TP, tstudent_250_100_MR143_sf$data_TP)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_sf$data_TP, tstudent_50_100_MR143_sf$data_TP)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_sf$data_TP, normal_250_100_MR143_sf$data_TP)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_sf$data_TP, normal_50_100_MR143_sf$data_TP)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_sf$data_TP, mix_250_100_MR143_sf$data_TP)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_sf$data_TP, mix_50_100_MR143_sf$data_TP)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

TP_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "TP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

TP_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("TP_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################FP############################################
################################CLUSTER#########################################
tstudent_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_cl$data_FP, tstudent_250_100_MR143_cl$data_FP)
)

tstudent_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_cl$data_FP, tstudent_50_100_MR143_cl$data_FP)
)

normal_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_cl$data_FP, normal_250_100_MR143_cl$data_FP)
)

normal_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_cl$data_FP, normal_50_100_MR143_cl$data_FP)
)

mix_cl_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_cl$data_FP, mix_250_100_MR143_cl$data_FP)
)

mix_cl_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_cl$data_FP, mix_50_100_MR143_cl$data_FP)
)

data_tstudent_cl = rbind(tstudent_cl_250, tstudent_cl_50)
data_normal_cl = rbind(normal_cl_250, normal_cl_50)
data_mix_cl = rbind(mix_cl_250, mix_cl_50)

data_cl = rbind(data_tstudent_cl, data_normal_cl, data_mix_cl)

data_cl$Distr = factor(data_cl$Distr, levels = c('Normal','Tstudent','Mixture'))

data_cl$Label = paste(data_cl$Distr, "-", data_cl$MR)

data_cl$Label = factor(data_cl$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FP_plot_cl <- data_cl %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Cluster",
       x = "Sample Size",
       y = "FP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FP_plot_cl + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FP_plot_cl.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################RANDOM#########################################
tstudent_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_ra$data_FP, tstudent_250_100_MR143_ra$data_FP)
)

tstudent_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_ra$data_FP, tstudent_50_100_MR143_ra$data_FP)
)

normal_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_ra$data_FP, normal_250_100_MR143_ra$data_FP)
)

normal_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_ra$data_FP, normal_50_100_MR143_ra$data_FP)
)

mix_ra_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_ra$data_FP, mix_250_100_MR143_ra$data_FP)
)

mix_ra_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_ra$data_FP, mix_50_100_MR143_ra$data_FP)
)

data_tstudent_ra = rbind(tstudent_ra_250, tstudent_ra_50)
data_normal_ra = rbind(normal_ra_250, normal_ra_50)
data_mix_ra = rbind(mix_ra_250, mix_ra_50)

data_ra = rbind(data_tstudent_ra, data_normal_ra, data_mix_ra)

data_ra$Distr = factor(data_ra$Distr, levels = c('Normal','Tstudent','Mixture'))

data_ra$Label = paste(data_ra$Distr, "-", data_ra$MR)

data_ra$Label = factor(data_ra$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FP_plot_ra <- data_ra %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Random",
       x = "Sample Size",
       y = "FP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FP_plot_ra + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FP_plot_ra.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

##################################HUB###########################################
tstudent_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_hub$data_FP, tstudent_250_100_MR143_hub$data_FP)
)

tstudent_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_hub$data_FP, tstudent_50_100_MR143_hub$data_FP)
)

normal_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_hub$data_FP, normal_250_100_MR143_hub$data_FP)
)

normal_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_hub$data_FP, normal_50_100_MR143_hub$data_FP)
)

mix_hub_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_hub$data_FP, mix_250_100_MR143_hub$data_FP)
)

mix_hub_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_hub$data_FP, mix_50_100_MR143_hub$data_FP)
)

data_tstudent_hub = rbind(tstudent_hub_250, tstudent_hub_50)
data_normal_hub = rbind(normal_hub_250, normal_hub_50)
data_mix_hub = rbind(mix_hub_250, mix_hub_50)

data_hub = rbind(data_tstudent_hub, data_normal_hub, data_mix_hub)

data_hub$Distr = factor(data_hub$Distr, levels = c('Normal','Tstudent','Mixture'))

data_hub$Label = paste(data_hub$Distr, "-", data_hub$MR)

data_hub$Label = factor(data_hub$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                   'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FP_plot_hub <- data_hub %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Hub",
       x = "Sample Size",
       y = "FP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FP_plot_hub + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FP_plot_hub.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################BAND############################################
tstudent_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_band$data_FP, tstudent_250_100_MR143_band$data_FP)
)

tstudent_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_band$data_FP, tstudent_50_100_MR143_band$data_FP)
)

normal_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_band$data_FP, normal_250_100_MR143_band$data_FP)
)

normal_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_band$data_FP, normal_50_100_MR143_band$data_FP)
)

mix_band_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_band$data_FP, mix_250_100_MR143_band$data_FP)
)

mix_band_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_band$data_FP, mix_50_100_MR143_band$data_FP)
)

data_tstudent_band = rbind(tstudent_band_250, tstudent_band_50)
data_normal_band = rbind(normal_band_250, normal_band_50)
data_mix_band = rbind(mix_band_250, mix_band_50)

data_band = rbind(data_tstudent_band, data_normal_band, data_mix_band)

data_band$Distr = factor(data_band$Distr, levels = c('Normal','Tstudent','Mixture'))

data_band$Label = paste(data_band$Distr, "-", data_band$MR)

data_band$Label = factor(data_band$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                     'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FP_plot_band <- data_band %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Band",
       x = "Sample Size",
       y = "FP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FP_plot_band + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FP_plot_band.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)

################################SCALE-FREE######################################
tstudent_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Tstudent", 1400),
  rbind(tstudent_250_100_MR0_sf$data_FP, tstudent_250_100_MR143_sf$data_FP)
)

tstudent_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Tstudent", 1200),
  rbind(tstudent_50_100_MR0_sf$data_FP, tstudent_50_100_MR143_sf$data_FP)
)

normal_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Normal", 1400),
  rbind(normal_250_100_MR0_sf$data_FP, normal_250_100_MR143_sf$data_FP)
)

normal_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Normal", 1200),
  rbind(normal_50_100_MR0_sf$data_FP, normal_50_100_MR143_sf$data_FP)
)

mix_sf_250 = tibble(
  "MR" = c(rep("MR = 0", 700), rep("MR = 1.43", 700)),
  "N"  = rep(250, 1400),
  "Distr" = rep("Mixture", 1400),
  rbind(mix_250_100_MR0_sf$data_FP, mix_250_100_MR143_sf$data_FP)
)

mix_sf_50 = tibble(
  "MR" = c(rep("MR = 0", 600), rep("MR = 1.43", 600)),
  "N"  = rep(50, 1200),
  "Distr" = rep("Mixture", 1200),
  rbind(mix_50_100_MR0_sf$data_FP, mix_50_100_MR143_sf$data_FP)
)

data_tstudent_sf = rbind(tstudent_sf_250, tstudent_sf_50)
data_normal_sf = rbind(normal_sf_250, normal_sf_50)
data_mix_sf = rbind(mix_sf_250, mix_sf_50)

data_sf = rbind(data_tstudent_sf, data_normal_sf, data_mix_sf)

data_sf$Distr = factor(data_sf$Distr, levels = c('Normal','Tstudent','Mixture'))

data_sf$Label = paste(data_sf$Distr, "-", data_sf$MR)

data_sf$Label = factor(data_sf$Label, levels = c('Normal - MR = 0','Tstudent - MR = 0','Mixture - MR = 0',
                                                 'Normal - MR = 1.43','Tstudent - MR = 1.43','Mixture - MR = 1.43'))

FP_plot_sf <- data_sf %>%
  ggplot(aes(
    x = factor(
      N,
      levels = c(
        250,
        50
      )
    ),
    y = value,
    fill = factor(
      name,
      levels = c(
        "Sample",
        "Elastic",
        "Rope",
        "Glasso",
        "Tlasso",
        "Gslope",
        "Tslope"
      )
    )
  )) +
  geom_boxplot(width = 0.65, position = position_dodge(0.75)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, name = "Methods") +
  #scale_y_log10 +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  ylim(c(0, 1)) +
  labs(title = "Scale-free",
       x = "Sample Size",
       y = "FP rate") +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("250", "50")) +
  guides(fill = guide_legend(nrow = 1))

FP_plot_sf + facet_wrap( ~ Label, nrow = 2, scales = "free") +
  theme(strip.background = element_rect(
    color = "grey",
    fill = "#D7D7D7",
    size = 0.5,
    linetype = "solid"
  ),
  strip.text.x = element_text(
    size = 8, color = "black", face = "bold"
  ))

ggsave("FP_plot_sc_free.pdf", width = 12, height = 8, units = "in", device = cairo_pdf)





