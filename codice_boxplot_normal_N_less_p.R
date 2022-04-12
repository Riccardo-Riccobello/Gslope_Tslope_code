library(tidyverse)
library(hrbrthemes)
library(viridis)

#Codice per BoxPlot con nube di punti
Box_Plot_sim_norm_N_less_p <- function(output_performance){
  
  cond_plot <- output_performance$data_cond %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ggtitle("Condition Number BoxPlot") +
    xlab("Methods")
  
  frob_plot <- output_performance$data_frob %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ggtitle("Frobenius Norm BoxPlot") +
    xlab("Methods")
  
  TP_plot <- output_performance$data_TP %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0,1)) +
    ggtitle("TP_rate BoxPlot") +
    xlab("Methods")
  
  FP_plot <- output_performance$data_FP %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0,1)) +
    ggtitle("FP_rate BoxPlot") +
    xlab("Methods")
  
  TN_plot <- output_performance$data_TN %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0,1)) +
    ggtitle("TN_rate BoxPlot") +
    xlab("Methods")
  
  FN_plot <- output_performance$data_FN %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0,1)) +
    ggtitle("FN_rate BoxPlot") +
    xlab("Methods")
  
  F1_plot <- output_performance$data_F1 %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0,1)) +
    ggtitle("F1_score BoxPlot") +
    xlab("Methods")
  
  Acc_plot <- output_performance$data_ACC %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0,1)) +
    ggtitle("Accuracy BoxPlot") +
    xlab("Methods")
  
  entropy_plot <- output_performance$data_entropy %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ggtitle("Entropy BoxPlot") +
    xlab("Methods")
  
  FDR_plot <- output_performance$data_FDR %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0,1)) +
    ggtitle("FDR BoxPlot") +
    xlab("Methods")
  
  localFDR_plot <- output_performance$data_localFDR %>%
    ggplot( aes(x=factor(name, levels = c("Elastic", "Rope",
                                          "Glasso", "Tlasso", "Gslope", "Tslope")),
                y=value, fill=factor(name, levels = c("Elastic", "Rope",
                                                      "Glasso", "Tlasso", "Gslope", "Tslope")))) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0,1)) +
    ggtitle("localFDR BoxPlot") +
    xlab("Methods")
  
  output   = list(cond_plot, frob_plot, TP_plot, FP_plot, TN_plot, FN_plot,
                  F1_plot, Acc_plot, entropy_plot, FDR_plot, localFDR_plot)
  
  names(output) <-
    paste(
      list(
        "cond_plot",
        "frob_plot",
        "TP_plot",
        "FP_plot",
        "TN_plot",
        "FN_plot",
        "F1_plot",
        "Acc_plot",
        "entropy_plot",
        "FDR_plot",
        "localFDR_plot"
      )
    )
  
  return(output)
  
}

