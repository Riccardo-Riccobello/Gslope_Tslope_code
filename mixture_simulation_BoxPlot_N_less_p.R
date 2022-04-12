library(nvmix)

BoxPlot_simulation_mix_N_less_p <- function(N, p, graph_type, v, u, i){
  
  a = oracle_precision_matrix_generation(N, p, graph_type, v, u)
  
  i = i
  
  perf_measures = list()
  
  for (i in 1:i) {
    b = rnvmix(N, rmix = "inverse.gamma", df = 4, scale = a$sigma, loc = rep(0.05, p))
    
    c = sample_precision_matrix_estimation_N_less_p(b, N, 0.05, 4, progress = TRUE)
    
    d = performance_measures_N_less_p(
      a$omega,
      c$inv_glasso,
      c$inv_elastic,
      c$inv_rope,
      c$inv_tlasso,
      c$inv_gslope,
      c$inv_tslope
    )
    
    perf_measures[[i]] = d
    
    print(paste0("Iteration number: ", i))
  }
  
  ##########################AVERAGE PERFORMANCE MEASURES##########################
  cond = data.frame()
  for (i in 1:i) {
    cond = rbind(perf_measures[[i]]$condition_number, cond)
  }
  
  frob = data.frame()
  for (i in 1:i) {
    frob = rbind(perf_measures[[i]]$frobenius_norm, frob)
  }
  
  TP_rt = data.frame()
  for (i in 1:i) {
    TP_rt = rbind(perf_measures[[i]]$TP_rate, TP_rt)
  }
  
  FP_rt = data.frame()
  for (i in 1:i) {
    FP_rt = rbind(perf_measures[[i]]$FP_rate, FP_rt)
  }
  
  TN_rt = data.frame()
  for (i in 1:i) {
    TN_rt = rbind(perf_measures[[i]]$TN_rate, TN_rt)
  }
  
  FN_rt = data.frame()
  for (i in 1:i) {
    FN_rt = rbind(perf_measures[[i]]$FN_rate, FN_rt)
  }
  
  F1 = data.frame()
  for (i in 1:i) {
    F1 = rbind(perf_measures[[i]]$F1_score, F1)
  }
  
  Acc = data.frame()
  for (i in 1:i) {
    Acc = rbind(perf_measures[[i]]$Accuracy, Acc)
  }
  
  Entropy = data.frame()
  for (i in 1:i) {
    Entropy = rbind(perf_measures[[i]]$Entropy_loss, Entropy)
  }
  
  FDR_data = data.frame()
  for (i in 1:i) {
    FDR_data = rbind(perf_measures[[i]]$FDR, FDR_data)
  }
  
  localFDR_data = data.frame()
  for (i in 1:i) {
    localFDR_data = rbind(perf_measures[[i]]$local_FDR, localFDR_data)
  }
  
  Avg_cond = colMeans(cond)
  Avg_frob = colMeans(frob)
  Avg_TP = colMeans(TP_rt)
  Avg_FP = colMeans(FP_rt)
  Avg_TN = colMeans(TN_rt)
  Avg_FN = colMeans(FN_rt)
  Avg_F1 = colMeans(F1)
  Avg_Acc = colMeans(Acc)
  Avg_Entropy = colMeans(Entropy)
  Avg_FDR = colMeans(FDR_data)
  Avg_localFDR = colMeans(localFDR_data)
  
  Std_cond = apply(cond, 2, sd)
  Std_frob = apply(frob, 2, sd)
  Std_TP = apply(TP_rt, 2, sd)
  Std_FP = apply(FP_rt, 2, sd)
  Std_TN = apply(TN_rt, 2, sd)
  Std_FN = apply(FN_rt, 2, sd)
  Std_F1 = apply(F1, 2, sd)
  Std_Acc = apply(Acc, 2, sd)
  Std_Entropy = apply(Entropy, 2, sd)
  Std_FDR = apply(FDR_data, 2, sd)
  Std_localFDR = apply(localFDR_data, 2, sd)
  
  row = list(
             "glasso",
             "elastic",
             "rope",
             "tlasso",
             "gslope",
             "tslope")
  
  Avg_means = data.frame(
    row.names = row,
    Avg_cond,
    Avg_frob,
    Avg_TP,
    Avg_FP,
    Avg_TN,
    Avg_FN,
    Avg_F1,
    Avg_Acc,
    Avg_Entropy,
    Avg_FDR,
    Avg_localFDR
  )
  
  Avg_std = data.frame(
    row.names = row,
    Std_cond,
    Std_frob,
    Std_TP,
    Std_FP,
    Std_TN,
    Std_FN,
    Std_F1,
    Std_Acc,
    Std_Entropy,
    Std_FDR,
    Std_localFDR
  )
  
  data_cond <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c( cond$cond_sample, cond$cond_ledoit, cond$cond_glasso, cond$cond_elastic,
             cond$cond_rope, cond$cond_tlasso, cond$cond_gslope, cond$cond_tslope))
  
  data_frob <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(frob$frob_glasso, frob$frob_elastic,
             frob$frob_rope, frob$frob_tlasso, frob$frob_gslope, frob$frob_tslope))
  
  data_TP <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(TP_rt$TP_glasso, TP_rt$TP_elastic,
             TP_rt$TP_rope, TP_rt$TP_tlasso, TP_rt$TP_gslope, TP_rt$TP_tslope))
  
  data_FP <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(FP_rt$FP_glasso, FP_rt$FP_elastic,
             FP_rt$FP_rope, FP_rt$FP_tlasso, FP_rt$FP_gslope, FP_rt$FP_tslope))
  
  data_TN <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(TN_rt$TN_glasso, TN_rt$TN_elastic,
             TN_rt$TN_rope, TN_rt$TN_tlasso, TN_rt$TN_gslope, TN_rt$TN_tslope))
  
  
  data_FN <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(FN_rt$FN_glasso, FN_rt$FN_elastic,
             FN_rt$FN_rope, FN_rt$FN_tlasso, FN_rt$FN_gslope, FN_rt$FN_tslope))
  
  data_F1 <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(F1$F1_glasso, F1$F1_elastic,
             F1$F1_rope, F1$F1_tlasso, F1$F1_gslope, F1$F1_tslope))
  
  data_ACC <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(Acc$ACC_glasso, Acc$ACC_elastic,
             Acc$ACC_rope, Acc$ACC_tlasso, Acc$ACC_gslope, Acc$ACC_tslope))
  
  data_entropy <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(Entropy$entropy_glasso, Entropy$entropy_elastic,
             Entropy$entropy_rope, Entropy$entropy_tlasso, Entropy$entropy_gslope, Entropy$entropy_tslope))
  
  data_FDR <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(FDR_data$FDP_glasso, FDR_data$FDP_elastic,
             FDR_data$FDP_rope, FDR_data$FDP_tlasso, FDR_data$FDP_gslope, FDR_data$FDP_tslope))
  
  data_localFDR <- data.frame(
    name=c(rep("Glasso",i), rep("Elastic",i), rep("Rope",i),
            rep("Tlasso",i), rep("Gslope",i), rep("Tslope",i)),
    value=c(localFDR_data$localFDP_glasso, localFDR_data$localFDP_elastic,
             localFDR_data$localFDP_rope, localFDR_data$localFDP_tlasso, localFDR_data$localFDP_gslope, localFDR_data$localFDP_tslope))
  
  
  output   = list(data_cond, data_frob, data_TP, data_FP, data_TN, data_FN,
                  data_F1, data_ACC, data_entropy, data_FDR, data_localFDR,
                  Avg_means, Avg_std)
  
  names(output) <-
    paste(
      list(
        "data_cond",
        "data_frob",
        "data_TP",
        "data_FP",
        "data_TN",
        "data_FN",
        "data_F1",
        "data_ACC",
        "data_entropy",
        "data_FDR",
        "data_localFDR",
        "Average_performance_measures",
        "Std_performance_measures"
      )
    )
  
  return(output)
}
