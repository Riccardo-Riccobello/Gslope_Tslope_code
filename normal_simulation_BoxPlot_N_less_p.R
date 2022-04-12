sample_precision_matrix_estimation_normal_N_less_p <-
  function(data, n, alpha_level, df, progress = TRUE) {
    
    #GLASSO
    lambda = lambdaSelector(data, n, alpha_level, method = "banerjee")
    
    output_glasso = glasso(cor(data),
                           lambda,
                           nobs = n,
                           penalize.diagonal = FALSE) #corr to avoid scale effect on penalization, then transform in a covariance matrix
    
    #From corr matrix to covariance, then precision
    sigma_glasso = cor2cov(data, output_glasso$w)
    inv_glasso   = solve(sigma_glasso)
    inv_glasso[abs(inv_glasso) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #GLASSO ELASTIC NET
    output_elastic = gelnet(S = cor(data), lambda = lambda, alpha = 0.5)
    
    #From corr matrix to covariance, then precision
    sigma_elastic = cor2cov(data, output_elastic$W)
    inv_elastic   = solve(sigma_elastic)
    inv_elastic[abs(inv_elastic) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #GLASSO RIDGE (ROPE)
    output_rope = gelnet(S = cor(data), lambda = lambda, alpha = 0)
    
    #From corr matrix to covariance, then precision
    sigma_rope = cor2cov(data, output_rope$W)
    inv_rope   = solve(sigma_rope)
    inv_rope[abs(inv_rope) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #TLASSO
    v = df
    output_tlasso = tlasso_fast_rolling(data, lambda, v, 0.001, F, init_theta = matrix(0, ncol(data), ncol(data)))
    
    sigma_tlasso  = (output_tlasso$S) #* (v/(v - 2))
    inv_tlasso    = (output_tlasso$theta) #* ((v - 2)/v)
    inv_tlasso[abs(inv_tlasso) < 1e-04] <-
      0 #threshold to consider an entry equal to zero
    
    #GSLOPE
    lambdas = create_lambda(cor(data), n = n, alpha = alpha_level)
    
    output_gslope = gslope_new(cor(data), lambdas, progress = progress)
    
    #From corr matrix to covariance, then precision
    inv_gslope  = invcor2invcov(data, output_gslope$precision_matrix)
    sigma_gslope    = solve(inv_gslope)
    inv_gslope[abs(inv_gslope) < 1e-04] <-
      0 #threshold to consider an entry equal to zero
    
    #TSLOPE
    output_tslope = tslope_fast_rolling(data, v, 0.001, F, init_theta = matrix(0, ncol(data), ncol(data)), alpha_value = alpha_level)
    sigma_tslope  = (output_tslope$S) #* (v/(v - 2))
    inv_tslope    = solve(sigma_tslope)
    inv_tslope[abs(inv_tslope) < 1e-04] <- 0
    
    #TOTAL OUTPUT
    output = list(
      sigma_glasso,
      sigma_elastic,
      sigma_rope,
      sigma_tlasso,
      sigma_gslope,
      sigma_tslope,
      inv_glasso,
      inv_elastic,
      inv_rope,
      inv_tlasso,
      inv_gslope,
      inv_tslope
    )
    names(output) <-
      paste(
        list(
          "sigma_glasso",
          "sigma_elastic",
          "sigma_rope",
          "sigma_tlasso",
          "sigma_gslope",
          "sigma_tslope",
          "inv_glasso",
          "inv_elastic",
          "inv_rope",
          "inv_tlasso",
          "inv_gslope",
          "inv_tslope"
        )
      )
    
    return(output)
  }

performance_measures_normal_N_less_p <- function(omega,
                                                 inv_glasso,
                                                 inv_elastic,
                                                 inv_rope,
                                                 inv_tlasso,
                                                 inv_gslope,
                                                 inv_tslope) {
  #CONDITION NUMBER
  condition = list(
    cond(inv_glasso),
    cond(inv_elastic),
    cond(inv_rope),
    cond(inv_tlasso),
    cond(inv_gslope),
    cond(inv_tslope)
  )
  names(condition) <-
    paste(
      list(
        "cond_glasso",
        "cond_elastic",
        "cond_rope",
        "cond_tlasso",
        "cond_gslope",
        "cond_tslope"
      )
    )
  
  #FROBENIUS NORM
  frobenius = list(
    norm(omega - inv_glasso, type = "F"),
    norm(omega - inv_elastic, type = "F"),
    norm(omega - inv_rope, type = "F"),
    norm(omega - inv_tlasso, type = "F"),
    norm(omega - inv_gslope, type = "F"),
    norm(omega - inv_tslope, type = "F")
  )
  
  names(frobenius) <-
    paste(
      list(
        "frob_glasso",
        "frob_elastic",
        "frob_rope",
        "frob_tlasso",
        "frob_gslope",
        "frob_tslope"
      )
    )
  
  #ENTROPY LOSS (KL)
  
  #TP and FP rate
  TPrate = list(
    TP(inv_glasso, omega)[[2]],
    TP(inv_elastic, omega)[[2]],
    TP(inv_rope, omega)[[2]],
    TP(inv_tlasso, omega)[[2]],
    TP(inv_gslope, omega)[[2]],
    TP(inv_tslope, omega)[[2]]
  )
  
  names(TPrate) <-
    paste(
      list(
        "TP_glasso",
        "TP_elastic",
        "TP_rope",
        "TP_tlasso",
        "TP_gslope",
        "TP_tslope"
      )
    )
  
  TNrate = list(
    TN(inv_glasso, omega)[[2]],
    TN(inv_elastic, omega)[[2]],
    TN(inv_rope, omega)[[2]],
    TN(inv_tlasso, omega)[[2]],
    TN(inv_gslope, omega)[[2]],
    TN(inv_tslope, omega)[[2]]
  )
  
  names(TNrate) <-
    paste(
      list(
        "TN_glasso",
        "TN_elastic",
        "TN_rope",
        "TN_tlasso",
        "TN_gslope",
        "TN_tslope"
      )
    )
  
  FPrate = list(
    FP(inv_glasso, omega)[[2]],
    FP(inv_elastic, omega)[[2]],
    FP(inv_rope, omega)[[2]],
    FP(inv_tlasso, omega)[[2]],
    FP(inv_gslope, omega)[[2]],
    FP(inv_tslope, omega)[[2]]
  )
  
  names(FPrate) <-
    paste(
      list(
        "FP_glasso",
        "FP_elastic",
        "FP_rope",
        "FP_tlasso",
        "FP_gslope",
        "FP_tslope"
      )
    )
  
  FNrate = list(
    FN(inv_glasso, omega)[[2]],
    FN(inv_elastic, omega)[[2]],
    FN(inv_rope, omega)[[2]],
    FN(inv_tlasso, omega)[[2]],
    FN(inv_gslope, omega)[[2]],
    FN(inv_tslope, omega)[[2]]
  )
  
  names(FNrate) <-
    paste(
      list(
        "FN_glasso",
        "FN_elastic",
        "FN_rope",
        "FN_tlasso",
        "FN_gslope",
        "FN_tslope"
      )
    )
  
  #F1 score
  F1 = list(
    TP(inv_glasso, omega)[[1]] / (TP(inv_glasso, omega)[[1]] + (0.5 * (
      FP(inv_glasso, omega)[[1]] + FN(inv_glasso, omega)[[1]]
    ))),
    TP(inv_elastic, omega)[[1]] / (TP(inv_elastic, omega)[[1]] + (0.5 * (
      FP(inv_elastic, omega)[[1]] + FN(inv_elastic, omega)[[1]]
    ))),
    TP(inv_rope, omega)[[1]] / (TP(inv_rope, omega)[[1]] + (0.5 * (
      FP(inv_rope, omega)[[1]] + FN(inv_rope, omega)[[1]]
    ))),
    TP(inv_tlasso, omega)[[1]] / (TP(inv_tlasso, omega)[[1]] + (0.5 * (
      FP(inv_tlasso, omega)[[1]] + FN(inv_tlasso, omega)[[1]]
    ))),
    TP(inv_gslope, omega)[[1]] / (TP(inv_gslope, omega)[[1]] + (0.5 * (
      FP(inv_gslope, omega)[[1]] + FN(inv_gslope, omega)[[1]]
    ))),
    TP(inv_tslope, omega)[[1]] / (TP(inv_tslope, omega)[[1]] + (0.5 * (
      FP(inv_tslope, omega)[[1]] + FN(inv_tslope, omega)[[1]]
    )))
  )
  names(F1) <-
    paste(
      list(
        "F1_glasso",
        "F1_elastic",
        "F1_rope",
        "F1_tlasso",
        "F1_gslope",
        "F1_tslope"
      )
    )
  
  #ACCURACY
  positive <- max(c(sum(upper(properAdjacent(omega))), 1))
  p = nrow(omega)
  negative <- max(c(sum(!upper(properAdjacent(omega))), 1))
  
  ACC = list(
    (TP(inv_glasso, omega)[[1]] + (TN(inv_glasso, omega)[[1]])) / (positive + negative),
    (TP(inv_elastic, omega)[[1]] + (TN(inv_elastic, omega)[[1]])) / (positive + negative),
    (TP(inv_rope, omega)[[1]] + (TN(inv_rope, omega)[[1]])) / (positive + negative),
    (TP(inv_tlasso, omega)[[1]] + (TN(inv_tlasso, omega)[[1]])) / (positive + negative),
    (TP(inv_gslope, omega)[[1]] + (TN(inv_gslope, omega)[[1]])) / (positive + negative),
    (TP(inv_tslope, omega)[[1]] + (TN(inv_tslope, omega)[[1]])) / (positive + negative)
  )
  
  names(ACC) <-
    paste(
      list(
        "ACC_glasso",
        "ACC_elastic",
        "ACC_rope",
        "ACC_tlasso",
        "ACC_gslope",
        "ACC_tslope"
      )
    )
  
  entropy = list(
    sum(diag(omega %*% inv_glasso)) - log(det(omega %*% inv_glasso)) - p,
    sum(diag(omega %*% inv_elastic)) - log(det(omega %*% inv_elastic)) - p,
    sum(diag(omega %*% inv_rope)) - log(det(omega %*% inv_rope)) - p,
    sum(diag(omega %*% inv_tlasso)) - log(det(omega %*% inv_tlasso)) - p,
    sum(diag(omega %*% inv_gslope)) - log(det(omega %*% inv_gslope)) - p,
    sum(diag(omega %*% inv_tslope)) - log(det(omega %*% inv_tslope)) - p
  )
  
  names(entropy) <-
    paste(
      list(
        "entropy_glasso",
        "entropy_elastic",
        "entropy_rope",
        "entropy_tlasso",
        "entropy_gslope",
        "entropy_tslope"
      )
    )
  
  FDR = list(
    FDP(inv_glasso, omega),
    FDP(inv_elastic, omega),
    FDP(inv_rope, omega),
    FDP(inv_tlasso, omega),
    FDP(inv_gslope, omega),
    FDP(inv_tslope, omega)
  )
  
  names(FDR) <-
    paste(
      list(
        "FDP_glasso",
        "FDP_elastic",
        "FDP_rope",
        "FDP_tlasso",
        "FDP_gslope",
        "FDP_tslope"
      )
    )
  
  localFDR = list(
    localFDP(properAdjacent(inv_glasso), properAdjacent(omega)),
    localFDP(properAdjacent(inv_elastic), properAdjacent(omega)),
    localFDP(properAdjacent(inv_rope), properAdjacent(omega)),
    localFDP(properAdjacent(inv_tlasso), properAdjacent(omega)),
    localFDP(properAdjacent(inv_gslope), properAdjacent(omega)),
    localFDP(properAdjacent(inv_tslope), properAdjacent(omega))
  )
  
  names(localFDR) <-
    paste(
      list(
        "localFDP_glasso",
        "localFDP_elastic",
        "localFDP_rope",
        "localFDP_tlasso",
        "localFDP_gslope",
        "localFDP_tslope"
      )
    )
  
  output <-
    list(condition,
         frobenius,
         TPrate,
         FPrate,
         TNrate,
         FNrate,
         F1,
         ACC,
         entropy,
         FDR,
         localFDR)
  
  names(output) <-
    paste(
      list(
        "condition_number",
        "frobenius_norm",
        "TP_rate",
        "FP_rate",
        "TN_rate",
        "FN_rate",
        "F1_score",
        "Accuracy",
        "Entropy_loss",
        "FDR",
        "local_FDR"
      )
    )
  
  return(output)
}

BoxPlot_simulation_normal_N_less_p <- function(N, p, graph_type, v, u, i){
  
  a = oracle_precision_matrix_generation(N, p, graph_type, v, u)
  
  i = i
  
  perf_measures = list()
  
  for (i in 1:i) {
    b = data_series_generation(N, a$sigma, 0.05, 4, T_student = FALSE)
    
    c = sample_precision_matrix_estimation_normal_N_less_p(b, N, 0.05, 4, progress = TRUE)
    
    d = performance_measures_normal_N_less_p(
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
    value=c(cond$cond_glasso, cond$cond_elastic,
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
    value=c( FP_rt$FP_sample, FP_rt$FP_glasso, FP_rt$FP_elastic,
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