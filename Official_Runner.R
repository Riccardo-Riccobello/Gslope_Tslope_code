################################################################################
###########################      DISCLAIMER       ##############################
################################################################################
################ below all you need to run the simulation ######################
################################################################################


################################################################################
# AUTHOR: Riccardo Riccobello                                                  #
# EMAIL: riccardo.riccobello@alumni.unitn.it/rriccobello@deloitte.it           #
# PAPER: this is the code for the Graphical Models paper - Machine Learning    #
################################################################################


################################################################################
###  On two occasions I have been asked, 'Pray, Mr. Babbage, if you put into ###
###  the machine wrong figures, will the right answers come out?'            ###
###  I am not able rightly to apprehend the kind of confusion of ideas that  ###
###  could provoke such a question.                                          ###
###  C. B.                                                                   ###
################################################################################


###############################REQUIRED PACKAGES################################
library('grpSLOPE') #To use the proximity operator to compute the L1 Sorted Norm
library('huge') #To simulate clustered data, the core of my simulation!
library('igraph') #Used to plot the network structure (e.g. colored graph)
library('glasso') #Tlasso is implemented through my function
library('mvtnorm') #To generate t-student data
library('nlshrink') #To implement LedoitWolf shrinkage procedure
library('MASS') #To estimate the df of t-student data
library('ggplot2')
library('reshape2')
library('dplyr')
library('pracma')
library('qgraph') #To plot the precision matrix
library('ggrepel')
library('tidyverse')
library('xtable')
library('zoo')
library('GLassoElnetFast') #Methods: Elastic Net and Rope
library('grid') #Save dataframe
library('gridExtra') #Save dataframe
library('hrbrthemes') #Themes for heatmaps
library('viridis') #Themes for heatmaps
library('nvmix') #To generate data from a Mixture Distribution (Tstu. + Gauss.)

############################Generic Functions###################################

################################################################################
#    Functions used to convert the correlation (or the inv. corr.) in the      #
#    covariance (or the inv. cov.) matrices                                    #
#                                                                              #
# INPUT: data series Nxp and the correlation matrix (or the inv. corr.) pxp    #
# OUTPUT: the covariance or the precision matrices                             #
################################################################################

##1##
cor2cov = function(data, cor_mat) {
  dev = apply(data, 2, sd)
  b   = dev %*% t(dev)
  return(cor_mat * b)
}

##2##
invcor2invcov = function(data, invcor_mat) {
  dev = apply(data, 2, sd)
  b   = dev %*% t(dev)
  return(invcor_mat / b)
}

################################################################################
#    Function used to generate the lambdas for Gslope/Tslope using BH          #
#                                                                              #
# INPUT: sample corr. pxp, number of obs, level of significance                #
# OUTPUT: the sorted lambdas to use with Gslope/Tslope                         #
################################################################################

##3##
create_lambda = function(sample_cov, n, alpha = 0.05) {
  p = ncol(sample_cov)
  low_tri_size = p * (p - 1) / 2
  k = 1:low_tri_size
  
  two_largest_prod = prod(-sort(-diag(sample_cov), partial = 2)[1:2])
  fraction_seq = qt(1 - alpha * k / (2 * low_tri_size), n - 2) /
    sqrt(n - 2 + qt(1 - alpha * k / (2 * low_tri_size), n - 2) ^ 2)
  
  two_largest_prod * fraction_seq
}

################################################################################
#    Functions used to generate the lambdas for Gslope/Tslope/Glasso/Tlasso    #
################################################################################

##4##
lambdaSelector =
  function(input,
           n,
           alpha = 0.05,
           method = "banerjee",
           verbose = TRUE)
  {
    p = ncol(input)
    
    if (alpha != 0)
    {
      if (!is.matrix(input))
      {
        if (verbose)
          cat("The input is identified as a dimension.\n")
        if (!is.numeric(input[1]))
          stop(paste("The input must be numeric, but is", typeof(input[1])))
        
        p = input[1]
        twoLargestProd = 1
        
      } else if (!isSymmetric(input))
      {
        if (verbose)
          cat("The input is identified as the data matrix.\n")
        
        n = nrow(input)
        input = cov(scale(input))
        twoLargestProd = 1
      } else
      {
        if (verbose)
          cat("The input is identified as the covariance matrix.\n")
        
        twoLargestProd =
          prod(-sort(-diag(input), partial = 2)[1:2]) # In case data wasn't scaled
      }
      
      out = switch(
        method,
        glasso = lambdaGLASSO(p, n, alpha, twoLargestProd),
        banerjee = lambdaBanerjee(p, n, alpha, twoLargestProd),
        BH = lambdaBH(p, n, alpha, twoLargestProd),
        holm = lambdaHolm(p, n, alpha, twoLargestProd)
      )
    } else
    {
      out = 0
    }
    
    return(out)
  }

##5## gLASSO
lambdaGLASSO <- function(p,
                         n,
                         alpha = 0.05,
                         twoLargestProd = 1)
{
  pGLASSO <- p * (p - 1) / 2
  fraction <-
    qt(1 - alpha / 2 / pGLASSO, df = n - 2) / sqrt(n - 2 + qt(1 - alpha / 2 /
                                                                pGLASSO, df = n - 2) ^ 2)
  
  return(twoLargestProd * fraction)
}

##6## Banerjee for gLASSO
lambdaBanerjee <- function(p,
                           n,
                           alpha = 0.05,
                           twoLargestProd = 1)
{
  pBanerjee <- p ^ 2
  fraction <-
    qt(1 - alpha / 2 / pBanerjee, df = n - 2) / sqrt(n - 2 + qt(1 - alpha /
                                                                  2 / pBanerjee, df = n - 2) ^ 2)
  
  return(twoLargestProd * fraction)
}

##7## BH for SLOPE
lambdaBH <- function(p,
                     n,
                     alpha = 0.05,
                     twoLargestProd = 1)
{
  pBH <- p * (p - 1) / 2
  k <- 1:pBH
  # k <- k + (p^2 - pBH)
  fractionSeq <-
    qt(1 - alpha * k / 2 / pBH, df = n - 2) / sqrt(n - 2 + qt(1 - alpha * k /
                                                                2 / pBH, df = n - 2) ^ 2)
  fractionSeq <-
    c(rep(fractionSeq[1], p), rep(fractionSeq, each = 2))
  
  return(twoLargestProd * fractionSeq)
}

##8## Holm for SLOPE
lambdaHolm <- function(p,
                       n,
                       alpha = 0.05,
                       twoLargestProd = 1)
{
  pHolm <- p * (p - 1) / 2
  k <- 1:pHolm
  # k <- k + (p^2 - pHolm)
  fractionSeq <-
    qt(1 - alpha / 2 / (pHolm + 1 - k), df = n - 2) / sqrt(n - 2 + qt(1 - alpha /
                                                                        2 / (pHolm + 1 - k), df = n - 2) ^ 2)
  fractionSeq <-
    c(rep(fractionSeq[1], p), rep(fractionSeq, each = 2))
  
  return(twoLargestProd * fractionSeq)
}

################################################################################
#    Function used to compute the sorted L1 Norm for Gslope/Tslope             #
#                                                                              #
# INPUT: sample corr. pxp, lambdas p*(p-1)/2                                   #
# OUTPUT: the sorted L1 norm on the matrix                                     #
################################################################################

##9##
proximity_matrix = function(matrix_in, lambdas) {
  output = matrix_in
  precision_entries  = matrix_in[lower.tri(matrix_in, FALSE)]
  calculated_entries = grpSLOPE::prox_sorted_L1(as.matrix(precision_entries),
                                                lambdas,
                                                method = c("c"))
  output[lower.tri(output, FALSE)] = calculated_entries
  output[upper.tri(output, FALSE)] = calculated_entries
  
  output
}

################################################################################
#    Functions used to compute a series of performance measures to compare     #
#    the estimated matrices with the real ones                                 #
################################################################################

##10##
properAdjacent <- function(input)
{
  output <- as.matrix(input != 0)
  if (sum(diag(output)) == 0)
    diag(output) <- TRUE
  
  return(output)
}

##11#
upper <- function(input)
{
  return(input[upper.tri(input, diag = FALSE)])
}

##12## False positive
FP <- function(estimatedMatrix,
               adjacentMatrix)
{
  estimatedMatrix <- properAdjacent(estimatedMatrix)
  adjacentMatrix <- properAdjacent(adjacentMatrix)
  
  estimatedMatrix <- upper(estimatedMatrix)
  adjacentMatrix <- upper(adjacentMatrix)
  
  realNegative <- max(c(sum(!(adjacentMatrix)), 1))
  FP <- sum((estimatedMatrix != 0) & !adjacentMatrix)
  FP_rate <- FP / realNegative
  
  return(list(FP, FP_rate))
}

##13## True positive
TP <- function(estimatedMatrix,
               adjacentMatrix)
{
  estimatedMatrix <- properAdjacent(estimatedMatrix)
  adjacentMatrix <- properAdjacent(adjacentMatrix)
  
  estimatedMatrix <- upper(estimatedMatrix)
  adjacentMatrix <- upper(adjacentMatrix)
  
  realPositive <- max(c(sum((adjacentMatrix)), 1))
  TP <- sum((estimatedMatrix != 0) & adjacentMatrix)
  TP_rate <- TP / realPositive
  
  return(list(TP, TP_rate))
}

##14## False negative
FN <- function(estimatedMatrix,
               adjacentMatrix)
{
  estimatedMatrix <- properAdjacent(estimatedMatrix)
  adjacentMatrix <- properAdjacent(adjacentMatrix)
  
  estimatedMatrix <- upper(estimatedMatrix)
  adjacentMatrix <- upper(adjacentMatrix)
  
  realPositive <- max(c(sum((adjacentMatrix)), 1))
  FN <- sum((estimatedMatrix == 0) & adjacentMatrix)
  FN_rate <- FN / realPositive
  
  return(list(FN, FN_rate))
}

##15## True negative

TN <- function(estimatedMatrix,
               adjacentMatrix)
{
  estimatedMatrix <- properAdjacent(estimatedMatrix)
  adjacentMatrix <- properAdjacent(adjacentMatrix)
  
  estimatedMatrix <- upper(estimatedMatrix)
  adjacentMatrix <- upper(adjacentMatrix)
  
  realNegative <- max(c(sum(!(adjacentMatrix)), 1))
  TN <- sum((estimatedMatrix == 0) & !adjacentMatrix)
  TN_rate <- TN / realNegative
  
  return(list(TN, TN_rate))
}

##16## False dicovery proportion
FDP <- function(estimatedMatrix,
                adjacentMatrix)
{
  estimatedMatrix <- properAdjacent(estimatedMatrix)
  adjacentMatrix <- properAdjacent(adjacentMatrix)
  
  predictedPositive <- max(c(sum(upper(estimatedMatrix) != 0), 1))
  
  return(FP(estimatedMatrix, adjacentMatrix)[[1]] / predictedPositive)
}

##17## Local FDP
localFDP <- function(estimatedMatrix,
                     adjacentMatrix)
{
  estimatedMatrix <- properAdjacent(estimatedMatrix)
  adjacentMatrix <- properAdjacent(adjacentMatrix)
  
  adjacentMatrixIG <- graph_from_adjacency_matrix(adjacentMatrix)
  
  for (i in seq_len(NCOL(adjacentMatrix) - 1) + 1)
  {
    for (j in seq_len(i - 1))
    {
      adjacentMatrix[j, i] <-
        !(edge_connectivity(adjacentMatrixIG, j, i) == 0)
    }
    
  }
  
  predictedPositive <- max(c(sum(upper(estimatedMatrix) != 0), 1))
  
  return(FP(estimatedMatrix, adjacentMatrix)[[1]] / predictedPositive)
  # return(adjacentMatrix)
}


##18## Sensitivity
SN <- function(estimatedMatrix,
               adjacentMatrix)
{
  estimatedMatrix <- properAdjacent(estimatedMatrix)
  adjacentMatrix <- properAdjacent(adjacentMatrix)
  
  realPositive <- max(c(sum(upper(adjacentMatrix)), 1))
  
  return(TP(estimatedMatrix, adjacentMatrix)[[1]] / realPositive)
}

##19## Specificity
SP <- function(estimatedMatrix,
               adjacentMatrix)
{
  estimatedMatrix <- properAdjacent(estimatedMatrix)
  adjacentMatrix <- properAdjacent(adjacentMatrix)
  
  realNegative <- max(c(sum(!upper(adjacentMatrix)), 1))
  
  return(TN(estimatedMatrix, adjacentMatrix)[[1]] / realNegative)
}

##########################Algorithms Functions##################################

##20##
gslope_new = function(sample_cov,
                      lambdas,
                      rho = 1.1,
                      max_iter = 500,
                      epsilon = 1e-04,
                      progress = TRUE) {
  if (!(nrow(sample_cov) == ncol(sample_cov)))
    stop("Covariance matrix must be square.")
  
  #Parameters initialization
  Z = sample_cov #Initialized to zero, probably it is the best choice
  Y = Z
  X = diag(nrow(sample_cov))
  
  #Start iteration
  if (progress) {
    for (iter in 1:max_iter) {
      C_tilde = Y - Z - (sample_cov / rho)
      
      #Perform the eigenvalue decomposition
      C_eigen = eigen(C_tilde, symmetric = TRUE)
      C_eigen_val = C_eigen$val #Eigenvalues
      C_eigen_vec = C_eigen$vec #Eigenvectors
      
      #Formula implementation
      F_rho = 0.5 * diag(C_eigen_val + sqrt(C_eigen_val ^ 2 + 4 / rho))
      X    = C_eigen_vec %*% F_rho %*% t(C_eigen_vec)
      
      Y_old = Y
      Y = proximity_matrix(X + Z, lambdas / rho)
      
      #Update step
      Z = Z + rho * (X - Y)
      
      #Compute the primal and dual gap
      primal_residual = norm(X - Y, type = "F")
      dual_residual = norm(rho * (Y - Y_old), type = "F")
      
      print(paste0("Current progress Gslope: ", (iter / max_iter) * 100))
      
      #Stop condition
      if (primal_residual < epsilon & dual_residual < epsilon) {
        break
      }
    }
  }
  
  else{
    for (iter in 1:max_iter) {
      C_tilde = Y - Z - (sample_cov / rho)
      
      #Perform the eigenvalue decomposition
      C_eigen = eigen(C_tilde, symmetric = TRUE)
      C_eigen_val = C_eigen$val #Eigenvalues
      C_eigen_vec = C_eigen$vec #Eigenvectors
      
      #Formula implementation
      F_rho = 0.5 * diag(C_eigen_val + sqrt(C_eigen_val ^ 2 + 4 / rho))
      X    = C_eigen_vec %*% F_rho %*% t(C_eigen_vec)
      
      Y_old = Y
      Y = proximity_matrix(X + Z, lambdas / rho)
      
      #Update step
      Z = Z + rho * (X - Y)
      
      #Compute the primal and dual gap
      primal_residual = norm(X - Y, type = "F")
      dual_residual = norm(rho * (Y - Y_old), type = "F")
      
      #Stop condition
      if (primal_residual < epsilon & dual_residual < epsilon) {
        break
      }
    }
  }
  
  
  X[abs(X) < 1e-04] <- 0 #Thresholding if abs(entries) <= 10e-4
  
  list(
    precision_matrix = X,
    iterations = iter,
    prim_res   = primal_residual,
    dual_res   = dual_residual
  )
}

##21##
tlasso_fast_rolling <-
  function(Y,
           lambda,
           v,
           toler,
           symmetric ,
           init_theta = NULL,
           penalize = F) {
    t = dim(Y)[1]
    p = dim(Y)[2]
    
    
    #Initialization of S,theta, tau, mu and convergence vector
    S = cov(Y) * (v / (v - 2))
    
    #If an initial value is not provided, use inverse of sample covariance
    if (is.null(init_theta)) {
      theta = solve(S)
    } else {
      theta = init_theta
    }
    tau = rep(0, t)
    mu = apply(Y, 2, mean)
    conv_hist = vector(mode = "numeric", 100)
    conv_hist[1] = 1000
    
    # loop E and M steps
    i = 1
    while (conv_hist[i] > toler &&
           i < 50) {
      # stop when convergence is reached or after 50 iterations
      #E step
      for (j in 1:t) {
        tau[j] = (v + p) / (t(Y[j, ] - mu) %*% theta %*% (Y[j, ] - mu) + v)
      }
      #M step
      mu = (t(Y) %*% tau) / sum(tau)
      mu_large = t(matrix(rep(mu, t), p, t))
      tau_large = matrix(rep(tau, p), t, p)
      S = t(tau_large * (Y - mu_large)) %*% (Y - mu_large) / t
      theta_old = theta
      
      output_glasso = glasso(cov2cor(S), lambda, penalize.diagonal = penalize)
      
      # force symmetry (required sometimes for numerical issues)
      if (symmetric == TRUE) {
        theta = 0.5 * (output_glasso$wi + t(output_glasso$wi))
        S = output_glasso$w
      } else {
        theta     = output_glasso$wi
        S         = solve(theta)
        S         = cor2cov(Y, S)
        theta     = invcor2invcov(Y, output_glasso$wi)
        
      }
      i = i + 1
      conv_hist[i] = max(abs(theta - theta_old))
    }
    
    output = list(theta, S, tau, mu, conv_hist)
    names(output) = paste(list("theta", "S", "tau", "mu", "conv_hist"))
    
    return(output)
  }

##22##
tslope_fast_rolling <-
  function(Y,
           v,
           toler,
           symmetric ,
           init_theta = NULL,
           factor = 1,
           alpha_value = 0.05) {
    t = dim(Y)[1]
    p = dim(Y)[2]
    
    #Initialization of S,theta, tau, mu and convergence vector
    S = cov(Y) * (v / (v - 2))
    lambdas = create_lambda(cov2cor(S), n = t, alpha = alpha_value) * factor
    
    #If an initial value is not provided, use inverse of sample covariance
    if (is.null(init_theta)) {
      theta = solve(S)
      
    } else {
      theta = init_theta
    }
    tau = rep(0, t)
    mu = apply(Y, 2, mean)
    conv_hist = vector(mode = "numeric", 100)
    conv_hist[1] = 1000
    
    # loop E and M steps
    i = 1
    while (conv_hist[i] > toler &&
           i < 50) {
      # stop when convergence is reached or after 50 iterations
      #E step
      for (j in 1:t) {
        tau[j] = (v + p) / (t(Y[j, ] - mu) %*% theta %*% (Y[j, ] - mu) + v)
      }
      #M step
      mu = (t(Y) %*% tau) / sum(tau)
      mu_large = t(matrix(rep(mu, t), p, t))
      tau_large = matrix(rep(tau, p), t, p)
      S = t(tau_large * (Y - mu_large)) %*% (Y - mu_large) / t
      theta_old = theta
      output_gslope = gslope_new(
        cov2cor(S),
        lambdas,
        rho = 1.1,
        max_iter = 500,
        epsilon = 1e-04,
        progress = FALSE
      )
      
      # force symmetry (required sometimes for numerical issues)
      if (symmetric == TRUE) {
        theta = 0.5 * (output_gslope$precision_matrix + t(output_gslope$precision_matrix))
        S     = solve(theta)
        
      } else {
        theta     = output_gslope$precision_matrix
        S         = solve(theta)
        S         = cor2cov(Y, S)
        theta     = invcor2invcov(Y, output_gslope$precision_matrix)
        
      }
      print(paste0("Iteration Tslope: ", (i)))
      i = i + 1
      conv_hist[i] = max(abs(theta - theta_old))
    }
    
    output = list(theta, S, tau, mu, conv_hist)
    names(output) = paste(list("theta", "S", "tau", "mu", "conv_hist"))
    
    return(output)
  }

##23##
tslope_fast_reg <-
  function(Y,
           v,
           toler,
           symmetric ,
           init_theta = NULL,
           lambdas) {
    t = dim(Y)[1]
    p = dim(Y)[2]
    
    #Initialization of S,theta, tau, mu and convergence vector
    S = cov(Y) * (v / (v - 2))
    
    #If an initial value is not provided, use inverse of sample covariance
    if (is.null(init_theta)) {
      theta = solve(S)
      
    } else {
      theta = init_theta
    }
    
    tau = rep(0, t)
    mu = apply(Y, 2, mean)
    conv_hist = vector(mode = "numeric", 100)
    conv_hist[1] = 1000
    
    #Loop E and M steps
    i = 1
    while (conv_hist[i] > toler &&
           i < 50) {
      # stop when convergence is reached or after 50 iterations
      #E step
      for (j in 1:t) {
        tau[j] = (v + p) / (t(Y[j, ] - mu) %*% theta %*% (Y[j, ] - mu) + v)
      }
      #M step
      mu = (t(Y) %*% tau) / sum(tau)
      mu_large = t(matrix(rep(mu, t), p, t))
      tau_large = matrix(rep(tau, p), t, p)
      S = t(tau_large * (Y - mu_large)) %*% (Y - mu_large) / t
      theta_old = theta
      
      output_gslope = gslope_new(
        cov2cor(S),
        lambdas,
        rho = 1.1,
        max_iter = 500,
        epsilon = 1e-04
      )
      # force symmetry (required sometimes for numerical issues)
      if (symmetric == TRUE) {
        theta = 0.5 * (output_gslope$precision_matrix + t(output_gslope$precision_matrix))
        S     = solve(theta)
        
      } else {
        theta     = output_gslope$precision_matrix
        S         = solve(theta)
        S         = cor2cov(Y, S)
        theta     = invcor2invcov(Y, output_gslope$precision_matrix)
        
      }
      i = i + 1
      conv_hist[i] = max(abs(theta - theta_old))
    }
    
    output = list(theta, S, tau, mu, conv_hist)
    names(output) = paste(list("theta", "S", "tau", "mu", "conv_hist"))
    
    return(output)
  }

##############################Simulations#######################################

#Function to simulate the ORACLE precision matrix using the HUGE package

#INPUT
#
# 1)N: int, sample size
# 2)p: int, number of components
# 3)graph_type: string, ["cluster", "random", "hub", "scale-free"]
# 4)v: int
# 5)u: int
# 6)number_clusters: useful if you choose to simulate the cluster structure,
#   you can set how many clusters to have
# 7)sparsity_level: range[0,1], it is the probability that a given node connects
#   to another node (lower probability = sparse graph)

#OUTPUT
#
# 1)sigma: the oracle covariance matrix
# 2)omega: the oracle precision matrix
# 3)rho: the correlation matrix

##24##
oracle_precision_matrix_generation <-
  function(N,
           p,
           graph_type,
           v,
           u,
           number_clusters = 10,
           sparsity_level = NULL)
  {
    sim <-
      huge.generator(
        n = N,
        d = p,
        graph = graph_type,
        v = v,
        u = u,
        g = number_clusters,
        prob = sparsity_level,
        vis = FALSE,
        verbose = FALSE
      )
    
    data = sim$data
    
    #last term is = v in huge.generator
    omega        = (sim$theta) * v
    #last term is = u in huge.generator
    diag(omega)  = abs(min(eigen(omega)$values)) + 0.1 + u
    
    sigma        = solve(omega) #oracle covariance
    
    omega        = solve(sigma) #oracle precision
    omega[abs(omega) < 1e-04] <- 0
    
    rho  = cov2cor(sigma) #correlation equal to sim$sigma, well done!
    
    output <- list(sigma, omega, rho, data)
    names(output) <- paste(list("sigma", "omega", "rho", "data"))
    
    return(output)
    
  }

# Function to generate samples from a multivariate Normal or T-student
# distributions

#INPUT
#
# 1)N: int, sample size
# 2)sigma: matrix, oracle covariance matrix
# 3)mean: numeric, value of the mean
# 4)df: int, degrees of freedom for T_distribution
# 5)T_student: logic, if TRUE the function generates sample from a multivariate
#   T_student distribution

#OUTPUT
#
#A]If T_student = TRUE
# the function returns:
#   1)data series
#   2)covariance matrix sigma_T
#   3)precision matrix omega_T
#
#B]else
#  returns the samples extracted from a multivariate normal distribution

##25##
data_series_generation <-
  function(N,
           sigma,
           mean,
           df,
           T_student = TRUE) {
    mu    <- rep(mean, nrow(sigma))
    if (T_student == FALSE) {
      sim_N <- mvrnorm(N, mu, sigma)
      return(sim_N)
      
    } else{
      sim_T <- rmvt(N, delta = mu, ((df / (df - 2))) * sigma, df)
      
      sigma_T <- (df / (df - 2)) * sigma
      omega_T <- solve(sigma_T)
      omega_T[abs(omega_T) < 1e-04] <- 0
      output = list(sim_T, sigma_T, omega_T)
      names(output) <- paste(list("data", "sigma_T", "omega_T"))
      return(output)
    }
    
  }

#Function to run Glasso, Tlasso, Gslope and Tslope to estimate the precision
#matrix given the random samples
#It estimates also the sample precision and the Ledoit&Wolf precision matrix
#INPUT
#
# 1)data: the data series
# 2)n: sample size
# 3)alpha_level: float, confidence level for lambda selection
# 4)df: int, degrees of freedom for T_distribution

#OUTPUT
#
#It returns all the estimated covariance and precision matrices

##26##
sample_precision_matrix_estimation <-
  function(data, n, alpha_level, df, progress = TRUE) {
    #SAMPLE
    sigma_sample = cov(data)
    rho_sample   = cov2cor(sigma_sample)
    inv_sample = inv(sigma_sample)
    
    #LEDOIT
    sigma_ledoit = nlshrink_cov(data)
    rho_ledoit   = cov2cor(sigma_ledoit)
    inv_ledoit   = inv(sigma_ledoit)
    
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
    output_elastic = gelnet(S = cor(data),
                            lambda = lambda,
                            alpha = 0.5)
    
    #From corr matrix to covariance, then precision
    sigma_elastic = cor2cov(data, output_elastic$W)
    inv_elastic   = solve(sigma_elastic)
    inv_elastic[abs(inv_elastic) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #GLASSO RIDGE (ROPE)
    output_rope = gelnet(S = cor(data),
                         lambda = lambda,
                         alpha = 0)
    
    #From corr matrix to covariance, then precision
    sigma_rope = cor2cov(data, output_rope$W)
    inv_rope   = solve(sigma_rope)
    inv_rope[abs(inv_rope) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #TLASSO
    v = df
    output_tlasso = tlasso_fast_rolling(data, lambda, v, 0.001, F)
    
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
    output_tslope = tslope_fast_rolling(data, v, 0.001, F, alpha_value = alpha_level)
    sigma_tslope  = (output_tslope$S) #* (v/(v - 2))
    inv_tslope    = solve(sigma_tslope)
    inv_tslope[abs(inv_tslope) < 1e-04] <- 0
    
    #TOTAL OUTPUT
    output = list(
      sigma_sample,
      sigma_ledoit,
      sigma_glasso,
      sigma_elastic,
      sigma_rope,
      sigma_tlasso,
      sigma_gslope,
      sigma_tslope,
      inv_sample,
      inv_ledoit,
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
          "sigma_sample",
          "sigma_ledoit",
          "sigma_glasso",
          "sigma_elastic",
          "sigma_rope",
          "sigma_tlasso",
          "sigma_gslope",
          "sigma_tslope",
          "inv_sample",
          "inv_ledoit",
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

##27##
sample_precision_matrix_estimation_normal <-
  function(data, n, alpha_level, progress = TRUE) {
    #SAMPLE
    sigma_sample = cov(data)
    rho_sample   = cov2cor(sigma_sample)
    inv_sample = inv(sigma_sample)
    
    #LEDOIT
    sigma_ledoit = nlshrink_cov(data)
    rho_ledoit   = cov2cor(sigma_ledoit)
    inv_ledoit   = inv(sigma_ledoit)
    
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
    output_elastic = gelnet(S = cor(data),
                            lambda = lambda,
                            alpha = 0.5)
    
    #From corr matrix to covariance, then precision
    sigma_elastic = cor2cov(data, output_elastic$W)
    inv_elastic   = solve(sigma_elastic)
    inv_elastic[abs(inv_elastic) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #GLASSO RIDGE (ROPE)
    output_rope = gelnet(S = cor(data),
                         lambda = lambda,
                         alpha = 0)
    
    #From corr matrix to covariance, then precision
    sigma_rope = cor2cov(data, output_rope$W)
    inv_rope   = solve(sigma_rope)
    inv_rope[abs(inv_rope) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #GSLOPE
    lambdas = create_lambda(cor(data), n = n, alpha = alpha_level)
    
    output_gslope = gslope_new(cor(data), lambdas, progress = progress)
    
    #From corr matrix to covariance, then precision
    inv_gslope  = invcor2invcov(data, output_gslope$precision_matrix)
    sigma_gslope    = solve(inv_gslope)
    inv_gslope[abs(inv_gslope) < 1e-04] <-
      0 #threshold to consider an entry equal to zero
    
    #TSLOPE
    output_tslope = tslope_fast_rolling(data, v, 0.001, F, alpha_value = alpha_level)
    sigma_tslope  = (output_tslope$S) #* (v/(v - 2))
    inv_tslope    = solve(sigma_tslope)
    inv_tslope[abs(inv_tslope) < 1e-04] <- 0
    
    #TOTAL OUTPUT
    output = list(
      sigma_sample,
      sigma_ledoit,
      sigma_glasso,
      sigma_elastic,
      sigma_rope,
      sigma_gslope,
      sigma_tslope,
      inv_sample,
      inv_ledoit,
      inv_glasso,
      inv_elastic,
      inv_rope,
      inv_gslope,
      inv_tslope,
    )
    
    names(output) <-
      paste(
        list(
          "sigma_sample",
          "sigma_ledoit",
          "sigma_glasso",
          "sigma_elastic",
          "sigma_rope",
          "sigma_gslope",
          "sigma_tslope",
          "inv_sample",
          "inv_ledoit",
          "inv_glasso",
          "inv_elastic",
          "inv_rope",
          "inv_gslope",
          "inv_tslope",
        )
      )
    
    return(output)
  }

#Function to compute several performance measures to compare the ORACLE
#precision with the estimated one

#INPUT
# 1)omega: the oracle precision matrix
# ...And all the estimated precision matrix

#OUTPUT
#
#Different performance measures

##28##
performance_measures_OLD_With_Ledoit <- function(omega,
                                 inv_sample,
                                 inv_ledoit,
                                 inv_glasso,
                                 inv_elastic,
                                 inv_rope,
                                 inv_tlasso,
                                 inv_gslope,
                                 inv_tslope) {
  #CONDITION NUMBER
  condition = list(
    cond(inv_sample),
    cond(inv_ledoit),
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
        "cond_sample",
        "cond_ledoit",
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
    norm(omega - inv_sample, type = "F"),
    norm(omega - inv_ledoit, type = "F"),
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
        "frob_sample",
        "frob_ledoit",
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
    TP(inv_sample, omega)[[2]],
    TP(inv_ledoit, omega)[[2]],
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
        "TP_sample",
        "TP_ledoit",
        "TP_glasso",
        "TP_elastic",
        "TP_rope",
        "TP_tlasso",
        "TP_gslope",
        "TP_tslope"
      )
    )
  
  TNrate = list(
    TN(inv_sample, omega)[[2]],
    TN(inv_ledoit, omega)[[2]],
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
        "TN_sample",
        "TN_ledoit",
        "TN_glasso",
        "TN_elastic",
        "TN_rope",
        "TN_tlasso",
        "TN_gslope",
        "TN_tslope"
      )
    )
  
  FPrate = list(
    FP(inv_sample, omega)[[2]],
    FP(inv_ledoit, omega)[[2]],
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
        "FP_sample",
        "FP_ledoit",
        "FP_glasso",
        "FP_elastic",
        "FP_rope",
        "FP_tlasso",
        "FP_gslope",
        "FP_tslope"
      )
    )
  
  FNrate = list(
    FN(inv_sample, omega)[[2]],
    FN(inv_ledoit, omega)[[2]],
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
        "FN_sample",
        "FN_ledoit",
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
    TP(inv_sample, omega)[[1]] / (TP(inv_sample, omega)[[1]] + (0.5 * (
      FP(inv_sample, omega)[[1]] + FN(inv_sample, omega)[[1]]
    ))),
    TP(inv_ledoit, omega)[[1]] / (TP(inv_ledoit, omega)[[1]] + (0.5 * (
      FP(inv_ledoit, omega)[[1]] + FN(inv_ledoit, omega)[[1]]
    ))),
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
        "F1_sample",
        "F1_ledoit",
        "F1_glasso",
        "F1_elastic",
        "F1_rope",
        "F1_tlasso",
        "F1_gslope",
        "F1_tslope"
      )
    )
  
  #ACCURACY
  positive <- max(c(sum(upper(
    properAdjacent(omega)
  )), 1))
  p = nrow(omega)
  negative <- max(c(sum(!upper(
    properAdjacent(omega)
  )), 1))
  
  ACC = list(
    (TP(inv_sample, omega)[[1]] + (TN(inv_sample, omega)[[1]])) / (positive + negative),
    (TP(inv_ledoit, omega)[[1]] + (TN(inv_ledoit, omega)[[1]])) / (positive + negative),
    (TP(inv_glasso, omega)[[1]] + (TN(inv_glasso, omega)[[1]])) / (positive + negative),
    (TP(inv_elastic, omega)[[1]] + (TN(
      inv_elastic, omega
    )[[1]])) / (positive + negative),
    (TP(inv_rope, omega)[[1]] + (TN(inv_rope, omega)[[1]])) / (positive + negative),
    (TP(inv_tlasso, omega)[[1]] + (TN(inv_tlasso, omega)[[1]])) / (positive + negative),
    (TP(inv_gslope, omega)[[1]] + (TN(inv_gslope, omega)[[1]])) / (positive + negative),
    (TP(inv_tslope, omega)[[1]] + (TN(inv_tslope, omega)[[1]])) / (positive + negative)
  )
  
  names(ACC) <-
    paste(
      list(
        "ACC_sample",
        "ACC_ledoit",
        "ACC_glasso",
        "ACC_elastic",
        "ACC_rope",
        "ACC_tlasso",
        "ACC_gslope",
        "ACC_tslope"
      )
    )
  
  entropy = list(
    sum(diag(omega %*% inv_sample)) - log(det(omega %*% inv_sample)) - p,
    sum(diag(omega %*% inv_ledoit)) - log(det(omega %*% inv_ledoit)) - p,
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
        "entropy_sample",
        "entropy_ledoit",
        "entropy_glasso",
        "entropy_elastic",
        "entropy_rope",
        "entropy_tlasso",
        "entropy_gslope",
        "entropy_tslope"
      )
    )
  
  FDR = list(
    FDP(inv_sample, omega),
    FDP(inv_ledoit, omega),
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
        "FDP_sample",
        "FDP_ledoit",
        "FDP_glasso",
        "FDP_elastic",
        "FDP_rope",
        "FDP_tlasso",
        "FDP_gslope",
        "FDP_tslope"
      )
    )
  
  localFDR = list(
    localFDP(properAdjacent(inv_sample), properAdjacent(omega)),
    localFDP(properAdjacent(inv_ledoit), properAdjacent(omega)),
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
        "localFDP_sample",
        "localFDP_ledoit",
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

performance_measures <- function(omega,
                                 inv_sample,
                                 inv_glasso,
                                 inv_elastic,
                                 inv_rope,
                                 inv_tlasso,
                                 inv_gslope,
                                 inv_tslope) {
  #CONDITION NUMBER
  condition = list(
    cond(inv_sample),
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
        "cond_sample",
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
    norm(omega - inv_sample, type = "F"),
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
        "frob_sample",
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
    TP(inv_sample, omega)[[2]],
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
        "TP_sample",
        "TP_glasso",
        "TP_elastic",
        "TP_rope",
        "TP_tlasso",
        "TP_gslope",
        "TP_tslope"
      )
    )
  
  TNrate = list(
    TN(inv_sample, omega)[[2]],
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
        "TN_sample",
        "TN_glasso",
        "TN_elastic",
        "TN_rope",
        "TN_tlasso",
        "TN_gslope",
        "TN_tslope"
      )
    )
  
  FPrate = list(
    FP(inv_sample, omega)[[2]],
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
        "FP_sample",
        "FP_glasso",
        "FP_elastic",
        "FP_rope",
        "FP_tlasso",
        "FP_gslope",
        "FP_tslope"
      )
    )
  
  FNrate = list(
    FN(inv_sample, omega)[[2]],
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
        "FN_sample",
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
    TP(inv_sample, omega)[[1]] / (TP(inv_sample, omega)[[1]] + (0.5 * (
      FP(inv_sample, omega)[[1]] + FN(inv_sample, omega)[[1]]
    ))),
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
        "F1_sample",
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
    (TP(inv_sample, omega)[[1]] + (TN(inv_sample, omega)[[1]])) / (positive + negative),
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
        "ACC_sample",
        "ACC_glasso",
        "ACC_elastic",
        "ACC_rope",
        "ACC_tlasso",
        "ACC_gslope",
        "ACC_tslope"
      )
    )
  
  entropy = list(
    sum(diag(omega %*% inv_sample)) - log(det(omega %*% inv_sample)) - p,
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
        "entropy_sample",
        "entropy_glasso",
        "entropy_elastic",
        "entropy_rope",
        "entropy_tlasso",
        "entropy_gslope",
        "entropy_tslope"
      )
    )
  
  FDR = list(
    FDP(inv_sample, omega),
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
        "FDP_sample",
        "FDP_glasso",
        "FDP_elastic",
        "FDP_rope",
        "FDP_tlasso",
        "FDP_gslope",
        "FDP_tslope"
      )
    )
  
  localFDR = list(
    localFDP(properAdjacent(inv_sample), properAdjacent(omega)),
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
        "localFDP_sample",
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

#Function to compute portfolio performance (oracle, empirical and actual risk)
#INPUT
# 1)omega: the oracle precision matrix
# ...And all the estimated precision matrix

#OUTPUT
#
#Oracle, empirical and actual risk

##29.A##
portfolio_risk_measures <- function(data_series,
                                    omega,
                                    sigma,
                                    sigma_sample,
                                    sigma_ledoit,
                                    sigma_glasso,
                                    sigma_elastic,
                                    sigma_rope,
                                    sigma_tlasso,
                                    sigma_gslope,
                                    sigma_tslope,
                                    inv_sample,
                                    inv_ledoit,
                                    inv_glasso,
                                    inv_elastic,
                                    inv_rope,
                                    inv_tlasso,
                                    inv_gslope,
                                    inv_tslope) {
  data = data_series
  p = nrow(omega)
  
  #ORACLE PORTFOLIO
  ones  <- rep.int(1, p)
  wa <- (omega %*% ones) #Numerator
  wb <- (t(ones) %*% omega %*% ones) #Denominator
  
  w_oracle  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_oracle[i] <- wa[i] / wb
    
    if (w_oracle[i] < 1 / p) {
      w_oracle[i] = 0
    }
  }
  
  oracle_risk <- sqrt(t(w_oracle) %*% sigma %*% w_oracle)
  
  #SAMPLE RISK
  wa <- (inv_sample %*% ones) #Numerator
  wb <- (t(ones) %*% inv_sample %*% ones) #Denominator
  
  w_sample  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_sample[i] <- wa[i] / wb
    
    if (w_sample[i] < 1 / p) {
      w_sample[i] = 0
    }
  }
  
  mean_sample <- t(w_sample) %*% colMeans(data)
  sample_empirical_risk <-
    sqrt(t(w_sample) %*% sigma_sample %*% w_sample)
  sample_actual_risk   <-
    sqrt(t(w_sample) %*% sigma %*% w_sample)
  SR_sample <- mean_sample / sample_empirical_risk
  
  #LEDOIT RISK
  wa <- (inv_ledoit %*% ones) #Numerator
  wb <- (t(ones) %*% inv_ledoit %*% ones) #Denominator
  
  w_ledoit  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_ledoit[i] <- wa[i] / wb
    
    if (w_ledoit[i] < 1 / p) {
      w_ledoit[i] = 0
    }
    
  }
  
  mean_ledoit <- t(w_ledoit) %*% colMeans(data)
  ledoit_empirical_risk <-
    sqrt(t(w_ledoit) %*% sigma_ledoit %*% w_ledoit)
  ledoit_actual_risk   <-
    sqrt(t(w_ledoit) %*% sigma %*% w_ledoit)
  SR_ledoit <- mean_ledoit / ledoit_empirical_risk
  
  #GLASSO RISK
  wa <- (inv_glasso %*% ones) #Numerator
  wb <- (t(ones) %*% inv_glasso %*% ones) #Denominator
  
  w_glasso  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_glasso[i] <- wa[i] / wb
    
    if (w_glasso[i] < 1 / p) {
      w_glasso[i] = 0
    }
    
  }
  
  mean_glasso <- t(w_glasso) %*% colMeans(data)
  glasso_empirical_risk <-
    sqrt(t(w_glasso) %*% sigma_glasso %*% w_glasso)
  glasso_actual_risk   <-
    sqrt(t(w_glasso) %*% sigma %*% w_glasso)
  SR_glasso <- mean_glasso / glasso_empirical_risk
  
  #ELASTIC RISK
  wa <- (inv_elastic %*% ones) #Numerator
  wb <- (t(ones) %*% inv_elastic %*% ones) #Denominator
  
  w_elastic  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_elastic[i] <- wa[i] / wb
    
    if (w_elastic[i] < 1 / p) {
      w_elastic[i] = 0
    }
  }
  
  mean_elastic <- t(w_elastic) %*% colMeans(data)
  elastic_empirical_risk <-
    sqrt(t(w_elastic) %*% sigma_elastic %*% w_elastic)
  elastic_actual_risk   <-
    sqrt(t(w_elastic) %*% sigma %*% w_elastic)
  SR_elastic <- mean_elastic / elastic_empirical_risk
  
  #ROPE RISK
  wa <- (inv_rope %*% ones) #Numerator
  wb <- (t(ones) %*% inv_rope %*% ones) #Denominator
  
  w_rope  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_rope[i] <- wa[i] / wb
    
    if (w_rope[i] < 1 / p) {
      w_rope[i] = 0
    }
  }
  
  mean_rope <- t(w_rope) %*% colMeans(data)
  rope_empirical_risk <-
    sqrt(t(w_rope) %*% sigma_rope %*% w_rope)
  rope_actual_risk   <-
    sqrt(t(w_rope) %*% sigma %*% w_rope)
  SR_rope <- mean_rope / rope_actual_risk
  
  #TLASSO RISK
  wa <- (inv_tlasso %*% ones) #Numerator
  wb <- (t(ones) %*% inv_tlasso %*% ones) #Denominator
  
  w_tlasso  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_tlasso[i] <- wa[i] / wb
    
    if (w_tlasso[i] < 1 / p) {
      w_tlasso[i] = 0
    }
  }
  
  mean_tlasso <- t(w_tlasso) %*% colMeans(data)
  tlasso_empirical_risk <-
    sqrt(t(w_tlasso) %*% sigma_tlasso %*% w_tlasso)
  tlasso_actual_risk   <-
    sqrt(t(w_tlasso) %*% sigma %*% w_tlasso)
  SR_tlasso <- mean_tlasso / tlasso_empirical_risk
  
  #GSLOPE RISK
  wa <- (inv_gslope %*% ones) #Numerator
  wb <- (t(ones) %*% inv_gslope %*% ones) #Denominator
  
  w_gslope  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_gslope[i] <- wa[i] / wb
    
    if (w_gslope[i] < 1 / p) {
      w_gslope[i] = 0
    }
  }
  
  mean_gslope <- t(w_gslope) %*% colMeans(data)
  gslope_empirical_risk <-
    sqrt(t(w_gslope) %*% sigma_gslope %*% w_gslope)
  gslope_actual_risk   <-
    sqrt(t(w_gslope) %*% sigma %*% w_gslope)
  SR_gslope <- mean_gslope / gslope_empirical_risk
  
  #TSLOPE RISK
  wa <- (inv_tslope %*% ones) #Numerator
  wb <- (t(ones) %*% inv_tslope %*% ones) #Denominator
  
  w_tslope  <- rep.int(0, p)
  
  for (i in 1:p) {
    w_tslope[i] <- wa[i] / wb
    
    if (w_tslope[i] < 1 / p) {
      w_tslope[i] = 0
    }
  }
  
  mean_tslope <- t(w_tslope) %*% colMeans(data)
  tslope_empirical_risk <-
    sqrt(t(w_tslope) %*% sigma_tslope %*% w_tslope)
  tslope_actual_risk   <-
    sqrt(t(w_tslope) %*% sigma %*% w_tslope)
  SR_tslope <- mean_tslope / tslope_empirical_risk
  
  #ORACLE RISK
  oracle = list(
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk
  )
  
  names(oracle) <-
    paste(
      list(
        "oracle_sample",
        "oracle_ledoit",
        "oracle_glasso",
        "oracle_elastic",
        "oracle_rope",
        "oracle_tlasso",
        "oracle_gslope",
        "oracle_tslope"
      )
    )
  
  #EMPIRICAL RISK
  empirical = list(
    sample_empirical_risk,
    ledoit_empirical_risk,
    glasso_empirical_risk,
    elastic_empirical_risk,
    rope_empirical_risk,
    tlasso_empirical_risk,
    gslope_empirical_risk,
    tslope_empirical_risk
  )
  
  names(empirical) <-
    paste(
      list(
        "empirical_sample",
        "empirical_ledoit",
        "empirical_glasso",
        "empirical_elastic",
        "empirical_rope",
        "empirical_tlasso",
        "empirical_gslope",
        "empirical_tslope"
      )
    )
  
  #ACTUAL RISK
  actual = list(
    sample_actual_risk,
    ledoit_actual_risk,
    glasso_actual_risk,
    elastic_actual_risk,
    rope_actual_risk,
    tlasso_actual_risk,
    gslope_actual_risk,
    tslope_actual_risk
  )
  
  names(actual) <-
    paste(
      list(
        "actual_sample",
        "actual_ledoit",
        "actual_glasso",
        "actual_elastic",
        "actual_rope",
        "actual_tlasso",
        "actual_gslope",
        "actual_tslope"
      )
    )
  
  #MEAN
  means = list(
    mean_sample,
    mean_ledoit,
    mean_glasso,
    mean_elastic,
    mean_rope,
    mean_tlasso,
    mean_gslope,
    mean_tslope
  )
  
  names(means) <-
    paste(
      list(
        "mean_sample",
        "mean_ledoit",
        "mean_glasso",
        "mean_elastic",
        "mean_rope",
        "mean_tlasso",
        "mean_gslope",
        "mean_tslope"
      )
    )

  #SR
  SR = list(
    SR_sample,
    SR_ledoit,
    SR_glasso,
    SR_elastic,
    SR_rope,
    SR_tlasso,
    SR_gslope,
    SR_tslope
  )
  
  names(SR) <-
    paste(
      list(
        "SR_sample",
        "SR_ledoit",
        "SR_glasso",
        "SR_elastic",
        "SR_rope",
        "SR_tlasso",
        "SR_gslope",
        "SR_tslope"
      )
    )
  
  #Weights
  weights = list(
    w_oracle,
    w_sample,
    w_ledoit,
    w_glasso,
    w_elastic,
    w_rope,
    w_tlasso,
    w_gslope,
    w_tslope
  )
  
  names(weights) <-
    paste(
      list(
        "w_oracle",
        "w_sample",
        "w_ledoit",
        "w_glasso",
        "w_elastic",
        "w_rope",
        "w_tlasso",
        "w_gslope",
        "w_tslope"
      )
    )
  
  output <-
    list(oracle, empirical, actual, means, SR, weights)
  
  names(output) <-
    paste(list("oracle_risk", "empirical_risk", "actual_risk", "means", "SR", "weights"))
  
  return(output)
}

##29.B##
portfolio_risk_measures_N_less_p <- function(data_series,
                                    omega,
                                    sigma,
                                    sigma_ledoit,
                                    sigma_glasso,
                                    sigma_elastic,
                                    sigma_rope,
                                    sigma_tlasso,
                                    sigma_gslope,
                                    sigma_tslope,
                                    inv_ledoit,
                                    inv_glasso,
                                    inv_elastic,
                                    inv_rope,
                                    inv_tlasso,
                                    inv_gslope,
                                    inv_tslope) {
  data = data_series
  p = nrow(omega)
  
  #ORACLE PORTFOLIO
  ones  <- rep.int(1, p)
  wa <- (omega %*% ones) #Numerator
  wb <- (t(ones) %*% omega %*% ones) #Denominator
  
  w_oracle  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_oracle[i] <- wa[i] / wb
    
    if (w_oracle[i] < 1 / p) {
      w_oracle[i] = 0
    }
  }
  
  oracle_risk <- sqrt(t(w_oracle) %*% sigma %*% w_oracle)
  
  #LEDOIT RISK
  wa <- (inv_ledoit %*% ones) #Numerator
  wb <- (t(ones) %*% inv_ledoit %*% ones) #Denominator
  
  w_ledoit  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_ledoit[i] <- wa[i] / wb
    
    if (w_ledoit[i] < 1 / p) {
      w_ledoit[i] = 0
    }
  }
  
  mean_ledoit <- t(w_ledoit) %*% colMeans(data)
  ledoit_empirical_risk <-
    sqrt(t(w_ledoit) %*% sigma_ledoit %*% w_ledoit)
  ledoit_actual_risk   <-
    sqrt(t(w_ledoit) %*% sigma %*% w_ledoit)
  SR_ledoit <- mean_ledoit / ledoit_empirical_risk
  
  #GLASSO RISK
  wa <- (inv_glasso %*% ones) #Numerator
  wb <- (t(ones) %*% inv_glasso %*% ones) #Denominator
  
  w_glasso  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_glasso[i] <- wa[i] / wb
    
    if (w_glasso[i] < 1 / p) {
      w_glasso[i] = 0
    }
  }
  
  mean_glasso <- t(w_glasso) %*% colMeans(data)
  glasso_empirical_risk <-
    sqrt(t(w_glasso) %*% sigma_glasso %*% w_glasso)
  glasso_actual_risk   <-
    sqrt(t(w_glasso) %*% sigma %*% w_glasso)
  SR_glasso <- mean_glasso / glasso_empirical_risk
  
  #ELASTIC RISK
  wa <- (inv_elastic %*% ones) #Numerator
  wb <- (t(ones) %*% inv_elastic %*% ones) #Denominator
  
  w_elastic  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_elastic[i] <- wa[i] / wb
    
    if (w_elastic[i] < 1 / p) {
      w_elastic[i] = 0
    }
  }
  
  mean_elastic <- t(w_elastic) %*% colMeans(data)
  elastic_empirical_risk <-
    sqrt(t(w_elastic) %*% sigma_elastic %*% w_elastic)
  elastic_actual_risk   <-
    sqrt(t(w_elastic) %*% sigma %*% w_elastic)
  SR_elastic <- mean_elastic / elastic_empirical_risk
  
  #ROPE RISK
  wa <- (inv_rope %*% ones) #Numerator
  wb <- (t(ones) %*% inv_rope %*% ones) #Denominator
  
  w_rope  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_rope[i] <- wa[i] / wb
    
    if (w_rope[i] < 1 / p) {
      w_rope[i] = 0
    }
  }
  
  mean_rope <- t(w_rope) %*% colMeans(data)
  rope_empirical_risk <-
    sqrt(t(w_rope) %*% sigma_rope %*% w_rope)
  rope_actual_risk   <-
    sqrt(t(w_rope) %*% sigma %*% w_rope)
  SR_rope <- mean_rope / rope_actual_risk
  
  #TLASSO RISK
  wa <- (inv_tlasso %*% ones) #Numerator
  wb <- (t(ones) %*% inv_tlasso %*% ones) #Denominator
  
  w_tlasso  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_tlasso[i] <- wa[i] / wb
    
    if (w_tlasso[i] < 1 / p) {
      w_tlasso[i] = 0
    }
  }
  
  mean_tlasso <- t(w_tlasso) %*% colMeans(data)
  tlasso_empirical_risk <-
    sqrt(t(w_tlasso) %*% sigma_tlasso %*% w_tlasso)
  tlasso_actual_risk   <-
    sqrt(t(w_tlasso) %*% sigma %*% w_tlasso)
  SR_tlasso <- mean_tlasso / tlasso_empirical_risk
  
  #GSLOPE RISK
  wa <- (inv_gslope %*% ones) #Numerator
  wb <- (t(ones) %*% inv_gslope %*% ones) #Denominator
  
  w_gslope  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_gslope[i] <- wa[i] / wb
    
    if (w_gslope[i] < 1 / p) {
      w_gslope[i] = 0
    }
  }
  
  mean_gslope <- t(w_gslope) %*% colMeans(data)
  gslope_empirical_risk <-
    sqrt(t(w_gslope) %*% sigma_gslope %*% w_gslope)
  gslope_actual_risk   <-
    sqrt(t(w_gslope) %*% sigma %*% w_gslope)
  SR_gslope <- mean_gslope / gslope_empirical_risk
  
  #TSLOPE RISK
  wa <- (inv_tslope %*% ones) #Numerator
  wb <- (t(ones) %*% inv_tslope %*% ones) #Denominator
  
  w_tslope  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_tslope[i] <- wa[i] / wb
    
    if (w_tslope[i] < 1 / p) {
      w_tslope[i] = 0
    }
  }
  
  mean_tslope <- t(w_tslope) %*% colMeans(data)
  tslope_empirical_risk <-
    sqrt(t(w_tslope) %*% sigma_tslope %*% w_tslope)
  tslope_actual_risk   <-
    sqrt(t(w_tslope) %*% sigma %*% w_tslope)
  SR_tslope <- mean_tslope / tslope_empirical_risk
  
  #ORACLE RISK
  oracle = list(
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk
  )
  
  names(oracle) <-
    paste(
      list(
        "oracle_ledoit",
        "oracle_glasso",
        "oracle_elastic",
        "oracle_rope",
        "oracle_tlasso",
        "oracle_gslope",
        "oracle_tslope"
      )
    )
  
  #EMPIRICAL RISK
  empirical = list(
    ledoit_empirical_risk,
    glasso_empirical_risk,
    elastic_empirical_risk,
    rope_empirical_risk,
    tlasso_empirical_risk,
    gslope_empirical_risk,
    tslope_empirical_risk
  )
  
  names(empirical) <-
    paste(
      list(
        "empirical_ledoit",
        "empirical_glasso",
        "empirical_elastic",
        "empirical_rope",
        "empirical_tlasso",
        "empirical_gslope",
        "empirical_tslope"
      )
    )
  
  #ACTUAL RISK
  actual = list(
    ledoit_actual_risk,
    glasso_actual_risk,
    elastic_actual_risk,
    rope_actual_risk,
    tlasso_actual_risk,
    gslope_actual_risk,
    tslope_actual_risk
  )
  
  names(actual) <-
    paste(
      list(
        "actual_ledoit",
        "actual_glasso",
        "actual_elastic",
        "actual_rope",
        "actual_tlasso",
        "actual_gslope",
        "actual_tslope"
      )
    )
  
  #MEAN
  means = list(
    mean_ledoit,
    mean_glasso,
    mean_elastic,
    mean_rope,
    mean_tlasso,
    mean_gslope,
    mean_tslope
  )
  
  names(means) <-
    paste(
      list(
        "mean_ledoit",
        "mean_glasso",
        "mean_elastic",
        "mean_rope",
        "mean_tlasso",
        "mean_gslope",
        "mean_tslope"
      )
    )
  
  #SR
  SR = list(
    SR_ledoit,
    SR_glasso,
    SR_elastic,
    SR_rope,
    SR_tlasso,
    SR_gslope,
    SR_tslope
  )
  
  names(SR) <-
    paste(
      list(
        "SR_ledoit",
        "SR_glasso",
        "SR_elastic",
        "SR_rope",
        "SR_tlasso",
        "SR_gslope",
        "SR_tslope"
      )
    )
  
  #Weights
  weights = list(
    w_ledoit,
    w_glasso,
    w_elastic,
    w_rope,
    w_tlasso,
    w_gslope,
    w_tslope
  )
  
  names(weights) <-
    paste(
      list(
        "w_ledoit",
        "w_glasso",
        "w_elastic",
        "w_rope",
        "w_tlasso",
        "w_gslope",
        "w_tslope"
      )
    )
  
  output <-
    list(oracle, empirical, actual, means, SR, weights)
  
  names(output) <-
    paste(list("oracle_risk", "empirical_risk", "actual_risk", "means", "SR", "weights"))
  
  return(output)
}

##29.C##
portfolio_risk_measures_no_constraint <- function(data_series,
                                    omega,
                                    sigma,
                                    sigma_sample,
                                    sigma_ledoit,
                                    sigma_glasso,
                                    sigma_elastic,
                                    sigma_rope,
                                    sigma_tlasso,
                                    sigma_gslope,
                                    sigma_tslope,
                                    inv_sample,
                                    inv_ledoit,
                                    inv_glasso,
                                    inv_elastic,
                                    inv_rope,
                                    inv_tlasso,
                                    inv_gslope,
                                    inv_tslope) {
  data = data_series
  p = nrow(omega)
  
  #ORACLE PORTFOLIO
  ones  <- rep.int(1, p)
  wa <- (omega %*% ones) #Numerator
  wb <- (t(ones) %*% omega %*% ones) #Denominator
  
  w_oracle  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_oracle[i] <- wa[i] / wb
  }
  
  oracle_risk <- sqrt(t(w_oracle) %*% sigma %*% w_oracle)
  
  #SAMPLE RISK
  wa <- (inv_sample %*% ones) #Numerator
  wb <- (t(ones) %*% inv_sample %*% ones) #Denominator
  
  w_sample  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_sample[i] <- wa[i] / wb
  }
  
  mean_sample <- t(w_sample) %*% colMeans(data)
  sample_empirical_risk <-
    sqrt(t(w_sample) %*% sigma_sample %*% w_sample)
  sample_actual_risk   <-
    sqrt(t(w_sample) %*% sigma %*% w_sample)
  SR_sample <- mean_sample / sample_empirical_risk
  
  #LEDOIT RISK
  wa <- (inv_ledoit %*% ones) #Numerator
  wb <- (t(ones) %*% inv_ledoit %*% ones) #Denominator
  
  w_ledoit  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_ledoit[i] <- wa[i] / wb
  }
  
  mean_ledoit <- t(w_ledoit) %*% colMeans(data)
  ledoit_empirical_risk <-
    sqrt(t(w_ledoit) %*% sigma_ledoit %*% w_ledoit)
  ledoit_actual_risk   <-
    sqrt(t(w_ledoit) %*% sigma %*% w_ledoit)
  SR_ledoit <- mean_ledoit / ledoit_empirical_risk
  
  #GLASSO RISK
  wa <- (inv_glasso %*% ones) #Numerator
  wb <- (t(ones) %*% inv_glasso %*% ones) #Denominator
  
  w_glasso  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_glasso[i] <- wa[i] / wb
  }
  
  mean_glasso <- t(w_glasso) %*% colMeans(data)
  glasso_empirical_risk <-
    sqrt(t(w_glasso) %*% sigma_glasso %*% w_glasso)
  glasso_actual_risk   <-
    sqrt(t(w_glasso) %*% sigma %*% w_glasso)
  SR_glasso <- mean_glasso / glasso_empirical_risk
  
  #ELASTIC RISK
  wa <- (inv_elastic %*% ones) #Numerator
  wb <- (t(ones) %*% inv_elastic %*% ones) #Denominator
  
  w_elastic  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_elastic[i] <- wa[i] / wb
  }
  
  mean_elastic <- t(w_elastic) %*% colMeans(data)
  elastic_empirical_risk <-
    sqrt(t(w_elastic) %*% sigma_elastic %*% w_elastic)
  elastic_actual_risk   <-
    sqrt(t(w_elastic) %*% sigma %*% w_elastic)
  SR_elastic <- mean_elastic / elastic_empirical_risk
  
  #ROPE RISK
  wa <- (inv_rope %*% ones) #Numerator
  wb <- (t(ones) %*% inv_rope %*% ones) #Denominator
  
  w_rope  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_rope[i] <- wa[i] / wb
  }
  
  mean_rope <- t(w_rope) %*% colMeans(data)
  rope_empirical_risk <-
    sqrt(t(w_rope) %*% sigma_rope %*% w_rope)
  rope_actual_risk   <-
    sqrt(t(w_rope) %*% sigma %*% w_rope)
  SR_rope <- mean_rope / rope_actual_risk
  
  #TLASSO RISK
  wa <- (inv_tlasso %*% ones) #Numerator
  wb <- (t(ones) %*% inv_tlasso %*% ones) #Denominator
  
  w_tlasso  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_tlasso[i] <- wa[i] / wb
  }
  
  mean_tlasso <- t(w_tlasso) %*% colMeans(data)
  tlasso_empirical_risk <-
    sqrt(t(w_tlasso) %*% sigma_tlasso %*% w_tlasso)
  tlasso_actual_risk   <-
    sqrt(t(w_tlasso) %*% sigma %*% w_tlasso)
  SR_tlasso <- mean_tlasso / tlasso_empirical_risk
  
  #GSLOPE RISK
  wa <- (inv_gslope %*% ones) #Numerator
  wb <- (t(ones) %*% inv_gslope %*% ones) #Denominator
  
  w_gslope  <- rep.int(0, p)
  
  for (i in 1:p) {
    
    w_gslope[i] <- wa[i] / wb
  }
  
  mean_gslope <- t(w_gslope) %*% colMeans(data)
  gslope_empirical_risk <-
    sqrt(t(w_gslope) %*% sigma_gslope %*% w_gslope)
  gslope_actual_risk   <-
    sqrt(t(w_gslope) %*% sigma %*% w_gslope)
  SR_gslope <- mean_gslope / gslope_empirical_risk
  
  #TSLOPE RISK
  wa <- (inv_tslope %*% ones) #Numerator
  wb <- (t(ones) %*% inv_tslope %*% ones) #Denominator
  
  w_tslope  <- rep.int(0, p)
  
  for (i in 1:p) {
    w_tslope[i] <- wa[i] / wb
  }
  
  mean_tslope <- t(w_tslope) %*% colMeans(data)
  tslope_empirical_risk <-
    sqrt(t(w_tslope) %*% sigma_tslope %*% w_tslope)
  tslope_actual_risk   <-
    sqrt(t(w_tslope) %*% sigma %*% w_tslope)
  SR_tslope <- mean_tslope / tslope_empirical_risk
  
  #ORACLE RISK
  oracle = list(
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk,
    oracle_risk
  )
  
  names(oracle) <-
    paste(
      list(
        "oracle_sample",
        "oracle_ledoit",
        "oracle_glasso",
        "oracle_elastic",
        "oracle_rope",
        "oracle_tlasso",
        "oracle_gslope",
        "oracle_tslope"
      )
    )
  
  #EMPIRICAL RISK
  empirical = list(
    sample_empirical_risk,
    ledoit_empirical_risk,
    glasso_empirical_risk,
    elastic_empirical_risk,
    rope_empirical_risk,
    tlasso_empirical_risk,
    gslope_empirical_risk,
    tslope_empirical_risk
  )
  
  names(empirical) <-
    paste(
      list(
        "empirical_sample",
        "empirical_ledoit",
        "empirical_glasso",
        "empirical_elastic",
        "empirical_rope",
        "empirical_tlasso",
        "empirical_gslope",
        "empirical_tslope"
      )
    )
  
  #ACTUAL RISK
  actual = list(
    sample_actual_risk,
    ledoit_actual_risk,
    glasso_actual_risk,
    elastic_actual_risk,
    rope_actual_risk,
    tlasso_actual_risk,
    gslope_actual_risk,
    tslope_actual_risk
  )
  
  names(actual) <-
    paste(
      list(
        "actual_sample",
        "actual_ledoit",
        "actual_glasso",
        "actual_elastic",
        "actual_rope",
        "actual_tlasso",
        "actual_gslope",
        "actual_tslope"
      )
    )
  
  #MEAN
  means = list(
    mean_sample,
    mean_ledoit,
    mean_glasso,
    mean_elastic,
    mean_rope,
    mean_tlasso,
    mean_gslope,
    mean_tslope
  )
  
  names(means) <-
    paste(
      list(
        "mean_sample",
        "mean_ledoit",
        "mean_glasso",
        "mean_elastic",
        "mean_rope",
        "mean_tlasso",
        "mean_gslope",
        "mean_tslope"
      )
    )
  
  #SR
  SR = list(
    SR_sample,
    SR_ledoit,
    SR_glasso,
    SR_elastic,
    SR_rope,
    SR_tlasso,
    SR_gslope,
    SR_tslope
  )
  
  names(SR) <-
    paste(
      list(
        "SR_sample",
        "SR_ledoit",
        "SR_glasso",
        "SR_elastic",
        "SR_rope",
        "SR_tlasso",
        "SR_gslope",
        "SR_tslope"
      )
    )
  
  #Weights
  weights = list(
    w_oracle,
    w_sample,
    w_ledoit,
    w_glasso,
    w_elastic,
    w_rope,
    w_tlasso,
    w_gslope,
    w_tslope
  )
  
  names(weights) <-
    paste(
      list(
        "w_oracle",
        "w_sample",
        "w_ledoit",
        "w_glasso",
        "w_elastic",
        "w_rope",
        "w_tlasso",
        "w_gslope",
        "w_tslope"
      )
    )
  
  output <-
    list(oracle, empirical, actual, means, SR, weights)
  
  names(output) <-
    paste(list("oracle_risk", "empirical_risk", "actual_risk", "means", "SR", "weights"))
  
  return(output)
}

#Function to generate the heatmap from the correlation matrix
#INPUT
#
#A covariance_matrix
#
#OUTPUT
#
#The corresponding heatmap

##30##
heatmap_generation <- function(covariance_matrix, title_text) {
  sigma       = covariance_matrix
  melted      = melt(cov2cor(sigma))
  melted$Var1 = reorder(melted$Var1, as.numeric(melted$Var1))
  melted$Var2 = reorder(melted$Var2, as.numeric(melted$Var2))
  
  heatmap = ggplot(data = melted, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() + labs(title = title_text, x = element_blank(), y = element_blank()) +
    theme(axis.text.x =  element_blank(), axis.text.y = element_blank()) +
    scale_fill_gradient(low = "white", high = "red")
  
  return(heatmap)
}

#Function to generate the network graph
#INPUT
#
#All the precision matrix (inverse of the covariance matrix)
#
#OUTPUT
#
#The corresponding network and the corresponding weights

##31##
network_generation <- function(oracle_precision_matrix,
                               inv_sample,
                               inv_glasso,
                               inv_elastic,
                               inv_rope,
                               inv_tlasso,
                               inv_gslope,
                               inv_tslope) {
  inv_oracle      = oracle_precision_matrix
  
  oracle_partial_correlation_matrix  = wi2net(inv_oracle)
  sample_partial_correlation_matrix  = wi2net(inv_sample)
  glasso_partial_correlation_matrix  = wi2net(inv_glasso)
  elastic_partial_correlation_matrix = wi2net(inv_elastic)
  rope_partial_correlation_matrix    = wi2net(inv_rope)
  tlasso_partial_correlation_matrix  = wi2net(inv_tlasso)
  gslope_partial_correlation_matrix  = wi2net(inv_gslope)
  tslope_partial_correlation_matrix  = wi2net(inv_tslope)
  
  output <- list(
    graph_oracle      = qgraph(
      oracle_partial_correlation_matrix,
      layout = "spring",
      title = "Oracle Network",
      cut = 0.005
    ),
    
    graph_sample      = qgraph(
      sample_partial_correlation_matrix,
      layout = "spring",
      title = "Sample Network",
      cut = 0.005
    ),
    
    graph_glasso      = qgraph(
      glasso_partial_correlation_matrix,
      layout = "spring",
      title = "Glasso Network",
      cut = 0.005
    ),
    
    graph_elastic      = qgraph(
      elastic_partial_correlation_matrix,
      layout = "spring",
      title = "Elastic Network",
      cut = 0.005
    ),
    
    graph_rope      = qgraph(
      rope_partial_correlation_matrix,
      layout = "spring",
      title = "Rope Network",
      cut = 0.005
    ),
    
    graph_tlasso      = qgraph(
      tlasso_partial_correlation_matrix,
      layout = "spring",
      title = "Tlasso Network",
      cut = 0.005
    ),
    
    graph_gslope      = qgraph(
      gslope_partial_correlation_matrix,
      layout = "spring",
      title = "Gslope Network",
      cut = 0.005
    ),
    
    graph_tslope      = qgraph(
      tslope_partial_correlation_matrix,
      layout = "spring",
      title = "Tslope Network",
      cut = 0.005
    )
  )
  
  names(output) <-
    paste(
      list(
        "oracle_network",
        "sample_network",
        "glasso_network",
        "elastic_network",
        "rope_network",
        "tlasso_network",
        "gslope_network",
        "tslope_network"
      )
    )
  
  return(output)
}

#Function to generate the ROC curves
#INPUT
#
#1)Data series
#2)alpha, confidence level to generate the initial series of lambdas
#
#OUTPUT
#
#The corresponding ROC curves for all the methods

##32##
ROC_generation <-
  function(data_series,
           df,
           alpha,
           oracle_precision_matrix) {
    scal_l <- seq(0.01, 1.75, 0.0175)
    n      <- nrow(data_series)
    p      <- ncol(data_series)
    data   <- data_series
    omega  <- oracle_precision_matrix
    v      <- df
    k      <- 100
    
    #GLASSO
    lambda_scaled <-
      (lambdaSelector(data, n, alpha, "banerjee")) * scal_l
    
    cov_glasso <- matrix(c(seq(0, 0)), p, p)
    cov_glasso <- matrix(list(cov_glasso), k)
    inv_glasso <- matrix(c(seq(0, 0)), p, p)
    inv_glasso <- matrix(list(inv_glasso), k)
    
    tpGl <- matrix(c(seq(0, 0)), 1, 1)
    tpGl <- matrix(list(tpGl), k + 1)
    TPrateGl <- matrix(c(seq(0, 0)), 1, 1)
    TPrateGl <- matrix(list(TPrateGl), k + 1)
    
    fpGl <- matrix(c(seq(0, 0)), 1, 1)
    fpGl <- matrix(list(fpGl), k)
    FPrateGl <- matrix(c(seq(0, 0)), 1, 1)
    FPrateGl <- matrix(list(FPrateGl), k)
    
    time <- 0
    for (i in 1:k) {
      output_glasso = glasso(cor(data),
                             lambda_scaled[i],
                             nobs = n,
                             penalize.diagonal = FALSE)
      cov_glasso[[i]] = cor2cov(data, output_glasso$w)
      inv_glasso[[i]] = solve(cov_glasso[[i]])
      inv_glasso[[i]][abs(inv_glasso[[i]]) < 1e-04] <- 0
      
      tpGl[[i]] <- TP(inv_glasso[[i]], omega)
      TPrateGl[[i]] <- tpGl[[i]][[2]]
      fpGl[[i]] <- FP(inv_glasso[[i]], omega)
      FPrateGl[[i]] <- fpGl[[i]][[2]]
      time <- time + 1
      print(time)
    }
    
    #TLASSO
    cov_tlasso <- matrix(c(seq(0, 0)), p, p)
    cov_tlasso <- matrix(list(cov_tlasso), k)
    inv_tlasso <- matrix(c(seq(0, 0)), p, p)
    inv_tlasso <- matrix(list(inv_tlasso), k)
    
    tpTl <- matrix(c(seq(0, 0)), 1, 1)
    tpTl <- matrix(list(tpTl), k + 1)
    TPrateTl <- matrix(c(seq(0, 0)), 1, 1)
    TPrateTl <- matrix(list(TPrateTl), k + 1)
    
    fpTl <- matrix(c(seq(0, 0)), 1, 1)
    fpTl <- matrix(list(fpTl), k)
    FPrateTl <- matrix(c(seq(0, 0)), 1, 1)
    FPrateTl <- matrix(list(FPrateTl), k)
    
    time <- 0
    for (i in 1:k) {
      output_tlasso = tlasso_fast_rolling(data, lambda_scaled[[i]], v, 0.001, F)
      cov_tlasso[[i]] = (output_tlasso$S) #* (v/(v - 2))
      inv_tlasso[[i]]  = (output_tlasso$theta) #* ((v - 2)/v)
      inv_tlasso[[i]][abs(inv_tlasso[[i]]) < 1e-04] <- 0
      
      tpTl[[i]] <- TP(inv_tlasso[[i]], omega)
      TPrateTl[[i]] <- tpTl[[i]][[2]]
      fpTl[[i]] <- FP(inv_tlasso[[i]], omega)
      FPrateTl[[i]] <- fpTl[[i]][[2]]
      time <- time + 1
      print(time)
    }
    
    #GSLOPE
    lambdas_gs <- matrix(c(seq(0, 0)), 1, 1)
    lambdas_gs <- matrix(list(lambdas_gs), k + 1)
    
    lambdas <- create_lambda(cor(data), n, alpha)
    
    for (i in 1:k) {
      lambdas_gs[[i]] <- lambdas * scal_l[[i]]
    }
    
    cov_gslope <- matrix(c(seq(0, 0)), p, p)
    cov_gslope <- matrix(list(cov_gslope), k + 1)
    inv_gslope <- matrix(c(seq(0, 0)), p, p)
    inv_gslope <- matrix(list(inv_gslope), k + 1)
    
    tpGs <- matrix(c(seq(0, 0)), 1, 1)
    tpGs <- matrix(list(tpGs), k + 1)
    TPrateGs <- matrix(c(seq(0, 0)), 1, 1)
    TPrateGs <- matrix(list(TPrateGs), k + 1)
    
    fpGs <- matrix(c(seq(0, 0)), 1, 1)
    fpGs <- matrix(list(fpGs), k + 1)
    FPrateGs <- matrix(c(seq(0, 0)), 1, 1)
    FPrateGs <- matrix(list(FPrateGs), k + 1)
    
    time <- 0
    for (i in 1:k) {
      lambdas <- lambdas_gs[[i]]
      output_gslope <- gslope_new(cor(data), lambdas)
      inv_gslope[[i]]  = invcor2invcov(data, output_gslope$precision_matrix)
      cov_gslope[[i]]    = solve(inv_gslope[[i]])
      inv_gslope[[i]][abs(inv_gslope[[i]]) < 1e-04] <- 0
      
      tpGs[[i]] <- TP(inv_gslope[[i]], omega)
      TPrateGs[[i]] <- tpGs[[i]][[2]]
      fpGs[[i]] <- FP(inv_gslope[[i]], omega)
      FPrateGs[[i]] <- fpGs[[i]][[2]]
      time <- time + 1
      print(time)
    }
    
    #TSLOPE
    lambdas_ts <- matrix(c(seq(0, 0)), 1, 1)
    lambdas_ts <- matrix(list(lambdas_ts), k + 1)
    
    lambdas <- create_lambda(cor(data), n, alpha)
    
    for (i in 1:k) {
      lambdas_ts[[i]] <- lambdas * scal_l[[i]]
    }
    
    cov_tslope <- matrix(c(seq(0, 0)), p, p)
    cov_tslope <- matrix(list(cov_tslope), k + 1)
    inv_tslope <- matrix(c(seq(0, 0)), p, p)
    inv_tslope <- matrix(list(inv_tslope), k + 1)
    
    tpTs <- matrix(c(seq(0, 0)), 1, 1)
    tpTs <- matrix(list(tpTs), k + 1)
    TPrateTs <- matrix(c(seq(0, 0)), 1, 1)
    TPrateTs <- matrix(list(TPrateTs), k + 1)
    
    fpTs <- matrix(c(seq(0, 0)), 1, 1)
    fpTs <- matrix(list(fpTs), k + 1)
    FPrateTs <- matrix(c(seq(0, 0)), 1, 1)
    FPrateTs <- matrix(list(FPrateTs), k + 1)
    
    time <- 0
    for (i in 1:k) {
      lambdas <- lambdas_ts[[i]]
      output_tslope = tslope_fast_reg(data, df, 0.001, F, lambdas = lambdas)
      cov_tslope[[i]] <- (output_tslope$S) #* (v/(v - 2))
      inv_tslope[[i]] <- solve(cov_tslope[[i]])
      inv_tslope[[i]][abs(inv_tslope[[i]]) < 1e-04] <- 0
      
      tpTs[[i]] <- TP(inv_tslope[[i]], omega)
      TPrateTs[[i]] <- tpTs[[i]][[2]]
      fpTs[[i]] <- FP(inv_tslope[[i]], omega)
      FPrateTs[[i]] <- fpTs[[i]][[2]]
      time <- time + 1
      print(time)
    }
    
    #ROC curve creation
    #ROC (no scale)
    #TPrateGl <- sort(unlist(TPrateGl), decreasing = TRUE)
    #TPrateGs <- sort(unlist(TPrateGs), decreasing = TRUE)
    #TPrateTs <- sort(unlist(TPrateTs), decreasing = TRUE)
    #TPrateTl <- sort(unlist(TPrateTl), decreasing = TRUE)
    
    Gl_data <-
      tibble(
        'method' = rep('Glasso', k),
        'FP' = unlist(FPrateGl[1:k]),
        'TP' = unlist(TPrateGl[1:k])
      )
    Gs_data <-
      tibble(
        'method' = rep('Gslope', k),
        'FP' = unlist(FPrateGs[1:k]),
        'TP' = unlist(TPrateGs[1:k])
      )
    Ts_data <-
      tibble(
        'method' = rep('Tslope', k),
        'FP' = unlist(FPrateTs[1:k]),
        'TP' = unlist(TPrateTs[1:k])
      )
    Tl_data <-
      tibble(
        'method' = rep('Tlasso', k),
        'FP' = unlist(FPrateTl[1:k]),
        'TP' = unlist(TPrateTl[1:k])
      )
    
    ROC_data <- bind_rows(Gl_data, Gs_data, Ts_data, Tl_data)
    
    plot <- ggplot(ROC_data) +
      geom_line(aes(x = FP, y = TP, col = method)) + xlab('FP rate') +
      ylab('TP rate') +
      labs(title = 'ROC curve') +
      theme(plot.title = element_text(size = 9))
    
    output <-
      list(plot,
           ROC_data,
           inv_glasso,
           inv_tlasso,
           inv_gslope,
           inv_tslope)
    
    names(output) <-
      paste(list(
        "plot",
        "ROC_data",
        "inv_glasso",
        "inv_tlasso",
        "inv_gslope",
        "inv_tslope"
      ))
    
    return(output)
  }


##############################BoxPlots_Simulations##############################

##33##
sample_precision_matrix_estimation_normal <-
  function(data, n, alpha_level, df, progress = TRUE) {
    #SAMPLE
    sigma_sample = cov(data)
    rho_sample   = cov2cor(sigma_sample)
    inv_sample = inv(sigma_sample)
    
    #LEDOIT
    sigma_ledoit = nlshrink_cov(data)
    rho_ledoit   = cov2cor(sigma_ledoit)
    inv_ledoit   = inv(sigma_ledoit)
    
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
    output_elastic = gelnet(S = cor(data),
                            lambda = lambda,
                            alpha = 0.5)
    
    #From corr matrix to covariance, then precision
    sigma_elastic = cor2cov(data, output_elastic$W)
    inv_elastic   = solve(sigma_elastic)
    inv_elastic[abs(inv_elastic) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #GLASSO RIDGE (ROPE)
    output_rope = gelnet(S = cor(data),
                         lambda = lambda,
                         alpha = 0)
    
    #From corr matrix to covariance, then precision
    sigma_rope = cor2cov(data, output_rope$W)
    inv_rope   = solve(sigma_rope)
    inv_rope[abs(inv_rope) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #TLASSO
    v = df
    output_tlasso = tlasso_fast_rolling(data, lambda, v, 0.001, F)
    
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
    output_tslope = tslope_fast_rolling(data, v, 0.001, F, alpha_value = alpha_level)
    sigma_tslope  = (output_tslope$S) #* (v/(v - 2))
    inv_tslope    = solve(sigma_tslope)
    inv_tslope[abs(inv_tslope) < 1e-04] <- 0
    
    #TOTAL OUTPUT
    output = list(
      sigma_sample,
      sigma_ledoit,
      sigma_glasso,
      sigma_elastic,
      sigma_rope,
      sigma_tlasso,
      sigma_gslope,
      sigma_tslope,
      inv_sample,
      inv_ledoit,
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
          "sigma_sample",
          "sigma_ledoit",
          "sigma_glasso",
          "sigma_elastic",
          "sigma_rope",
          "sigma_tlasso",
          "sigma_gslope",
          "sigma_tslope",
          "inv_sample",
          "inv_ledoit",
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

##34##
performance_measures_normal <- function(omega,
                                        inv_sample,
                                        inv_glasso,
                                        inv_elastic,
                                        inv_rope,
                                        inv_tlasso,
                                        inv_gslope,
                                        inv_tslope) {
  #CONDITION NUMBER
  condition = list(
    cond(inv_sample),
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
        "cond_sample",
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
    norm(omega - inv_sample, type = "F"),
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
        "frob_sample",
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
    TP(inv_sample, omega)[[2]],
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
        "TP_sample",
        "TP_glasso",
        "TP_elastic",
        "TP_rope",
        "TP_tlasso",
        "TP_gslope",
        "TP_tslope"
      )
    )
  
  TNrate = list(
    TN(inv_sample, omega)[[2]],
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
        "TN_sample",
        "TN_glasso",
        "TN_elastic",
        "TN_rope",
        "TN_tlasso",
        "TN_gslope",
        "TN_tslope"
      )
    )
  
  FPrate = list(
    FP(inv_sample, omega)[[2]],
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
        "FP_sample",
        "FP_glasso",
        "FP_elastic",
        "FP_rope",
        "FP_tlasso",
        "FP_gslope",
        "FP_tslope"
      )
    )
  
  FNrate = list(
    FN(inv_sample, omega)[[2]],
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
        "FN_sample",
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
    TP(inv_sample, omega)[[1]] / (TP(inv_sample, omega)[[1]] + (0.5 * (
      FP(inv_sample, omega)[[1]] + FN(inv_sample, omega)[[1]]
    ))),
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
        "F1_sample",
        "F1_glasso",
        "F1_elastic",
        "F1_rope",
        "F1_tlasso",
        "F1_gslope",
        "F1_tslope"
      )
    )
  
  #ACCURACY
  positive <- max(c(sum(upper(
    properAdjacent(omega)
  )), 1))
  p = nrow(omega)
  negative <- max(c(sum(!upper(
    properAdjacent(omega)
  )), 1))
  
  ACC = list(
    (TP(inv_sample, omega)[[1]] + (TN(inv_sample, omega)[[1]])) / (positive + negative),
    (TP(inv_glasso, omega)[[1]] + (TN(inv_glasso, omega)[[1]])) / (positive + negative),
    (TP(inv_elastic, omega)[[1]] + (TN(
      inv_elastic, omega
    )[[1]])) / (positive + negative),
    (TP(inv_rope, omega)[[1]] + (TN(inv_rope, omega)[[1]])) / (positive + negative),
    (TP(inv_tlasso, omega)[[1]] + (TN(inv_tlasso, omega)[[1]])) / (positive + negative),
    (TP(inv_gslope, omega)[[1]] + (TN(inv_gslope, omega)[[1]])) / (positive + negative),
    (TP(inv_tslope, omega)[[1]] + (TN(inv_tslope, omega)[[1]])) / (positive + negative)
  )
  
  names(ACC) <-
    paste(
      list(
        "ACC_sample",
        "ACC_glasso",
        "ACC_elastic",
        "ACC_rope",
        "ACC_tlasso",
        "ACC_gslope",
        "ACC_tslope"
      )
    )
  
  entropy = list(
    sum(diag(omega %*% inv_sample)) - log(det(omega %*% inv_sample)) - p,
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
        "entropy_sample",
        "entropy_glasso",
        "entropy_elastic",
        "entropy_rope",
        "entropy_tlasso",
        "entropy_gslope",
        "entropy_tslope"
      )
    )
  
  FDR = list(
    FDP(inv_sample, omega),
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
        "FDP_sample",
        "FDP_glasso",
        "FDP_elastic",
        "FDP_rope",
        "FDP_tlasso",
        "FDP_gslope",
        "FDP_tslope"
      )
    )
  
  localFDR = list(
    localFDP(properAdjacent(inv_sample), properAdjacent(omega)),
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
        "localFDP_sample",
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

##35##
BoxPlot_simulation_normal <- function(N, p, graph_type, v, u, i) {
  a = oracle_precision_matrix_generation(N, p, graph_type, v, u)
  
  i = i
  
  perf_measures = list()
  port_measures = list()
  
  for (i in 1:i) {
    b = data_series_generation(N, a$sigma, 0.05, 4, T_student = FALSE)
    
    c = sample_precision_matrix_estimation_normal(b, N, 0.05, 4, progress = TRUE)
    
    d = performance_measures_normal(
      a$omega,
      c$inv_sample,
      c$inv_glasso,
      c$inv_elastic,
      c$inv_rope,
      c$inv_tlasso,
      c$inv_gslope,
      c$inv_tslope)
    
    e = portfolio_risk_measures(
      b,
      a$omega,
      a$sigma,
      c$sigma_sample,
      c$sigma_ledoit,
      c$sigma_glasso,
      c$sigma_elastic,
      c$sigma_rope,
      c$sigma_tlasso,
      c$sigma_gslope,
      c$sigma_tslope,
      c$inv_sample,
      c$inv_ledoit,
      c$inv_glasso,
      c$inv_elastic,
      c$inv_rope,
      c$inv_tlasso,
      c$inv_gslope,
      c$inv_tslope
    )
    
    perf_measures[[i]] = d
    port_measures[[i]] = e
    
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
  
  oracle_risk_data = data.frame()
  for (i in 1:i) {
    oracle_risk_data = rbind(port_measures[[i]]$oracle_risk, oracle_risk_data)
  }
  
  empirical_risk_data = data.frame()
  for (i in 1:i) {
    empirical_risk_data = rbind(port_measures[[i]]$empirical_risk, empirical_risk_data)
  }
  
  actual_risk_data = data.frame()
  for (i in 1:i) {
    actual_risk_data = rbind(port_measures[[i]]$actual_risk, actual_risk_data)
  }
  
  port_mean_data = data.frame()
  for (i in 1:i) {
    port_mean_data = rbind(port_measures[[i]]$means, port_mean_data)
  }
  
  port_SR_data = data.frame()
  for (i in 1:i) {
    port_SR_data = rbind(port_measures[[i]]$SR, port_SR_data)
  }
  
  port_weights_data = data.frame()
  for (i in 1:i) {
    port_weights_data = rbind(port_measures[[i]]$weights, port_weights_data)
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
  Avg_oracle = colMeans(oracle_risk_data)
  Avg_empirical = colMeans(empirical_risk_data)
  Avg_actual = colMeans(actual_risk_data)
  Avg_mean = colMeans(port_mean_data)
  Avg_SR = colMeans(port_SR_data)
  
  Std_cond = apply(cond, 2, std)
  Std_frob = apply(frob, 2, std)
  Std_TP = apply(TP_rt, 2, std)
  Std_FP = apply(FP_rt, 2, std)
  Std_TN = apply(TN_rt, 2, std)
  Std_FN = apply(FN_rt, 2, std)
  Std_F1 = apply(F1, 2, std)
  Std_Acc = apply(Acc, 2, std)
  Std_Entropy = apply(Entropy, 2, std)
  Std_FDR = apply(FDR_data, 2, std)
  Std_localFDR = apply(localFDR_data, 2, std)
  Std_oracle = apply(oracle_risk_data, 2, std)
  Std_empirical = apply(empirical_risk_data, 2, std)
  Std_actual = apply(actual_risk_data, 2, std)
  Std_mean = apply(port_mean_data, 2, std)
  Std_SR = apply(port_SR_data, 2, std)
  
  
  row = list("sample",
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
  
  row_port = list("sample",
                  "ledoit",
                  "glasso",
                  "elastic",
                  "rope",
                  "tlasso",
                  "gslope",
                  "tslope")
  
  Avg_means_port = data.frame(
    row.names = row_port,
    Avg_oracle,
    Avg_empirical,
    Avg_actual,
    Avg_mean,
    Avg_SR
  )
  
  Avg_std_port = data.frame(
    row.names = row_port,
    Std_oracle,
    Std_empirical,
    Std_actual,
    Std_mean,
    Std_SR
  )
  
  data_oracle <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      oracle_risk_data$oracle_sample,
      oracle_risk_data$oracle_ledoit,
      oracle_risk_data$oracle_glasso,
      oracle_risk_data$oracle_elastic,
      oracle_risk_data$oracle_rope,
      oracle_risk_data$oracle_tlasso,
      oracle_risk_data$oracle_gslope,
      oracle_risk_data$oracle_tslope
    )
  )
  
  data_empirical <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      empirical_risk_data$empirical_sample,
      empirical_risk_data$empirical_ledoit,
      empirical_risk_data$empirical_glasso,
      empirical_risk_data$empirical_elastic,
      empirical_risk_data$empirical_rope,
      empirical_risk_data$empirical_tlasso,
      empirical_risk_data$empirical_gslope,
      empirical_risk_data$empirical_tslope
    )
  )
  
  data_actual <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      actual_risk_data$actual_sample,
      actual_risk_data$actual_ledoit,
      actual_risk_data$actual_glasso,
      actual_risk_data$actual_elastic,
      actual_risk_data$actual_rope,
      actual_risk_data$actual_tlasso,
      actual_risk_data$actual_gslope,
      actual_risk_data$actual_tslope
    )
  )
  
  data_means <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      port_mean_data$mean_sample,
      port_mean_data$mean_ledoit,
      port_mean_data$mean_glasso,
      port_mean_data$mean_elastic,
      port_mean_data$mean_rope,
      port_mean_data$mean_tlasso,
      port_mean_data$mean_gslope,
      port_mean_data$mean_tslope
    )
  )
  
  data_SR <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      port_SR_data$SR_sample,
      port_SR_data$SR_ledoit,
      port_SR_data$SR_glasso,
      port_SR_data$SR_elastic,
      port_SR_data$SR_rope,
      port_SR_data$SR_tlasso,
      port_SR_data$SR_gslope,
      port_SR_data$SR_tslope
    )
  )
  
  data_weights <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      port_weights_data$w_sample,
      port_weights_data$w_ledoit,
      port_weights_data$w_glasso,
      port_weights_data$w_elastic,
      port_weights_data$w_rope,
      port_weights_data$w_tlasso,
      port_weights_data$w_gslope,
      port_weights_data$w_tslope
    )
  )
  
  data_cond <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      cond$cond_sample,
      cond$cond_glasso,
      cond$cond_elastic,
      cond$cond_rope,
      cond$cond_tlasso,
      cond$cond_gslope,
      cond$cond_tslope
    )
  )
  
  data_frob <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      frob$frob_sample,
      frob$frob_glasso,
      frob$frob_elastic,
      frob$frob_rope,
      frob$frob_tlasso,
      frob$frob_gslope,
      frob$frob_tslope
    )
  )
  
  data_TP <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      TP_rt$TP_sample,
      TP_rt$TP_glasso,
      TP_rt$TP_elastic,
      TP_rt$TP_rope,
      TP_rt$TP_tlasso,
      TP_rt$TP_gslope,
      TP_rt$TP_tslope
    )
  )
  
  data_FP <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      FP_rt$FP_sample,
      FP_rt$FP_glasso,
      FP_rt$FP_elastic,
      FP_rt$FP_rope,
      FP_rt$FP_tlasso,
      FP_rt$FP_gslope,
      FP_rt$FP_tslope
    )
  )
  
  data_TN <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      TN_rt$TN_sample,
      TN_rt$TN_glasso,
      TN_rt$TN_elastic,
      TN_rt$TN_rope,
      TN_rt$TN_tlasso,
      TN_rt$TN_gslope,
      TN_rt$TN_tslope
    )
  )
  
  
  data_FN <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      FN_rt$FN_sample,
      FN_rt$FN_glasso,
      FN_rt$FN_elastic,
      FN_rt$FN_rope,
      FN_rt$FN_tlasso,
      FN_rt$FN_gslope,
      FN_rt$FN_tslope
    )
  )
  
  data_F1 <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      F1$F1_sample,
      F1$F1_glasso,
      F1$F1_elastic,
      F1$F1_rope,
      F1$F1_tlasso,
      F1$F1_gslope,
      F1$F1_tslope
    )
  )
  
  data_ACC <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      Acc$ACC_sample,
      Acc$ACC_glasso,
      Acc$ACC_elastic,
      Acc$ACC_rope,
      Acc$ACC_tlasso,
      Acc$ACC_gslope,
      Acc$ACC_tslope
    )
  )
  
  data_entropy <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      Entropy$entropy_sample,
      Entropy$entropy_glasso,
      Entropy$entropy_elastic,
      Entropy$entropy_rope,
      Entropy$entropy_tlasso,
      Entropy$entropy_gslope,
      Entropy$entropy_tslope
    )
  )
  
  data_FDR <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      FDR_data$FDP_sample,
      FDR_data$FDP_glasso,
      FDR_data$FDP_elastic,
      FDR_data$FDP_rope,
      FDR_data$FDP_tlasso,
      FDR_data$FDP_gslope,
      FDR_data$FDP_tslope
    )
  )
  
  data_localFDR <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      localFDR_data$localFDP_sample,
      localFDR_data$localFDP_glasso,
      localFDR_data$localFDP_elastic,
      localFDR_data$localFDP_rope,
      localFDR_data$localFDP_tlasso,
      localFDR_data$localFDP_gslope,
      localFDR_data$localFDP_tslope
    )
  )
  
  output   = list(
    data_oracle,
    data_empirical,
    data_actual,
    data_means,
    data_SR,
    data_weights,
    data_cond,
    data_frob,
    data_TP,
    data_FP,
    data_TN,
    data_FN,
    data_F1,
    data_ACC,
    data_entropy,
    data_FDR,
    data_localFDR,
    Avg_means,
    Avg_std,
    Avg_means_port,
    Avg_std_port
  )
  
  names(output) <-
    paste(
      list(
        "data_oracle",
        "data_empirical",
        "data_actual",
        "data_means",
        "data_SR",
        "data_weights",
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
        "Std_performance_measures",
        "Average_portfolio_measures",
        "Std_portfolio_measures"
      )
    )
  
  return(output)
}

##36##
sample_precision_matrix_estimation_normal_N_less_p <-
  function(data, n, alpha_level, df, progress = TRUE, factor = 0.75) {
    
    #LEDOIT
    sigma_ledoit = nlshrink_cov(data)
    rho_ledoit   = cov2cor(sigma_ledoit)
    inv_ledoit   = inv(sigma_ledoit)
    
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
    output_elastic = gelnet(S = cor(data),
                            lambda = lambda,
                            alpha = 0.5)
    
    #From corr matrix to covariance, then precision
    sigma_elastic = cor2cov(data, output_elastic$W)
    inv_elastic   = solve(sigma_elastic)
    inv_elastic[abs(inv_elastic) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #GLASSO RIDGE (ROPE)
    output_rope = gelnet(S = cor(data),
                         lambda = lambda,
                         alpha = 0)
    
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
    lambdas = create_lambda(cor(data), n = n, alpha = alpha_level) * factor
    
    output_gslope = gslope_new(cor(data), lambdas, progress = progress)
    
    #From corr matrix to covariance, then precision
    inv_gslope  = invcor2invcov(data, output_gslope$precision_matrix)
    sigma_gslope    = solve(inv_gslope)
    inv_gslope[abs(inv_gslope) < 1e-04] <-
      0 #threshold to consider an entry equal to zero
    
    #TSLOPE
    output_tslope = tslope_fast_rolling(data,
                                        v,
                                        0.001,
                                        F,
                                        init_theta = matrix(0, ncol(data), ncol(data)),
                                        alpha_value = alpha_level,
                                        factor)
    sigma_tslope  = (output_tslope$S) #* (v/(v - 2))
    inv_tslope    = solve(sigma_tslope)
    inv_tslope[abs(inv_tslope) < 1e-04] <- 0
    
    #TOTAL OUTPUT
    output = list(
      sigma_ledoit,
      sigma_glasso,
      sigma_elastic,
      sigma_rope,
      sigma_tlasso,
      sigma_gslope,
      sigma_tslope,
      inv_ledoit,
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
          "sigma_ledoit",
          "sigma_glasso",
          "sigma_elastic",
          "sigma_rope",
          "sigma_tlasso",
          "sigma_gslope",
          "sigma_tslope",
          "inv_ledoit",
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

##37##
performance_measures_normal_N_less_p <- function(omega,
                                                 inv_ledoit,
                                                 inv_glasso,
                                                 inv_elastic,
                                                 inv_rope,
                                                 inv_tlasso,
                                                 inv_gslope,
                                                 inv_tslope) {
  #CONDITION NUMBER
  condition = list(
    cond(inv_ledoit),
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
        "cond_ledoit",
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
    norm(omega - inv_ledoit, type = "F"),
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
        "frob_ledoit",
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
    TP(inv_ledoit, omega)[[2]],
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
        "TP_ledoit",
        "TP_glasso",
        "TP_elastic",
        "TP_rope",
        "TP_tlasso",
        "TP_gslope",
        "TP_tslope"
      )
    )
  
  TNrate = list(
    TN(inv_ledoit, omega)[[2]],
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
        "TN_ledoit",
        "TN_glasso",
        "TN_elastic",
        "TN_rope",
        "TN_tlasso",
        "TN_gslope",
        "TN_tslope"
      )
    )
  
  FPrate = list(
    FP(inv_ledoit, omega)[[2]],
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
        "FP_ledoit",
        "FP_glasso",
        "FP_elastic",
        "FP_rope",
        "FP_tlasso",
        "FP_gslope",
        "FP_tslope"
      )
    )
  
  FNrate = list(
    FN(inv_ledoit, omega)[[2]],
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
        "FN_ledoit",
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
    TP(inv_ledoit, omega)[[1]] / (TP(inv_ledoit, omega)[[1]] + (0.5 * (
      FP(inv_ledoit, omega)[[1]] + FN(inv_ledoit, omega)[[1]]
    ))),
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
        "F1_ledoit",
        "F1_glasso",
        "F1_elastic",
        "F1_rope",
        "F1_tlasso",
        "F1_gslope",
        "F1_tslope"
      )
    )
  
  #ACCURACY
  positive <- max(c(sum(upper(
    properAdjacent(omega)
  )), 1))
  p = nrow(omega)
  negative <- max(c(sum(!upper(
    properAdjacent(omega)
  )), 1))
  
  ACC = list(
    (TP(inv_ledoit, omega)[[1]] + (TN(inv_ledoit, omega)[[1]])) / (positive + negative),
    (TP(inv_glasso, omega)[[1]] + (TN(inv_glasso, omega)[[1]])) / (positive + negative),
    (TP(inv_elastic, omega)[[1]] + (TN(
      inv_elastic, omega
    )[[1]])) / (positive + negative),
    (TP(inv_rope, omega)[[1]] + (TN(inv_rope, omega)[[1]])) / (positive + negative),
    (TP(inv_tlasso, omega)[[1]] + (TN(inv_tlasso, omega)[[1]])) / (positive + negative),
    (TP(inv_gslope, omega)[[1]] + (TN(inv_gslope, omega)[[1]])) / (positive + negative),
    (TP(inv_tslope, omega)[[1]] + (TN(inv_tslope, omega)[[1]])) / (positive + negative)
  )
  
  names(ACC) <-
    paste(
      list(
        "ACC_ledoit",
        "ACC_glasso",
        "ACC_elastic",
        "ACC_rope",
        "ACC_tlasso",
        "ACC_gslope",
        "ACC_tslope"
      )
    )
  
  entropy = list(
    sum(diag(omega %*% inv_ledoit)) - log(det(omega %*% inv_ledoit)) - p,
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
        "entropy_ledoit",
        "entropy_glasso",
        "entropy_elastic",
        "entropy_rope",
        "entropy_tlasso",
        "entropy_gslope",
        "entropy_tslope"
      )
    )
  
  FDR = list(
    FDP(inv_ledoit, omega),
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
        "FDP_ledoit",
        "FDP_glasso",
        "FDP_elastic",
        "FDP_rope",
        "FDP_tlasso",
        "FDP_gslope",
        "FDP_tslope"
      )
    )
  
  localFDR = list(
    localFDP(properAdjacent(inv_ledoit), properAdjacent(omega)),
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
        "localFDP_ledoit",
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

performance_measures <- function(omega,
                                 inv_sample,
                                 inv_glasso,
                                 inv_elastic,
                                 inv_rope,
                                 inv_tlasso,
                                 inv_gslope,
                                 inv_tslope) {
  #CONDITION NUMBER
  condition = list(
    cond(inv_sample),
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
        "cond_sample",
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
    norm(omega - inv_sample, type = "F"),
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
        "frob_sample",
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
    TP(inv_sample, omega)[[2]],
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
        "TP_sample",
        "TP_glasso",
        "TP_elastic",
        "TP_rope",
        "TP_tlasso",
        "TP_gslope",
        "TP_tslope"
      )
    )
  
  TNrate = list(
    TN(inv_sample, omega)[[2]],
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
        "TN_sample",
        "TN_glasso",
        "TN_elastic",
        "TN_rope",
        "TN_tlasso",
        "TN_gslope",
        "TN_tslope"
      )
    )
  
  FPrate = list(
    FP(inv_sample, omega)[[2]],
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
        "FP_sample",
        "FP_glasso",
        "FP_elastic",
        "FP_rope",
        "FP_tlasso",
        "FP_gslope",
        "FP_tslope"
      )
    )
  
  FNrate = list(
    FN(inv_sample, omega)[[2]],
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
        "FN_sample",
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
    TP(inv_sample, omega)[[1]] / (TP(inv_sample, omega)[[1]] + (0.5 * (
      FP(inv_sample, omega)[[1]] + FN(inv_sample, omega)[[1]]
    ))),
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
        "F1_sample",
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
    (TP(inv_sample, omega)[[1]] + (TN(inv_sample, omega)[[1]])) / (positive + negative),
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
        "ACC_sample",
        "ACC_glasso",
        "ACC_elastic",
        "ACC_rope",
        "ACC_tlasso",
        "ACC_gslope",
        "ACC_tslope"
      )
    )
  
  entropy = list(
    sum(diag(omega %*% inv_sample)) - log(det(omega %*% inv_sample)) - p,
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
        "entropy_sample",
        "entropy_glasso",
        "entropy_elastic",
        "entropy_rope",
        "entropy_tlasso",
        "entropy_gslope",
        "entropy_tslope"
      )
    )
  
  FDR = list(
    FDP(inv_sample, omega),
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
        "FDP_sample",
        "FDP_glasso",
        "FDP_elastic",
        "FDP_rope",
        "FDP_tlasso",
        "FDP_gslope",
        "FDP_tslope"
      )
    )
  
  localFDR = list(
    localFDP(properAdjacent(inv_sample), properAdjacent(omega)),
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
        "localFDP_sample",
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

##38##
BoxPlot_simulation_normal_N_less_p <-
  function(N, p, graph_type, v, u, i) {
    a = oracle_precision_matrix_generation(N, p, graph_type, v, u)
    
    i = i
    
    perf_measures = list()
    port_measures = list()
    
    for (i in 1:i) {
      b = data_series_generation(N, a$sigma, 0.05, 4, T_student = FALSE)
      
      c = sample_precision_matrix_estimation_normal_N_less_p(b, N, 0.05, 4, progress = TRUE)
      
      d = performance_measures_normal_N_less_p(
        a$omega,
        c$inv_ledoit,
        c$inv_glasso,
        c$inv_elastic,
        c$inv_rope,
        c$inv_tlasso,
        c$inv_gslope,
        c$inv_tslope)
      
      e = portfolio_risk_measures_N_less_p(
        b,
        a$omega,
        a$sigma,
        c$sigma_ledoit,
        c$sigma_glasso,
        c$sigma_elastic,
        c$sigma_rope,
        c$sigma_tlasso,
        c$sigma_gslope,
        c$sigma_tslope,
        c$inv_ledoit,
        c$inv_glasso,
        c$inv_elastic,
        c$inv_rope,
        c$inv_tlasso,
        c$inv_gslope,
        c$inv_tslope
      )
      
      perf_measures[[i]] = d
      port_measures[[i]] = e
      
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
    
    oracle_risk_data = data.frame()
    for (i in 1:i) {
      oracle_risk_data = rbind(port_measures[[i]]$oracle_risk, oracle_risk_data)
    }
    
    empirical_risk_data = data.frame()
    for (i in 1:i) {
      empirical_risk_data = rbind(port_measures[[i]]$empirical_risk, empirical_risk_data)
    }
    
    actual_risk_data = data.frame()
    for (i in 1:i) {
      actual_risk_data = rbind(port_measures[[i]]$actual_risk, actual_risk_data)
    }
    
    port_mean_data = data.frame()
    for (i in 1:i) {
      port_mean_data = rbind(port_measures[[i]]$means, port_mean_data)
    }
    
    port_SR_data = data.frame()
    for (i in 1:i) {
      port_SR_data = rbind(port_measures[[i]]$SR, port_SR_data)
    }
    
    port_weights_data = data.frame()
    for (i in 1:i) {
      port_weights_data = rbind(port_measures[[i]]$weights, port_weights_data)
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
    Avg_oracle = colMeans(oracle_risk_data)
    Avg_empirical = colMeans(empirical_risk_data)
    Avg_actual = colMeans(actual_risk_data)
    Avg_mean = colMeans(port_mean_data)
    Avg_SR = colMeans(port_SR_data)
    
    Std_cond = apply(cond, 2, std)
    Std_frob = apply(frob, 2, std)
    Std_TP = apply(TP_rt, 2, std)
    Std_FP = apply(FP_rt, 2, std)
    Std_TN = apply(TN_rt, 2, std)
    Std_FN = apply(FN_rt, 2, std)
    Std_F1 = apply(F1, 2, std)
    Std_Acc = apply(Acc, 2, std)
    Std_Entropy = apply(Entropy, 2, std)
    Std_FDR = apply(FDR_data, 2, std)
    Std_localFDR = apply(localFDR_data, 2, std)
    Std_oracle = apply(oracle_risk_data, 2, std)
    Std_empirical = apply(empirical_risk_data, 2, std)
    Std_actual = apply(actual_risk_data, 2, std)
    Std_mean = apply(port_mean_data, 2, std)
    Std_SR = apply(port_SR_data, 2, std)
    
    row = list("ledoit",
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
    
    row_port = list("ledoit",
                    "glasso",
                    "elastic",
                    "rope",
                    "tlasso",
                    "gslope",
                    "tslope")
    
    Avg_means_port = data.frame(
      row.names = row_port,
      Avg_oracle,
      Avg_empirical,
      Avg_actual,
      Avg_mean,
      Avg_SR
    )
    
    Avg_std_port = data.frame(
      row.names = row_port,
      Std_oracle,
      Std_empirical,
      Std_actual,
      Std_mean,
      Std_SR
    )
    
    data_oracle <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        oracle_risk_data$oracle_ledoit,
        oracle_risk_data$oracle_glasso,
        oracle_risk_data$oracle_elastic,
        oracle_risk_data$oracle_rope,
        oracle_risk_data$oracle_tlasso,
        oracle_risk_data$oracle_gslope,
        oracle_risk_data$oracle_tslope
      )
    )
    
    data_empirical <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        empirical_risk_data$empirical_ledoit,
        empirical_risk_data$empirical_glasso,
        empirical_risk_data$empirical_elastic,
        empirical_risk_data$empirical_rope,
        empirical_risk_data$empirical_tlasso,
        empirical_risk_data$empirical_gslope,
        empirical_risk_data$empirical_tslope
      )
    )
    
    data_actual <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        actual_risk_data$actual_ledoit,
        actual_risk_data$actual_glasso,
        actual_risk_data$actual_elastic,
        actual_risk_data$actual_rope,
        actual_risk_data$actual_tlasso,
        actual_risk_data$actual_gslope,
        actual_risk_data$actual_tslope
      )
    )
    
    data_means <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        port_mean_data$mean_ledoit,
        port_mean_data$mean_glasso,
        port_mean_data$mean_elastic,
        port_mean_data$mean_rope,
        port_mean_data$mean_tlasso,
        port_mean_data$mean_gslope,
        port_mean_data$mean_tslope
      )
    )
    
    data_SR <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        port_SR_data$SR_ledoit,
        port_SR_data$SR_glasso,
        port_SR_data$SR_elastic,
        port_SR_data$SR_rope,
        port_SR_data$SR_tlasso,
        port_SR_data$SR_gslope,
        port_SR_data$SR_tslope
      )
    )
    
    data_weights <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        port_weights_data$w_ledoit,
        port_weights_data$w_glasso,
        port_weights_data$w_elastic,
        port_weights_data$w_rope,
        port_weights_data$w_tlasso,
        port_weights_data$w_gslope,
        port_weights_data$w_tslope
      )
    )
    
    data_cond <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        cond$cond_glasso,
        cond$cond_elastic,
        cond$cond_rope,
        cond$cond_tlasso,
        cond$cond_gslope,
        cond$cond_tslope
      )
    )
    
    data_frob <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        frob$frob_glasso,
        frob$frob_elastic,
        frob$frob_rope,
        frob$frob_tlasso,
        frob$frob_gslope,
        frob$frob_tslope
      )
    )
    
    data_TP <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        TP_rt$TP_glasso,
        TP_rt$TP_elastic,
        TP_rt$TP_rope,
        TP_rt$TP_tlasso,
        TP_rt$TP_gslope,
        TP_rt$TP_tslope
      )
    )
    
    data_FP <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        FP_rt$FP_sample,
        FP_rt$FP_glasso,
        FP_rt$FP_elastic,
        FP_rt$FP_rope,
        FP_rt$FP_tlasso,
        FP_rt$FP_gslope,
        FP_rt$FP_tslope
      )
    )
    
    data_TN <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        TN_rt$TN_glasso,
        TN_rt$TN_elastic,
        TN_rt$TN_rope,
        TN_rt$TN_tlasso,
        TN_rt$TN_gslope,
        TN_rt$TN_tslope
      )
    )
    
    
    data_FN <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        FN_rt$FN_glasso,
        FN_rt$FN_elastic,
        FN_rt$FN_rope,
        FN_rt$FN_tlasso,
        FN_rt$FN_gslope,
        FN_rt$FN_tslope
      )
    )
    
    data_F1 <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        F1$F1_glasso,
        F1$F1_elastic,
        F1$F1_rope,
        F1$F1_tlasso,
        F1$F1_gslope,
        F1$F1_tslope
      )
    )
    
    data_ACC <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        Acc$ACC_glasso,
        Acc$ACC_elastic,
        Acc$ACC_rope,
        Acc$ACC_tlasso,
        Acc$ACC_gslope,
        Acc$ACC_tslope
      )
    )
    
    data_entropy <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        Entropy$entropy_glasso,
        Entropy$entropy_elastic,
        Entropy$entropy_rope,
        Entropy$entropy_tlasso,
        Entropy$entropy_gslope,
        Entropy$entropy_tslope
      )
    )
    
    data_FDR <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        FDR_data$FDP_glasso,
        FDR_data$FDP_elastic,
        FDR_data$FDP_rope,
        FDR_data$FDP_tlasso,
        FDR_data$FDP_gslope,
        FDR_data$FDP_tslope
      )
    )
    
    data_localFDR <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        localFDR_data$localFDP_glasso,
        localFDR_data$localFDP_elastic,
        localFDR_data$localFDP_rope,
        localFDR_data$localFDP_tlasso,
        localFDR_data$localFDP_gslope,
        localFDR_data$localFDP_tslope
      )
    )
    
    output   = list(
      data_oracle,
      data_empirical,
      data_actual,
      data_means,
      data_SR,
      data_weights,
      data_cond,
      data_frob,
      data_TP,
      data_FP,
      data_TN,
      data_FN,
      data_F1,
      data_ACC,
      data_entropy,
      data_FDR,
      data_localFDR,
      Avg_means,
      Avg_std,
      Avg_means_port,
      Avg_std_port
    )
    
    names(output) <-
      paste(
        list(
          "data_oracle",
          "data_empirical",
          "data_actual",
          "data_means",
          "data_SR",
          "data_weights",
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
          "Std_performance_measures",
          "Average_portfolio_measures",
          "Std_portfolio_measures"
        )
      )
    
    return(output)
  }

##39##
BoxPlot_simulation_tstudent <- function(N, p, graph_type, v, u, i) {
  a = oracle_precision_matrix_generation(N, p, graph_type, v, u)
  
  i = i
  
  perf_measures = list()
  port_measures = list()
  
  for (i in 1:i) {
    b = data_series_generation(N, a$sigma, 0.05, 4, T_student = TRUE)
    
    c = sample_precision_matrix_estimation(b$data, N, 0.05, 4, progress = TRUE)
    
    d = performance_measures(
      b$omega_T,
      c$inv_sample,
      c$inv_glasso,
      c$inv_elastic,
      c$inv_rope,
      c$inv_tlasso,
      c$inv_gslope,
      c$inv_tslope
    )
    
    e = portfolio_risk_measures(
      b$data,
      b$omega_T,
      b$sigma_T,
      c$sigma_sample,
      c$sigma_ledoit,
      c$sigma_glasso,
      c$sigma_elastic,
      c$sigma_rope,
      c$sigma_tlasso,
      c$sigma_gslope,
      c$sigma_tslope,
      c$inv_sample,
      c$inv_ledoit,
      c$inv_glasso,
      c$inv_elastic,
      c$inv_rope,
      c$inv_tlasso,
      c$inv_gslope,
      c$inv_tslope
    )
    
    perf_measures[[i]] = d
    port_measures[[i]] = e
    
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
  
  oracle_risk_data = data.frame()
  for (i in 1:i) {
    oracle_risk_data = rbind(port_measures[[i]]$oracle_risk, oracle_risk_data)
  }
  
  empirical_risk_data = data.frame()
  for (i in 1:i) {
    empirical_risk_data = rbind(port_measures[[i]]$empirical_risk, empirical_risk_data)
  }
  
  actual_risk_data = data.frame()
  for (i in 1:i) {
    actual_risk_data = rbind(port_measures[[i]]$actual_risk, actual_risk_data)
  }
  
  port_mean_data = data.frame()
  for (i in 1:i) {
    port_mean_data = rbind(port_measures[[i]]$means, port_mean_data)
  }
  
  port_SR_data = data.frame()
  for (i in 1:i) {
    port_SR_data = rbind(port_measures[[i]]$SR, port_SR_data)
  }
  
  port_weights_data = data.frame()
  for (i in 1:i) {
    port_weights_data = rbind(port_measures[[i]]$weights, port_weights_data)
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
  Avg_oracle = colMeans(oracle_risk_data)
  Avg_empirical = colMeans(empirical_risk_data)
  Avg_actual = colMeans(actual_risk_data)
  Avg_mean = colMeans(port_mean_data)
  Avg_SR = colMeans(port_SR_data)
  
  Std_cond = apply(cond, 2, std)
  Std_frob = apply(frob, 2, std)
  Std_TP = apply(TP_rt, 2, std)
  Std_FP = apply(FP_rt, 2, std)
  Std_TN = apply(TN_rt, 2, std)
  Std_FN = apply(FN_rt, 2, std)
  Std_F1 = apply(F1, 2, std)
  Std_Acc = apply(Acc, 2, std)
  Std_Entropy = apply(Entropy, 2, std)
  Std_FDR = apply(FDR_data, 2, std)
  Std_localFDR = apply(localFDR_data, 2, std)
  Std_oracle = apply(oracle_risk_data, 2, std)
  Std_empirical = apply(empirical_risk_data, 2, std)
  Std_actual = apply(actual_risk_data, 2, std)
  Std_mean = apply(port_mean_data, 2, std)
  Std_SR = apply(port_SR_data, 2, std)
  
  
  row = list("sample",
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
  
  row_port = list("sample",
                  "ledoit",
                  "glasso",
                  "elastic",
                  "rope",
                  "tlasso",
                  "gslope",
                  "tslope")
  
  Avg_means_port = data.frame(
    row.names = row_port,
    Avg_oracle,
    Avg_empirical,
    Avg_actual,
    Avg_mean,
    Avg_SR
  )
  
  Avg_std_port = data.frame(
    row.names = row_port,
    Std_oracle,
    Std_empirical,
    Std_actual,
    Std_mean,
    Std_SR
  )
  
  data_oracle <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      oracle_risk_data$oracle_sample,
      oracle_risk_data$oracle_ledoit,
      oracle_risk_data$oracle_glasso,
      oracle_risk_data$oracle_elastic,
      oracle_risk_data$oracle_rope,
      oracle_risk_data$oracle_tlasso,
      oracle_risk_data$oracle_gslope,
      oracle_risk_data$oracle_tslope
    )
  )
  
  data_empirical <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      empirical_risk_data$empirical_sample,
      empirical_risk_data$empirical_ledoit,
      empirical_risk_data$empirical_glasso,
      empirical_risk_data$empirical_elastic,
      empirical_risk_data$empirical_rope,
      empirical_risk_data$empirical_tlasso,
      empirical_risk_data$empirical_gslope,
      empirical_risk_data$empirical_tslope
    )
  )
  
  data_actual <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      actual_risk_data$actual_sample,
      actual_risk_data$actual_ledoit,
      actual_risk_data$actual_glasso,
      actual_risk_data$actual_elastic,
      actual_risk_data$actual_rope,
      actual_risk_data$actual_tlasso,
      actual_risk_data$actual_gslope,
      actual_risk_data$actual_tslope
    )
  )
  
  data_means <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      port_mean_data$mean_sample,
      port_mean_data$mean_ledoit,
      port_mean_data$mean_glasso,
      port_mean_data$mean_elastic,
      port_mean_data$mean_rope,
      port_mean_data$mean_tlasso,
      port_mean_data$mean_gslope,
      port_mean_data$mean_tslope
    )
  )
  
  data_SR <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      port_SR_data$SR_sample,
      port_SR_data$SR_ledoit,
      port_SR_data$SR_glasso,
      port_SR_data$SR_elastic,
      port_SR_data$SR_rope,
      port_SR_data$SR_tlasso,
      port_SR_data$SR_gslope,
      port_SR_data$SR_tslope
    )
  )
  
  data_weights <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      port_weights_data$w_sample,
      port_weights_data$w_ledoit,
      port_weights_data$w_glasso,
      port_weights_data$w_elastic,
      port_weights_data$w_rope,
      port_weights_data$w_tlasso,
      port_weights_data$w_gslope,
      port_weights_data$w_tslope
    )
  )
  
  data_cond <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      cond$cond_sample,
      cond$cond_glasso,
      cond$cond_elastic,
      cond$cond_rope,
      cond$cond_tlasso,
      cond$cond_gslope,
      cond$cond_tslope
    )
  )
  
  data_frob <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      frob$frob_sample,
      frob$frob_glasso,
      frob$frob_elastic,
      frob$frob_rope,
      frob$frob_tlasso,
      frob$frob_gslope,
      frob$frob_tslope
    )
  )
  
  data_TP <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      TP_rt$TP_sample,
      TP_rt$TP_glasso,
      TP_rt$TP_elastic,
      TP_rt$TP_rope,
      TP_rt$TP_tlasso,
      TP_rt$TP_gslope,
      TP_rt$TP_tslope
    )
  )
  
  data_FP <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      FP_rt$FP_sample,
      FP_rt$FP_glasso,
      FP_rt$FP_elastic,
      FP_rt$FP_rope,
      FP_rt$FP_tlasso,
      FP_rt$FP_gslope,
      FP_rt$FP_tslope
    )
  )
  
  data_TN <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      TN_rt$TN_sample,
      TN_rt$TN_glasso,
      TN_rt$TN_elastic,
      TN_rt$TN_rope,
      TN_rt$TN_tlasso,
      TN_rt$TN_gslope,
      TN_rt$TN_tslope
    )
  )
  
  
  data_FN <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      FN_rt$FN_sample,
      FN_rt$FN_glasso,
      FN_rt$FN_elastic,
      FN_rt$FN_rope,
      FN_rt$FN_tlasso,
      FN_rt$FN_gslope,
      FN_rt$FN_tslope
    )
  )
  
  data_F1 <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      F1$F1_sample,
      F1$F1_glasso,
      F1$F1_elastic,
      F1$F1_rope,
      F1$F1_tlasso,
      F1$F1_gslope,
      F1$F1_tslope
    )
  )
  
  data_ACC <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      Acc$ACC_sample,
      Acc$ACC_glasso,
      Acc$ACC_elastic,
      Acc$ACC_rope,
      Acc$ACC_tlasso,
      Acc$ACC_gslope,
      Acc$ACC_tslope
    )
  )
  
  data_entropy <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      Entropy$entropy_sample,
      Entropy$entropy_glasso,
      Entropy$entropy_elastic,
      Entropy$entropy_rope,
      Entropy$entropy_tlasso,
      Entropy$entropy_gslope,
      Entropy$entropy_tslope
    )
  )
  
  data_FDR <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      FDR_data$FDP_sample,
      FDR_data$FDP_glasso,
      FDR_data$FDP_elastic,
      FDR_data$FDP_rope,
      FDR_data$FDP_tlasso,
      FDR_data$FDP_gslope,
      FDR_data$FDP_tslope
    )
  )
  
  data_localFDR <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      localFDR_data$localFDP_sample,
      localFDR_data$localFDP_glasso,
      localFDR_data$localFDP_elastic,
      localFDR_data$localFDP_rope,
      localFDR_data$localFDP_tlasso,
      localFDR_data$localFDP_gslope,
      localFDR_data$localFDP_tslope
    )
  )
  
  output   = list(
    data_oracle,
    data_empirical,
    data_actual,
    data_means,
    data_SR,
    data_weights,
    data_cond,
    data_frob,
    data_TP,
    data_FP,
    data_TN,
    data_FN,
    data_F1,
    data_ACC,
    data_entropy,
    data_FDR,
    data_localFDR,
    Avg_means,
    Avg_std,
    Avg_means_port,
    Avg_std_port
  )
  
  names(output) <-
    paste(
      list(
        "data_oracle",
        "data_empirical",
        "data_actual",
        "data_means",
        "data_SR",
        "data_weights",
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
        "Std_performance_measures",
        "Average_portfolio_measures",
        "Std_portfolio_measures"
      )
    )
  
  return(output)
}

##40##
sample_precision_matrix_estimation_N_less_p <-
  function(data, n, alpha_level, df, progress = TRUE, factor = 0.75) {
    
    #LEDOIT
    sigma_ledoit = nlshrink_cov(data)
    rho_ledoit   = cov2cor(sigma_ledoit)
    inv_ledoit   = inv(sigma_ledoit)
    
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
    output_elastic = gelnet(S = cor(data),
                            lambda = lambda,
                            alpha = 0.5)
    
    #From corr matrix to covariance, then precision
    sigma_elastic = cor2cov(data, output_elastic$W)
    inv_elastic   = solve(sigma_elastic)
    inv_elastic[abs(inv_elastic) < 1e-04] = 0 #threshold to consider an entry equal to zero
    
    #GLASSO RIDGE (ROPE)
    output_rope = gelnet(S = cor(data),
                         lambda = lambda,
                         alpha = 0)
    
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
    lambdas = create_lambda(cor(data), n = n, alpha = alpha_level) * factor
    
    output_gslope = gslope_new(cor(data), lambdas, progress = progress)
    
    #From corr matrix to covariance, then precision
    inv_gslope  = invcor2invcov(data, output_gslope$precision_matrix)
    sigma_gslope    = solve(inv_gslope)
    inv_gslope[abs(inv_gslope) < 1e-04] <-
      0 #threshold to consider an entry equal to zero
    
    #TSLOPE
    output_tslope = tslope_fast_rolling(data,
                                        v,
                                        0.001,
                                        F,
                                        init_theta = matrix(0, ncol(data), ncol(data)),
                                        alpha_value = alpha_level,
                                        factor)
    sigma_tslope  = (output_tslope$S) #* (v/(v - 2))
    inv_tslope    = solve(sigma_tslope)
    inv_tslope[abs(inv_tslope) < 1e-04] <- 0
    
    #TOTAL OUTPUT
    output = list(
      sigma_glasso,
      sigma_ledoit,
      sigma_elastic,
      sigma_rope,
      sigma_tlasso,
      sigma_gslope,
      sigma_tslope,
      inv_ledoit,
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
          "sigma_ledoit",
          "sigma_glasso",
          "sigma_elastic",
          "sigma_rope",
          "sigma_tlasso",
          "sigma_gslope",
          "sigma_tslope",
          "inv_ledoit",
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

##41##
performance_measures_N_less_p <- function(omega,
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
    paste(list(
      "TP_glasso",
      "TP_elastic",
      "TP_rope",
      "TP_tlasso",
      "TP_gslope",
      "TP_tslope"
    ))
  
  TNrate = list(
    TN(inv_glasso, omega)[[2]],
    TN(inv_elastic, omega)[[2]],
    TN(inv_rope, omega)[[2]],
    TN(inv_tlasso, omega)[[2]],
    TN(inv_gslope, omega)[[2]],
    TN(inv_tslope, omega)[[2]]
  )
  
  names(TNrate) <-
    paste(list(
      "TN_glasso",
      "TN_elastic",
      "TN_rope",
      "TN_tlasso",
      "TN_gslope",
      "TN_tslope"
    ))
  
  FPrate = list(
    FP(inv_glasso, omega)[[2]],
    FP(inv_elastic, omega)[[2]],
    FP(inv_rope, omega)[[2]],
    FP(inv_tlasso, omega)[[2]],
    FP(inv_gslope, omega)[[2]],
    FP(inv_tslope, omega)[[2]]
  )
  
  names(FPrate) <-
    paste(list(
      "FP_glasso",
      "FP_elastic",
      "FP_rope",
      "FP_tlasso",
      "FP_gslope",
      "FP_tslope"
    ))
  
  FNrate = list(
    FN(inv_glasso, omega)[[2]],
    FN(inv_elastic, omega)[[2]],
    FN(inv_rope, omega)[[2]],
    FN(inv_tlasso, omega)[[2]],
    FN(inv_gslope, omega)[[2]],
    FN(inv_tslope, omega)[[2]]
  )
  
  names(FNrate) <-
    paste(list(
      "FN_glasso",
      "FN_elastic",
      "FN_rope",
      "FN_tlasso",
      "FN_gslope",
      "FN_tslope"
    ))
  
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
    paste(list(
      "F1_glasso",
      "F1_elastic",
      "F1_rope",
      "F1_tlasso",
      "F1_gslope",
      "F1_tslope"
    ))
  
  #ACCURACY
  positive <- max(c(sum(upper(
    properAdjacent(omega)
  )), 1))
  p = nrow(omega)
  negative <- max(c(sum(!upper(
    properAdjacent(omega)
  )), 1))
  
  ACC = list(
    (TP(inv_glasso, omega)[[1]] + (TN(inv_glasso, omega)[[1]])) / (positive + negative),
    (TP(inv_elastic, omega)[[1]] + (TN(
      inv_elastic, omega
    )[[1]])) / (positive + negative),
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

##42##
BoxPlot_simulation_tstudent_N_less_p <-
  function(N, p, graph_type, v, u, i) {
    a = oracle_precision_matrix_generation(N, p, graph_type, v, u)
    
    i = i
    
    perf_measures = list()
    port_measures = list()
    
    for (i in 1:i) {
      b = data_series_generation(N, a$sigma, 0.05, 4, T_student = TRUE)
      
      c = sample_precision_matrix_estimation_N_less_p(b$data, N, 0.05, 4, progress = TRUE)
      
      d = performance_measures_N_less_p(
        b$omega_T,
        c$inv_glasso,
        c$inv_elastic,
        c$inv_rope,
        c$inv_tlasso,
        c$inv_gslope,
        c$inv_tslope
      )
      
      e = portfolio_risk_measures_N_less_p(
        b$data,
        b$omega_T,
        b$sigma_T,
        c$sigma_ledoit,
        c$sigma_glasso,
        c$sigma_elastic,
        c$sigma_rope,
        c$sigma_tlasso,
        c$sigma_gslope,
        c$sigma_tslope,
        c$inv_ledoit,
        c$inv_glasso,
        c$inv_elastic,
        c$inv_rope,
        c$inv_tlasso,
        c$inv_gslope,
        c$inv_tslope
      )
      
      perf_measures[[i]] = d
      port_measures[[i]] = e
      
      
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
    
    oracle_risk_data = data.frame()
    for (i in 1:i) {
      oracle_risk_data = rbind(port_measures[[i]]$oracle_risk, oracle_risk_data)
    }
    
    empirical_risk_data = data.frame()
    for (i in 1:i) {
      empirical_risk_data = rbind(port_measures[[i]]$empirical_risk, empirical_risk_data)
    }
    
    actual_risk_data = data.frame()
    for (i in 1:i) {
      actual_risk_data = rbind(port_measures[[i]]$actual_risk, actual_risk_data)
    }
    
    port_mean_data = data.frame()
    for (i in 1:i) {
      port_mean_data = rbind(port_measures[[i]]$means, port_mean_data)
    }
    
    port_SR_data = data.frame()
    for (i in 1:i) {
      port_SR_data = rbind(port_measures[[i]]$SR, port_SR_data)
    }
    
    port_weights_data = data.frame()
    for (i in 1:i) {
      port_weights_data = rbind(port_measures[[i]]$weights, port_weights_data)
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
    Avg_oracle = colMeans(oracle_risk_data)
    Avg_empirical = colMeans(empirical_risk_data)
    Avg_actual = colMeans(actual_risk_data)
    Avg_mean = colMeans(port_mean_data)
    Avg_SR = colMeans(port_SR_data)
    
    Std_cond = apply(cond, 2, std)
    Std_frob = apply(frob, 2, std)
    Std_TP = apply(TP_rt, 2, std)
    Std_FP = apply(FP_rt, 2, std)
    Std_TN = apply(TN_rt, 2, std)
    Std_FN = apply(FN_rt, 2, std)
    Std_F1 = apply(F1, 2, std)
    Std_Acc = apply(Acc, 2, std)
    Std_Entropy = apply(Entropy, 2, std)
    Std_FDR = apply(FDR_data, 2, std)
    Std_localFDR = apply(localFDR_data, 2, std)
    Std_oracle = apply(oracle_risk_data, 2, std)
    Std_empirical = apply(empirical_risk_data, 2, std)
    Std_actual = apply(actual_risk_data, 2, std)
    Std_mean = apply(port_mean_data, 2, std)
    Std_SR = apply(port_SR_data, 2, std)
    
    row = list("glasso",
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
    
    row_port = list("ledoit",
                    "glasso",
                    "elastic",
                    "rope",
                    "tlasso",
                    "gslope",
                    "tslope")
    
    Avg_means_port = data.frame(
      row.names = row_port,
      Avg_oracle,
      Avg_empirical,
      Avg_actual,
      Avg_mean,
      Avg_SR
    )
    
    Avg_std_port = data.frame(
      row.names = row_port,
      Std_oracle,
      Std_empirical,
      Std_actual,
      Std_mean,
      Std_SR
    )
    
    data_oracle <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        oracle_risk_data$oracle_ledoit,
        oracle_risk_data$oracle_glasso,
        oracle_risk_data$oracle_elastic,
        oracle_risk_data$oracle_rope,
        oracle_risk_data$oracle_tlasso,
        oracle_risk_data$oracle_gslope,
        oracle_risk_data$oracle_tslope
      )
    )
    
    data_empirical <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        empirical_risk_data$empirical_ledoit,
        empirical_risk_data$empirical_glasso,
        empirical_risk_data$empirical_elastic,
        empirical_risk_data$empirical_rope,
        empirical_risk_data$empirical_tlasso,
        empirical_risk_data$empirical_gslope,
        empirical_risk_data$empirical_tslope
      )
    )
    
    data_actual <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        actual_risk_data$actual_ledoit,
        actual_risk_data$actual_glasso,
        actual_risk_data$actual_elastic,
        actual_risk_data$actual_rope,
        actual_risk_data$actual_tlasso,
        actual_risk_data$actual_gslope,
        actual_risk_data$actual_tslope
      )
    )
    
    data_means <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        port_mean_data$mean_ledoit,
        port_mean_data$mean_glasso,
        port_mean_data$mean_elastic,
        port_mean_data$mean_rope,
        port_mean_data$mean_tlasso,
        port_mean_data$mean_gslope,
        port_mean_data$mean_tslope
      )
    )
    
    data_SR <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        port_SR_data$SR_ledoit,
        port_SR_data$SR_glasso,
        port_SR_data$SR_elastic,
        port_SR_data$SR_rope,
        port_SR_data$SR_tlasso,
        port_SR_data$SR_gslope,
        port_SR_data$SR_tslope
      )
    )
    
    data_weights <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        port_weights_data$w_ledoit,
        port_weights_data$w_glasso,
        port_weights_data$w_elastic,
        port_weights_data$w_rope,
        port_weights_data$w_tlasso,
        port_weights_data$w_gslope,
        port_weights_data$w_tslope
      )
    )
    
    data_cond <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        cond$cond_sample,
        cond$cond_ledoit,
        cond$cond_glasso,
        cond$cond_elastic,
        cond$cond_rope,
        cond$cond_tlasso,
        cond$cond_gslope,
        cond$cond_tslope
      )
    )
    
    data_frob <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        frob$frob_glasso,
        frob$frob_elastic,
        frob$frob_rope,
        frob$frob_tlasso,
        frob$frob_gslope,
        frob$frob_tslope
      )
    )
    
    data_TP <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        TP_rt$TP_glasso,
        TP_rt$TP_elastic,
        TP_rt$TP_rope,
        TP_rt$TP_tlasso,
        TP_rt$TP_gslope,
        TP_rt$TP_tslope
      )
    )
    
    data_FP <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        FP_rt$FP_glasso,
        FP_rt$FP_elastic,
        FP_rt$FP_rope,
        FP_rt$FP_tlasso,
        FP_rt$FP_gslope,
        FP_rt$FP_tslope
      )
    )
    
    data_TN <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        TN_rt$TN_glasso,
        TN_rt$TN_elastic,
        TN_rt$TN_rope,
        TN_rt$TN_tlasso,
        TN_rt$TN_gslope,
        TN_rt$TN_tslope
      )
    )
    
    
    data_FN <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        FN_rt$FN_glasso,
        FN_rt$FN_elastic,
        FN_rt$FN_rope,
        FN_rt$FN_tlasso,
        FN_rt$FN_gslope,
        FN_rt$FN_tslope
      )
    )
    
    data_F1 <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        F1$F1_glasso,
        F1$F1_elastic,
        F1$F1_rope,
        F1$F1_tlasso,
        F1$F1_gslope,
        F1$F1_tslope
      )
    )
    
    data_ACC <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        Acc$ACC_glasso,
        Acc$ACC_elastic,
        Acc$ACC_rope,
        Acc$ACC_tlasso,
        Acc$ACC_gslope,
        Acc$ACC_tslope
      )
    )
    
    data_entropy <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        Entropy$entropy_glasso,
        Entropy$entropy_elastic,
        Entropy$entropy_rope,
        Entropy$entropy_tlasso,
        Entropy$entropy_gslope,
        Entropy$entropy_tslope
      )
    )
    
    data_FDR <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        FDR_data$FDP_glasso,
        FDR_data$FDP_elastic,
        FDR_data$FDP_rope,
        FDR_data$FDP_tlasso,
        FDR_data$FDP_gslope,
        FDR_data$FDP_tslope
      )
    )
    
    data_localFDR <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        localFDR_data$localFDP_glasso,
        localFDR_data$localFDP_elastic,
        localFDR_data$localFDP_rope,
        localFDR_data$localFDP_tlasso,
        localFDR_data$localFDP_gslope,
        localFDR_data$localFDP_tslope
      )
    )
    
    output   = list(
      data_oracle,
      data_empirical,
      data_actual,
      data_means,
      data_SR,
      data_weights,
      data_cond,
      data_frob,
      data_TP,
      data_FP,
      data_TN,
      data_FN,
      data_F1,
      data_ACC,
      data_entropy,
      data_FDR,
      data_localFDR,
      Avg_means,
      Avg_std,
      Avg_means_port,
      Avg_std_port
    )
    
    names(output) <-
      paste(
        list(
          "data_oracle",
          "data_empirical",
          "data_actual",
          "data_means",
          "data_SR",
          "data_weights",
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
          "Std_performance_measures",
          "Average_portfolio_measures",
          "Std_portfolio_measures"
        )
      )
    
    return(output)
  }

##43##
BoxPlot_simulation_mix <- function(N, p, graph_type, v, u, i) {
  a = oracle_precision_matrix_generation(N, p, graph_type, v, u)
  
  i = i
  
  perf_measures = list()
  port_measures = list()
  
  for (i in 1:i) {
    b = rnvmix(
      N,
      rmix = "inverse.gamma",
      df = 4,
      scale = a$sigma,
      loc = rep(0.05, p)
    )
    
    c = sample_precision_matrix_estimation(b, N, 0.05, 4, progress = TRUE)
    
    d = performance_measures(
      a$omega,
      c$inv_sample,
      c$inv_glasso,
      c$inv_elastic,
      c$inv_rope,
      c$inv_tlasso,
      c$inv_gslope,
      c$inv_tslope
    )
    
    e = portfolio_risk_measures(
      b,
      a$omega,
      a$sigma,
      c$sigma_sample,
      c$sigma_ledoit,
      c$sigma_glasso,
      c$sigma_elastic,
      c$sigma_rope,
      c$sigma_tlasso,
      c$sigma_gslope,
      c$sigma_tslope,
      c$inv_sample,
      c$inv_ledoit,
      c$inv_glasso,
      c$inv_elastic,
      c$inv_rope,
      c$inv_tlasso,
      c$inv_gslope,
      c$inv_tslope
    )
    
    perf_measures[[i]] = d
    port_measures[[i]] = e
    
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
  
  oracle_risk_data = data.frame()
  for (i in 1:i) {
    oracle_risk_data = rbind(port_measures[[i]]$oracle_risk, oracle_risk_data)
  }
  
  empirical_risk_data = data.frame()
  for (i in 1:i) {
    empirical_risk_data = rbind(port_measures[[i]]$empirical_risk, empirical_risk_data)
  }
  
  actual_risk_data = data.frame()
  for (i in 1:i) {
    actual_risk_data = rbind(port_measures[[i]]$actual_risk, actual_risk_data)
  }
  
  port_mean_data = data.frame()
  for (i in 1:i) {
    port_mean_data = rbind(port_measures[[i]]$means, port_mean_data)
  }
  
  port_SR_data = data.frame()
  for (i in 1:i) {
    port_SR_data = rbind(port_measures[[i]]$SR, port_SR_data)
  }
  
  port_weights_data = data.frame()
  for (i in 1:i) {
    port_weights_data = rbind(port_measures[[i]]$weights, port_weights_data)
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
  Avg_oracle = colMeans(oracle_risk_data)
  Avg_empirical = colMeans(empirical_risk_data)
  Avg_actual = colMeans(actual_risk_data)
  Avg_mean = colMeans(port_mean_data)
  Avg_SR = colMeans(port_SR_data)
  
  Std_cond = apply(cond, 2, std)
  Std_frob = apply(frob, 2, std)
  Std_TP = apply(TP_rt, 2, std)
  Std_FP = apply(FP_rt, 2, std)
  Std_TN = apply(TN_rt, 2, std)
  Std_FN = apply(FN_rt, 2, std)
  Std_F1 = apply(F1, 2, std)
  Std_Acc = apply(Acc, 2, std)
  Std_Entropy = apply(Entropy, 2, std)
  Std_FDR = apply(FDR_data, 2, std)
  Std_localFDR = apply(localFDR_data, 2, std)
  Std_oracle = apply(oracle_risk_data, 2, std)
  Std_empirical = apply(empirical_risk_data, 2, std)
  Std_actual = apply(actual_risk_data, 2, std)
  Std_mean = apply(port_mean_data, 2, std)
  Std_SR = apply(port_SR_data, 2, std)
  
  
  row = list("sample",
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
  
  row_port = list("sample",
                  "ledoit",
                  "glasso",
                  "elastic",
                  "rope",
                  "tlasso",
                  "gslope",
                  "tslope")
  
  Avg_means_port = data.frame(
    row.names = row_port,
    Avg_oracle,
    Avg_empirical,
    Avg_actual,
    Avg_mean,
    Avg_SR
  )
  
  Avg_std_port = data.frame(
    row.names = row_port,
    Std_oracle,
    Std_empirical,
    Std_actual,
    Std_mean,
    Std_SR
  )
  
  data_oracle <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      oracle_risk_data$oracle_sample,
      oracle_risk_data$oracle_ledoit,
      oracle_risk_data$oracle_glasso,
      oracle_risk_data$oracle_elastic,
      oracle_risk_data$oracle_rope,
      oracle_risk_data$oracle_tlasso,
      oracle_risk_data$oracle_gslope,
      oracle_risk_data$oracle_tslope
    )
  )
  
  data_empirical <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      empirical_risk_data$empirical_sample,
      empirical_risk_data$empirical_ledoit,
      empirical_risk_data$empirical_glasso,
      empirical_risk_data$empirical_elastic,
      empirical_risk_data$empirical_rope,
      empirical_risk_data$empirical_tlasso,
      empirical_risk_data$empirical_gslope,
      empirical_risk_data$empirical_tslope
    )
  )
  
  data_actual <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      actual_risk_data$actual_sample,
      actual_risk_data$actual_ledoit,
      actual_risk_data$actual_glasso,
      actual_risk_data$actual_elastic,
      actual_risk_data$actual_rope,
      actual_risk_data$actual_tlasso,
      actual_risk_data$actual_gslope,
      actual_risk_data$actual_tslope
    )
  )
  
  data_means <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      port_mean_data$mean_sample,
      port_mean_data$mean_ledoit,
      port_mean_data$mean_glasso,
      port_mean_data$mean_elastic,
      port_mean_data$mean_rope,
      port_mean_data$mean_tlasso,
      port_mean_data$mean_gslope,
      port_mean_data$mean_tslope
    )
  )
  
  data_SR <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      port_SR_data$SR_sample,
      port_SR_data$SR_ledoit,
      port_SR_data$SR_glasso,
      port_SR_data$SR_elastic,
      port_SR_data$SR_rope,
      port_SR_data$SR_tlasso,
      port_SR_data$SR_gslope,
      port_SR_data$SR_tslope
    )
  )
  
  data_weights <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Ledoit", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      port_weights_data$w_sample,
      port_weights_data$w_ledoit,
      port_weights_data$w_glasso,
      port_weights_data$w_elastic,
      port_weights_data$w_rope,
      port_weights_data$w_tlasso,
      port_weights_data$w_gslope,
      port_weights_data$w_tslope
    )
  )
  
  data_cond <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      cond$cond_sample,
      cond$cond_glasso,
      cond$cond_elastic,
      cond$cond_rope,
      cond$cond_tlasso,
      cond$cond_gslope,
      cond$cond_tslope
    )
  )
  
  data_frob <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      frob$frob_sample,
      frob$frob_glasso,
      frob$frob_elastic,
      frob$frob_rope,
      frob$frob_tlasso,
      frob$frob_gslope,
      frob$frob_tslope
    )
  )
  
  data_TP <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      TP_rt$TP_sample,
      TP_rt$TP_glasso,
      TP_rt$TP_elastic,
      TP_rt$TP_rope,
      TP_rt$TP_tlasso,
      TP_rt$TP_gslope,
      TP_rt$TP_tslope
    )
  )
  
  data_FP <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      FP_rt$FP_sample,
      FP_rt$FP_glasso,
      FP_rt$FP_elastic,
      FP_rt$FP_rope,
      FP_rt$FP_tlasso,
      FP_rt$FP_gslope,
      FP_rt$FP_tslope
    )
  )
  
  data_TN <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      TN_rt$TN_sample,
      TN_rt$TN_glasso,
      TN_rt$TN_elastic,
      TN_rt$TN_rope,
      TN_rt$TN_tlasso,
      TN_rt$TN_gslope,
      TN_rt$TN_tslope
    )
  )
  
  
  data_FN <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      FN_rt$FN_sample,
      FN_rt$FN_glasso,
      FN_rt$FN_elastic,
      FN_rt$FN_rope,
      FN_rt$FN_tlasso,
      FN_rt$FN_gslope,
      FN_rt$FN_tslope
    )
  )
  
  data_F1 <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      F1$F1_sample,
      F1$F1_glasso,
      F1$F1_elastic,
      F1$F1_rope,
      F1$F1_tlasso,
      F1$F1_gslope,
      F1$F1_tslope
    )
  )
  
  data_ACC <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      Acc$ACC_sample,
      Acc$ACC_glasso,
      Acc$ACC_elastic,
      Acc$ACC_rope,
      Acc$ACC_tlasso,
      Acc$ACC_gslope,
      Acc$ACC_tslope
    )
  )
  
  data_entropy <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      Entropy$entropy_sample,
      Entropy$entropy_glasso,
      Entropy$entropy_elastic,
      Entropy$entropy_rope,
      Entropy$entropy_tlasso,
      Entropy$entropy_gslope,
      Entropy$entropy_tslope
    )
  )
  
  data_FDR <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      FDR_data$FDP_sample,
      FDR_data$FDP_glasso,
      FDR_data$FDP_elastic,
      FDR_data$FDP_rope,
      FDR_data$FDP_tlasso,
      FDR_data$FDP_gslope,
      FDR_data$FDP_tslope
    )
  )
  
  data_localFDR <- data.frame(
    name = c(
      rep("Sample", i),
      rep("Glasso", i),
      rep("Elastic", i),
      rep("Rope", i),
      rep("Tlasso", i),
      rep("Gslope", i),
      rep("Tslope", i)
    ),
    value = c(
      localFDR_data$localFDP_sample,
      localFDR_data$localFDP_glasso,
      localFDR_data$localFDP_elastic,
      localFDR_data$localFDP_rope,
      localFDR_data$localFDP_tlasso,
      localFDR_data$localFDP_gslope,
      localFDR_data$localFDP_tslope
    )
  )
  
  output   = list(
    data_oracle,
    data_empirical,
    data_actual,
    data_means,
    data_SR,
    data_weights,
    data_cond,
    data_frob,
    data_TP,
    data_FP,
    data_TN,
    data_FN,
    data_F1,
    data_ACC,
    data_entropy,
    data_FDR,
    data_localFDR,
    Avg_means,
    Avg_std,
    Avg_means_port,
    Avg_std_port
  )
  
  names(output) <-
    paste(
      list(
        "data_oracle",
        "data_empirical",
        "data_actual",
        "data_means",
        "data_SR",
        "data_weights",
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
        "Std_performance_measures",
        "Average_portfolio_measures",
        "Std_portfolio_measures"
      )
    )
  
  return(output)
}

##44##
BoxPlot_simulation_mix_N_less_p <-
  function(N, p, graph_type, v, u, i) {
    a = oracle_precision_matrix_generation(N, p, graph_type, v, u)
    
    i = i
    
    perf_measures = list()
    port_measures = list()
    
    for (i in 1:i) {
      b = rnvmix(
        N,
        rmix = "inverse.gamma",
        df = 4,
        scale = a$sigma,
        loc = rep(0.05, p)
      )
      
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
      
      e = portfolio_risk_measures_N_less_p(
        b,
        a$omega,
        a$sigma,
        c$sigma_ledoit,
        c$sigma_glasso,
        c$sigma_elastic,
        c$sigma_rope,
        c$sigma_tlasso,
        c$sigma_gslope,
        c$sigma_tslope,
        c$inv_ledoit,
        c$inv_glasso,
        c$inv_elastic,
        c$inv_rope,
        c$inv_tlasso,
        c$inv_gslope,
        c$inv_tslope
      )
      
      perf_measures[[i]] = d
      port_measures[[i]] = e
      
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
    
    oracle_risk_data = data.frame()
    for (i in 1:i) {
      oracle_risk_data = rbind(port_measures[[i]]$oracle_risk, oracle_risk_data)
    }
    
    empirical_risk_data = data.frame()
    for (i in 1:i) {
      empirical_risk_data = rbind(port_measures[[i]]$empirical_risk, empirical_risk_data)
    }
    
    actual_risk_data = data.frame()
    for (i in 1:i) {
      actual_risk_data = rbind(port_measures[[i]]$actual_risk, actual_risk_data)
    }
    
    port_mean_data = data.frame()
    for (i in 1:i) {
      port_mean_data = rbind(port_measures[[i]]$means, port_mean_data)
    }
    
    port_SR_data = data.frame()
    for (i in 1:i) {
      port_SR_data = rbind(port_measures[[i]]$SR, port_SR_data)
    }
    
    port_weights_data = data.frame()
    for (i in 1:i) {
      port_weights_data = rbind(port_measures[[i]]$weights, port_weights_data)
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
    Avg_oracle = colMeans(oracle_risk_data)
    Avg_empirical = colMeans(empirical_risk_data)
    Avg_actual = colMeans(actual_risk_data)
    Avg_mean = colMeans(port_mean_data)
    Avg_SR = colMeans(port_SR_data)
    
    Std_cond = apply(cond, 2, std)
    Std_frob = apply(frob, 2, std)
    Std_TP = apply(TP_rt, 2, std)
    Std_FP = apply(FP_rt, 2, std)
    Std_TN = apply(TN_rt, 2, std)
    Std_FN = apply(FN_rt, 2, std)
    Std_F1 = apply(F1, 2, std)
    Std_Acc = apply(Acc, 2, std)
    Std_Entropy = apply(Entropy, 2, std)
    Std_FDR = apply(FDR_data, 2, std)
    Std_localFDR = apply(localFDR_data, 2, std)
    Std_oracle = apply(oracle_risk_data, 2, std)
    Std_empirical = apply(empirical_risk_data, 2, std)
    Std_actual = apply(actual_risk_data, 2, std)
    Std_mean = apply(port_mean_data, 2, std)
    Std_SR = apply(port_SR_data, 2, std)
    
    row = list("glasso",
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
    
    row_port = list("ledoit",
                    "glasso",
                    "elastic",
                    "rope",
                    "tlasso",
                    "gslope",
                    "tslope")
    
    Avg_means_port = data.frame(
      row.names = row_port,
      Avg_oracle,
      Avg_empirical,
      Avg_actual,
      Avg_mean,
      Avg_SR
    )
    
    Avg_std_port = data.frame(
      row.names = row_port,
      Std_oracle,
      Std_empirical,
      Std_actual,
      Std_mean,
      Std_SR
    )
    
    data_oracle <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        oracle_risk_data$oracle_ledoit,
        oracle_risk_data$oracle_glasso,
        oracle_risk_data$oracle_elastic,
        oracle_risk_data$oracle_rope,
        oracle_risk_data$oracle_tlasso,
        oracle_risk_data$oracle_gslope,
        oracle_risk_data$oracle_tslope
      )
    )
    
    data_empirical <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        empirical_risk_data$empirical_ledoit,
        empirical_risk_data$empirical_glasso,
        empirical_risk_data$empirical_elastic,
        empirical_risk_data$empirical_rope,
        empirical_risk_data$empirical_tlasso,
        empirical_risk_data$empirical_gslope,
        empirical_risk_data$empirical_tslope
      )
    )
    
    data_actual <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        actual_risk_data$actual_ledoit,
        actual_risk_data$actual_glasso,
        actual_risk_data$actual_elastic,
        actual_risk_data$actual_rope,
        actual_risk_data$actual_tlasso,
        actual_risk_data$actual_gslope,
        actual_risk_data$actual_tslope
      )
    )
    
    data_means <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        port_mean_data$mean_ledoit,
        port_mean_data$mean_glasso,
        port_mean_data$mean_elastic,
        port_mean_data$mean_rope,
        port_mean_data$mean_tlasso,
        port_mean_data$mean_gslope,
        port_mean_data$mean_tslope
      )
    )
    
    data_SR <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        port_SR_data$SR_ledoit,
        port_SR_data$SR_glasso,
        port_SR_data$SR_elastic,
        port_SR_data$SR_rope,
        port_SR_data$SR_tlasso,
        port_SR_data$SR_gslope,
        port_SR_data$SR_tslope
      )
    )
    
    data_weights <- data.frame(
      name = c(
        rep("Ledoit", i),
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        port_weights_data$w_ledoit,
        port_weights_data$w_glasso,
        port_weights_data$w_elastic,
        port_weights_data$w_rope,
        port_weights_data$w_tlasso,
        port_weights_data$w_gslope,
        port_weights_data$w_tslope
      )
    )
    
    data_cond <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        cond$cond_sample,
        cond$cond_ledoit,
        cond$cond_glasso,
        cond$cond_elastic,
        cond$cond_rope,
        cond$cond_tlasso,
        cond$cond_gslope,
        cond$cond_tslope
      )
    )
    
    data_frob <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        frob$frob_glasso,
        frob$frob_elastic,
        frob$frob_rope,
        frob$frob_tlasso,
        frob$frob_gslope,
        frob$frob_tslope
      )
    )
    
    data_TP <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        TP_rt$TP_glasso,
        TP_rt$TP_elastic,
        TP_rt$TP_rope,
        TP_rt$TP_tlasso,
        TP_rt$TP_gslope,
        TP_rt$TP_tslope
      )
    )
    
    data_FP <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        FP_rt$FP_glasso,
        FP_rt$FP_elastic,
        FP_rt$FP_rope,
        FP_rt$FP_tlasso,
        FP_rt$FP_gslope,
        FP_rt$FP_tslope
      )
    )
    
    data_TN <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        TN_rt$TN_glasso,
        TN_rt$TN_elastic,
        TN_rt$TN_rope,
        TN_rt$TN_tlasso,
        TN_rt$TN_gslope,
        TN_rt$TN_tslope
      )
    )
    
    
    data_FN <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        FN_rt$FN_glasso,
        FN_rt$FN_elastic,
        FN_rt$FN_rope,
        FN_rt$FN_tlasso,
        FN_rt$FN_gslope,
        FN_rt$FN_tslope
      )
    )
    
    data_F1 <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        F1$F1_glasso,
        F1$F1_elastic,
        F1$F1_rope,
        F1$F1_tlasso,
        F1$F1_gslope,
        F1$F1_tslope
      )
    )
    
    data_ACC <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        Acc$ACC_glasso,
        Acc$ACC_elastic,
        Acc$ACC_rope,
        Acc$ACC_tlasso,
        Acc$ACC_gslope,
        Acc$ACC_tslope
      )
    )
    
    data_entropy <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        Entropy$entropy_glasso,
        Entropy$entropy_elastic,
        Entropy$entropy_rope,
        Entropy$entropy_tlasso,
        Entropy$entropy_gslope,
        Entropy$entropy_tslope
      )
    )
    
    data_FDR <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        FDR_data$FDP_glasso,
        FDR_data$FDP_elastic,
        FDR_data$FDP_rope,
        FDR_data$FDP_tlasso,
        FDR_data$FDP_gslope,
        FDR_data$FDP_tslope
      )
    )
    
    data_localFDR <- data.frame(
      name = c(
        rep("Glasso", i),
        rep("Elastic", i),
        rep("Rope", i),
        rep("Tlasso", i),
        rep("Gslope", i),
        rep("Tslope", i)
      ),
      value = c(
        localFDR_data$localFDP_glasso,
        localFDR_data$localFDP_elastic,
        localFDR_data$localFDP_rope,
        localFDR_data$localFDP_tlasso,
        localFDR_data$localFDP_gslope,
        localFDR_data$localFDP_tslope
      )
    )
    
    output   = list(
      data_oracle,
      data_empirical,
      data_actual,
      data_means,
      data_SR,
      data_weights,
      data_cond,
      data_frob,
      data_TP,
      data_FP,
      data_TN,
      data_FN,
      data_F1,
      data_ACC,
      data_entropy,
      data_FDR,
      data_localFDR,
      Avg_means,
      Avg_std,
      Avg_means_port,
      Avg_std_port
    )
    
    names(output) <-
      paste(
        list(
          "data_oracle",
          "data_empirical",
          "data_actual",
          "data_means",
          "data_SR",
          "data_weights",
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
          "Std_performance_measures",
          "Average_portfolio_measures",
          "Std_portfolio_measures"
        )
      )
    
    return(output)
  }



##############################BoxPlots_Save#####################################

##45##
Box_Plot_sim_norm <- function(output_performance) {
  cond_plot <- output_performance$data_cond %>%
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
    ggtitle("Condition Number BoxPlot") +
    xlab("Methods")
  
  frob_plot <- output_performance$data_frob %>%
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
    ggtitle("Frobenius Norm BoxPlot") +
    xlab("Methods")
  
  TP_plot <- output_performance$data_TP %>%
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
    ggtitle("TP_rate BoxPlot") +
    xlab("Methods")
  
  FP_plot <- output_performance$data_FP %>%
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
    ggtitle("FP_rate BoxPlot") +
    xlab("Methods")
  
  TN_plot <- output_performance$data_TN %>%
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
    ggtitle("TN_rate BoxPlot") +
    xlab("Methods")
  
  FN_plot <- output_performance$data_FN %>%
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
    ggtitle("FN_rate BoxPlot") +
    xlab("Methods")
  
  F1_plot <- output_performance$data_F1 %>%
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
    ggtitle("F1_score BoxPlot") +
    xlab("Methods")
  
  Acc_plot <- output_performance$data_ACC %>%
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
    ggtitle("Accuracy BoxPlot") +
    xlab("Methods")
  
  entropy_plot <- output_performance$data_entropy %>%
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
    ggtitle("Entropy BoxPlot") +
    xlab("Methods")
  
  FDR_plot <- output_performance$data_FDR %>%
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
    ggtitle("FDR BoxPlot") +
    xlab("Methods")
  
  localFDR_plot <- output_performance$data_localFDR %>%
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
    ggtitle("localFDR BoxPlot") +
    xlab("Methods")
  
  output   = list(
    cond_plot,
    frob_plot,
    TP_plot,
    FP_plot,
    TN_plot,
    FN_plot,
    F1_plot,
    Acc_plot,
    entropy_plot,
    FDR_plot,
    localFDR_plot
  )
  
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

##46##
Box_Plot_sim_norm_N_less_p <- function(output_performance) {
  cond_plot <- output_performance$data_cond %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ggtitle("Condition Number BoxPlot") +
    xlab("Methods")
  
  frob_plot <- output_performance$data_frob %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ggtitle("Frobenius Norm BoxPlot") +
    xlab("Methods")
  
  TP_plot <- output_performance$data_TP %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("TP_rate BoxPlot") +
    xlab("Methods")
  
  FP_plot <- output_performance$data_FP %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("FP_rate BoxPlot") +
    xlab("Methods")
  
  TN_plot <- output_performance$data_TN %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("TN_rate BoxPlot") +
    xlab("Methods")
  
  FN_plot <- output_performance$data_FN %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("FN_rate BoxPlot") +
    xlab("Methods")
  
  F1_plot <- output_performance$data_F1 %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("F1_score BoxPlot") +
    xlab("Methods")
  
  Acc_plot <- output_performance$data_ACC %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("Accuracy BoxPlot") +
    xlab("Methods")
  
  entropy_plot <- output_performance$data_entropy %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ggtitle("Entropy BoxPlot") +
    xlab("Methods")
  
  FDR_plot <- output_performance$data_FDR %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("FDR BoxPlot") +
    xlab("Methods")
  
  localFDR_plot <- output_performance$data_localFDR %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.positon = "none", plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("localFDR BoxPlot") +
    xlab("Methods")
  
  output   = list(
    cond_plot,
    frob_plot,
    TP_plot,
    FP_plot,
    TN_plot,
    FN_plot,
    F1_plot,
    Acc_plot,
    entropy_plot,
    FDR_plot,
    localFDR_plot
  )
  
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

##47##
Box_Plot_sim <- function(output_performance) {
  cond_plot <- output_performance$data_cond %>%
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
    ggtitle("Condition Number BoxPlot") +
    xlab("Methods")
  
  frob_plot <- output_performance$data_frob %>%
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
    ggtitle("Frobenius Norm BoxPlot") +
    xlab("Methods")
  
  TP_plot <- output_performance$data_TP %>%
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
    ggtitle("TP_rate BoxPlot") +
    xlab("Methods")
  
  FP_plot <- output_performance$data_FP %>%
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
    ggtitle("FP_rate BoxPlot") +
    xlab("Methods")
  
  TN_plot <- output_performance$data_TN %>%
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
    ggtitle("TN_rate BoxPlot") +
    xlab("Methods")
  
  FN_plot <- output_performance$data_FN %>%
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
    ggtitle("FN_rate BoxPlot") +
    xlab("Methods")
  
  F1_plot <- output_performance$data_F1 %>%
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
    ggtitle("F1_score BoxPlot") +
    xlab("Methods")
  
  Acc_plot <- output_performance$data_ACC %>%
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
    ggtitle("Accuracy BoxPlot") +
    xlab("Methods")
  
  entropy_plot <- output_performance$data_entropy %>%
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
    ggtitle("Entropy BoxPlot") +
    xlab("Methods")
  
  FDR_plot <- output_performance$data_FDR %>%
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
    ggtitle("FDR BoxPlot") +
    xlab("Methods")
  
  localFDR_plot <- output_performance$data_localFDR %>%
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
    ggtitle("localFDR BoxPlot") +
    xlab("Methods")
  
  output   = list(
    cond_plot,
    frob_plot,
    TP_plot,
    FP_plot,
    TN_plot,
    FN_plot,
    F1_plot,
    Acc_plot,
    entropy_plot,
    FDR_plot,
    localFDR_plot
  )
  
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

##48##
Box_Plot_sim_N_less_p <- function(output_performance) {
  cond_plot <- output_performance$data_cond %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ggtitle("Condition Number BoxPlot") +
    xlab("Methods")
  
  frob_plot <- output_performance$data_frob %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ggtitle("Frobenius Norm BoxPlot") +
    xlab("Methods")
  
  TP_plot <- output_performance$data_TP %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("TP_rate BoxPlot") +
    xlab("Methods")
  
  FP_plot <- output_performance$data_FP %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("FP_rate BoxPlot") +
    xlab("Methods")
  
  TN_plot <- output_performance$data_TN %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("TN_rate BoxPlot") +
    xlab("Methods")
  
  FN_plot <- output_performance$data_FN %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("FN_rate BoxPlot") +
    xlab("Methods")
  
  F1_plot <- output_performance$data_F1 %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("F1_score BoxPlot") +
    xlab("Methods")
  
  Acc_plot <- output_performance$data_ACC %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("Accuracy BoxPlot") +
    xlab("Methods")
  
  entropy_plot <- output_performance$data_entropy %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ggtitle("Entropy BoxPlot") +
    xlab("Methods")
  
  FDR_plot <- output_performance$data_FDR %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("FDR BoxPlot") +
    xlab("Methods")
  
  localFDR_plot <- output_performance$data_localFDR %>%
    ggplot(aes(
      x = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      ),
      y = value,
      fill = factor(
        name,
        levels = c("Elastic", "Rope",
                   "Glasso", "Tlasso", "Gslope", "Tslope")
      )
    )) +
    geom_boxplot(width = 0.45) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    #scale_y_log10 +
    theme(legend.position = "none",
          plot.title = element_text(size = 10)) +
    ylim(c(0, 1)) +
    ggtitle("localFDR BoxPlot") +
    xlab("Methods")
  
  output   = list(
    cond_plot,
    frob_plot,
    TP_plot,
    FP_plot,
    TN_plot,
    FN_plot,
    F1_plot,
    Acc_plot,
    entropy_plot,
    FDR_plot,
    localFDR_plot
  )
  
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

