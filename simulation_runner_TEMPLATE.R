#Template to easily run simulation

#Create arbitrary positive and symmetric matrix
n = 50
A = matrix(runif(n^2)*2-1, ncol=n) 
sigma = t(A) %*% A

#N>p
a = oracle_precision_matrix_generation(250, 50, "cluster", 0.7, 0.3)

b = data_series_generation(250, a$sigma, 0.05, 4, T_student = TRUE)

c = sample_precision_matrix_estimation(b$data, 250, 0.05, 4, progress = TRUE)

d = performance_measures(b$omega_T, c$inv_sample, c$inv_glasso, c$inv_elastic, c$inv_rope, c$inv_tlasso, c$inv_gslope, c$inv_tslope)

e = portfolio_risk_measures_no_constraint(b$data, b$omega_T, b$sigma_T,
                            c$sigma_sample, c$sigma_ledoit, c$sigma_glasso, c$sigma_elastic, c$sigma_rope, c$sigma_tlasso, c$sigma_gslope, c$sigma_tslope,
                            c$inv_sample, c$inv_ledoit, c$inv_glasso, c$inv_elastic, c$inv_rope, c$inv_tlasso, c$inv_gslope, c$inv_tslope)

net = network_generation(b$omega_T, c$inv_sample, c$inv_glasso, c$inv_elastic, c$inv_rope, c$inv_tlasso, c$inv_gslope, c$inv_tslope)

#N<p
a = oracle_precision_matrix_generation(50, 100, "cluster", 0.7, 0.3)

b = data_series_generation(50, a$sigma, 0.05, 4, T_student = TRUE)

c = sample_precision_matrix_estimation_N_less_p(b$data, 50, 0.05, 4, progress = TRUE, 0.75)

d = performance_measures_N_less_p(b$omega_T, c$inv_glasso, c$inv_elastic, c$inv_rope, c$inv_tlasso, c$inv_gslope, c$inv_tslope)

#Prove histogram per pesi portafoglio
n = 50
pesi_oracle = e$weights$w_oracle
pesi_sample = e$weights$w_sample
pesi_ledoit = e$weights$w_ledoit
pesi_glasso = e$weights$w_tlasso
pesi_tlasso = e$weights$w_glasso
pesi_gslope = e$weights$w_gslope
pesi_tslope = e$weights$w_tslope

dati_oracle = tibble("asset" = rep(1:n), "pesi" = pesi_oracle, metodo = "Oracle")
dati_sample = tibble("asset" = rep(1:n), "pesi" = pesi_sample, metodo = "Sample")
dati_ledoit = tibble("asset" = rep(1:n), "pesi" = pesi_ledoit, metodo = "LW")
dati_glasso = tibble("asset" = rep(1:n), "pesi" = pesi_glasso, metodo = "Glasso")
dati_tlasso = tibble("asset" = rep(1:n), "pesi" = pesi_tlasso, metodo = "Tlasso")
dati_gslope = tibble("asset" = rep(1:n), "pesi" = pesi_gslope, metodo = "Gslope")
dati_tslope = tibble("asset" = rep(1:n), "pesi" = pesi_tslope, metodo = "Tslope")

dati = rbind(dati_oracle, dati_sample, dati_ledoit, dati_glasso, dati_tlasso, dati_gslope, dati_tslope)
dati$metodo = factor(dati$metodo, levels = c("Oracle", "Sample", "LW", "Glasso", "Tlasso", "Gslope", "Tslope"))

ggplot(data = dati, aes(x=asset, y=pesi, fill = pesi > 0.02)) + 
  geom_col(width = 0.75, show.legend = F) + labs(x = 'stocks', y = 'weights') +
  scale_fill_manual(values = c("#747474","#fa9734")) +
  ylim(c(0, max(dati$pesi))) +
  geom_hline(data = dati, aes(yintercept = pesi[1]), linetype = "dotted", col = 'black', lwd = 0.75) +
  facet_wrap(~ metodo, scales = "free", nrow = 4)

