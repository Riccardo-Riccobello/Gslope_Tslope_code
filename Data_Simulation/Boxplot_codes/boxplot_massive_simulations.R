#Tramite questa funzione puoi lanciare in modo massivo le simulazioni che ti
#interessano, sia per N > p che N < p

source("Official_runner.R")

###############################N > p############################################

#GAUSSIAN SIMULATION#
normal_250_100_MR0_cl = BoxPlot_simulation_normal(250, 100, "cluster", -1, 1, 100)
normal_250_100_MR0_ra = BoxPlot_simulation_normal(250, 100, "random", -1, 1, 100)
normal_250_100_MR0_hub = BoxPlot_simulation_normal(250, 100, "hub", -1, 1, 100)
normal_250_100_MR0_sf = BoxPlot_simulation_normal(250, 100, "scale-free", -1, 1, 100)
normal_250_100_MR0_band = BoxPlot_simulation_normal(250, 100, "band", -1, 1, 100)

normal_250_100_MR143_cl = BoxPlot_simulation_normal(250, 100, "cluster", 0.7, 0.3, 100)
normal_250_100_MR143_ra = BoxPlot_simulation_normal(250, 100, "random", 0.7, 0.3, 100)
normal_250_100_MR143_hub = BoxPlot_simulation_normal(250, 100, "hub", 0.7, 0.3, 100)
normal_250_100_MR143_sf = BoxPlot_simulation_normal(250, 100, "scale-free", 0.7, 0.3, 100)
normal_250_100_MR143_band = BoxPlot_simulation_normal(250, 100, "band", 0.7, 0.3, 100)

#T-STUDENT SIMULATION#
tstudent_250_100_MR0_cl = BoxPlot_simulation_tstudent(250, 100, "cluster", -1, 1, 100)
tstudent_250_100_MR0_ra = BoxPlot_simulation_tstudent(250, 100, "random", -1, 1, 100)
tstudent_250_100_MR0_hub = BoxPlot_simulation_tstudent(250, 100, "hub", -1, 1, 100)
tstudent_250_100_MR0_sf = BoxPlot_simulation_tstudent(250, 100, "scale-free", -1, 1, 100)
tstudent_250_100_MR0_band = BoxPlot_simulation_tstudent(250, 100, "band", -1, 1, 100)

tstudent_250_100_MR143_cl = BoxPlot_simulation_tstudent(250, 100, "cluster", 0.7, 0.3, 100)
tstudent_250_100_MR143_ra = BoxPlot_simulation_tstudent(250, 100, "random", 0.7, 0.3, 100)
tstudent_250_100_MR143_hub = BoxPlot_simulation_tstudent(250, 100, "hub", 0.7, 0.3, 100)
tstudent_250_100_MR143_sf = BoxPlot_simulation_tstudent(250, 100, "scale-free", 0.7, 0.3, 100)
tstudent_250_100_MR143_band = BoxPlot_simulation_tstudent(250, 100, "band", 0.7, 0.3, 100)

#MIX SIMULATION#
mix_250_100_MR0_cl = BoxPlot_simulation_mix(250, 100, "cluster", -1, 1, 100)
mix_250_100_MR0_ra = BoxPlot_simulation_mix(250, 100, "random", -1, 1, 100)
mix_250_100_MR0_hub = BoxPlot_simulation_mix(250, 100, "hub", -1, 1, 100)
mix_250_100_MR0_sf = BoxPlot_simulation_mix(250, 100, "scale-free", -1, 1, 100)
mix_250_100_MR0_band = BoxPlot_simulation_mix(250, 100, "band", -1, 1, 100)

mix_250_100_MR143_cl = BoxPlot_simulation_mix(250, 100, "cluster", 0.7, 0.3, 100)
mix_250_100_MR143_ra = BoxPlot_simulation_mix(250, 100, "random", 0.7, 0.3, 100)
mix_250_100_MR143_hub = BoxPlot_simulation_mix(250, 100, "hub", 0.7, 0.3, 100)
mix_250_100_MR143_sf = BoxPlot_simulation_mix(250, 100, "scale-free", 0.7, 0.3, 100)
mix_250_100_MR143_band = BoxPlot_simulation_mix(250, 100, "band", 0.7, 0.3, 100)

###############################N < p############################################
#GAUSSIAN SIMULATION#
normal_50_100_MR0_cl = BoxPlot_simulation_normal_N_less_p(50, 100, "cluster", -1, 1, 100)
normal_50_100_MR0_ra = BoxPlot_simulation_normal_N_less_p(50, 100, "random", -1, 1, 100)
normal_50_100_MR0_hub = BoxPlot_simulation_normal_N_less_p(50, 100, "hub", -1, 1, 100)
normal_50_100_MR0_sf = BoxPlot_simulation_normal_N_less_p(50, 100, "scale-free", -1, 1, 100)
normal_50_100_MR0_band = BoxPlot_simulation_normal_N_less_p(50, 100, "band", -1, 1, 100)

normal_50_100_MR143_cl = BoxPlot_simulation_normal_N_less_p(50, 100, "cluster", 0.7, 0.3, 100)
normal_50_100_MR143_ra = BoxPlot_simulation_normal_N_less_p(50, 100, "random", 0.7, 0.3, 100)
normal_50_100_MR143_hub = BoxPlot_simulation_normal_N_less_p(50, 100, "hub", 0.7, 0.3, 100)
normal_50_100_MR143_sf = BoxPlot_simulation_normal_N_less_p(50, 100, "scale-free", 0.7, 0.3, 100)
normal_50_100_MR143_band = BoxPlot_simulation_normal_N_less_p(50, 100, "band", 0.7, 0.3, 100)

#T-STUDENT SIMULATION#
tstudent_50_100_MR0_cl = BoxPlot_simulation_tstudent_N_less_p(50, 100, "cluster", -1, 1, 100)
tstudent_50_100_MR0_ra = BoxPlot_simulation_tstudent_N_less_p(50, 100, "random", -1, 1, 100)
tstudent_50_100_MR0_hub = BoxPlot_simulation_tstudent_N_less_p(50, 100, "hub", -1, 1, 100)
tstudent_50_100_MR0_sf = BoxPlot_simulation_tstudent_N_less_p(50, 100, "scale-free", -1, 1, 100)
tstudent_50_100_MR0_band = BoxPlot_simulation_tstudent_N_less_p(50, 100, "band", -1, 1, 100)

tstudent_50_100_MR143_cl = BoxPlot_simulation_tstudent_N_less_p(50, 100, "cluster", 0.7, 0.3, 100)
tstudent_50_100_MR143_ra = BoxPlot_simulation_tstudent_N_less_p(50, 100, "random", 0.7, 0.3, 100)
tstudent_50_100_MR143_hub = BoxPlot_simulation_tstudent_N_less_p(50, 100, "hub", 0.7, 0.3, 100)
tstudent_50_100_MR143_sf = BoxPlot_simulation_tstudent_N_less_p(50, 100, "scale-free", 0.7, 0.3, 100)
tstudent_50_100_MR143_band = BoxPlot_simulation_tstudent_N_less_p(50, 100, "band", 0.7, 0.3, 100)

#MIX SIMULATION#
mix_50_100_MR0_cl = BoxPlot_simulation_mix_N_less_p(50, 100, "cluster", -1, 1, 100)
mix_50_100_MR0_ra = BoxPlot_simulation_mix_N_less_p(50, 100, "random", -1, 1, 100)
mix_50_100_MR0_hub = BoxPlot_simulation_mix_N_less_p(50, 100, "hub", -1, 1, 100)
mix_50_100_MR0_sf = BoxPlot_simulation_mix_N_less_p(50, 100, "scale-free", -1, 1, 100)
mix_50_100_MR0_band = BoxPlot_simulation_mix_N_less_p(50, 100, "band", -1, 1, 100)

mix_50_100_MR143_cl = BoxPlot_simulation_mix_N_less_p(50, 100, "cluster", 0.7, 0.3, 100)
mix_50_100_MR143_ra = BoxPlot_simulation_mix_N_less_p(50, 100, "random", 0.7, 0.3, 100)
mix_50_100_MR143_hub = BoxPlot_simulation_mix_N_less_p(50, 100, "hub", 0.7, 0.3, 100)
mix_50_100_MR143_sf = BoxPlot_simulation_mix_N_less_p(50, 100, "scale-free", 0.7, 0.3, 100)
mix_50_100_MR143_band = BoxPlot_simulation_mix_N_less_p(50, 100, "band", 0.7, 0.3, 100)

