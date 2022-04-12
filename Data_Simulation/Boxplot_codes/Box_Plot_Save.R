#Mediante questa funzione puoi salvare i BoxPlot che hai creato#

###############################N > p############################################

#Code to save the BoxPlots#
library(grid)
library(gridExtra)

#NORMAL#

#MR_0#
box_normal_250_100_MR0_cl = Box_Plot_sim_norm(normal_250_100_MR0_cl)

cond_plot_250_100_0_cl = box_normal_250_100_MR0_cl$cond_plot
cond_plot_250_100_0_cl
ggsave("cond_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_cl = box_normal_250_100_MR0_cl$frob_plot
frob_plot_250_100_0_cl
ggsave("frob_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_cl = box_normal_250_100_MR0_cl$TP_plot
TP_plot_250_100_0_cl
ggsave("TP_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_cl = box_normal_250_100_MR0_cl$FP_plot
FP_plot_250_100_0_cl
ggsave("FP_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_cl = box_normal_250_100_MR0_cl$F1_plot
F1_plot_250_100_0_cl
ggsave("F1_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_cl = box_normal_250_100_MR0_cl$Acc_plot
Acc_plot_250_100_0_cl
ggsave("Acc_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_cl = box_normal_250_100_MR0_cl$entropy_plot
entropy_plot_250_100_0_cl
ggsave("entropy_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_cl = box_normal_250_100_MR0_cl$FDR_plot
FDR_plot_250_100_0_cl
ggsave("FDR_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_cl = box_normal_250_100_MR0_cl$localFDR_plot
localFDR_plot_250_100_0_cl
ggsave("localFDR_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_250_100_MR0_ra = Box_Plot_sim_norm(normal_250_100_MR0_ra)

cond_plot_250_100_0_ra = box_normal_250_100_MR0_ra$cond_plot
cond_plot_250_100_0_ra
ggsave("cond_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_ra = box_normal_250_100_MR0_ra$frob_plot
frob_plot_250_100_0_ra
ggsave("frob_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_ra = box_normal_250_100_MR0_ra$TP_plot
TP_plot_250_100_0_ra
ggsave("TP_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_ra = box_normal_250_100_MR0_ra$FP_plot
FP_plot_250_100_0_ra
ggsave("FP_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_ra = box_normal_250_100_MR0_ra$F1_plot
F1_plot_250_100_0_ra
ggsave("F1_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_ra = box_normal_250_100_MR0_ra$Acc_plot
Acc_plot_250_100_0_ra
ggsave("Acc_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_ra = box_normal_250_100_MR0_ra$entropy_plot
entropy_plot_250_100_0_ra
ggsave("entropy_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_ra = box_normal_250_100_MR0_ra$FDR_plot
FDR_plot_250_100_0_ra
ggsave("FDR_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_ra = box_normal_250_100_MR0_ra$localFDR_plot
localFDR_plot_250_100_0_ra
ggsave("localFDR_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_250_100_MR0_hub = Box_Plot_sim_norm(normal_250_100_MR0_hub)

cond_plot_250_100_0_hub = box_normal_250_100_MR0_hub$cond_plot
cond_plot_250_100_0_hub
ggsave("cond_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_hub = box_normal_250_100_MR0_hub$frob_plot
frob_plot_250_100_0_hub
ggsave("frob_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_hub = box_normal_250_100_MR0_hub$TP_plot
TP_plot_250_100_0_hub
ggsave("TP_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_hub = box_normal_250_100_MR0_hub$FP_plot
FP_plot_250_100_0_hub
ggsave("FP_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_hub = box_normal_250_100_MR0_hub$F1_plot
F1_plot_250_100_0_hub
ggsave("F1_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_hub = box_normal_250_100_MR0_hub$Acc_plot
Acc_plot_250_100_0_hub
ggsave("Acc_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_hub = box_normal_250_100_MR0_hub$entropy_plot
entropy_plot_250_100_0_hub
ggsave("entropy_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_hub = box_normal_250_100_MR0_hub$FDR_plot
FDR_plot_250_100_0_hub
ggsave("FDR_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_hub = box_normal_250_100_MR0_hub$localFDR_plot
localFDR_plot_250_100_0_hub
ggsave("localFDR_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_250_100_MR0_sf = Box_Plot_sim_norm(normal_250_100_MR0_sf)

cond_plot_250_100_0_sf = box_normal_250_100_MR0_sf$cond_plot
cond_plot_250_100_0_sf
ggsave("cond_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_sf = box_normal_250_100_MR0_sf$frob_plot
frob_plot_250_100_0_sf
ggsave("frob_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_sf = box_normal_250_100_MR0_sf$TP_plot
TP_plot_250_100_0_sf
ggsave("TP_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_sf = box_normal_250_100_MR0_sf$FP_plot
FP_plot_250_100_0_sf
ggsave("FP_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_sf = box_normal_250_100_MR0_sf$F1_plot
F1_plot_250_100_0_sf
ggsave("F1_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_sf = box_normal_250_100_MR0_sf$Acc_plot
Acc_plot_250_100_0_sf
ggsave("Acc_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_sf = box_normal_250_100_MR0_sf$entropy_plot
entropy_plot_250_100_0_sf
ggsave("entropy_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_sf = box_normal_250_100_MR0_sf$FDR_plot
FDR_plot_250_100_0_sf
ggsave("FDR_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_sf = box_normal_250_100_MR0_sf$localFDR_plot
localFDR_plot_250_100_0_sf
ggsave("localFDR_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_250_100_MR0_band = Box_Plot_sim_norm(normal_250_100_MR0_band)

cond_plot_250_100_0_band = box_normal_250_100_MR0_band$cond_plot
cond_plot_250_100_0_band
ggsave("cond_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_band = box_normal_250_100_MR0_band$frob_plot
frob_plot_250_100_0_band
ggsave("frob_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_band = box_normal_250_100_MR0_band$TP_plot
TP_plot_250_100_0_band
ggsave("TP_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_band = box_normal_250_100_MR0_band$FP_plot
FP_plot_250_100_0_band
ggsave("FP_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_band = box_normal_250_100_MR0_band$F1_plot
F1_plot_250_100_0_band
ggsave("F1_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_band = box_normal_250_100_MR0_band$Acc_plot
Acc_plot_250_100_0_band
ggsave("Acc_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_band = box_normal_250_100_MR0_band$entropy_plot
entropy_plot_250_100_0_band
ggsave("entropy_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_band = box_normal_250_100_MR0_band$FDR_plot
FDR_plot_250_100_0_band
ggsave("FDR_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_band = box_normal_250_100_MR0_band$localFDR_plot
localFDR_plot_250_100_0_band
ggsave("localFDR_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

#MR_1_43#
box_normal_250_100_MR143_cl = Box_Plot_sim_norm(normal_250_100_MR143_cl)

cond_plot_250_100_143_cl = box_normal_250_100_MR143_cl$cond_plot
cond_plot_250_100_143_cl
ggsave("cond_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_cl = box_normal_250_100_MR143_cl$frob_plot
frob_plot_250_100_143_cl
ggsave("frob_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_cl = box_normal_250_100_MR143_cl$TP_plot
TP_plot_250_100_143_cl
ggsave("TP_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_cl = box_normal_250_100_MR143_cl$FP_plot
FP_plot_250_100_143_cl
ggsave("FP_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_cl = box_normal_250_100_MR143_cl$F1_plot
F1_plot_250_100_143_cl
ggsave("F1_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_cl = box_normal_250_100_MR143_cl$Acc_plot
Acc_plot_250_100_143_cl
ggsave("Acc_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_cl = box_normal_250_100_MR143_cl$entropy_plot
entropy_plot_250_100_143_cl
ggsave("entropy_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_cl = box_normal_250_100_MR143_cl$FDR_plot
FDR_plot_250_100_143_cl
ggsave("FDR_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_cl = box_normal_250_100_MR143_cl$localFDR_plot
localFDR_plot_250_100_143_cl
ggsave("localFDR_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)


box_normal_250_100_MR143_ra = Box_Plot_sim_norm(normal_250_100_MR143_ra)

cond_plot_250_100_143_ra = box_normal_250_100_MR143_ra$cond_plot
cond_plot_250_100_143_ra
ggsave("cond_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_ra = box_normal_250_100_MR143_ra$frob_plot
frob_plot_250_100_143_ra
ggsave("frob_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_ra = box_normal_250_100_MR143_ra$TP_plot
TP_plot_250_100_143_ra
ggsave("TP_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_ra = box_normal_250_100_MR143_ra$FP_plot
FP_plot_250_100_143_ra
ggsave("FP_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_ra = box_normal_250_100_MR143_ra$F1_plot
F1_plot_250_100_143_ra
ggsave("F1_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_ra = box_normal_250_100_MR143_ra$Acc_plot
Acc_plot_250_100_143_ra
ggsave("Acc_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_ra = box_normal_250_100_MR143_ra$entropy_plot
entropy_plot_250_100_143_ra
ggsave("entropy_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_ra = box_normal_250_100_MR143_ra$FDR_plot
FDR_plot_250_100_143_ra
ggsave("FDR_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_ra = box_normal_250_100_MR143_ra$localFDR_plot
localFDR_plot_250_100_143_ra
ggsave("localFDR_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_250_100_MR143_hub = Box_Plot_sim_norm(normal_250_100_MR143_hub)

cond_plot_250_100_143_hub = box_normal_250_100_MR143_hub$cond_plot
cond_plot_250_100_143_hub
ggsave("cond_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_hub = box_normal_250_100_MR143_hub$frob_plot
frob_plot_250_100_143_hub
ggsave("frob_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_hub = box_normal_250_100_MR143_hub$TP_plot
TP_plot_250_100_143_hub
ggsave("TP_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_hub = box_normal_250_100_MR143_hub$FP_plot
FP_plot_250_100_143_hub
ggsave("FP_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_hub = box_normal_250_100_MR143_hub$F1_plot
F1_plot_250_100_143_hub
ggsave("F1_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_hub = box_normal_250_100_MR143_hub$Acc_plot
Acc_plot_250_100_143_hub
ggsave("Acc_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_hub = box_normal_250_100_MR143_hub$entropy_plot
entropy_plot_250_100_143_hub
ggsave("entropy_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_hub = box_normal_250_100_MR143_hub$FDR_plot
FDR_plot_250_100_143_hub
ggsave("FDR_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_hub = box_normal_250_100_MR143_hub$localFDR_plot
localFDR_plot_250_100_143_hub
ggsave("localFDR_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_250_100_MR143_sf = Box_Plot_sim_norm(normal_250_100_MR143_sf)

cond_plot_250_100_143_sf = box_normal_250_100_MR143_sf$cond_plot
cond_plot_250_100_143_sf
ggsave("cond_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_sf = box_normal_250_100_MR143_sf$frob_plot
frob_plot_250_100_143_sf
ggsave("frob_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_sf = box_normal_250_100_MR143_sf$TP_plot
TP_plot_250_100_143_sf
ggsave("TP_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_sf = box_normal_250_100_MR143_sf$FP_plot
FP_plot_250_100_143_sf
ggsave("FP_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_sf = box_normal_250_100_MR143_sf$F1_plot
F1_plot_250_100_143_sf
ggsave("F1_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_sf = box_normal_250_100_MR143_sf$Acc_plot
Acc_plot_250_100_143_sf
ggsave("Acc_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_sf = box_normal_250_100_MR143_sf$entropy_plot
entropy_plot_250_100_143_sf
ggsave("entropy_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_sf = box_normal_250_100_MR143_sf$FDR_plot
FDR_plot_250_100_143_sf
ggsave("FDR_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_sf = box_normal_250_100_MR143_sf$localFDR_plot
localFDR_plot_250_100_143_sf
ggsave("localFDR_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_250_100_MR143_band = Box_Plot_sim_norm(normal_250_100_MR143_band)

cond_plot_250_100_143_band = box_normal_250_100_MR143_band$cond_plot
cond_plot_250_100_143_band
ggsave("cond_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_band = box_normal_250_100_MR143_band$frob_plot
frob_plot_250_100_143_band
ggsave("frob_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_band = box_normal_250_100_MR143_band$TP_plot
TP_plot_250_100_143_band
ggsave("TP_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_band = box_normal_250_100_MR143_band$FP_plot
FP_plot_250_100_143_band
ggsave("FP_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_band = box_normal_250_100_MR143_band$F1_plot
F1_plot_250_100_143_band
ggsave("F1_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_band = box_normal_250_100_MR143_band$Acc_plot
Acc_plot_250_100_143_band
ggsave("Acc_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_band = box_normal_250_100_MR143_band$entropy_plot
entropy_plot_250_100_143_band
ggsave("entropy_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_band = box_normal_250_100_MR143_band$FDR_plot
FDR_plot_250_100_143_band
ggsave("FDR_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_band = box_normal_250_100_MR143_band$localFDR_plot
localFDR_plot_250_100_143_band
ggsave("localFDR_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

#Dataframes save#

#MR_0#
mean_res_250_100_0_cl = tableGrob(round(normal_250_100_MR0_cl$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_cl)
ggsave("mean_res_250_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_cl)

std_res_250_100_0_cl = tableGrob(round(normal_250_100_MR0_cl$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_cl)
ggsave("std_res_250_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_cl)

mean_res_250_100_0_ra = tableGrob(round(normal_250_100_MR0_ra$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_ra)
ggsave("mean_res_250_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_ra)

std_res_250_100_0_ra = tableGrob(round(normal_250_100_MR0_ra$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_ra)
ggsave("std_res_250_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_ra)

mean_res_250_100_0_hub = tableGrob(round(normal_250_100_MR0_hub$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_hub)
ggsave("mean_res_250_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_hub)

std_res_250_100_0_hub = tableGrob(round(normal_250_100_MR0_hub$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_hub)
ggsave("std_res_250_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_hub)

mean_res_250_100_0_sf = tableGrob(round(normal_250_100_MR0_sf$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_sf)
ggsave("mean_res_250_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_sf)

std_res_250_100_0_sf = tableGrob(round(normal_250_100_MR0_sf$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_sf)
ggsave("std_res_250_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_sf)

mean_res_250_100_0_band = tableGrob(round(normal_250_100_MR0_band$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_band)
ggsave("mean_res_250_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_band)

std_res_250_100_0_band = tableGrob(round(normal_250_100_MR0_band$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_band)
ggsave("std_res_250_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_band)

#MR_1_43#
mean_res_250_100_143_cl = tableGrob(round(normal_250_100_MR143_cl$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_cl)
ggsave("mean_res_250_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_cl)

std_res_250_100_143_cl = tableGrob(round(normal_250_100_MR143_cl$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_cl)
ggsave("std_res_250_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_cl)

mean_res_250_100_143_ra = tableGrob(round(normal_250_100_MR143_ra$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_ra)
ggsave("mean_res_250_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_ra)

std_res_250_100_143_ra = tableGrob(round(normal_250_100_MR143_ra$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_ra)
ggsave("std_res_250_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_ra)

mean_res_250_100_143_hub = tableGrob(round(normal_250_100_MR143_hub$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_hub)
ggsave("mean_res_250_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_hub)

std_res_250_100_143_hub = tableGrob(round(normal_250_100_MR143_hub$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_hub)
ggsave("std_res_250_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_hub)

mean_res_250_100_143_sf = tableGrob(round(normal_250_100_MR143_sf$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_sf)
ggsave("mean_res_250_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_sf)

std_res_250_100_143_sf = tableGrob(round(normal_250_100_MR143_sf$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_sf)
ggsave("std_res_250_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_sf)

mean_res_250_100_143_band = tableGrob(round(normal_250_100_MR143_band$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_band)
ggsave("mean_res_250_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_band)

std_res_250_100_143_band = tableGrob(round(normal_250_100_MR143_band$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_band)
ggsave("std_res_250_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_band)

#TSTUDENT#

#MR_0#
box_tstudent_250_100_MR0_cl = Box_Plot_sim(tstudent_250_100_MR0_cl)

cond_plot_250_100_0_cl = box_tstudent_250_100_MR0_cl$cond_plot
cond_plot_250_100_0_cl
ggsave("cond_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_cl = box_tstudent_250_100_MR0_cl$frob_plot
frob_plot_250_100_0_cl
ggsave("frob_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_cl = box_tstudent_250_100_MR0_cl$TP_plot
TP_plot_250_100_0_cl
ggsave("TP_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_cl = box_tstudent_250_100_MR0_cl$FP_plot
FP_plot_250_100_0_cl
ggsave("FP_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_cl = box_tstudent_250_100_MR0_cl$F1_plot
F1_plot_250_100_0_cl
ggsave("F1_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_cl = box_tstudent_250_100_MR0_cl$Acc_plot
Acc_plot_250_100_0_cl
ggsave("Acc_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_cl = box_tstudent_250_100_MR0_cl$entropy_plot
entropy_plot_250_100_0_cl
ggsave("entropy_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_cl = box_tstudent_250_100_MR0_cl$FDR_plot
FDR_plot_250_100_0_cl
ggsave("FDR_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_cl = box_tstudent_250_100_MR0_cl$localFDR_plot
localFDR_plot_250_100_0_cl
ggsave("localFDR_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_250_100_MR0_ra = Box_Plot_sim(tstudent_250_100_MR0_ra)

cond_plot_250_100_0_ra = box_tstudent_250_100_MR0_ra$cond_plot
cond_plot_250_100_0_ra
ggsave("cond_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_ra = box_tstudent_250_100_MR0_ra$frob_plot
frob_plot_250_100_0_ra
ggsave("frob_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_ra = box_tstudent_250_100_MR0_ra$TP_plot
TP_plot_250_100_0_ra
ggsave("TP_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_ra = box_tstudent_250_100_MR0_ra$FP_plot
FP_plot_250_100_0_ra
ggsave("FP_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_ra = box_tstudent_250_100_MR0_ra$F1_plot
F1_plot_250_100_0_ra
ggsave("F1_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_ra = box_tstudent_250_100_MR0_ra$Acc_plot
Acc_plot_250_100_0_ra
ggsave("Acc_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_ra = box_tstudent_250_100_MR0_ra$entropy_plot
entropy_plot_250_100_0_ra
ggsave("entropy_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_ra = box_tstudent_250_100_MR0_ra$FDR_plot
FDR_plot_250_100_0_ra
ggsave("FDR_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_ra = box_tstudent_250_100_MR0_ra$localFDR_plot
localFDR_plot_250_100_0_ra
ggsave("localFDR_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_250_100_MR0_hub = Box_Plot_sim(tstudent_250_100_MR0_hub)

cond_plot_250_100_0_hub = box_tstudent_250_100_MR0_hub$cond_plot
cond_plot_250_100_0_hub
ggsave("cond_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_hub = box_tstudent_250_100_MR0_hub$frob_plot
frob_plot_250_100_0_hub
ggsave("frob_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_hub = box_tstudent_250_100_MR0_hub$TP_plot
TP_plot_250_100_0_hub
ggsave("TP_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_hub = box_tstudent_250_100_MR0_hub$FP_plot
FP_plot_250_100_0_hub
ggsave("FP_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_hub = box_tstudent_250_100_MR0_hub$F1_plot
F1_plot_250_100_0_hub
ggsave("F1_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_hub = box_tstudent_250_100_MR0_hub$Acc_plot
Acc_plot_250_100_0_hub
ggsave("Acc_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_hub = box_tstudent_250_100_MR0_hub$entropy_plot
entropy_plot_250_100_0_hub
ggsave("entropy_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_hub = box_tstudent_250_100_MR0_hub$FDR_plot
FDR_plot_250_100_0_hub
ggsave("FDR_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_hub = box_tstudent_250_100_MR0_hub$localFDR_plot
localFDR_plot_250_100_0_hub
ggsave("localFDR_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_250_100_MR0_sf = Box_Plot_sim(tstudent_250_100_MR0_sf)

cond_plot_250_100_0_sf = box_tstudent_250_100_MR0_sf$cond_plot
cond_plot_250_100_0_sf
ggsave("cond_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_sf = box_tstudent_250_100_MR0_sf$frob_plot
frob_plot_250_100_0_sf
ggsave("frob_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_sf = box_tstudent_250_100_MR0_sf$TP_plot
TP_plot_250_100_0_sf
ggsave("TP_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_sf = box_tstudent_250_100_MR0_sf$FP_plot
FP_plot_250_100_0_sf
ggsave("FP_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_sf = box_tstudent_250_100_MR0_sf$F1_plot
F1_plot_250_100_0_sf
ggsave("F1_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_sf = box_tstudent_250_100_MR0_sf$Acc_plot
Acc_plot_250_100_0_sf
ggsave("Acc_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_sf = box_tstudent_250_100_MR0_sf$entropy_plot
entropy_plot_250_100_0_sf
ggsave("entropy_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_sf = box_tstudent_250_100_MR0_sf$FDR_plot
FDR_plot_250_100_0_sf
ggsave("FDR_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_sf = box_tstudent_250_100_MR0_sf$localFDR_plot
localFDR_plot_250_100_0_sf
ggsave("localFDR_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_250_100_MR0_band = Box_Plot_sim(tstudent_250_100_MR0_band)

cond_plot_250_100_0_band = box_tstudent_250_100_MR0_band$cond_plot
cond_plot_250_100_0_band
ggsave("cond_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_band = box_tstudent_250_100_MR0_band$frob_plot
frob_plot_250_100_0_band
ggsave("frob_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_band = box_tstudent_250_100_MR0_band$TP_plot
TP_plot_250_100_0_band
ggsave("TP_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_band = box_tstudent_250_100_MR0_band$FP_plot
FP_plot_250_100_0_band
ggsave("FP_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_band = box_tstudent_250_100_MR0_band$F1_plot
F1_plot_250_100_0_band
ggsave("F1_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_band = box_tstudent_250_100_MR0_band$Acc_plot
Acc_plot_250_100_0_band
ggsave("Acc_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_band = box_tstudent_250_100_MR0_band$entropy_plot
entropy_plot_250_100_0_band
ggsave("entropy_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_band = box_tstudent_250_100_MR0_band$FDR_plot
FDR_plot_250_100_0_band
ggsave("FDR_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_band = box_tstudent_250_100_MR0_band$localFDR_plot
localFDR_plot_250_100_0_band
ggsave("localFDR_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

#MR_1_43#
box_tstudent_250_100_MR143_cl = Box_Plot_sim(tstudent_250_100_MR143_cl)

cond_plot_250_100_143_cl = box_tstudent_250_100_MR143_cl$cond_plot
cond_plot_250_100_143_cl
ggsave("cond_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_cl = box_tstudent_250_100_MR143_cl$frob_plot
frob_plot_250_100_143_cl
ggsave("frob_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_cl = box_tstudent_250_100_MR143_cl$TP_plot
TP_plot_250_100_143_cl
ggsave("TP_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_cl = box_tstudent_250_100_MR143_cl$FP_plot
FP_plot_250_100_143_cl
ggsave("FP_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_cl = box_tstudent_250_100_MR143_cl$F1_plot
F1_plot_250_100_143_cl
ggsave("F1_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_cl = box_tstudent_250_100_MR143_cl$Acc_plot
Acc_plot_250_100_143_cl
ggsave("Acc_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_cl = box_tstudent_250_100_MR143_cl$entropy_plot
entropy_plot_250_100_143_cl
ggsave("entropy_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_cl = box_tstudent_250_100_MR143_cl$FDR_plot
FDR_plot_250_100_143_cl
ggsave("FDR_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_cl = box_tstudent_250_100_MR143_cl$localFDR_plot
localFDR_plot_250_100_143_cl
ggsave("localFDR_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_250_100_MR143_ra = Box_Plot_sim(tstudent_250_100_MR143_ra)

cond_plot_250_100_143_ra = box_tstudent_250_100_MR143_ra$cond_plot
cond_plot_250_100_143_ra
ggsave("cond_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_ra = box_tstudent_250_100_MR143_ra$frob_plot
frob_plot_250_100_143_ra
ggsave("frob_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_ra = box_tstudent_250_100_MR143_ra$TP_plot
TP_plot_250_100_143_ra
ggsave("TP_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_ra = box_tstudent_250_100_MR143_ra$FP_plot
FP_plot_250_100_143_ra
ggsave("FP_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_ra = box_tstudent_250_100_MR143_ra$F1_plot
F1_plot_250_100_143_ra
ggsave("F1_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_ra = box_tstudent_250_100_MR143_ra$Acc_plot
Acc_plot_250_100_143_ra
ggsave("Acc_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_ra = box_tstudent_250_100_MR143_ra$entropy_plot
entropy_plot_250_100_143_ra
ggsave("entropy_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_ra = box_tstudent_250_100_MR143_ra$FDR_plot
FDR_plot_250_100_143_ra
ggsave("FDR_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_ra = box_tstudent_250_100_MR143_ra$localFDR_plot
localFDR_plot_250_100_143_ra
ggsave("localFDR_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_250_100_MR143_hub = Box_Plot_sim(tstudent_250_100_MR143_hub)

cond_plot_250_100_143_hub = box_tstudent_250_100_MR143_hub$cond_plot
cond_plot_250_100_143_hub
ggsave("cond_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_hub = box_tstudent_250_100_MR143_hub$frob_plot
frob_plot_250_100_143_hub
ggsave("frob_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_hub = box_tstudent_250_100_MR143_hub$TP_plot
TP_plot_250_100_143_hub
ggsave("TP_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_hub = box_tstudent_250_100_MR143_hub$FP_plot
FP_plot_250_100_143_hub
ggsave("FP_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_hub = box_tstudent_250_100_MR143_hub$F1_plot
F1_plot_250_100_143_hub
ggsave("F1_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_hub = box_tstudent_250_100_MR143_hub$Acc_plot
Acc_plot_250_100_143_hub
ggsave("Acc_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_hub = box_tstudent_250_100_MR143_hub$entropy_plot
entropy_plot_250_100_143_hub
ggsave("entropy_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_hub = box_tstudent_250_100_MR143_hub$FDR_plot
FDR_plot_250_100_143_hub
ggsave("FDR_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_hub = box_tstudent_250_100_MR143_hub$localFDR_plot
localFDR_plot_250_100_143_hub
ggsave("localFDR_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_250_100_MR143_sf = Box_Plot_sim(tstudent_250_100_MR143_sf)

cond_plot_250_100_143_sf = box_tstudent_250_100_MR143_sf$cond_plot
cond_plot_250_100_143_sf
ggsave("cond_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_sf = box_tstudent_250_100_MR143_sf$frob_plot
frob_plot_250_100_143_sf
ggsave("frob_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_sf = box_tstudent_250_100_MR143_sf$TP_plot
TP_plot_250_100_143_sf
ggsave("TP_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_sf = box_tstudent_250_100_MR143_sf$FP_plot
FP_plot_250_100_143_sf
ggsave("FP_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_sf = box_tstudent_250_100_MR143_sf$F1_plot
F1_plot_250_100_143_sf
ggsave("F1_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_sf = box_tstudent_250_100_MR143_sf$Acc_plot
Acc_plot_250_100_143_sf
ggsave("Acc_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_sf = box_tstudent_250_100_MR143_sf$entropy_plot
entropy_plot_250_100_143_sf
ggsave("entropy_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_sf = box_tstudent_250_100_MR143_sf$FDR_plot
FDR_plot_250_100_143_sf
ggsave("FDR_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_sf = box_tstudent_250_100_MR143_sf$localFDR_plot
localFDR_plot_250_100_143_sf
ggsave("localFDR_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_250_100_MR143_band = Box_Plot_sim(tstudent_250_100_MR143_band)

cond_plot_250_100_143_band = box_tstudent_250_100_MR143_band$cond_plot
cond_plot_250_100_143_band
ggsave("cond_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_band = box_tstudent_250_100_MR143_band$frob_plot
frob_plot_250_100_143_band
ggsave("frob_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_band = box_tstudent_250_100_MR143_band$TP_plot
TP_plot_250_100_143_band
ggsave("TP_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_band = box_tstudent_250_100_MR143_band$FP_plot
FP_plot_250_100_143_band
ggsave("FP_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_band = box_tstudent_250_100_MR143_band$F1_plot
F1_plot_250_100_143_band
ggsave("F1_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_band = box_tstudent_250_100_MR143_band$Acc_plot
Acc_plot_250_100_143_band
ggsave("Acc_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_band = box_tstudent_250_100_MR143_band$entropy_plot
entropy_plot_250_100_143_band
ggsave("entropy_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_band = box_tstudent_250_100_MR143_band$FDR_plot
FDR_plot_250_100_143_band
ggsave("FDR_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_band = box_tstudent_250_100_MR143_band$localFDR_plot
localFDR_plot_250_100_143_band
ggsave("localFDR_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)


#Dataframes save#

#MR_0#
mean_res_250_100_0_cl = tableGrob(round(tstudent_250_100_MR0_cl$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_cl)
ggsave("mean_res_250_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_cl)

std_res_250_100_0_cl = tableGrob(round(tstudent_250_100_MR0_cl$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_cl)
ggsave("std_res_250_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_cl)

mean_res_250_100_0_ra = tableGrob(round(tstudent_250_100_MR0_ra$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_ra)
ggsave("mean_res_250_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_ra)

std_res_250_100_0_ra = tableGrob(round(tstudent_250_100_MR0_ra$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_ra)
ggsave("std_res_250_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_ra)

mean_res_250_100_0_hub = tableGrob(round(tstudent_250_100_MR0_hub$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_hub)
ggsave("mean_res_250_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_hub)

std_res_250_100_0_hub = tableGrob(round(tstudent_250_100_MR0_hub$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_hub)
ggsave("std_res_250_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_hub)

mean_res_250_100_0_sf = tableGrob(round(tstudent_250_100_MR0_sf$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_sf)
ggsave("mean_res_250_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_sf)

std_res_250_100_0_sf = tableGrob(round(tstudent_250_100_MR0_sf$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_sf)
ggsave("std_res_250_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_sf)

mean_res_250_100_0_band = tableGrob(round(tstudent_250_100_MR0_band$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_band)
ggsave("mean_res_250_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_band)

std_res_250_100_0_band = tableGrob(round(tstudent_250_100_MR0_band$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_band)
ggsave("std_res_250_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_band)

#MR_1_43#
mean_res_250_100_143_cl = tableGrob(round(tstudent_250_100_MR143_cl$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_cl)
ggsave("mean_res_250_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_cl)

std_res_250_100_143_cl = tableGrob(round(tstudent_250_100_MR143_cl$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_cl)
ggsave("std_res_250_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_cl)

mean_res_250_100_143_ra = tableGrob(round(tstudent_250_100_MR143_ra$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_ra)
ggsave("mean_res_250_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_ra)

std_res_250_100_143_ra = tableGrob(round(tstudent_250_100_MR143_ra$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_ra)
ggsave("std_res_250_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_ra)

mean_res_250_100_143_hub = tableGrob(round(tstudent_250_100_MR143_hub$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_hub)
ggsave("mean_res_250_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_hub)

std_res_250_100_143_hub = tableGrob(round(tstudent_250_100_MR143_hub$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_hub)
ggsave("std_res_250_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_hub)

mean_res_250_100_143_sf = tableGrob(round(tstudent_250_100_MR143_sf$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_sf)
ggsave("mean_res_250_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_sf)

std_res_250_100_143_sf = tableGrob(round(tstudent_250_100_MR143_sf$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_sf)
ggsave("std_res_250_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_sf)

mean_res_250_100_143_band = tableGrob(round(tstudent_250_100_MR143_band$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_band)
ggsave("mean_res_250_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_band)

std_res_250_100_143_band = tableGrob(round(tstudent_250_100_MR143_band$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_band)
ggsave("std_res_250_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_band)

#MIXTURE#

#MR_0#
box_mix_250_100_MR0_cl = Box_Plot_sim(mix_250_100_MR0_cl)

cond_plot_250_100_0_cl = box_mix_250_100_MR0_cl$cond_plot
cond_plot_250_100_0_cl
ggsave("cond_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_cl = box_mix_250_100_MR0_cl$frob_plot
frob_plot_250_100_0_cl
ggsave("frob_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_cl = box_mix_250_100_MR0_cl$TP_plot
TP_plot_250_100_0_cl
ggsave("TP_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_cl = box_mix_250_100_MR0_cl$FP_plot
FP_plot_250_100_0_cl
ggsave("FP_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_cl = box_mix_250_100_MR0_cl$F1_plot
F1_plot_250_100_0_cl
ggsave("F1_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_cl = box_mix_250_100_MR0_cl$Acc_plot
Acc_plot_250_100_0_cl
ggsave("Acc_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_cl = box_mix_250_100_MR0_cl$entropy_plot
entropy_plot_250_100_0_cl
ggsave("entropy_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_cl = box_mix_250_100_MR0_cl$FDR_plot
FDR_plot_250_100_0_cl
ggsave("FDR_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_cl = box_mix_250_100_MR0_cl$localFDR_plot
localFDR_plot_250_100_0_cl
ggsave("localFDR_plot_250_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_250_100_MR0_ra = Box_Plot_sim(mix_250_100_MR0_ra)

cond_plot_250_100_0_ra = box_mix_250_100_MR0_ra$cond_plot
cond_plot_250_100_0_ra
ggsave("cond_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_ra = box_mix_250_100_MR0_ra$frob_plot
frob_plot_250_100_0_ra
ggsave("frob_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_ra = box_mix_250_100_MR0_ra$TP_plot
TP_plot_250_100_0_ra
ggsave("TP_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_ra = box_mix_250_100_MR0_ra$FP_plot
FP_plot_250_100_0_ra
ggsave("FP_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_ra = box_mix_250_100_MR0_ra$F1_plot
F1_plot_250_100_0_ra
ggsave("F1_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_ra = box_mix_250_100_MR0_ra$Acc_plot
Acc_plot_250_100_0_ra
ggsave("Acc_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_ra = box_mix_250_100_MR0_ra$entropy_plot
entropy_plot_250_100_0_ra
ggsave("entropy_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_ra = box_mix_250_100_MR0_ra$FDR_plot
FDR_plot_250_100_0_ra
ggsave("FDR_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_ra = box_mix_250_100_MR0_ra$localFDR_plot
localFDR_plot_250_100_0_ra
ggsave("localFDR_plot_250_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_250_100_MR0_hub = Box_Plot_sim(mix_250_100_MR0_hub)

cond_plot_250_100_0_hub = box_mix_250_100_MR0_hub$cond_plot
cond_plot_250_100_0_hub
ggsave("cond_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_hub = box_mix_250_100_MR0_hub$frob_plot
frob_plot_250_100_0_hub
ggsave("frob_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_hub = box_mix_250_100_MR0_hub$TP_plot
TP_plot_250_100_0_hub
ggsave("TP_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_hub = box_mix_250_100_MR0_hub$FP_plot
FP_plot_250_100_0_hub
ggsave("FP_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_hub = box_mix_250_100_MR0_hub$F1_plot
F1_plot_250_100_0_hub
ggsave("F1_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_hub = box_mix_250_100_MR0_hub$Acc_plot
Acc_plot_250_100_0_hub
ggsave("Acc_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_hub = box_mix_250_100_MR0_hub$entropy_plot
entropy_plot_250_100_0_hub
ggsave("entropy_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_hub = box_mix_250_100_MR0_hub$FDR_plot
FDR_plot_250_100_0_hub
ggsave("FDR_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_hub = box_mix_250_100_MR0_hub$localFDR_plot
localFDR_plot_250_100_0_hub
ggsave("localFDR_plot_250_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_250_100_MR0_sf = Box_Plot_sim(mix_250_100_MR0_sf)

cond_plot_250_100_0_sf = box_mix_250_100_MR0_sf$cond_plot
cond_plot_250_100_0_sf
ggsave("cond_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_sf = box_mix_250_100_MR0_sf$frob_plot
frob_plot_250_100_0_sf
ggsave("frob_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_sf = box_mix_250_100_MR0_sf$TP_plot
TP_plot_250_100_0_sf
ggsave("TP_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_sf = box_mix_250_100_MR0_sf$FP_plot
FP_plot_250_100_0_sf
ggsave("FP_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_sf = box_mix_250_100_MR0_sf$F1_plot
F1_plot_250_100_0_sf
ggsave("F1_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_sf = box_mix_250_100_MR0_sf$Acc_plot
Acc_plot_250_100_0_sf
ggsave("Acc_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_sf = box_mix_250_100_MR0_sf$entropy_plot
entropy_plot_250_100_0_sf
ggsave("entropy_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_sf = box_mix_250_100_MR0_sf$FDR_plot
FDR_plot_250_100_0_sf
ggsave("FDR_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_sf = box_mix_250_100_MR0_sf$localFDR_plot
localFDR_plot_250_100_0_sf
ggsave("localFDR_plot_250_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_250_100_MR0_band = Box_Plot_sim(mix_250_100_MR0_band)

cond_plot_250_100_0_band = box_mix_250_100_MR0_band$cond_plot
cond_plot_250_100_0_band
ggsave("cond_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_0_band = box_mix_250_100_MR0_band$frob_plot
frob_plot_250_100_0_band
ggsave("frob_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_0_band = box_mix_250_100_MR0_band$TP_plot
TP_plot_250_100_0_band
ggsave("TP_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_0_band = box_mix_250_100_MR0_band$FP_plot
FP_plot_250_100_0_band
ggsave("FP_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_0_band = box_mix_250_100_MR0_band$F1_plot
F1_plot_250_100_0_band
ggsave("F1_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_0_band = box_mix_250_100_MR0_band$Acc_plot
Acc_plot_250_100_0_band
ggsave("Acc_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_0_band = box_mix_250_100_MR0_band$entropy_plot
entropy_plot_250_100_0_band
ggsave("entropy_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_0_band = box_mix_250_100_MR0_band$FDR_plot
FDR_plot_250_100_0_band
ggsave("FDR_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_0_band = box_mix_250_100_MR0_band$localFDR_plot
localFDR_plot_250_100_0_band
ggsave("localFDR_plot_250_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

#MR_1_43#
box_mix_250_100_MR143_cl = Box_Plot_sim(mix_250_100_MR143_cl)

cond_plot_250_100_143_cl = box_mix_250_100_MR143_cl$cond_plot
cond_plot_250_100_143_cl
ggsave("cond_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_cl = box_mix_250_100_MR143_cl$frob_plot
frob_plot_250_100_143_cl
ggsave("frob_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_cl = box_mix_250_100_MR143_cl$TP_plot
TP_plot_250_100_143_cl
ggsave("TP_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_cl = box_mix_250_100_MR143_cl$FP_plot
FP_plot_250_100_143_cl
ggsave("FP_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_cl = box_mix_250_100_MR143_cl$F1_plot
F1_plot_250_100_143_cl
ggsave("F1_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_cl = box_mix_250_100_MR143_cl$Acc_plot
Acc_plot_250_100_143_cl
ggsave("Acc_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_cl = box_mix_250_100_MR143_cl$entropy_plot
entropy_plot_250_100_143_cl
ggsave("entropy_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_cl = box_mix_250_100_MR143_cl$FDR_plot
FDR_plot_250_100_143_cl
ggsave("FDR_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_cl = box_mix_250_100_MR143_cl$localFDR_plot
localFDR_plot_250_100_143_cl
ggsave("localFDR_plot_250_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_250_100_MR143_ra = Box_Plot_sim(mix_250_100_MR143_ra)

cond_plot_250_100_143_ra = box_mix_250_100_MR143_ra$cond_plot
cond_plot_250_100_143_ra
ggsave("cond_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_ra = box_mix_250_100_MR143_ra$frob_plot
frob_plot_250_100_143_ra
ggsave("frob_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_ra = box_mix_250_100_MR143_ra$TP_plot
TP_plot_250_100_143_ra
ggsave("TP_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_ra = box_mix_250_100_MR143_ra$FP_plot
FP_plot_250_100_143_ra
ggsave("FP_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_ra = box_mix_250_100_MR143_ra$F1_plot
F1_plot_250_100_143_ra
ggsave("F1_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_ra = box_mix_250_100_MR143_ra$Acc_plot
Acc_plot_250_100_143_ra
ggsave("Acc_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_ra = box_mix_250_100_MR143_ra$entropy_plot
entropy_plot_250_100_143_ra
ggsave("entropy_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_ra = box_mix_250_100_MR143_ra$FDR_plot
FDR_plot_250_100_143_ra
ggsave("FDR_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_ra = box_mix_250_100_MR143_ra$localFDR_plot
localFDR_plot_250_100_143_ra
ggsave("localFDR_plot_250_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_250_100_MR143_hub = Box_Plot_sim(mix_250_100_MR143_hub)

cond_plot_250_100_143_hub = box_mix_250_100_MR143_hub$cond_plot
cond_plot_250_100_143_hub
ggsave("cond_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_hub = box_mix_250_100_MR143_hub$frob_plot
frob_plot_250_100_143_hub
ggsave("frob_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_hub = box_mix_250_100_MR143_hub$TP_plot
TP_plot_250_100_143_hub
ggsave("TP_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_hub = box_mix_250_100_MR143_hub$FP_plot
FP_plot_250_100_143_hub
ggsave("FP_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_hub = box_mix_250_100_MR143_hub$F1_plot
F1_plot_250_100_143_hub
ggsave("F1_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_hub = box_mix_250_100_MR143_hub$Acc_plot
Acc_plot_250_100_143_hub
ggsave("Acc_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_hub = box_mix_250_100_MR143_hub$entropy_plot
entropy_plot_250_100_143_hub
ggsave("entropy_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_hub = box_mix_250_100_MR143_hub$FDR_plot
FDR_plot_250_100_143_hub
ggsave("FDR_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_hub = box_mix_250_100_MR143_hub$localFDR_plot
localFDR_plot_250_100_143_hub
ggsave("localFDR_plot_250_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_250_100_MR143_sf = Box_Plot_sim(mix_250_100_MR143_sf)

cond_plot_250_100_143_sf = box_mix_250_100_MR143_sf$cond_plot
cond_plot_250_100_143_sf
ggsave("cond_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_sf = box_mix_250_100_MR143_sf$frob_plot
frob_plot_250_100_143_sf
ggsave("frob_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_sf = box_mix_250_100_MR143_sf$TP_plot
TP_plot_250_100_143_sf
ggsave("TP_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_sf = box_mix_250_100_MR143_sf$FP_plot
FP_plot_250_100_143_sf
ggsave("FP_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_sf = box_mix_250_100_MR143_sf$F1_plot
F1_plot_250_100_143_sf
ggsave("F1_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_sf = box_mix_250_100_MR143_sf$Acc_plot
Acc_plot_250_100_143_sf
ggsave("Acc_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_sf = box_mix_250_100_MR143_sf$entropy_plot
entropy_plot_250_100_143_sf
ggsave("entropy_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_sf = box_mix_250_100_MR143_sf$FDR_plot
FDR_plot_250_100_143_sf
ggsave("FDR_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_sf = box_mix_250_100_MR143_sf$localFDR_plot
localFDR_plot_250_100_143_sf
ggsave("localFDR_plot_250_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_250_100_MR143_band = Box_Plot_sim(mix_250_100_MR143_band)

cond_plot_250_100_143_band = box_mix_250_100_MR143_band$cond_plot
cond_plot_250_100_143_band
ggsave("cond_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_250_100_143_band = box_mix_250_100_MR143_band$frob_plot
frob_plot_250_100_143_band
ggsave("frob_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_250_100_143_band = box_mix_250_100_MR143_band$TP_plot
TP_plot_250_100_143_band
ggsave("TP_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_250_100_143_band = box_mix_250_100_MR143_band$FP_plot
FP_plot_250_100_143_band
ggsave("FP_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_250_100_143_band = box_mix_250_100_MR143_band$F1_plot
F1_plot_250_100_143_band
ggsave("F1_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_250_100_143_band = box_mix_250_100_MR143_band$Acc_plot
Acc_plot_250_100_143_band
ggsave("Acc_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_250_100_143_band = box_mix_250_100_MR143_band$entropy_plot
entropy_plot_250_100_143_band
ggsave("entropy_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_250_100_143_band = box_mix_250_100_MR143_band$FDR_plot
FDR_plot_250_100_143_band
ggsave("FDR_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_250_100_143_band = box_mix_250_100_MR143_band$localFDR_plot
localFDR_plot_250_100_143_band
ggsave("localFDR_plot_250_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)


#Dataframes save#

#MR_0#
mean_res_250_100_0_cl = tableGrob(round(mix_250_100_MR0_cl$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_cl)
ggsave("mean_res_250_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_cl)

std_res_250_100_0_cl = tableGrob(round(mix_250_100_MR0_cl$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_cl)
ggsave("std_res_250_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_cl)

mean_res_250_100_0_ra = tableGrob(round(mix_250_100_MR0_ra$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_ra)
ggsave("mean_res_250_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_ra)

std_res_250_100_0_ra = tableGrob(round(mix_250_100_MR0_ra$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_ra)
ggsave("std_res_250_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_ra)

mean_res_250_100_0_hub = tableGrob(round(mix_250_100_MR0_hub$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_hub)
ggsave("mean_res_250_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_hub)

std_res_250_100_0_hub = tableGrob(round(mix_250_100_MR0_hub$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_hub)
ggsave("std_res_250_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_hub)

mean_res_250_100_0_sf = tableGrob(round(mix_250_100_MR0_sf$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_sf)
ggsave("mean_res_250_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_sf)

std_res_250_100_0_sf = tableGrob(round(mix_250_100_MR0_sf$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_sf)
ggsave("std_res_250_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_sf)

mean_res_250_100_0_band = tableGrob(round(mix_250_100_MR0_band$Average_performance_measures, 4))
grid.draw(mean_res_250_100_0_band)
ggsave("mean_res_250_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_0_band)

std_res_250_100_0_band = tableGrob(round(mix_250_100_MR0_band$Std_performance_measures, 4))
grid.draw(std_res_250_100_0_band)
ggsave("std_res_250_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_0_band)

#MR_1_43#
mean_res_250_100_143_cl = tableGrob(round(mix_250_100_MR143_cl$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_cl)
ggsave("mean_res_250_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_cl)

std_res_250_100_143_cl = tableGrob(round(mix_250_100_MR143_cl$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_cl)
ggsave("std_res_250_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_cl)

mean_res_250_100_143_ra = tableGrob(round(mix_250_100_MR143_ra$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_ra)
ggsave("mean_res_250_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_ra)

std_res_250_100_143_ra = tableGrob(round(mix_250_100_MR143_ra$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_ra)
ggsave("std_res_250_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_ra)

mean_res_250_100_143_hub = tableGrob(round(mix_250_100_MR143_hub$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_hub)
ggsave("mean_res_250_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_hub)

std_res_250_100_143_hub = tableGrob(round(mix_250_100_MR143_hub$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_hub)
ggsave("std_res_250_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_hub)

mean_res_250_100_143_sf = tableGrob(round(mix_250_100_MR143_sf$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_sf)
ggsave("mean_res_250_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_sf)

std_res_250_100_143_sf = tableGrob(round(mix_250_100_MR143_sf$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_sf)
ggsave("std_res_250_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_sf)

mean_res_250_100_143_band = tableGrob(round(mix_250_100_MR143_band$Average_performance_measures, 4))
grid.draw(mean_res_250_100_143_band)
ggsave("mean_res_250_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_250_100_143_band)

std_res_250_100_143_band = tableGrob(round(mix_250_100_MR143_band$Std_performance_measures, 4))
grid.draw(std_res_250_100_143_band)
ggsave("std_res_250_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_250_100_143_band)


###############################N < p############################################

#Code to save the BoxPlots#
library(grid)
library(gridExtra)

#NORMAL#

#MR_0#
box_normal_50_100_MR0_cl = Box_Plot_sim_N_less_p(normal_50_100_MR0_cl)

cond_plot_50_100_0_cl = box_normal_50_100_MR0_cl$cond_plot
cond_plot_50_100_0_cl
ggsave("cond_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_cl = box_normal_50_100_MR0_cl$frob_plot
frob_plot_50_100_0_cl
ggsave("frob_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_cl = box_normal_50_100_MR0_cl$TP_plot
TP_plot_50_100_0_cl
ggsave("TP_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_cl = box_normal_50_100_MR0_cl$FP_plot
FP_plot_50_100_0_cl
ggsave("FP_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_cl = box_normal_50_100_MR0_cl$F1_plot
F1_plot_50_100_0_cl
ggsave("F1_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_cl = box_normal_50_100_MR0_cl$Acc_plot
Acc_plot_50_100_0_cl
ggsave("Acc_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_cl = box_normal_50_100_MR0_cl$entropy_plot
entropy_plot_50_100_0_cl
ggsave("entropy_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_cl = box_normal_50_100_MR0_cl$FDR_plot
FDR_plot_50_100_0_cl
ggsave("FDR_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_cl = box_normal_50_100_MR0_cl$localFDR_plot
localFDR_plot_50_100_0_cl
ggsave("localFDR_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_50_100_MR0_ra = Box_Plot_sim_N_less_p(normal_50_100_MR0_ra)

cond_plot_50_100_0_ra = box_normal_50_100_MR0_ra$cond_plot
cond_plot_50_100_0_ra
ggsave("cond_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_ra = box_normal_50_100_MR0_ra$frob_plot
frob_plot_50_100_0_ra
ggsave("frob_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_ra = box_normal_50_100_MR0_ra$TP_plot
TP_plot_50_100_0_ra
ggsave("TP_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_ra = box_normal_50_100_MR0_ra$FP_plot
FP_plot_50_100_0_ra
ggsave("FP_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_ra = box_normal_50_100_MR0_ra$F1_plot
F1_plot_50_100_0_ra
ggsave("F1_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_ra = box_normal_50_100_MR0_ra$Acc_plot
Acc_plot_50_100_0_ra
ggsave("Acc_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_ra = box_normal_50_100_MR0_ra$entropy_plot
entropy_plot_50_100_0_ra
ggsave("entropy_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_ra = box_normal_50_100_MR0_ra$FDR_plot
FDR_plot_50_100_0_ra
ggsave("FDR_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_ra = box_normal_50_100_MR0_ra$localFDR_plot
localFDR_plot_50_100_0_ra
ggsave("localFDR_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_50_100_MR0_hub = Box_Plot_sim_N_less_p(normal_50_100_MR0_hub)

cond_plot_50_100_0_hub = box_normal_50_100_MR0_hub$cond_plot
cond_plot_50_100_0_hub
ggsave("cond_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_hub = box_normal_50_100_MR0_hub$frob_plot
frob_plot_50_100_0_hub
ggsave("frob_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_hub = box_normal_50_100_MR0_hub$TP_plot
TP_plot_50_100_0_hub
ggsave("TP_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_hub = box_normal_50_100_MR0_hub$FP_plot
FP_plot_50_100_0_hub
ggsave("FP_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_hub = box_normal_50_100_MR0_hub$F1_plot
F1_plot_50_100_0_hub
ggsave("F1_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_hub = box_normal_50_100_MR0_hub$Acc_plot
Acc_plot_50_100_0_hub
ggsave("Acc_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_hub = box_normal_50_100_MR0_hub$entropy_plot
entropy_plot_50_100_0_hub
ggsave("entropy_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_hub = box_normal_50_100_MR0_hub$FDR_plot
FDR_plot_50_100_0_hub
ggsave("FDR_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_hub = box_normal_50_100_MR0_hub$localFDR_plot
localFDR_plot_50_100_0_hub
ggsave("localFDR_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_50_100_MR0_sf = Box_Plot_sim_N_less_p(normal_50_100_MR0_sf)

cond_plot_50_100_0_sf = box_normal_50_100_MR0_sf$cond_plot
cond_plot_50_100_0_sf
ggsave("cond_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_sf = box_normal_50_100_MR0_sf$frob_plot
frob_plot_50_100_0_sf
ggsave("frob_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_sf = box_normal_50_100_MR0_sf$TP_plot
TP_plot_50_100_0_sf
ggsave("TP_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_sf = box_normal_50_100_MR0_sf$FP_plot
FP_plot_50_100_0_sf
ggsave("FP_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_sf = box_normal_50_100_MR0_sf$F1_plot
F1_plot_50_100_0_sf
ggsave("F1_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_sf = box_normal_50_100_MR0_sf$Acc_plot
Acc_plot_50_100_0_sf
ggsave("Acc_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_sf = box_normal_50_100_MR0_sf$entropy_plot
entropy_plot_50_100_0_sf
ggsave("entropy_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_sf = box_normal_50_100_MR0_sf$FDR_plot
FDR_plot_50_100_0_sf
ggsave("FDR_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_sf = box_normal_50_100_MR0_sf$localFDR_plot
localFDR_plot_50_100_0_sf
ggsave("localFDR_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_50_100_MR0_band = Box_Plot_sim_N_less_p(normal_50_100_MR0_band)

cond_plot_50_100_0_band = box_normal_50_100_MR0_band$cond_plot
cond_plot_50_100_0_band
ggsave("cond_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_band = box_normal_50_100_MR0_band$frob_plot
frob_plot_50_100_0_band
ggsave("frob_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_band = box_normal_50_100_MR0_band$TP_plot
TP_plot_50_100_0_band
ggsave("TP_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_band = box_normal_50_100_MR0_band$FP_plot
FP_plot_50_100_0_band
ggsave("FP_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_band = box_normal_50_100_MR0_band$F1_plot
F1_plot_50_100_0_band
ggsave("F1_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_band = box_normal_50_100_MR0_band$Acc_plot
Acc_plot_50_100_0_band
ggsave("Acc_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_band = box_normal_50_100_MR0_band$entropy_plot
entropy_plot_50_100_0_band
ggsave("entropy_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_band = box_normal_50_100_MR0_band$FDR_plot
FDR_plot_50_100_0_band
ggsave("FDR_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_band = box_normal_50_100_MR0_band$localFDR_plot
localFDR_plot_50_100_0_band
ggsave("localFDR_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

#MR_1_43#
box_normal_50_100_MR143_cl = Box_Plot_sim_N_less_p(normal_50_100_MR143_cl)

cond_plot_50_100_143_cl = box_normal_50_100_MR143_cl$cond_plot
cond_plot_50_100_143_cl
ggsave("cond_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_cl = box_normal_50_100_MR143_cl$frob_plot
frob_plot_50_100_143_cl
ggsave("frob_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_cl = box_normal_50_100_MR143_cl$TP_plot
TP_plot_50_100_143_cl
ggsave("TP_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_cl = box_normal_50_100_MR143_cl$FP_plot
FP_plot_50_100_143_cl
ggsave("FP_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_cl = box_normal_50_100_MR143_cl$F1_plot
F1_plot_50_100_143_cl
ggsave("F1_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_cl = box_normal_50_100_MR143_cl$Acc_plot
Acc_plot_50_100_143_cl
ggsave("Acc_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_cl = box_normal_50_100_MR143_cl$entropy_plot
entropy_plot_50_100_143_cl
ggsave("entropy_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_cl = box_normal_50_100_MR143_cl$FDR_plot
FDR_plot_50_100_143_cl
ggsave("FDR_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_cl = box_normal_50_100_MR143_cl$localFDR_plot
localFDR_plot_50_100_143_cl
ggsave("localFDR_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)


box_normal_50_100_MR143_ra = Box_Plot_sim_N_less_p(normal_50_100_MR143_ra)

cond_plot_50_100_143_ra = box_normal_50_100_MR143_ra$cond_plot
cond_plot_50_100_143_ra
ggsave("cond_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_ra = box_normal_50_100_MR143_ra$frob_plot
frob_plot_50_100_143_ra
ggsave("frob_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_ra = box_normal_50_100_MR143_ra$TP_plot
TP_plot_50_100_143_ra
ggsave("TP_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_ra = box_normal_50_100_MR143_ra$FP_plot
FP_plot_50_100_143_ra
ggsave("FP_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_ra = box_normal_50_100_MR143_ra$F1_plot
F1_plot_50_100_143_ra
ggsave("F1_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_ra = box_normal_50_100_MR143_ra$Acc_plot
Acc_plot_50_100_143_ra
ggsave("Acc_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_ra = box_normal_50_100_MR143_ra$entropy_plot
entropy_plot_50_100_143_ra
ggsave("entropy_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_ra = box_normal_50_100_MR143_ra$FDR_plot
FDR_plot_50_100_143_ra
ggsave("FDR_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_ra = box_normal_50_100_MR143_ra$localFDR_plot
localFDR_plot_50_100_143_ra
ggsave("localFDR_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_50_100_MR143_hub = Box_Plot_sim_N_less_p(normal_50_100_MR143_hub)

cond_plot_50_100_143_hub = box_normal_50_100_MR143_hub$cond_plot
cond_plot_50_100_143_hub
ggsave("cond_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_hub = box_normal_50_100_MR143_hub$frob_plot
frob_plot_50_100_143_hub
ggsave("frob_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_hub = box_normal_50_100_MR143_hub$TP_plot
TP_plot_50_100_143_hub
ggsave("TP_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_hub = box_normal_50_100_MR143_hub$FP_plot
FP_plot_50_100_143_hub
ggsave("FP_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_hub = box_normal_50_100_MR143_hub$F1_plot
F1_plot_50_100_143_hub
ggsave("F1_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_hub = box_normal_50_100_MR143_hub$Acc_plot
Acc_plot_50_100_143_hub
ggsave("Acc_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_hub = box_normal_50_100_MR143_hub$entropy_plot
entropy_plot_50_100_143_hub
ggsave("entropy_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_hub = box_normal_50_100_MR143_hub$FDR_plot
FDR_plot_50_100_143_hub
ggsave("FDR_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_hub = box_normal_50_100_MR143_hub$localFDR_plot
localFDR_plot_50_100_143_hub
ggsave("localFDR_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_50_100_MR143_sf = Box_Plot_sim_N_less_p(normal_50_100_MR143_sf)

cond_plot_50_100_143_sf = box_normal_50_100_MR143_sf$cond_plot
cond_plot_50_100_143_sf
ggsave("cond_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_sf = box_normal_50_100_MR143_sf$frob_plot
frob_plot_50_100_143_sf
ggsave("frob_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_sf = box_normal_50_100_MR143_sf$TP_plot
TP_plot_50_100_143_sf
ggsave("TP_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_sf = box_normal_50_100_MR143_sf$FP_plot
FP_plot_50_100_143_sf
ggsave("FP_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_sf = box_normal_50_100_MR143_sf$F1_plot
F1_plot_50_100_143_sf
ggsave("F1_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_sf = box_normal_50_100_MR143_sf$Acc_plot
Acc_plot_50_100_143_sf
ggsave("Acc_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_sf = box_normal_50_100_MR143_sf$entropy_plot
entropy_plot_50_100_143_sf
ggsave("entropy_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_sf = box_normal_50_100_MR143_sf$FDR_plot
FDR_plot_50_100_143_sf
ggsave("FDR_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_sf = box_normal_50_100_MR143_sf$localFDR_plot
localFDR_plot_50_100_143_sf
ggsave("localFDR_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_normal_50_100_MR143_band = Box_Plot_sim_N_less_p(normal_50_100_MR143_band)

cond_plot_50_100_143_band = box_normal_50_100_MR143_band$cond_plot
cond_plot_50_100_143_band
ggsave("cond_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_band = box_normal_50_100_MR143_band$frob_plot
frob_plot_50_100_143_band
ggsave("frob_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_band = box_normal_50_100_MR143_band$TP_plot
TP_plot_50_100_143_band
ggsave("TP_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_band = box_normal_50_100_MR143_band$FP_plot
FP_plot_50_100_143_band
ggsave("FP_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_band = box_normal_50_100_MR143_band$F1_plot
F1_plot_50_100_143_band
ggsave("F1_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_band = box_normal_50_100_MR143_band$Acc_plot
Acc_plot_50_100_143_band
ggsave("Acc_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_band = box_normal_50_100_MR143_band$entropy_plot
entropy_plot_50_100_143_band
ggsave("entropy_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_band = box_normal_50_100_MR143_band$FDR_plot
FDR_plot_50_100_143_band
ggsave("FDR_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_band = box_normal_50_100_MR143_band$localFDR_plot
localFDR_plot_50_100_143_band
ggsave("localFDR_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

#Dataframes save#

#MR_0#
mean_res_50_100_0_cl = tableGrob(round(normal_50_100_MR0_cl$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_cl)
ggsave("mean_res_50_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_cl)

std_res_50_100_0_cl = tableGrob(round(normal_50_100_MR0_cl$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_cl)
ggsave("std_res_50_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_cl)

mean_res_50_100_0_ra = tableGrob(round(normal_50_100_MR0_ra$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_ra)
ggsave("mean_res_50_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_ra)

std_res_50_100_0_ra = tableGrob(round(normal_50_100_MR0_ra$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_ra)
ggsave("std_res_50_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_ra)

mean_res_50_100_0_hub = tableGrob(round(normal_50_100_MR0_hub$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_hub)
ggsave("mean_res_50_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_hub)

std_res_50_100_0_hub = tableGrob(round(normal_50_100_MR0_hub$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_hub)
ggsave("std_res_50_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_hub)

mean_res_50_100_0_sf = tableGrob(round(normal_50_100_MR0_sf$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_sf)
ggsave("mean_res_50_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_sf)

std_res_50_100_0_sf = tableGrob(round(normal_50_100_MR0_sf$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_sf)
ggsave("std_res_50_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_sf)

mean_res_50_100_0_band = tableGrob(round(normal_50_100_MR0_band$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_band)
ggsave("mean_res_50_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_band)

std_res_50_100_0_band = tableGrob(round(normal_50_100_MR0_band$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_band)
ggsave("std_res_50_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_band)

#MR_1_43#
mean_res_50_100_143_cl = tableGrob(round(normal_50_100_MR143_cl$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_cl)
ggsave("mean_res_50_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_cl)

std_res_50_100_143_cl = tableGrob(round(normal_50_100_MR143_cl$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_cl)
ggsave("std_res_50_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_cl)

mean_res_50_100_143_ra = tableGrob(round(normal_50_100_MR143_ra$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_ra)
ggsave("mean_res_50_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_ra)

std_res_50_100_143_ra = tableGrob(round(normal_50_100_MR143_ra$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_ra)
ggsave("std_res_50_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_ra)

mean_res_50_100_143_hub = tableGrob(round(normal_50_100_MR143_hub$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_hub)
ggsave("mean_res_50_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_hub)

std_res_50_100_143_hub = tableGrob(round(normal_50_100_MR143_hub$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_hub)
ggsave("std_res_50_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_hub)

mean_res_50_100_143_sf = tableGrob(round(normal_50_100_MR143_sf$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_sf)
ggsave("mean_res_50_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_sf)

std_res_50_100_143_sf = tableGrob(round(normal_50_100_MR143_sf$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_sf)
ggsave("std_res_50_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_sf)

mean_res_50_100_143_band = tableGrob(round(normal_50_100_MR143_band$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_band)
ggsave("mean_res_50_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_band)

std_res_50_100_143_band = tableGrob(round(normal_50_100_MR143_band$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_band)
ggsave("std_res_50_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_band)

#TSTUDENT#

#MR_0#
box_tstudent_50_100_MR0_cl = Box_Plot_sim_N_less_p(tstudent_50_100_MR0_cl)

cond_plot_50_100_0_cl = box_tstudent_50_100_MR0_cl$cond_plot
cond_plot_50_100_0_cl
ggsave("cond_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_cl = box_tstudent_50_100_MR0_cl$frob_plot
frob_plot_50_100_0_cl
ggsave("frob_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_cl = box_tstudent_50_100_MR0_cl$TP_plot
TP_plot_50_100_0_cl
ggsave("TP_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_cl = box_tstudent_50_100_MR0_cl$FP_plot
FP_plot_50_100_0_cl
ggsave("FP_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_cl = box_tstudent_50_100_MR0_cl$F1_plot
F1_plot_50_100_0_cl
ggsave("F1_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_cl = box_tstudent_50_100_MR0_cl$Acc_plot
Acc_plot_50_100_0_cl
ggsave("Acc_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_cl = box_tstudent_50_100_MR0_cl$entropy_plot
entropy_plot_50_100_0_cl
ggsave("entropy_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_cl = box_tstudent_50_100_MR0_cl$FDR_plot
FDR_plot_50_100_0_cl
ggsave("FDR_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_cl = box_tstudent_50_100_MR0_cl$localFDR_plot
localFDR_plot_50_100_0_cl
ggsave("localFDR_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_50_100_MR0_ra = Box_Plot_sim_N_less_p(tstudent_50_100_MR0_ra)

cond_plot_50_100_0_ra = box_tstudent_50_100_MR0_ra$cond_plot
cond_plot_50_100_0_ra
ggsave("cond_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_ra = box_tstudent_50_100_MR0_ra$frob_plot
frob_plot_50_100_0_ra
ggsave("frob_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_ra = box_tstudent_50_100_MR0_ra$TP_plot
TP_plot_50_100_0_ra
ggsave("TP_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_ra = box_tstudent_50_100_MR0_ra$FP_plot
FP_plot_50_100_0_ra
ggsave("FP_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_ra = box_tstudent_50_100_MR0_ra$F1_plot
F1_plot_50_100_0_ra
ggsave("F1_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_ra = box_tstudent_50_100_MR0_ra$Acc_plot
Acc_plot_50_100_0_ra
ggsave("Acc_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_ra = box_tstudent_50_100_MR0_ra$entropy_plot
entropy_plot_50_100_0_ra
ggsave("entropy_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_ra = box_tstudent_50_100_MR0_ra$FDR_plot
FDR_plot_50_100_0_ra
ggsave("FDR_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_ra = box_tstudent_50_100_MR0_ra$localFDR_plot
localFDR_plot_50_100_0_ra
ggsave("localFDR_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_50_100_MR0_hub = Box_Plot_sim_N_less_p(tstudent_50_100_MR0_hub)

cond_plot_50_100_0_hub = box_tstudent_50_100_MR0_hub$cond_plot
cond_plot_50_100_0_hub
ggsave("cond_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_hub = box_tstudent_50_100_MR0_hub$frob_plot
frob_plot_50_100_0_hub
ggsave("frob_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_hub = box_tstudent_50_100_MR0_hub$TP_plot
TP_plot_50_100_0_hub
ggsave("TP_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_hub = box_tstudent_50_100_MR0_hub$FP_plot
FP_plot_50_100_0_hub
ggsave("FP_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_hub = box_tstudent_50_100_MR0_hub$F1_plot
F1_plot_50_100_0_hub
ggsave("F1_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_hub = box_tstudent_50_100_MR0_hub$Acc_plot
Acc_plot_50_100_0_hub
ggsave("Acc_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_hub = box_tstudent_50_100_MR0_hub$entropy_plot
entropy_plot_50_100_0_hub
ggsave("entropy_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_hub = box_tstudent_50_100_MR0_hub$FDR_plot
FDR_plot_50_100_0_hub
ggsave("FDR_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_hub = box_tstudent_50_100_MR0_hub$localFDR_plot
localFDR_plot_50_100_0_hub
ggsave("localFDR_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_50_100_MR0_sf = Box_Plot_sim_N_less_p(tstudent_50_100_MR0_sf)

cond_plot_50_100_0_sf = box_tstudent_50_100_MR0_sf$cond_plot
cond_plot_50_100_0_sf
ggsave("cond_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_sf = box_tstudent_50_100_MR0_sf$frob_plot
frob_plot_50_100_0_sf
ggsave("frob_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_sf = box_tstudent_50_100_MR0_sf$TP_plot
TP_plot_50_100_0_sf
ggsave("TP_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_sf = box_tstudent_50_100_MR0_sf$FP_plot
FP_plot_50_100_0_sf
ggsave("FP_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_sf = box_tstudent_50_100_MR0_sf$F1_plot
F1_plot_50_100_0_sf
ggsave("F1_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_sf = box_tstudent_50_100_MR0_sf$Acc_plot
Acc_plot_50_100_0_sf
ggsave("Acc_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_sf = box_tstudent_50_100_MR0_sf$entropy_plot
entropy_plot_50_100_0_sf
ggsave("entropy_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_sf = box_tstudent_50_100_MR0_sf$FDR_plot
FDR_plot_50_100_0_sf
ggsave("FDR_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_sf = box_tstudent_50_100_MR0_sf$localFDR_plot
localFDR_plot_50_100_0_sf
ggsave("localFDR_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_50_100_MR0_band = Box_Plot_sim_N_less_p(tstudent_50_100_MR0_band)

cond_plot_50_100_0_band = box_tstudent_50_100_MR0_band$cond_plot
cond_plot_50_100_0_band
ggsave("cond_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_band = box_tstudent_50_100_MR0_band$frob_plot
frob_plot_50_100_0_band
ggsave("frob_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_band = box_tstudent_50_100_MR0_band$TP_plot
TP_plot_50_100_0_band
ggsave("TP_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_band = box_tstudent_50_100_MR0_band$FP_plot
FP_plot_50_100_0_band
ggsave("FP_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_band = box_tstudent_50_100_MR0_band$F1_plot
F1_plot_50_100_0_band
ggsave("F1_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_band = box_tstudent_50_100_MR0_band$Acc_plot
Acc_plot_50_100_0_band
ggsave("Acc_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_band = box_tstudent_50_100_MR0_band$entropy_plot
entropy_plot_50_100_0_band
ggsave("entropy_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_band = box_tstudent_50_100_MR0_band$FDR_plot
FDR_plot_50_100_0_band
ggsave("FDR_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_band = box_tstudent_50_100_MR0_band$localFDR_plot
localFDR_plot_50_100_0_band
ggsave("localFDR_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

#MR_1_43#
box_tstudent_50_100_MR143_cl = Box_Plot_sim_N_less_p(tstudent_50_100_MR143_cl)

cond_plot_50_100_143_cl = box_tstudent_50_100_MR143_cl$cond_plot
cond_plot_50_100_143_cl
ggsave("cond_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_cl = box_tstudent_50_100_MR143_cl$frob_plot
frob_plot_50_100_143_cl
ggsave("frob_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_cl = box_tstudent_50_100_MR143_cl$TP_plot
TP_plot_50_100_143_cl
ggsave("TP_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_cl = box_tstudent_50_100_MR143_cl$FP_plot
FP_plot_50_100_143_cl
ggsave("FP_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_cl = box_tstudent_50_100_MR143_cl$F1_plot
F1_plot_50_100_143_cl
ggsave("F1_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_cl = box_tstudent_50_100_MR143_cl$Acc_plot
Acc_plot_50_100_143_cl
ggsave("Acc_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_cl = box_tstudent_50_100_MR143_cl$entropy_plot
entropy_plot_50_100_143_cl
ggsave("entropy_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_cl = box_tstudent_50_100_MR143_cl$FDR_plot
FDR_plot_50_100_143_cl
ggsave("FDR_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_cl = box_tstudent_50_100_MR143_cl$localFDR_plot
localFDR_plot_50_100_143_cl
ggsave("localFDR_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_50_100_MR143_ra = Box_Plot_sim_N_less_p(tstudent_50_100_MR143_ra)

cond_plot_50_100_143_ra = box_tstudent_50_100_MR143_ra$cond_plot
cond_plot_50_100_143_ra
ggsave("cond_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_ra = box_tstudent_50_100_MR143_ra$frob_plot
frob_plot_50_100_143_ra
ggsave("frob_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_ra = box_tstudent_50_100_MR143_ra$TP_plot
TP_plot_50_100_143_ra
ggsave("TP_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_ra = box_tstudent_50_100_MR143_ra$FP_plot
FP_plot_50_100_143_ra
ggsave("FP_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_ra = box_tstudent_50_100_MR143_ra$F1_plot
F1_plot_50_100_143_ra
ggsave("F1_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_ra = box_tstudent_50_100_MR143_ra$Acc_plot
Acc_plot_50_100_143_ra
ggsave("Acc_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_ra = box_tstudent_50_100_MR143_ra$entropy_plot
entropy_plot_50_100_143_ra
ggsave("entropy_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_ra = box_tstudent_50_100_MR143_ra$FDR_plot
FDR_plot_50_100_143_ra
ggsave("FDR_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_ra = box_tstudent_50_100_MR143_ra$localFDR_plot
localFDR_plot_50_100_143_ra
ggsave("localFDR_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_50_100_MR143_hub = Box_Plot_sim_N_less_p(tstudent_50_100_MR143_hub)

cond_plot_50_100_143_hub = box_tstudent_50_100_MR143_hub$cond_plot
cond_plot_50_100_143_hub
ggsave("cond_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_hub = box_tstudent_50_100_MR143_hub$frob_plot
frob_plot_50_100_143_hub
ggsave("frob_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_hub = box_tstudent_50_100_MR143_hub$TP_plot
TP_plot_50_100_143_hub
ggsave("TP_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_hub = box_tstudent_50_100_MR143_hub$FP_plot
FP_plot_50_100_143_hub
ggsave("FP_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_hub = box_tstudent_50_100_MR143_hub$F1_plot
F1_plot_50_100_143_hub
ggsave("F1_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_hub = box_tstudent_50_100_MR143_hub$Acc_plot
Acc_plot_50_100_143_hub
ggsave("Acc_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_hub = box_tstudent_50_100_MR143_hub$entropy_plot
entropy_plot_50_100_143_hub
ggsave("entropy_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_hub = box_tstudent_50_100_MR143_hub$FDR_plot
FDR_plot_50_100_143_hub
ggsave("FDR_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_hub = box_tstudent_50_100_MR143_hub$localFDR_plot
localFDR_plot_50_100_143_hub
ggsave("localFDR_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_50_100_MR143_sf = Box_Plot_sim_N_less_p(tstudent_50_100_MR143_sf)

cond_plot_50_100_143_sf = box_tstudent_50_100_MR143_sf$cond_plot
cond_plot_50_100_143_sf
ggsave("cond_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_sf = box_tstudent_50_100_MR143_sf$frob_plot
frob_plot_50_100_143_sf
ggsave("frob_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_sf = box_tstudent_50_100_MR143_sf$TP_plot
TP_plot_50_100_143_sf
ggsave("TP_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_sf = box_tstudent_50_100_MR143_sf$FP_plot
FP_plot_50_100_143_sf
ggsave("FP_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_sf = box_tstudent_50_100_MR143_sf$F1_plot
F1_plot_50_100_143_sf
ggsave("F1_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_sf = box_tstudent_50_100_MR143_sf$Acc_plot
Acc_plot_50_100_143_sf
ggsave("Acc_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_sf = box_tstudent_50_100_MR143_sf$entropy_plot
entropy_plot_50_100_143_sf
ggsave("entropy_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_sf = box_tstudent_50_100_MR143_sf$FDR_plot
FDR_plot_50_100_143_sf
ggsave("FDR_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_sf = box_tstudent_50_100_MR143_sf$localFDR_plot
localFDR_plot_50_100_143_sf
ggsave("localFDR_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_tstudent_50_100_MR143_band = Box_Plot_sim_N_less_p(tstudent_50_100_MR143_band)

cond_plot_50_100_143_band = box_tstudent_50_100_MR143_band$cond_plot
cond_plot_50_100_143_band
ggsave("cond_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_band = box_tstudent_50_100_MR143_band$frob_plot
frob_plot_50_100_143_band
ggsave("frob_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_band = box_tstudent_50_100_MR143_band$TP_plot
TP_plot_50_100_143_band
ggsave("TP_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_band = box_tstudent_50_100_MR143_band$FP_plot
FP_plot_50_100_143_band
ggsave("FP_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_band = box_tstudent_50_100_MR143_band$F1_plot
F1_plot_50_100_143_band
ggsave("F1_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_band = box_tstudent_50_100_MR143_band$Acc_plot
Acc_plot_50_100_143_band
ggsave("Acc_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_band = box_tstudent_50_100_MR143_band$entropy_plot
entropy_plot_50_100_143_band
ggsave("entropy_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_band = box_tstudent_50_100_MR143_band$FDR_plot
FDR_plot_50_100_143_band
ggsave("FDR_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_band = box_tstudent_50_100_MR143_band$localFDR_plot
localFDR_plot_50_100_143_band
ggsave("localFDR_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)


#Dataframes save#

#MR_0#
mean_res_50_100_0_cl = tableGrob(round(tstudent_50_100_MR0_cl$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_cl)
ggsave("mean_res_50_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_cl)

std_res_50_100_0_cl = tableGrob(round(tstudent_50_100_MR0_cl$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_cl)
ggsave("std_res_50_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_cl)

mean_res_50_100_0_ra = tableGrob(round(tstudent_50_100_MR0_ra$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_ra)
ggsave("mean_res_50_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_ra)

std_res_50_100_0_ra = tableGrob(round(tstudent_50_100_MR0_ra$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_ra)
ggsave("std_res_50_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_ra)

mean_res_50_100_0_hub = tableGrob(round(tstudent_50_100_MR0_hub$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_hub)
ggsave("mean_res_50_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_hub)

std_res_50_100_0_hub = tableGrob(round(tstudent_50_100_MR0_hub$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_hub)
ggsave("std_res_50_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_hub)

mean_res_50_100_0_sf = tableGrob(round(tstudent_50_100_MR0_sf$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_sf)
ggsave("mean_res_50_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_sf)

std_res_50_100_0_sf = tableGrob(round(tstudent_50_100_MR0_sf$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_sf)
ggsave("std_res_50_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_sf)

mean_res_50_100_0_band = tableGrob(round(tstudent_50_100_MR0_band$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_band)
ggsave("mean_res_50_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_band)

std_res_50_100_0_band = tableGrob(round(tstudent_50_100_MR0_band$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_band)
ggsave("std_res_50_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_band)

#MR_1_43#
mean_res_50_100_143_cl = tableGrob(round(tstudent_50_100_MR143_cl$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_cl)
ggsave("mean_res_50_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_cl)

std_res_50_100_143_cl = tableGrob(round(tstudent_50_100_MR143_cl$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_cl)
ggsave("std_res_50_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_cl)

mean_res_50_100_143_ra = tableGrob(round(tstudent_50_100_MR143_ra$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_ra)
ggsave("mean_res_50_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_ra)

std_res_50_100_143_ra = tableGrob(round(tstudent_50_100_MR143_ra$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_ra)
ggsave("std_res_50_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_ra)

mean_res_50_100_143_hub = tableGrob(round(tstudent_50_100_MR143_hub$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_hub)
ggsave("mean_res_50_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_hub)

std_res_50_100_143_hub = tableGrob(round(tstudent_50_100_MR143_hub$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_hub)
ggsave("std_res_50_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_hub)

mean_res_50_100_143_sf = tableGrob(round(tstudent_50_100_MR143_sf$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_sf)
ggsave("mean_res_50_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_sf)

std_res_50_100_143_sf = tableGrob(round(tstudent_50_100_MR143_sf$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_sf)
ggsave("std_res_50_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_sf)

mean_res_50_100_143_band = tableGrob(round(tstudent_50_100_MR143_band$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_band)
ggsave("mean_res_50_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_band)

std_res_50_100_143_band = tableGrob(round(tstudent_50_100_MR143_band$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_band)
ggsave("std_res_50_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_band)

#MIXTURE#

#MR_0#
box_mix_50_100_MR0_cl = Box_Plot_sim_N_less_p(mix_50_100_MR0_cl)

cond_plot_50_100_0_cl = box_mix_50_100_MR0_cl$cond_plot
cond_plot_50_100_0_cl
ggsave("cond_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_cl = box_mix_50_100_MR0_cl$frob_plot
frob_plot_50_100_0_cl
ggsave("frob_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_cl = box_mix_50_100_MR0_cl$TP_plot
TP_plot_50_100_0_cl
ggsave("TP_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_cl = box_mix_50_100_MR0_cl$FP_plot
FP_plot_50_100_0_cl
ggsave("FP_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_cl = box_mix_50_100_MR0_cl$F1_plot
F1_plot_50_100_0_cl
ggsave("F1_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_cl = box_mix_50_100_MR0_cl$Acc_plot
Acc_plot_50_100_0_cl
ggsave("Acc_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_cl = box_mix_50_100_MR0_cl$entropy_plot
entropy_plot_50_100_0_cl
ggsave("entropy_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_cl = box_mix_50_100_MR0_cl$FDR_plot
FDR_plot_50_100_0_cl
ggsave("FDR_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_cl = box_mix_50_100_MR0_cl$localFDR_plot
localFDR_plot_50_100_0_cl
ggsave("localFDR_plot_50_100_0_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_50_100_MR0_ra = Box_Plot_sim_N_less_p(mix_50_100_MR0_ra)

cond_plot_50_100_0_ra = box_mix_50_100_MR0_ra$cond_plot
cond_plot_50_100_0_ra
ggsave("cond_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_ra = box_mix_50_100_MR0_ra$frob_plot
frob_plot_50_100_0_ra
ggsave("frob_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_ra = box_mix_50_100_MR0_ra$TP_plot
TP_plot_50_100_0_ra
ggsave("TP_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_ra = box_mix_50_100_MR0_ra$FP_plot
FP_plot_50_100_0_ra
ggsave("FP_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_ra = box_mix_50_100_MR0_ra$F1_plot
F1_plot_50_100_0_ra
ggsave("F1_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_ra = box_mix_50_100_MR0_ra$Acc_plot
Acc_plot_50_100_0_ra
ggsave("Acc_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_ra = box_mix_50_100_MR0_ra$entropy_plot
entropy_plot_50_100_0_ra
ggsave("entropy_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_ra = box_mix_50_100_MR0_ra$FDR_plot
FDR_plot_50_100_0_ra
ggsave("FDR_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_ra = box_mix_50_100_MR0_ra$localFDR_plot
localFDR_plot_50_100_0_ra
ggsave("localFDR_plot_50_100_0_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_50_100_MR0_hub = Box_Plot_sim_N_less_p(mix_50_100_MR0_hub)

cond_plot_50_100_0_hub = box_mix_50_100_MR0_hub$cond_plot
cond_plot_50_100_0_hub
ggsave("cond_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_hub = box_mix_50_100_MR0_hub$frob_plot
frob_plot_50_100_0_hub
ggsave("frob_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_hub = box_mix_50_100_MR0_hub$TP_plot
TP_plot_50_100_0_hub
ggsave("TP_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_hub = box_mix_50_100_MR0_hub$FP_plot
FP_plot_50_100_0_hub
ggsave("FP_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_hub = box_mix_50_100_MR0_hub$F1_plot
F1_plot_50_100_0_hub
ggsave("F1_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_hub = box_mix_50_100_MR0_hub$Acc_plot
Acc_plot_50_100_0_hub
ggsave("Acc_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_hub = box_mix_50_100_MR0_hub$entropy_plot
entropy_plot_50_100_0_hub
ggsave("entropy_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_hub = box_mix_50_100_MR0_hub$FDR_plot
FDR_plot_50_100_0_hub
ggsave("FDR_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_hub = box_mix_50_100_MR0_hub$localFDR_plot
localFDR_plot_50_100_0_hub
ggsave("localFDR_plot_50_100_0_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_50_100_MR0_sf = Box_Plot_sim_N_less_p(mix_50_100_MR0_sf)

cond_plot_50_100_0_sf = box_mix_50_100_MR0_sf$cond_plot
cond_plot_50_100_0_sf
ggsave("cond_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_sf = box_mix_50_100_MR0_sf$frob_plot
frob_plot_50_100_0_sf
ggsave("frob_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_sf = box_mix_50_100_MR0_sf$TP_plot
TP_plot_50_100_0_sf
ggsave("TP_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_sf = box_mix_50_100_MR0_sf$FP_plot
FP_plot_50_100_0_sf
ggsave("FP_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_sf = box_mix_50_100_MR0_sf$F1_plot
F1_plot_50_100_0_sf
ggsave("F1_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_sf = box_mix_50_100_MR0_sf$Acc_plot
Acc_plot_50_100_0_sf
ggsave("Acc_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_sf = box_mix_50_100_MR0_sf$entropy_plot
entropy_plot_50_100_0_sf
ggsave("entropy_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_sf = box_mix_50_100_MR0_sf$FDR_plot
FDR_plot_50_100_0_sf
ggsave("FDR_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_sf = box_mix_50_100_MR0_sf$localFDR_plot
localFDR_plot_50_100_0_sf
ggsave("localFDR_plot_50_100_0_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_50_100_MR0_band = Box_Plot_sim_N_less_p(mix_50_100_MR0_band)

cond_plot_50_100_0_band = box_mix_50_100_MR0_band$cond_plot
cond_plot_50_100_0_band
ggsave("cond_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_0_band = box_mix_50_100_MR0_band$frob_plot
frob_plot_50_100_0_band
ggsave("frob_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_0_band = box_mix_50_100_MR0_band$TP_plot
TP_plot_50_100_0_band
ggsave("TP_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_0_band = box_mix_50_100_MR0_band$FP_plot
FP_plot_50_100_0_band
ggsave("FP_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_0_band = box_mix_50_100_MR0_band$F1_plot
F1_plot_50_100_0_band
ggsave("F1_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_0_band = box_mix_50_100_MR0_band$Acc_plot
Acc_plot_50_100_0_band
ggsave("Acc_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_0_band = box_mix_50_100_MR0_band$entropy_plot
entropy_plot_50_100_0_band
ggsave("entropy_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_0_band = box_mix_50_100_MR0_band$FDR_plot
FDR_plot_50_100_0_band
ggsave("FDR_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_0_band = box_mix_50_100_MR0_band$localFDR_plot
localFDR_plot_50_100_0_band
ggsave("localFDR_plot_50_100_0_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

#MR_1_43#
box_mix_50_100_MR143_cl = Box_Plot_sim_N_less_p(mix_50_100_MR143_cl)

cond_plot_50_100_143_cl = box_mix_50_100_MR143_cl$cond_plot
cond_plot_50_100_143_cl
ggsave("cond_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_cl = box_mix_50_100_MR143_cl$frob_plot
frob_plot_50_100_143_cl
ggsave("frob_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_cl = box_mix_50_100_MR143_cl$TP_plot
TP_plot_50_100_143_cl
ggsave("TP_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_cl = box_mix_50_100_MR143_cl$FP_plot
FP_plot_50_100_143_cl
ggsave("FP_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_cl = box_mix_50_100_MR143_cl$F1_plot
F1_plot_50_100_143_cl
ggsave("F1_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_cl = box_mix_50_100_MR143_cl$Acc_plot
Acc_plot_50_100_143_cl
ggsave("Acc_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_cl = box_mix_50_100_MR143_cl$entropy_plot
entropy_plot_50_100_143_cl
ggsave("entropy_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_cl = box_mix_50_100_MR143_cl$FDR_plot
FDR_plot_50_100_143_cl
ggsave("FDR_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_cl = box_mix_50_100_MR143_cl$localFDR_plot
localFDR_plot_50_100_143_cl
ggsave("localFDR_plot_50_100_143_cl.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_50_100_MR143_ra = Box_Plot_sim_N_less_p(mix_50_100_MR143_ra)

cond_plot_50_100_143_ra = box_mix_50_100_MR143_ra$cond_plot
cond_plot_50_100_143_ra
ggsave("cond_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_ra = box_mix_50_100_MR143_ra$frob_plot
frob_plot_50_100_143_ra
ggsave("frob_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_ra = box_mix_50_100_MR143_ra$TP_plot
TP_plot_50_100_143_ra
ggsave("TP_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_ra = box_mix_50_100_MR143_ra$FP_plot
FP_plot_50_100_143_ra
ggsave("FP_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_ra = box_mix_50_100_MR143_ra$F1_plot
F1_plot_50_100_143_ra
ggsave("F1_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_ra = box_mix_50_100_MR143_ra$Acc_plot
Acc_plot_50_100_143_ra
ggsave("Acc_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_ra = box_mix_50_100_MR143_ra$entropy_plot
entropy_plot_50_100_143_ra
ggsave("entropy_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_ra = box_mix_50_100_MR143_ra$FDR_plot
FDR_plot_50_100_143_ra
ggsave("FDR_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_ra = box_mix_50_100_MR143_ra$localFDR_plot
localFDR_plot_50_100_143_ra
ggsave("localFDR_plot_50_100_143_ra.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_50_100_MR143_hub = Box_Plot_sim_N_less_p(mix_50_100_MR143_hub)

cond_plot_50_100_143_hub = box_mix_50_100_MR143_hub$cond_plot
cond_plot_50_100_143_hub
ggsave("cond_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_hub = box_mix_50_100_MR143_hub$frob_plot
frob_plot_50_100_143_hub
ggsave("frob_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_hub = box_mix_50_100_MR143_hub$TP_plot
TP_plot_50_100_143_hub
ggsave("TP_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_hub = box_mix_50_100_MR143_hub$FP_plot
FP_plot_50_100_143_hub
ggsave("FP_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_hub = box_mix_50_100_MR143_hub$F1_plot
F1_plot_50_100_143_hub
ggsave("F1_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_hub = box_mix_50_100_MR143_hub$Acc_plot
Acc_plot_50_100_143_hub
ggsave("Acc_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_hub = box_mix_50_100_MR143_hub$entropy_plot
entropy_plot_50_100_143_hub
ggsave("entropy_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_hub = box_mix_50_100_MR143_hub$FDR_plot
FDR_plot_50_100_143_hub
ggsave("FDR_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_hub = box_mix_50_100_MR143_hub$localFDR_plot
localFDR_plot_50_100_143_hub
ggsave("localFDR_plot_50_100_143_hub.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_50_100_MR143_sf = Box_Plot_sim_N_less_p(mix_50_100_MR143_sf)

cond_plot_50_100_143_sf = box_mix_50_100_MR143_sf$cond_plot
cond_plot_50_100_143_sf
ggsave("cond_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_sf = box_mix_50_100_MR143_sf$frob_plot
frob_plot_50_100_143_sf
ggsave("frob_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_sf = box_mix_50_100_MR143_sf$TP_plot
TP_plot_50_100_143_sf
ggsave("TP_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_sf = box_mix_50_100_MR143_sf$FP_plot
FP_plot_50_100_143_sf
ggsave("FP_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_sf = box_mix_50_100_MR143_sf$F1_plot
F1_plot_50_100_143_sf
ggsave("F1_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_sf = box_mix_50_100_MR143_sf$Acc_plot
Acc_plot_50_100_143_sf
ggsave("Acc_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_sf = box_mix_50_100_MR143_sf$entropy_plot
entropy_plot_50_100_143_sf
ggsave("entropy_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_sf = box_mix_50_100_MR143_sf$FDR_plot
FDR_plot_50_100_143_sf
ggsave("FDR_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_sf = box_mix_50_100_MR143_sf$localFDR_plot
localFDR_plot_50_100_143_sf
ggsave("localFDR_plot_50_100_143_sf.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

box_mix_50_100_MR143_band = Box_Plot_sim_N_less_p(mix_50_100_MR143_band)

cond_plot_50_100_143_band = box_mix_50_100_MR143_band$cond_plot
cond_plot_50_100_143_band
ggsave("cond_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

frob_plot_50_100_143_band = box_mix_50_100_MR143_band$frob_plot
frob_plot_50_100_143_band
ggsave("frob_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

TP_plot_50_100_143_band = box_mix_50_100_MR143_band$TP_plot
TP_plot_50_100_143_band
ggsave("TP_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FP_plot_50_100_143_band = box_mix_50_100_MR143_band$FP_plot
FP_plot_50_100_143_band
ggsave("FP_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

F1_plot_50_100_143_band = box_mix_50_100_MR143_band$F1_plot
F1_plot_50_100_143_band
ggsave("F1_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

Acc_plot_50_100_143_band = box_mix_50_100_MR143_band$Acc_plot
Acc_plot_50_100_143_band
ggsave("Acc_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

entropy_plot_50_100_143_band = box_mix_50_100_MR143_band$entropy_plot
entropy_plot_50_100_143_band
ggsave("entropy_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

FDR_plot_50_100_143_band = box_mix_50_100_MR143_band$FDR_plot
FDR_plot_50_100_143_band
ggsave("FDR_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

localFDR_plot_50_100_143_band = box_mix_50_100_MR143_band$localFDR_plot
localFDR_plot_50_100_143_band
ggsave("localFDR_plot_50_100_143_band.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)


#Dataframes save#

#MR_0#
mean_res_50_100_0_cl = tableGrob(round(mix_50_100_MR0_cl$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_cl)
ggsave("mean_res_50_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_cl)

std_res_50_100_0_cl = tableGrob(round(mix_50_100_MR0_cl$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_cl)
ggsave("std_res_50_100_0_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_cl)

mean_res_50_100_0_ra = tableGrob(round(mix_50_100_MR0_ra$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_ra)
ggsave("mean_res_50_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_ra)

std_res_50_100_0_ra = tableGrob(round(mix_50_100_MR0_ra$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_ra)
ggsave("std_res_50_100_0_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_ra)

mean_res_50_100_0_hub = tableGrob(round(mix_50_100_MR0_hub$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_hub)
ggsave("mean_res_50_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_hub)

std_res_50_100_0_hub = tableGrob(round(mix_50_100_MR0_hub$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_hub)
ggsave("std_res_50_100_0_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_hub)

mean_res_50_100_0_sf = tableGrob(round(mix_50_100_MR0_sf$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_sf)
ggsave("mean_res_50_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_sf)

std_res_50_100_0_sf = tableGrob(round(mix_50_100_MR0_sf$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_sf)
ggsave("std_res_50_100_0_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_sf)

mean_res_50_100_0_band = tableGrob(round(mix_50_100_MR0_band$Average_performance_measures, 4))
grid.draw(mean_res_50_100_0_band)
ggsave("mean_res_50_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_0_band)

std_res_50_100_0_band = tableGrob(round(mix_50_100_MR0_band$Std_performance_measures, 4))
grid.draw(std_res_50_100_0_band)
ggsave("std_res_50_100_0_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_0_band)

#MR_1_43#
mean_res_50_100_143_cl = tableGrob(round(mix_50_100_MR143_cl$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_cl)
ggsave("mean_res_50_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_cl)

std_res_50_100_143_cl = tableGrob(round(mix_50_100_MR143_cl$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_cl)
ggsave("std_res_50_100_143_cl.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_cl)

mean_res_50_100_143_ra = tableGrob(round(mix_50_100_MR143_ra$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_ra)
ggsave("mean_res_50_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_ra)

std_res_50_100_143_ra = tableGrob(round(mix_50_100_MR143_ra$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_ra)
ggsave("std_res_50_100_143_ra.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_ra)

mean_res_50_100_143_hub = tableGrob(round(mix_50_100_MR143_hub$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_hub)
ggsave("mean_res_50_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_hub)

std_res_50_100_143_hub = tableGrob(round(mix_50_100_MR143_hub$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_hub)
ggsave("std_res_50_100_143_hub.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_hub)

mean_res_50_100_143_sf = tableGrob(round(mix_50_100_MR143_sf$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_sf)
ggsave("mean_res_50_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_sf)

std_res_50_100_143_sf = tableGrob(round(mix_50_100_MR143_sf$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_sf)
ggsave("std_res_50_100_143_sf.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_sf)

mean_res_50_100_143_band = tableGrob(round(mix_50_100_MR143_band$Average_performance_measures, 4))
grid.draw(mean_res_50_100_143_band)
ggsave("mean_res_50_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = mean_res_50_100_143_band)

std_res_50_100_143_band = tableGrob(round(mix_50_100_MR143_band$Std_performance_measures, 4))
grid.draw(std_res_50_100_143_band)
ggsave("std_res_50_100_143_band.pdf", width = 14, height = 5, units = "in", device = cairo_pdf, plot = std_res_50_100_143_band)

#Heatmap Generation#
heatmap_generation <- function(covariance_matrix, title_text) {
  sigma       = covariance_matrix
  melted      = melt(cov2cor(sigma))
  melted$Var1 = reorder(melted$Var1, as.numeric(melted$Var1))
  melted$Var2 = reorder(melted$Var2, as.numeric(melted$Var2))
  
  heatmap = ggplot(data = melted, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() + labs(title = title_text, x = element_blank(), y = element_blank()) +
    theme(axis.text.x=  element_blank(), axis.text.y = element_blank()) +
    scale_fill_gradient(low = "white", high = "red")
  
  return(heatmap)
}

cl_net_0 = oracle_precision_matrix_generation(250, 50, "cluster", -1, 1)
heatmap_generation(cl_net_0$sigma, "Cluster - MR = 0")
ggsave("HeatMap_cl_0.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

cl_net_143 = oracle_precision_matrix_generation(250, 50, "cluster", 0.7, 0.3)
heatmap_generation(cl_net_143$sigma, "Cluster - MR = 1.43")
ggsave("HeatMap_cl_143.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

ra_net_0 = oracle_precision_matrix_generation(250, 50, "random", -1, 1)
heatmap_generation(ra_net_0$sigma, "Random - MR = 0")
ggsave("HeatMap_ra_0.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

ra_net_143 = oracle_precision_matrix_generation(250, 50, "random", 0.7, 0.3)
heatmap_generation(ra_net_143$sigma, "Random - MR = 1.43")
ggsave("HeatMap_ra_143.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

hub_net_0 = oracle_precision_matrix_generation(250, 50, "hub", -1, 1)
heatmap_generation(hub_net_0$sigma, "Hub - MR = 0")
ggsave("HeatMap_hub_0.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

hub_net_143 = oracle_precision_matrix_generation(250, 50, "hub", 0.7, 0.3)
heatmap_generation(hub_net_143$sigma, "Hub - MR = 1.43")
ggsave("HeatMap_hub_143.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

sf_net_0 = oracle_precision_matrix_generation(250, 50, "scale-free", -1, 1)
heatmap_generation(sf_net_0$sigma, "Scale-Free - MR = 0")
ggsave("HeatMap_sf_0.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

sf_net_143 = oracle_precision_matrix_generation(250, 50, "scale-free", 0.7, 0.3)
heatmap_generation(sf_net_143$sigma, "Scale-Free - MR = 1.43")
ggsave("HeatMap_sf_143.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

band_net_0 = oracle_precision_matrix_generation(250, 50, "band", -1, 1)
heatmap_generation(band_net_0$sigma, "Band - MR = 0")
ggsave("HeatMap_band_0.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

band_net_143 = oracle_precision_matrix_generation(250, 50, "band", 0.7, 0.3)
heatmap_generation(band_net_143$sigma, "Band - MR = 1.43")
ggsave("HeatMap_band_143.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

sw_net_0 = smallworld_graph_gen(250, 50, 15, 0.20, -1, 1)
heatmap_generation(sw_net_0$sigma, "Heatmap Small-World")
ggsave("HeatMap_sw_0.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

sw_net_143 = smallworld_graph_gen(250, 50, 15, 0.20, 0.7, 0.3)
heatmap_generation(sw_net_143$sigma, "Heatmap Small-World")
ggsave("HeatMap_sw_143.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

core_net_0 = coreperiphery_graph_gen(250, 50, 15, 0.20, -1, 1)
heatmap_generation(core_net_0$sigma, "Heatmap Core-Pheriphery")
ggsave("HeatMap_core_0.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

core_net_143 = smallworld_graph_gen(250, 50, 15, 0.20, 0.7, 0.3)
heatmap_generation(core_net_143$sigma, "Heatmap Core-Pheriphery")
ggsave("HeatMap_core_143.pdf", width = 6, height = 5, units = "in", device = cairo_pdf)

