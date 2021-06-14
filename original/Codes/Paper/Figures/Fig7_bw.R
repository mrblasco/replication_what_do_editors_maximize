##### Housekeeping #####

rm(list=ls())
library('foreign')
library('rstudioapi')
library('readstata13')

# Please set path 
# setwd("/Users/username/Desktop/ReplicationFiles/Codes/Paper/Figures")

# Print-version black and white (bw=1: black and white)
bw = 1

outputfile.citations = "../../../Output/Paper/Figures/Fig7.pdf"
# Read in cleaned survey results
results_long = read.dta13("../../../Data/Raw/survey_results.dta")

# Set a few plotting parameters
if (bw == 1) {
    my.colors = colors()[175]
    outputfile.citations = "../../../Output/Paper/Figures/Fig7_bw.pdf"
} else {
    my.colors = "blue"
}

my.cex.main = 1.5
my.cex.lab = 1.2
my.cex.axis = 1.5
my.cex.text = 1.2
my.cex = 1.75
my.cex.sub = 1

### Function for Clustering Standard Errors (we cluster by paper pair in figure 10b)
cl = function(fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M = length(unique(cluster))
  N = length(cluster)
  K = fm$rank
  dfc = (M/(M-1))*((N-1)/(N-K))
  uj  = apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL = dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }


### Collapse data for figure down into deciles (since we are plotting a bin-scatter)
log_ratio_q = quantile(results_long$log_ratio, probs = seq(0,0.9,by=0.1))
results_long$decile = findInterval(x=results_long$log_ratio, vec=log_ratio_q)
results_deciles = aggregate(results_long[,c("log_ratio", "est_log_ratio", "promtoohigh", "promtoolow",
                                            "Nov", "Expo", "Rig", "Imp")],
                            by = list(results_long$decile), FUN = mean)

######################## Create and Save Plots #############################
####### PNG Plots
landscape.a4.width = 11.69
landscape.a4.height = 8.27
png.scale = 1

### Plot for Elicitation of Citation Ratios
png(filename = outputfile.citations,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)

### Figure 7
plot(x = results_deciles$log_ratio,
     y = results_deciles$est_log_ratio,
     xlim = c(log(1/5),log(5)), ylim = c(log(1/5),log(5)),
     xlab = "Actual Log Citation Ratio",
     ylab = "Log Citation Ratio Elicited from Survey",
     main = "",
     sub = "Plotted points correspond to mean within each decile of log citation ratio.",
     cex.sub = 1, type = "p", pch = 19,
     cex.main = my.cex.main, cex.lab = my.cex.lab, cex.axis = my.cex.axis, cex = my.cex, cex.sub = my.cex.sub, col=my.colors)
abline(v=0,lty=2)
abline(h=0,lty=2)
discount.reg = lm(est_log_ratio ~ log_ratio, data = results_long)
abline(reg = discount.reg, lwd=2, lty=5, col=my.colors)
text(x=0.5, y=-1, pos=4, cex = my.cex.text,
     labels = paste0("Slope = ", round(coef(discount.reg)[2],2),
                     " (", round(cl(discount.reg, cluster = results_long$title_id)[2,2],2),")",
                     "\nIntercept = ", round(coef(discount.reg)[1],2),
                     " (", round(cl(discount.reg, cluster = results_long$title_id)[1,2],2),
                     ")", 
                     "\nIntercept indicates quality by"
                     , "\ndiscount for paper by" 
                     , "\nprolific authors" ))
dev.off()


####### PDF Plots
pdf.scale = 1

### Figure 7
pdf(gsub("png", "pdf", outputfile.citations), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)

plot(x = results_deciles$log_ratio,
     y = results_deciles$est_log_ratio,
     xlim = c(log(1/5),log(5)), ylim = c(log(1/5),log(5)),
     xlab = "Actual Log Citation Ratio",
     ylab = "Log Citation Ratio Elicited from Survey",
     main = "",
     sub = "Plotted points correspond to mean within each decile of log citation ratio.",
     cex.sub = 1, type = "p", pch = 19,
     cex.main = my.cex.main, cex.lab = my.cex.lab, cex.axis = my.cex.axis, cex = my.cex, cex.sub = my.cex.sub, col=my.colors)
abline(v=0,lty=2)
abline(h=0,lty=2)
discount.reg = lm(est_log_ratio ~ log_ratio, data = results_long)
abline(reg = discount.reg, lwd=2, lty=5, col=my.colors)
text(x=0.5, y=-1, pos=4, cex = my.cex.text,
     labels = paste0("Slope = ", round(coef(discount.reg)[2],2),
                     " (", round(cl(discount.reg, cluster = results_long$title_id)[2,2],2),")",
                     "\nIntercept = ", round(coef(discount.reg)[1],2),
                     " (", round(cl(discount.reg, cluster = results_long$title_id)[1,2],2),
                      ")", 
                     "\nIntercept indicates quality by"
                     , "\ndiscount for paper by" 
                     , "\nprolific authors" ))
dev.off()

