##### Housekeeping #####

rm(list=ls())
library('foreign')
library('grid')
library('gridExtra')
library('splines')
library('rstudioapi')
library('readstata13')

# Please set path
# setwd("/Users/username/Desktop/ReplicationFiles/Codes/Paper/Figures")

# Print-version black and white (bw=1: black and white)
bw = 1

##### Output #####
outputfile = '../../../Output/Paper/Figures/Fig6a.pdf'

# Read in cleaned dataset where unit of observation is a paper
pooled = read.dta13('../../../Data/Main/Pooled_cleaned.dta')

# g and h are correction terms for selection that appear in the conditional expectation expression (under the normality assumption)
g = function(p){dnorm(qnorm(p))/p}
h = function(p){dnorm(qnorm(p))/(1-p)}

### Variables
author_pubs = c('pub1', 'pub2', 'pub3', 'pub45', 'pub6')
num_authors = c('auth2', 'auth3', 'auth4')
fields = c('fr_lab', 'fr_labor', 'fr_healthurblaw', 'fr_dev', 'fr_hist', 'fr_pub',
           'fr_io', 'fr_fin', 'fr_macro', 'missingfield', 'fr_micro', 'fr_other',
           'fr_theory', 'fr_metrics')

stage1.features = c(author_pubs, num_authors, fields, "yearsubmit", "journal")

### Function for Estimating Editor Signal Model for Desk-Rejection Stage
ndr.est = function(journal){
  single.journal = ifelse( ( length(unique(journal$journal))==1 ), TRUE, FALSE)
  
  if(single.journal){
    journal.ols = lm(asinh_cites ~ . + as.factor(yearsubmit) - yearsubmit,
                     data = journal[,c("asinh_cites", "notdeskrej", stage1.features[which(stage1.features!="journal")])])
  }else{
    journal.ols = lm(asinh_cites ~ . + as.factor(yearsubmit)*as.factor(journal) - yearsubmit - journal,
                     data = journal[,c("asinh_cites", "notdeskrej", stage1.features)])
  }
  est.sd.phi = summary(journal.ols)$sigma
  
  if(single.journal){
    journal.probit = glm(notdeskrej ~ . + as.factor(yearsubmit) - yearsubmit,
                         data = journal[,c("notdeskrej", "neditor_ndr_loo2", stage1.features[which(stage1.features!="journal")])],
                         family = binomial(link = "probit"))
  }else{
    journal.probit = glm(notdeskrej ~ . + as.factor(yearsubmit)*as.factor(journal) - yearsubmit - journal,
                         data = journal[,c("notdeskrej", "neditor_ndr_loo2", stage1.features)],
                         family = binomial(link = "probit"))
  }
  est.p = predict(journal.probit, journal, type = "response")
  
  journal$rho1 = est.sd.phi*( -(1-journal$notdeskrej) * h(est.p) +
                                journal$notdeskrej * g(est.p))
  if(single.journal){
    est.ols = lm(asinh_cites ~ . + as.factor(yearsubmit) - yearsubmit,
                 data = journal[,c("asinh_cites", "rho1", "notdeskrej", stage1.features[which(stage1.features!="journal")])])
  }else{
    est.ols = lm(asinh_cites ~ . + as.factor(yearsubmit)*as.factor(journal) - yearsubmit - journal,
                 data = journal[,c("asinh_cites", "rho1", "notdeskrej", stage1.features)])
  }
  sim.pred.cites = predict(est.ols, journal)
  return(list(Model = est.ols, PredCites = sim.pred.cites, p = est.p, ols.sigma = est.sd.phi))
}

### Function that uses model predictions and actual citations, and summarizes the data
### according to quantiles of probability of NDR (as used in the plots)
sim_quantiles = function(journal, x.probs, model.gs,
                         breakpoints = c(seq(0,0.9,by=0.1),0.95,1), min.obs=10){
  nbins = length(breakpoints)-1
  quantiles = quantile(x.probs, probs=breakpoints, na.rm=TRUE)
  prob.bins = findInterval(x.probs, quantiles, rightmost.closed = TRUE)
  # Plot matrix
  model_gs = matrix(NA, nrow = nbins, ncol = 4)
  avg_gs = matrix(NA, nrow = nbins, ncol = 4)
  num_obs = matrix(NA, nrow = nbins, ncol = 4)
  avg_prob = matrix(NA, nrow = nbins, ncol = 4)
  avg_prob_vec = rep(NA, nbins)
  for (i in 1:nbins){
    model_gs[i,] = c(mean(model.gs[journal$notdeskrej==0 & prob.bins == i]),
                     mean(model.gs[journal$notdeskrej==1 & prob.bins == i]),
                     mean(model.gs[journal$notdeskrej==1 & journal$rr==0 & prob.bins == i]),
                     mean(model.gs[journal$notdeskrej==1 & journal$rr==1 & prob.bins == i]))
    avg_gs[i,] = c(mean(journal$asinh_cites[journal$notdeskrej==0 & prob.bins == i]),
                   mean(journal$asinh_cites[journal$notdeskrej==1 & prob.bins == i]),
                   mean(journal$asinh_cites[journal$notdeskrej==1 & journal$rr==0 & prob.bins == i]),
                   mean(journal$asinh_cites[journal$notdeskrej==1 & journal$rr==1 & prob.bins == i]))
    num_obs[i,] = c(sum(journal$notdeskrej==0 & prob.bins == i),
                    sum(journal$notdeskrej==1 & prob.bins == i),
                    sum(journal$notdeskrej==1 & journal$rr==0 & prob.bins == i),
                    sum(journal$notdeskrej==1 & journal$rr==1 & prob.bins == i))
    avg_prob[i,] = rep(mean(x.probs[which(prob.bins==i)]), 4)
    avg_prob_vec[i] = mean(x.probs[which(prob.bins==i)])
  }
  for (j in 1:4){
    avg_prob[,j] = perc.rank(avg_prob[,j])
  }
  model_gs[which(num_obs<min.obs)] = NA
  avg_gs[which(num_obs<min.obs)] = NA
  avg_prob[which(num_obs<min.obs)] = NA
  bin.df = data.frame(Model.dr = model_gs[,1], Model.ndr = model_gs[,2],
                      Model.ndr.rej = model_gs[,3], Model.ndr.rr = model_gs[,4],
                      Actual.dr = avg_gs[,1], Actual.ndr = avg_gs[,2],
                      Actual.ndr.rej = avg_gs[,3], Actual.ndr.rr = avg_gs[,4],
                      Nobs.dr = num_obs[,1], Nobs.ndr = num_obs[,2],
                      Nobs.ndr.rej = num_obs[,3], Nobs.ndr.rr = num_obs[,4],
                      Probs.dr = avg_prob[,1], Probs.ndr = avg_prob[,2],
                      Probs.ndr.rej = avg_prob[,3], Probs.ndr.rr = avg_prob[,4],
                      AvgProbs = avg_prob_vec, Percentiles = perc.rank(avg_prob_vec))
  return(bin.df)
}

### Estimate the (Unrestricted) Editor Signal Model ###
pooled.unrestricted.est = ndr.est(pooled)
pooled.unrestricted.preds = pooled.unrestricted.est$PredCites
pooled.unrestricted.rho1 = round(coef(pooled.unrestricted.est$Model)["rho1"],3)
probit.probs = pooled.unrestricted.est$p

perc.rank = function(x) trunc(rank(x))/length(x) # Function that creates percentiles

# Indices of papers that were desk-rejected/not desk-rejected/received R&R later
dr.indices = which(pooled$notdeskrej==0)
ndr.indices = which(pooled$notdeskrej==1)
ndr.rej.indices = which(pooled$notdeskrej==1 & pooled$rr==0)
ndr.rr.indices = which(pooled$notdeskrej==1 & pooled$rr==1)

# "unrestricted.y" is the variable that the smoothing lines in this figure approximates.
# In this version of the plot, "unrestricted.y" is taken to be equal to the actual citations.
# In other versions, "unrestricted.y" is sometimes model predictions.
df = data.frame(actual.y = pooled$asinh_cites,
                unrestricted.y = pooled$asinh_cites,
                probs = perc.rank(probit.probs))

# Summarize average model predictions and actual data by probit quantiles
bins.df = sim_quantiles(journal = pooled, x.probs = probit.probs, model.gs = pooled.unrestricted.preds)

# Create data frames used to fit smoothed lines
pred.df.dr = data.frame(probs = seq(from = min(bins.df$Probs.dr, na.rm=TRUE),
                                    to = max(bins.df$Probs.dr, na.rm=TRUE), length = 100))
pred.df.ndr = data.frame(probs = seq(from = min(bins.df$Probs.ndr, na.rm=TRUE),
                                     to = max(bins.df$Probs.ndr, na.rm=TRUE), length = 100))
pred.df.ndr.rej = data.frame(probs = seq(from = min(bins.df$Probs.ndr.rej, na.rm=TRUE),
                                         to = max(bins.df$Probs.ndr.rej, na.rm=TRUE), length = 100))
pred.df.ndr.rr = data.frame(probs = seq(from = min(bins.df$Probs.ndr.rr, na.rm=TRUE),
                                        to = max(bins.df$Probs.ndr.rr, na.rm=TRUE), length = 100))

##################### Global Cubic Smoothing ######################
unres.cubic.fit.dr = lm( unrestricted.y ~ poly(probs, 3), data = df, subset = dr.indices)
unres.cubic.fit.ndr = lm( unrestricted.y ~ poly(probs, 3), data = df, subset = ndr.indices)
unres.cubic.fit.ndr.rej = lm( unrestricted.y ~ poly(probs, 3), data = df, subset = ndr.rej.indices)
unres.cubic.fit.ndr.rr = lm( unrestricted.y ~ poly(probs, 3), data = df, subset = ndr.rr.indices)

unres.cubic.pred.dr = predict(unres.cubic.fit.dr, pred.df.dr)
unres.cubic.pred.ndr = predict(unres.cubic.fit.ndr, pred.df.ndr)
unres.cubic.pred.ndr.rej = predict(unres.cubic.fit.ndr.rej, pred.df.ndr.rej)
unres.cubic.pred.ndr.rr = predict(unres.cubic.fit.ndr.rr, pred.df.ndr.rr)

############################### Create Plot #######################################
unres.rho.sub = bquote( hat(rho)[0] ==  .(pooled.unrestricted.rho1) )

xmat = matrix( c(pred.df.dr$probs, pred.df.ndr$probs, pred.df.ndr.rej$probs, pred.df.ndr.rr$probs), ncol = 4 )

my.legend = c("Desk-Rejected", "Not Desk-Rejected", "Not Desk-Rejected, then Rejected", "Not Desk-Rejected, then R&R")
my.xlab = "Average Predicted Probability of Avoiding Desk-Rejection"
my.ylab = "Asinh(Citations)"
my.axis.cex = 1.3
my.xvals = ifelse(bins.df$AvgProbs>=0.1, signif(bins.df$AvgProbs,2), signif(bins.df$AvgProbs,1) )

if (bw==1) {
  my.colors = c(colors()[175],colors()[211],colors()[350],colors()[195])
  outputfile = "../../../Output/Paper/Figures/Fig6a_bw.pdf"
} else {
  my.colors = c("red","blue", "darkgreen", "orange")
}

my.cex = 1.3
my.ylim = c(0.7,4.5)
my.xlim = c(0,1)
my.lty = 1
my.pch = c(19,18,17,15)
my.cex.main = 1.6
my.cex.lab = 1.4
my.cex.sub = 1.3
my.cex.legend = 1.3

my.par.mar = c(4, 4, 2, 2) + 0.1

### PNG Plot
landscape.a4.width = 11.69
landscape.a4.height = 8.27
png.scale = 0.85

png(filename = outputfile,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)

par(mar=my.par.mar)
matplot( x = xmat,
         y = matrix( c(unres.cubic.pred.dr, unres.cubic.pred.ndr,
                       unres.cubic.pred.ndr.rej, unres.cubic.pred.ndr.rr), ncol = 4), ann = F,
         type = "l", col = my.colors, lty = my.lty, xaxt = "n",
         xlim = my.xlim, ylim = my.ylim)
mtext(text = my.xlab, side = 1, line = 2.5, cex = my.cex.lab)
mtext(text = my.ylab, side = 2, line = 2.5, cex = my.cex.lab)
# mtext(text = unres.rho.sub, side = 1, line = 4, cex = my.cex.sub)
# legend("bottomright", col = my.colors, lty = my.lty, pch = my.pch, cex = my.cex.legend,
#        legend = my.legend)
points(x = bins.df$Probs.dr, y = bins.df$Actual.dr, pch = my.pch[1], cex = my.cex, col = my.colors[1])
points(x = bins.df$Probs.ndr, y = bins.df$Actual.ndr, pch = my.pch[2], cex = my.cex, col = my.colors[2])
points(x = bins.df$Probs.ndr.rej, y = bins.df$Actual.ndr.rej, pch = my.pch[3], cex = my.cex, col = my.colors[3])
points(x = bins.df$Probs.ndr.rej, y = bins.df$Actual.ndr.rr, pch = my.pch[4], cex = my.cex, col = my.colors[4])
axis(side = 1, at = bins.df$Percentiles, labels = my.xvals,
     cex.axis = my.axis.cex)

arrows(x0=0.9, x1=0.78, y0=1, y1=1.7, code=2, length=0.15, col="black")
text(x=0.9, y=1, labels="Desk-Rejected", cex=my.cex.legend, pos=1)
arrows(x0=0.5, x1=0.6, y0=1, y1=2.2, code=2, length=0.15, col="black")
text(x=0.5, y=1, labels="Not Desk-Rejected, but Rejected", cex=my.cex.legend, pos=1)
arrows(x0=0.7, x1=0.7, y0=3, y1=2.44, code=2, length=0.15, col="black")
text(x=0.7, y=3, labels="Not Desk-Rejected", cex=my.cex.legend, pos=3)
arrows(x0=0.2, x1=0.25, y0=4, y1=3.5, code=2, length=0.15, col="black")
text(x=0.2, y=4, labels="R&R", cex=my.cex.legend, pos=3)

dev.off()

### PDF Plot
pdf.scale = 0.85

pdf(gsub("png", "pdf", outputfile), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)

par(mar=my.par.mar)
matplot( x = xmat,
         y = matrix( c(unres.cubic.pred.dr, unres.cubic.pred.ndr,
                       unres.cubic.pred.ndr.rej, unres.cubic.pred.ndr.rr), ncol = 4), ann = F,
         type = "l", col = my.colors, lty = my.lty, xaxt = "n",
         xlim = my.xlim, ylim = my.ylim)
mtext(text = my.xlab, side = 1, line = 2.5, cex = my.cex.lab)
mtext(text = my.ylab, side = 2, line = 2.5, cex = my.cex.lab)
# mtext(text = unres.rho.sub, side = 1, line = 4, cex = my.cex.sub)
# legend("bottomright", col = my.colors, lty = my.lty, pch = my.pch, cex = my.cex.legend,
#        legend = my.legend)
points(x = bins.df$Probs.dr, y = bins.df$Actual.dr, pch = my.pch[1], cex = my.cex, col = my.colors[1])
points(x = bins.df$Probs.ndr, y = bins.df$Actual.ndr, pch = my.pch[2], cex = my.cex, col = my.colors[2])
points(x = bins.df$Probs.ndr.rej, y = bins.df$Actual.ndr.rej, pch = my.pch[3], cex = my.cex, col = my.colors[3])
points(x = bins.df$Probs.ndr.rej, y = bins.df$Actual.ndr.rr, pch = my.pch[4], cex = my.cex, col = my.colors[4])
axis(side = 1, at = bins.df$Percentiles, labels = my.xvals,
     cex.axis = my.axis.cex)

arrows(x0=0.9, x1=0.78, y0=1, y1=1.7, code=2, length=0.15, col="black")
text(x=0.9, y=1, labels="Desk-Rejected", cex=my.cex.legend, pos=1)
arrows(x0=0.5, x1=0.6, y0=1, y1=2.2, code=2, length=0.15, col="black")
text(x=0.5, y=1, labels="Not Desk-Rejected, but Rejected", cex=my.cex.legend, pos=1)
arrows(x0=0.7, x1=0.7, y0=3, y1=2.44, code=2, length=0.15, col="black")
text(x=0.7, y=3, labels="Not Desk-Rejected", cex=my.cex.legend, pos=3)
arrows(x0=0.2, x1=0.25, y0=4, y1=3.5, code=2, length=0.15, col="black")
text(x=0.2, y=4, labels="R&R", cex=my.cex.legend, pos=3)

dev.off()

print(coef(pooled.unrestricted.est$Model)["rho1"])
print(pooled.unrestricted.est$ols.sigma)