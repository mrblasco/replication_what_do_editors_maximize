##### Housekeeping #####

rm(list=ls())
library('diagram')
library('foreign')
library('grid')
library('gridExtra')
library('splines')
library('readstata13')

# Please set path 
# setwd("/Users/username/Desktop/ReplicationFiles/Codes/OA/Figures")

##### Output #####
outputfile = '../../../Output/OA/Figures/OA_Fig6.pdf'

# Read in cleaned dataset where unit of observation is a paper
pooled = read.dta13('../../../Data/Main/Pooled_cleaned.dta')
# Restrict the dataset to papers that were not desk-rejected and have more than 1 referee assigned
pooled = pooled[pooled$notdeskrej==1,]
pooled = pooled[pooled$noref!=1,]

# g and h are correction terms for selection that appear in the conditional expectation expression (under the normality assumption)
g = function(p){dnorm(qnorm(p))/p}
h = function(p){dnorm(qnorm(p))/(1-p)}

### Variables
reports = c('frReject', 'frNoRec', 'frWeakRR', 'frRR', 'frStrongRR', 'frAccept')
journals = c('qje','restat','jeea')
author_pubs = c('pub1', 'pub2', 'pub3', 'pub45', 'pub6')
num_authors = c('auth2', 'auth3', 'auth4')
fields = c('fr_lab', 'fr_labor', 'fr_healthurblaw', 'fr_dev', 'fr_hist', 'fr_pub',
           'fr_io', 'fr_fin', 'fr_macro', 'missingfield', 'fr_micro', 'fr_other',
           'fr_theory', 'fr_metrics')

stage1.features = c(author_pubs, num_authors, fields, "yearsubmit", "journal")
stage2.features = c(stage1.features, reports)

### Function for Estimating Editor Signal Model for R&R Stage
rr.est = function(journal){
  journal = journal[journal$notdeskrej==1,]
  journal = journal[journal$noref!=1,]
  single.journal = ifelse( ( length(unique(journal$journal))==1 ), TRUE, FALSE)
  
  if(single.journal){
    journal.ols = lm(asinh_cites ~ . + as.factor(yearsubmit) - yearsubmit,
                     data = journal[,c("asinh_cites", "rr", stage2.features[which(stage2.features!="journal")])])
  }else{
    journal.ols = lm(asinh_cites ~ . + as.factor(yearsubmit)*as.factor(journal) - yearsubmit - journal,
                     data = journal[,c("asinh_cites", "rr", stage2.features)])
  }
  est.sd.phi = summary(journal.ols)$sigma
  
  if(single.journal){
    journal.probit = glm(rr ~ . + as.factor(yearsubmit) - yearsubmit,
                         data = journal[,c("rr", "neditor_rr_loo2", stage2.features[which(stage2.features!="journal")])],
                         family = binomial(link = "probit"))
  }else{
    journal.probit = glm(rr ~ . + as.factor(yearsubmit)*as.factor(journal) - yearsubmit - journal,
                         data = journal[,c("rr", "neditor_rr_loo2", stage2.features)],
                         family = binomial(link = "probit"))
  }
  est.p = predict(journal.probit, journal, type = "response")
  
  journal$rho2 = est.sd.phi*( -(1-journal$rr) * h(est.p) +
                                journal$rr * g(est.p))
  if(single.journal){
    est.ols = lm(asinh_cites ~ . + as.factor(yearsubmit) - yearsubmit,
                 data = journal[,c("asinh_cites", "rho2", "rr", stage2.features[which(stage2.features!="journal")])])
  }else{
    est.ols = lm(asinh_cites ~ . + as.factor(yearsubmit)*as.factor(journal) - yearsubmit - journal,
                 data = journal[,c("asinh_cites", "rho2", "rr", stage2.features)])
  }
  sim.pred.cites = predict(est.ols, journal)
  return(list(Model = est.ols, PredCites = sim.pred.cites, p = est.p, ols.sigma = est.sd.phi))
}

### Function that uses model predictions and actual citations, and summarizes the data
### according to quantiles of probability of R&R (as used in the plots)
sim_quantiles = function(journal, x.probs, model.gs,
                         breakpoints = c(seq(0,0.9,by=0.1),0.95,1), min.obs=10){
  nbins = length(breakpoints)-1
  quantiles = quantile(x.probs, probs=breakpoints, na.rm=TRUE)
  prob.bins = findInterval(x.probs, quantiles, rightmost.closed = TRUE)
  # Plot matrix
  model_gs = matrix(NA, nrow = nbins, ncol = 2)
  avg_gs = matrix(NA, nrow = nbins, ncol = 2)
  num_obs = matrix(NA, nrow = nbins, ncol = 2)
  avg_prob = matrix(NA, nrow = nbins, ncol = 2)
  avg_prob_vec = rep(NA, nbins)
  for (i in 1:nbins){
    model_gs[i,] = c(mean(model.gs[journal$rr==0 & prob.bins == i]),
                     mean(model.gs[journal$rr==1 & prob.bins == i]))
    avg_gs[i,] = c(mean(journal$asinh_cites[journal$rr==0 & prob.bins == i]),
                   mean(journal$asinh_cites[journal$rr==1 & prob.bins == i]))
    num_obs[i,] = c(sum(journal$rr==0 & prob.bins == i),
                    sum(journal$rr==1 & prob.bins == i))
    avg_prob[i,] = rep(mean(x.probs[which(prob.bins==i)]), 2)
    avg_prob_vec[i] = mean(x.probs[which(prob.bins==i)])
  }
  for (j in 1:2){
    avg_prob[,j] = perc.rank(avg_prob[,j])
  }
  model_gs[which(num_obs<min.obs)] = NA
  avg_gs[which(num_obs<min.obs)] = NA
  avg_prob[which(num_obs<min.obs)] = NA
  bin.df = data.frame(Model.rej = model_gs[,1], Model.rr = model_gs[,2],
                      Actual.rej = avg_gs[,1], Actual.rr = avg_gs[,2],
                      Nobs.rej = num_obs[,1], Nobs.rr = num_obs[,2],
                      Probs.rej = avg_prob[,1], Probs.rr = avg_prob[,2],
                      AvgProbs = avg_prob_vec, Percentiles = perc.rank(avg_prob_vec))
  return(bin.df)
}


### Estimate the (Unrestricted) Editor Signal Model ###
pooled.unrestricted.est = rr.est(pooled)
pooled.unrestricted.preds = pooled.unrestricted.est$PredCites
pooled.unrestricted.rho2 = round(coef(pooled.unrestricted.est$Model)["rho2"],3)
probit.probs = pooled.unrestricted.est$p

perc.rank = function(x) trunc(rank(x))/length(x) # Function that creates percentiles

# Indices of papers that were rejected/received R&R's
rej.indices = which(pooled$rr==0)
rr.indices = which(pooled$rr==1)

# "unrestricted.y" is the variable that the smoothing lines in this figure approximates.
# In this version of the plot, "unrestricted.y" is taken to be equal to the model predictions.
df = data.frame(actual.y = pooled$asinh_cites,
                unrestricted.y = pooled.unrestricted.preds,
                probs = perc.rank(probit.probs))

# Summarize average model predictions and actual data by probit quantiles
bins.df = sim_quantiles(journal = pooled, x.probs = probit.probs, model.gs = pooled.unrestricted.preds)

# Create data frames used to fit smoothing lines
pred.df.rej = data.frame(probs = seq(from = min(bins.df$Probs.rej, na.rm=TRUE),
                                     to = max(bins.df$Probs.rej, na.rm=TRUE), length = 100))
pred.df.rr = data.frame(probs = seq(from = min(bins.df$Probs.rr, na.rm=TRUE),
                                    to = max(bins.df$Probs.rr, na.rm=TRUE), length = 100))

##################### Global Cubic Smoothing ######################
unres.cubic.fit.rej = lm( unrestricted.y ~ poly(probs, 3), data = df, subset = rej.indices)
unres.cubic.fit.rr = lm( unrestricted.y ~ poly(probs, 3), data = df, subset = rr.indices)

unres.cubic.pred.rej = predict(unres.cubic.fit.rej, pred.df.rej)
unres.cubic.pred.rr = predict(unres.cubic.fit.rr, pred.df.rr)

############################### Create Plot #######################################
unres.rho.sub = bquote( hat(rho) ==  .(pooled.unrestricted.rho2) )

xmat = matrix( c(pred.df.rej$probs, pred.df.rr$probs), ncol = 2 )

my.legend = c("Rejected", "R&R")
my.xlab = "Average Predicted Probability of R&R"
my.ylab = "Asinh(Citations)"
my.axis.cex = 1.3
my.xvals = ifelse(bins.df$AvgProbs>=0.1, signif(bins.df$AvgProbs,2), signif(bins.df$AvgProbs,1) )

my.colors = c("red","blue")
my.cex = 1.3
my.ylim = c(1.5,4.5)
my.xlim = c(0,1)
my.lty = 1
my.pch = c(19,17)
my.cex.main = 1.6
my.cex.lab = 1.4
my.cex.sub = 1.3
my.cex.legend = 1.3

my.par.mar = c(4, 4, 2, 2) + 0.1

### PNG Plot
landscape.a4.width = 11.69
landscape.a4.height = 8.27
png.scale = 0.9

png(filename = outputfile,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)

par(mar=my.par.mar)
matplot( x = xmat, y = matrix( c(unres.cubic.pred.rej, unres.cubic.pred.rr), ncol = 2), ann = F,
         type = "l", col = my.colors, lty = my.lty, xaxt = "n",
         xlim = my.xlim, ylim = my.ylim)
mtext(text = my.xlab, side = 1, line = 2.5, cex = my.cex.lab)
mtext(text = my.ylab, side = 2, line = 2.5, cex = my.cex.lab)
points(x = bins.df$Probs.rej, y = bins.df$Actual.rej, pch = my.pch[1], cex = my.cex, col = my.colors[1])
points(x = bins.df$Probs.rr, y = bins.df$Actual.rr, pch = my.pch[2], cex = my.cex, col = my.colors[2])
text(x = bins.df$Probs.rej, y = bins.df$Actual.rej + 0.1, labels = bins.df$Nobs.rej, cex = my.cex)
text(x = bins.df$Probs.rr, y = bins.df$Actual.rr + 0.1, labels = bins.df$Nobs.rr, cex = my.cex)
axis(side = 1, at = bins.df$Percentiles, labels = my.xvals,
     cex.axis = my.axis.cex)
# Add arrows and annotation on the interpretation of the gap
arrows(x0=0.87, x1=0.87, y0=3.24, y1=3.73, code=3, length=0.15, col="grey")
arrows(x0=0.6, x1=0.87, y0=4.1, y1=3.45, length=0.15, lwd=1.5)
interpretation = expression("(Vertical Gap at p=0.5) " %~~% "1.60" %*% sigma[phi] %*% rho %=>% rho %~~% 0.2)
text(x=0.6,y=4.2, labels = interpretation, cex=1.2)
arrows(x0=0.8, x1=0.8, y0=2.65, y1=3, code=2, length=0.15, col="black")
text(x=0.8, y=2.6, labels="Rejected", cex=my.cex.legend, pos=1)
arrows(x0=0.4, x1=0.5, y0=3.5, y1=3.75, code=2, length=0.15, col="black")
text(x=0.4, y=3.5, labels="R&R", cex=my.cex.legend, pos=1)

dev.off()

### PDF Plot
pdf.scale = 1

pdf(gsub("png", "pdf", outputfile), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)

par(mar=my.par.mar)
matplot( x = xmat, y = matrix( c(unres.cubic.pred.rej, unres.cubic.pred.rr), ncol = 2), ann = F,
         type = "l", col = my.colors, lty = my.lty, xaxt = "n",
         xlim = my.xlim, ylim = my.ylim)
mtext(text = my.xlab, side = 1, line = 2.5, cex = my.cex.lab)
mtext(text = my.ylab, side = 2, line = 2.5, cex = my.cex.lab)
points(x = bins.df$Probs.rej, y = bins.df$Actual.rej, pch = my.pch[1], cex = my.cex, col = my.colors[1])
points(x = bins.df$Probs.rr, y = bins.df$Actual.rr, pch = my.pch[2], cex = my.cex, col = my.colors[2])
text(x = bins.df$Probs.rej, y = bins.df$Actual.rej + 0.1, labels = bins.df$Nobs.rej, cex = my.cex)
text(x = bins.df$Probs.rr, y = bins.df$Actual.rr + 0.1, labels = bins.df$Nobs.rr, cex = my.cex)
axis(side = 1, at = bins.df$Percentiles, labels = my.xvals,
     cex.axis = my.axis.cex)
# Add arrows and annotation on the interpretation of the gap
arrows(x0=0.87, x1=0.87, y0=3.23, y1=3.74, code=3, length=0.15, col="grey")
arrows(x0=0.6, x1=0.87, y0=4.1, y1=3.45, length=0.15, lwd=1.5)
interpretation = expression("(Vertical Gap at p=0.5) " %~~% "1.60" %*% sigma[phi] %*% rho %=>% rho %~~% 0.2)
text(x=0.6,y=4.2, labels = interpretation, cex=1.2)
arrows(x0=0.8, x1=0.8, y0=2.65, y1=3, code=2, length=0.15, col="black")
text(x=0.8, y=2.6, labels="Rejected", cex=my.cex.legend, pos=1)
arrows(x0=0.4, x1=0.5, y0=3.5, y1=3.75, code=2, length=0.15, col="black")
text(x=0.4, y=3.5, labels="R&R", cex=my.cex.legend, pos=1)

dev.off()

print(coef(pooled.unrestricted.est$Model)["rho2"])
print(pooled.unrestricted.est$ols.sigma)