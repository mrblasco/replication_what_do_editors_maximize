##### Housekeeping #####

rm(list=ls())
library('foreign')
library('ggplot2')
library('sandwich')
library('lmtest')
library('readstata13')

# Please set path 
# setwd("/Users/username/Desktop/ReplicationFiles/Codes/OA/Figures")

##### Output #####

#citepct is the figure we need for the Online-Appendix of the Publication
outputfile.citepct = '../../../Output/OA/Figures/OA_Fig2a.pdf'
outputfile.topcited = '../../../Output/OA/Figures/OA_Fig2b.pdf' 
outputfile.wos = '../../../Output/OA/Figures/OA_Fig2c.pdf' 

# Read in cleaned dataset where the unit of observation is a referee report
pooled = read.dta13('../../../Data/Secondary/Pooled2_PaperReferee.dta')
pooled = pooled[pooled$NRef!=1,] # We discard papers with 1 referee assigned in our benchmark analysis
rownames(pooled) = paste0(pooled$new_pid, "-", pooled$RefNum) # Define unique rownames for the function that computes clustered standard errors

### Weight by inverse of number of referee reports
pooled$w = 1/pooled$NRefRespond

### Variables
reports = c("DefReject","Reject","NoRec","WeakRR","RR","StrongRR","Accept")

qje = pooled[pooled$Journal == "QJE",]
restud = pooled[pooled$Journal == "REStud",]
restat = pooled[pooled$Journal == "REStat",]
jeea = pooled[pooled$Journal == "JEEA",]

### Function for Clustering Standard Errors
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

### Function that takes as input 4 regressions (one for each Journal) and creates the corresponding plot
plot.cites = function(reg1, reg2, reg3, reg4, title.size = 30, axis.title.size = 24,
                      legend.size = 21, axis.size = 20, bar.width = 0.2,
                      my.shapes = c(19,17,15,18), my.linetype = c(1,2,3,4),
                      my.colors = c(colors()[38],colors()[81],colors()[33],colors()[107]),
                      symbol.size = 5, my.legend.width = 4){
  se1 = cl(reg1, cluster=sub("-.*", "", rownames(reg1$model)))[,2]
  se2 = cl(reg2, cluster=sub("-.*", "", rownames(reg2$model)))[,2]
  se3 = cl(reg3, cluster=sub("-.*", "", rownames(reg3$model)))[,2]
  se4 = cl(reg4, cluster=sub("-.*", "", rownames(reg4$model)))[,2]
  
  plot.df = data.frame(x = 1:length(reports),
                       y = c(coef(reg1)[reports], coef(reg2)[reports],
                             coef(reg3)[reports], coef(reg4)[reports]),
                       se = c(se1, se2, se3, se4),
                       type = legend.text)
  limits = aes(ymax = y + 2*se, ymin = y - 2*se)
  my.plot = ggplot(data = plot.df, aes(x = x, y = y, shape = type, linetype = type, color = type)) +
    geom_point(size = symbol.size) + geom_line(size=1) +
    geom_errorbar(limits, width = bar.width, linetype = 1, show.legend = FALSE) +
    xlab("Referee Reports") +
    scale_linetype_manual(values = my.linetype) + scale_shape_manual(values = my.shapes) +
    scale_color_manual(values = my.colors) + scale_x_discrete(limits = reports, expand = c(0.05,0.1)) +
    theme_classic() + theme(legend.title=element_blank()) +
    theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
    theme(plot.title = element_text(size=title.size)) +
    theme(axis.title = element_text(size=axis.title.size)) + 
    theme(axis.text.x = element_text(size=axis.size)) +
    theme(axis.text.y = element_text(size=axis.size)) +
    theme(legend.text = element_text(size=legend.size)) +
    theme(legend.key.width = unit(my.legend.width, "cm")) +
    theme(plot.title = element_text(hjust = 0.5))
}

reports = factor(c("DefReject","Reject","NoRec","WeakRR","RR","StrongRR","Accept"),
                 levels = c("DefReject","Reject","NoRec","WeakRR","RR","StrongRR","Accept"))
legend.text = factor(rep(c("QJE","REStud","REStat","JEEA"),
                         each = length(reports)),
                     levels = c("QJE","REStud","REStat","JEEA"))
my.legend.position = c(0.8,0.2)



############################# Citation Percentiles ##############################
reg.vars = c("GScites_pct", "DefReject","Reject","NoRec","WeakRR","RR","StrongRR","Accept")

########## Regressions ########

### QJE ###
w = qje$w
qje.reg = lm(GScites_pct ~ . -1, data = qje[,reg.vars], weights = w)

### REStud ###
w = restud$w
restud.reg = lm(GScites_pct ~ . -1, data = restud[,reg.vars], weights = w)

### REStat ###
w = restat$w
restat.reg = lm(GScites_pct ~ . -1, data = restat[,reg.vars], weights = w)

### JEEA ###
w = jeea$w
jeea.reg = lm(GScites_pct ~ . -1, data = jeea[,reg.vars], weights = w)

##### Create Plot
citepct.plot = plot.cites(qje.reg, restud.reg, restat.reg, jeea.reg) +
  ggtitle("") +
  ylab("Citation Percentile") +
  theme(legend.position = my.legend.position) + 
  theme(legend.background = element_rect(linetype = "solid", color = "black"))

### Weight by inverse of number of referee reports
pooled$w = 1/pooled$NRefRespond

### Variables
reports = c("DefReject","Reject","NoRec","WeakRR","RR","StrongRR","Accept")

### Create seperate data frames for each Journal
qje = pooled[pooled$Journal == "QJE",]
restud = pooled[pooled$Journal == "REStud",]
restat = pooled[pooled$Journal == "REStat",]
jeea = pooled[pooled$Journal == "JEEA",]

### Function for Clustering Standard Errors
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

### Function for plotting the two figures
plot.cites = function(reg1, reg2, reg3, reg4, title.size = 30, axis.title.size = 24,
                      legend.size = 21, axis.size = 20, bar.width = 0.2,
                      my.shapes = c(19,17,15,18), my.linetype = c(1,2,3,4),
                      my.colors = c(colors()[38],colors()[81],colors()[33],colors()[107]),
                      symbol.size = 5, my.legend.width = 4){
  se1 = cl(reg1, cluster=sub("-.*", "", rownames(reg1$model)))[,2]
  se2 = cl(reg2, cluster=sub("-.*", "", rownames(reg2$model)))[,2]
  se3 = cl(reg3, cluster=sub("-.*", "", rownames(reg3$model)))[,2]
  se4 = cl(reg4, cluster=sub("-.*", "", rownames(reg4$model)))[,2]
  
  plot.df = data.frame(x = 1:length(reports),
                       y = c(coef(reg1)[reports], coef(reg2)[reports],
                             coef(reg3)[reports], coef(reg4)[reports]),
                       se = c(se1, se2, se3, se4),
                       type = legend.text)
  limits = aes(ymax = y + 2*se, ymin = y - 2*se)
  my.plot = ggplot(data = plot.df, aes(x = x, y = y, shape = type, linetype = type, color = type)) +
    geom_point(size = symbol.size) + geom_line(size=1) +
    geom_errorbar(limits, width = bar.width, linetype = 1, show.legend = FALSE) +
    xlab("Referee Reports") +
    scale_linetype_manual(values = my.linetype) + scale_shape_manual(values = my.shapes) +
    scale_color_manual(values = my.colors) + scale_x_discrete(limits = reports, expand = c(0.05,0.1)) +
    theme_classic() + theme(legend.title=element_blank()) +
    theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
    theme(plot.title = element_text(size=title.size)) +
    theme(axis.title = element_text(size=axis.title.size)) + 
    theme(axis.text.x = element_text(size=axis.size)) +
    theme(axis.text.y = element_text(size=axis.size)) +
    theme(legend.text = element_text(size=legend.size)) +
    theme(legend.key.width = unit(my.legend.width, "cm")) +
    theme(plot.title = element_text(hjust = 0.5))
}

### Set the x-axis and legend of the plots
reports = factor(c("DefReject","Reject","NoRec","WeakRR","RR","StrongRR","Accept"),
                 levels = c("DefReject","Reject","NoRec","WeakRR","RR","StrongRR","Accept"))
legend.text = factor(rep(c("QJE","REStud","REStat","JEEA"),
                         each = length(reports)),
                     levels = c("QJE","REStud","REStat","JEEA"))
my.legend.position = c(0.8,0.2) # Set the position of the legend in the plot


########## Regressions with "top-cited" as the dependent variable ########
reg.vars = c("Gscites_TopCoded", "DefReject","Reject","NoRec","WeakRR","RR","StrongRR","Accept")

### QJE ###
w = qje$w
qje.reg = lm(Gscites_TopCoded ~ . -1, data = qje[,reg.vars], weights = w)

### REStud ###
w = restud$w
restud.reg = lm(Gscites_TopCoded ~ . -1, data = restud[,reg.vars], weights = w)

### REStat ###
w = restat$w
restat.reg = lm(Gscites_TopCoded ~ . -1, data = restat[,reg.vars], weights = w)

### JEEA ###
w = jeea$w
jeea.reg = lm(Gscites_TopCoded ~ . -1, data = jeea[,reg.vars], weights = w)

##### Create online appendix figure 3a
topcited.plot = plot.cites(qje.reg, restud.reg, restat.reg, jeea.reg) +
  ylab("Fraction Top-Cited") +
  theme(legend.position = my.legend.position) + 
  theme(legend.background = element_rect(linetype = "solid", color = "black"))

########## Regressions with Asinh(SSCI citations) as the dependent variable ########
reg.vars = c("asinh_wos", "DefReject","Reject","NoRec","WeakRR","RR","StrongRR","Accept")

### QJE ###
w = qje$w[qje$Year>=2006 & qje$Year<=2010]
qje.reg = lm(asinh_wos ~ . -1, data = qje[qje$Year>=2006 & qje$Year<=2010,reg.vars], weights = w)

### REStud ###
w = restud$w[restud$Year>=2006 & restud$Year<=2010]
restud.reg = lm(asinh_wos ~ . -1, data = restud[restud$Year>=2006 & restud$Year<=2010,reg.vars], weights = w)

### REStat ###
w = restat$w[restat$Year>=2006 & restat$Year<=2010]
restat.reg = lm(asinh_wos ~ . -1, data = restat[restat$Year>=2006 & restat$Year<=2010,reg.vars], weights = w)

### JEEA ###
w = jeea$w[jeea$Year>=2006 & jeea$Year<=2010]
jeea.reg = lm(asinh_wos ~ . -1, data = jeea[jeea$Year>=2006 & jeea$Year<=2010,reg.vars], weights = w)

##### Create online appendix figure 3b
wos.plot = plot.cites(qje.reg, restud.reg, restat.reg, jeea.reg) +
  ylab("Asinh(SSCI Citations)") +
  theme(legend.position = my.legend.position) + 
  theme(legend.background = element_rect(linetype = "solid", color = "black"))

################################## Save Plots ###################################
landscape.a4.width = 11.69
landscape.a4.height = 8.27
png.scale = 0.85
pdf.scale = 0.85


png(filename = outputfile.citepct,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
print(citepct.plot)
dev.off()
pdf(gsub("png", "pdf", outputfile.citepct), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
print(citepct.plot)
dev.off()


png(filename = outputfile.topcited,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
print(topcited.plot)
dev.off()
pdf(gsub("png", "pdf", outputfile.topcited), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
print(topcited.plot)
dev.off()


png(filename = outputfile.wos,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
print(wos.plot)
dev.off()
pdf(gsub("png", "pdf", outputfile.wos), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
print(wos.plot)
dev.off()
