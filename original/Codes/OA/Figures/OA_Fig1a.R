##### Housekeeping #####

rm(list=ls())
library('foreign')
library('ggplot2')
library('readstata13')

# Please set path 
# setwd("/Users/username/Desktop/ReplicationFiles/Codes/OA/Figures")

##### Output #####

outputfile.asinh = '../../../Output/OA/Figures/OA_Fig1a.pdf' # Set the output file path/name that you desire here.

# Read in cleaned dataset where unit of observation is a paper
pooled = read.dta13('../../../Data/Main/Pooled_cleaned.dta')

# Set some plotting parameters
my.cex.main = 1.8
my.cex.lab = 1.5
my.cex.axis = 1.5
my.cex.sub = 1.2

### Create plotting data for histograms and normalize bar heights so they sum to 1 ###
hist.asinhcites = hist(x=asinh(pooled$GScites), breaks = seq(asinh(0), asinh(500), length=15))
hist.asinhcites$density = hist.asinhcites$density/sum(hist.asinhcites$density)


################################# Save Plots ###########################################
landscape.a4.width = 11.69
landscape.a4.height = 8.27
scale = 1
pdf.scale = 1

### Distribution of Asinh(Citations)
png(filename = outputfile.asinh,
    height = landscape.a4.height*scale, width = landscape.a4.width*scale,
    units = "in", res = 300)
plot(hist.asinhcites, freq=FALSE,
     main = "", xlab = "Asinh(Citations)",
     cex.main = my.cex.main, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
dev.off()
pdf(gsub("png", "pdf", outputfile.asinh), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
plot(hist.asinhcites, freq=FALSE,
     main = "", xlab = "Asinh(Citations)",
     cex.main = my.cex.main, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
dev.off()
