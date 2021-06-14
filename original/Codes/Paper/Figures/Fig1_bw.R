##### Housekeeping #####

rm(list=ls())
library('foreign')
library('ggplot2')
library('rstudioapi')
library('readstata13')

# Please set path
#setwd("/Users/username/Desktop/ReplicationFiles/Codes/Paper/Figures")

# black and white dummy (bw=1: black and white)
bw = 1

##### Output #####
outputfile1 = '../../../Output/Paper/Figures/Fig1a.pdf' 
outputfile2 = '../../../Output/Paper/Figures/Fig1b.pdf' 
outputfile3 = '../../../Output/Paper/Figures/Fig1c.pdf' 
outputfile4 = '../../../Output/Paper/Figures/Fig1d.pdf' 

#### Data ####
pooled = read.dta13('../../../Data/Main/Pooled_cleaned.dta')
qje = pooled[pooled$journal == "QJE",]
restud = pooled[pooled$journal == "REStud",]
restat = pooled[pooled$journal == "REStat",]
jeea = pooled[pooled$journal == "JEEA",]

pooled.stage2 = pooled[pooled$notdeskrej==1,]
pooled.stage2 = pooled.stage2[pooled.stage2$noref!=1,] # We discard papers with 1 referee assigned in our benchmark analysis
qje.stage2 = pooled.stage2[pooled.stage2$journal == "QJE",]
restud.stage2 = pooled.stage2[pooled.stage2$journal == "REStud",]
restat.stage2 = pooled.stage2[pooled.stage2$journal == "REStat",]
jeea.stage2 = pooled.stage2[pooled.stage2$journal == "JEEA",]

pooled.ref = read.dta13('../../../Data/Secondary/Pooled2_PaperReferee.dta') # Read in cleaned dataset where the unit of observation is a referee report
pooled.ref = pooled.ref[pooled.ref$NRef!=1,]
qje.ref = pooled.ref[pooled.ref$Journal == "QJE",]
restud.ref = pooled.ref[pooled.ref$Journal == "REStud",]
restat.ref = pooled.ref[pooled.ref$Journal == "REStat",]
jeea.ref = pooled.ref[pooled.ref$Journal == "JEEA",]

# Create matrix for barplot showing the distribution editor decisions
decision.mat = matrix(c( 1-mean(qje$notdeskrej), 1-mean(restud$notdeskrej), 1-mean(restat$notdeskrej), 1-mean(jeea$notdeskrej),
                         mean(qje$notdeskrej==1 & qje$rr==0), mean(restud$notdeskrej==1 & restud$rr==0), mean(restat$notdeskrej==1 & restat$rr==0), mean(jeea$notdeskrej==1 & jeea$rr==0),
                         mean(qje$rr), mean(restud$rr), mean(restat$rr), mean(jeea$rr)),
                      nrow = 4)

# Create matrix for barplot showing the distribution referee recommendations
reports.mat = matrix( c( mean(qje.ref$DefReject), mean(restud.ref$DefReject), mean(restat.ref$DefReject), mean(jeea.ref$DefReject),
                         mean(qje.ref$Reject), mean(restud.ref$Reject), mean(restat.ref$Reject), mean(jeea.ref$Reject),
                         mean(qje.ref$NoRec), mean(restud.ref$NoRec), mean(restat.ref$NoRec), mean(jeea.ref$NoRec),
                         mean(qje.ref$WeakRR), mean(restud.ref$WeakRR), mean(restat.ref$WeakRR), mean(jeea.ref$WeakRR),
                         mean(qje.ref$RR), mean(restud.ref$RR), mean(restat.ref$RR), mean(jeea.ref$RR),
                         mean(qje.ref$StrongRR), mean(restud.ref$StrongRR), mean(restat.ref$StrongRR), mean(jeea.ref$StrongRR),
                         mean(qje.ref$Accept), mean(restud.ref$Accept), mean(restat.ref$Accept), mean(jeea.ref$Accept)),
                      nrow = 4)

# Create matrix for barplot showing the distribution recent author publications
pub.mat = matrix( c( mean(qje$pub0), mean(restud$pub0), mean(restat$pub0), mean(jeea$pub0),
                     mean(qje$pub1), mean(restud$pub1), mean(restat$pub1), mean(jeea$pub1),
                     mean(qje$pub2), mean(restud$pub2), mean(restat$pub2), mean(jeea$pub2),
                     mean(qje$pub3), mean(restud$pub3), mean(restat$pub3), mean(jeea$pub3),
                     mean(qje$pub45), mean(restud$pub45), mean(restat$pub45), mean(jeea$pub45),
                     mean(qje$pub6), mean(restud$pub6), mean(restat$pub6), mean(jeea$pub6)),
                  nrow = 4)

# Create matrix for barplot showing the distribution recent referee publications
refpub.mat = matrix( c( mean(qje.ref$NPubs35_Ref==0), mean(restud.ref$NPubs35_Ref==0), mean(restat.ref$NPubs35_Ref==0), mean(jeea.ref$NPubs35_Ref==0),
                     mean(qje.ref$NPubs35_Ref==1), mean(restud.ref$NPubs35_Ref==1), mean(restat.ref$NPubs35_Ref==1), mean(jeea.ref$NPubs35_Ref==1),
                     mean(qje.ref$NPubs35_Ref==2), mean(restud.ref$NPubs35_Ref==2), mean(restat.ref$NPubs35_Ref==2), mean(jeea.ref$NPubs35_Ref==2),
                     mean(qje.ref$NPubs35_Ref==3), mean(restud.ref$NPubs35_Ref==3), mean(restat.ref$NPubs35_Ref==3), mean(jeea.ref$NPubs35_Ref==3),
                     mean(qje.ref$NPubs35_Ref==4 | qje.ref$NPubs35_Ref==5), mean(restud.ref$NPubs35_Ref==4 | restud.ref$NPubs35_Ref_==5), mean(restat.ref$NPubs35_Ref==4 | restat.ref$NPubs35_Ref==5), mean(jeea.ref$NPubs35_Ref==4 | jeea.ref$NPubs35_Ref==5),
                     mean(qje.ref$NPubs35_Ref>=6), mean(restud.ref$NPubs35_Ref>=6), mean(restat.ref$NPubs35_Ref>=6), mean(jeea.ref$NPubs35_Ref>=6)),
                  nrow = 4)


################################# Create and Save Barplots ###########################################
landscape.a4.width = 11.69
landscape.a4.height = 8.27
png.scale = 0.9
pdf.scale = 0.9

if (bw == 1){
  my.colors = c(colors()[175],colors()[211],colors()[350],colors()[195])
  # Change the Figure names for bw-figures
  outputfile1 = '../../../Output/Paper/Figures/Fig1a_bw.pdf' 
  outputfile2 = '../../../Output/Paper/Figures/Fig1b_bw.pdf' 
  outputfile3 = '../../../Output/Paper/Figures/Fig1c_bw.pdf' 
  outputfile4 = '../../../Output/Paper/Figures/Fig1d_bw.pdf' 
} else {
  my.colors = c(colors()[38],colors()[81],colors()[33],colors()[107])
}
my.legend = c("QJE","REStud","REStat","JEEA")
my.cex.main = 2
my.cex.legend = 1.7
my.cex.axis = 1.6
my.cex.names = 1.45

### Distribution of Editor's First Decision
png(filename = outputfile1,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
barplot(height = decision.mat, beside = TRUE, col = my.colors,
        names.arg = c("Desk Reject", "Reject", "R&R"),
        main = "",
        cex.main = my.cex.main, cex.axis = my.cex.axis, cex.names = my.cex.names,
        legend.text = my.legend, args.legend = list(x="topright", cex = my.cex.legend))
dev.off()
pdf(gsub("png", "pdf", outputfile1), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
barplot(height = decision.mat, beside = TRUE, col = my.colors,
        names.arg = c("Desk Reject", "Reject", "R&R"),
        main = "",
        cex.main = my.cex.main, cex.axis = my.cex.axis, cex.names = my.cex.names,
        legend.text = my.legend, args.legend = list(x="topright", cex = my.cex.legend))
dev.off()

### Distribution of Referee Reports
png(filename = outputfile2,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
barplot(height = reports.mat, beside = TRUE, col = my.colors,
        names.arg = c("Def. Reject", "Reject", "No Rec.", "Weak R&R", "R&R", "Strong R&R", "Accept"),
        main = "",
        cex.main = my.cex.main, cex.axis = my.cex.axis, cex.names = my.cex.names,
        legend.text = my.legend, args.legend = list(x="topright", cex = my.cex.legend))
dev.off()
pdf(gsub("png", "pdf", outputfile2), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
barplot(height = reports.mat, beside = TRUE, col = my.colors,
        names.arg = c("Def. Reject", "Reject", "No Rec.", "Weak R&R", "R&R", "Strong R&R", "Accept"),
        main = "",
        cex.main = my.cex.main, cex.axis = my.cex.axis, cex.names = my.cex.names,
        legend.text = my.legend, args.legend = list(x="topright", cex = my.cex.legend))
dev.off()

### Distribution of Author Publications
png(filename = outputfile3,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
barplot(height = pub.mat, beside = TRUE, col = my.colors,
        names.arg = c("0","1", "2", "3", "4-5", "6+"),
        main = "",
        cex.main = my.cex.main, cex.axis = my.cex.axis, cex.names = my.cex.names,
        legend.text = my.legend, args.legend = list(x="topright", cex = my.cex.legend))
dev.off()
pdf(gsub("png", "pdf", outputfile3), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
barplot(height = pub.mat, beside = TRUE, col = my.colors,
        names.arg = c("0","1", "2", "3", "4-5", "6+"),
        main = "",
        cex.main = my.cex.main, cex.axis = my.cex.axis, cex.names = my.cex.names,
        legend.text = my.legend, args.legend = list(x="topright", cex = my.cex.legend))
dev.off()


### Distribution of Referee Publications
png(filename = outputfile4,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
barplot(height = refpub.mat, beside = TRUE, col = my.colors,
        names.arg = c("0","1", "2", "3", "4-5", "6+"),
        main = "",
        cex.main = my.cex.main, cex.axis = my.cex.axis, cex.names = my.cex.names,
        legend.text = my.legend, args.legend = list(x="topright", cex = my.cex.legend))
dev.off()
pdf(gsub("png", "pdf", outputfile4), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
barplot(height = refpub.mat, beside = TRUE, col = my.colors,
        names.arg = c("0","1", "2", "3", "4-5", "6+"),
        main = "",
        cex.main = my.cex.main, cex.axis = my.cex.axis, cex.names = my.cex.names,
        legend.text = my.legend, args.legend = list(x="topright", cex = my.cex.legend))
dev.off()