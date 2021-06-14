##### Housekeeping #####

rm(list=ls())
library('foreign')
library('RColorBrewer')
library('gplots')
library('readstata13')

# Please set path 
# setwd("/Users/username/Desktop/ReplicationFiles/Codes/OA/Figures")

##### Output #####

outputfile.actualcites = '../../../Output/OA/Figures/OA_Fig3c.pdf'
outputfile.predcites = '../../../Output/OA/Figures/OA_Fig3d.pdf'
outputfile.actualrr = '../../../Output/OA/Figures/OA_Fig4c.pdf'
outputfile.predrr = '../../../Output/OA/Figures/OA_Fig4d.pdf'

# Read in cleaned dataset where unit of observation is a paper
pooled = read.dta13('../../../Data/Main/Pooled_cleaned.dta')
# Keep only papers that were not desk-rejected and had more than 1 referee assigned
pooled = pooled[pooled$notdeskrej==1,]
pooled = pooled[pooled$noref!=1,]

# Fit citation model
lm.fit = lm(asinh_cites ~ frDefReject + frReject + frNoRec + frWeakRR + frRR + frStrongRR + frAccept + as.factor(yearsubmit)*as.factor(journal),
            data = pooled)
# Fit R&R model
probit.fit = glm(rr ~ frDefReject + frReject + frNoRec + frWeakRR + frRR + frStrongRR + frAccept + as.factor(yearsubmit)*as.factor(journal),
                 data = pooled, family = binomial(link = "probit"))
# Limit to the subset of papers with 3 referee reports
pooled = pooled[pooled$norefresp==3,]
# Compute predicted asinh(citations) and fraction R&R
pooled$predcites = predict(lm.fit, pooled)
pooled$predrr = predict(probit.fit, pooled, type="response")

### Re-format datasets so that recommendation variables can more easily be used to create heat maps
evals = paste0("eval", 1:10)
eval.mat = as.matrix(pooled[,evals])
refpubs = paste0("refpub5_",1:10)
pub.mat = as.matrix(pooled[,refpubs])

rec.mat = matrix(NA, nrow = nrow(pooled), ncol = 10)
refpub.mat = matrix(NA, nrow = nrow(pooled), ncol = 10)

rm.missing = function(vec){
  nonmissing.indices = which(vec!="")
  temp = vec[nonmissing.indices]
  result = rep(NA, length(vec))
  result[1:length(temp)] = temp
  return(result)
}
rm.na = function(vec){
  non.na.indices = which(!is.na(vec))
  temp = vec[non.na.indices]
  result = rep(NA, length(vec))
  result[1:length(temp)] = temp
  return(result)
}
convert.rec = function(rec){
  numeric.rec =
    ifelse(rec=="DefReject", 1,
           ifelse(rec=="Reject", 2,
                  ifelse(rec=="NoRec", 3,
                         ifelse(rec=="WeakRR", 4,
                                ifelse(rec=="RR", 5,
                                       ifelse(rec=="StrongRR", 6,
                                              ifelse(rec=="Accept", 7 , NA) ) ) ) ) ) )
  return(numeric.rec)
}

for (i in 1:nrow(eval.mat)){
  rec.mat[i,] = rm.missing(eval.mat[i,])
  refpub.mat[i,] = rm.na(pub.mat[i,])
}

pooled[,paste0("rec", 1:10)] = rec.mat
pooled[,paste0("ref", 1:10, "pub")] = refpub.mat

qje = pooled[pooled$journal == "QJE",]
restud = pooled[pooled$journal == "REStud",]
restat = pooled[pooled$journal == "REStat",]
jeea = pooled[pooled$journal == "JEEA",]

rm(pub.mat); rm(rec.mat); rm(evals); rm(refpubs)
rm(rm.missing); rm(rm.na)

##### Create data frame of all possible combinations of referee recommendations (papers with 3 reports) ####
create.duplicate = function(df){
  df2 = df
  df2$rec1 = df$rec2
  df2$rec2 = df$rec1
  
  df3 = df
  df3$rec1 = df$rec1
  df3$rec2 = df$rec3
  
  df4 = df
  df4$rec1 = df$rec3
  df4$rec2 = df$rec1
  
  df5 = df
  df5$rec1 = df$rec2
  df5$rec2 = df$rec3
  
  df6 = df
  df6$rec1 = df$rec3
  df6$rec2 = df$rec2
  
  return(rbind(df,df2,df3,df4,df5,df6))
}

pooled = create.duplicate(pooled)
qje = create.duplicate(qje)
restud = create.duplicate(restud)
restat = create.duplicate(restat)
jeea = create.duplicate(jeea)

### Compute Mean Citations/R&R for Each 2-Report Combination
reports = c("DefReject", "Reject", "NoRec", "WeakRR", "RR", "StrongRR", "Accept")

create.recmatrix.cites = function(df, pred = FALSE, reverse = TRUE){
  recmatrix = matrix( nrow=length(reports), ncol = length(reports) )
  if (pred){
    for (i in 1:length(reports) ){
      for (j in 1:length(reports) ){
        recmatrix[i,j] = mean(df$predcites[df$rec1==reports[i] & df$rec2==reports[j]])
      }
    }
  }else{
    for (i in 1:length(reports) ){
      for (j in 1:length(reports) ){
        recmatrix[i,j] = mean(df$asinh_cites[df$rec1==reports[i] & df$rec2==reports[j]])
      }
    }
  }
  rownames(recmatrix) = reports
  colnames(recmatrix) = reports
  if (reverse){
    recmatrix = recmatrix[nrow(recmatrix):1,]
  }
  return(recmatrix)
}

pooled.rec.cites = create.recmatrix.cites(pooled)
qje.rec.cites = create.recmatrix.cites(qje)
restud.rec.cites = create.recmatrix.cites(restud)
restat.rec.cites = create.recmatrix.cites(restat)
jeea.rec.cites = create.recmatrix.cites(jeea)

pooled.rec.predcites = create.recmatrix.cites(pooled, pred=TRUE)
qje.rec.predcites = create.recmatrix.cites(qje, pred=TRUE)
restud.rec.predcites = create.recmatrix.cites(restud, pred=TRUE)
restat.rec.predcites = create.recmatrix.cites(restat, pred=TRUE)
jeea.rec.predcites = create.recmatrix.cites(jeea, pred=TRUE)

create.recmatrix.rr = function(df, pred = FALSE, reverse = TRUE){
  recmatrix = matrix( nrow=length(reports), ncol = length(reports) )
  if (pred){
    for (i in 1:length(reports) ){
      for (j in 1:length(reports) ){
        recmatrix[i,j] = mean(df$predrr[df$rec1==reports[i] & df$rec2==reports[j]])
      }
    }
  }else{
    for (i in 1:length(reports) ){
      for (j in 1:length(reports) ){
        recmatrix[i,j] = mean(df$rr[df$rec1==reports[i] & df$rec2==reports[j]])
      }
    }
  }
  rownames(recmatrix) = reports
  colnames(recmatrix) = reports
  if (reverse){
    recmatrix = recmatrix[nrow(recmatrix):1,]
  }
  return(recmatrix)
}

pooled.rec.rr = create.recmatrix.rr(pooled)
qje.rec.rr = create.recmatrix.rr(qje)
restud.rec.rr = create.recmatrix.rr(restud)
restat.rec.rr = create.recmatrix.rr(restat)
jeea.rec.rr = create.recmatrix.rr(jeea)

pooled.rec.predrr = create.recmatrix.rr(pooled, pred=TRUE)
qje.rec.predrr = create.recmatrix.rr(qje, pred=TRUE)
restud.rec.predrr = create.recmatrix.rr(restud, pred=TRUE)
restat.rec.predrr = create.recmatrix.rr(restat, pred=TRUE)
jeea.rec.predrr = create.recmatrix.rr(jeea, pred=TRUE)

############################ Create heat maps ###################################
# my.main.cex = 1.8
my.notecex = 2.1
my.cexRow = 1.9
my.cexCol = 1.9
my.margins = c(8,8)
my.lmat = rbind(c(4,3,0), c(2,1,0))
my.lhei=c(0.2,9.8)
my.lwid=c(0.5,9,0.5)

landscape.a4.width = 11.69
landscape.a4.height = 8.27
png.scale = 1
pdf.scale = 1

### Heat Map for Actual Citations
png(filename = outputfile.actualcites,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
my_palette = rev(heat.colors(256))
my.breaks = seq(1.3,4.6,length=length(my_palette)+1)
# par(cex.main = my.main.cex)
heatmap.2(x = pooled.rec.cites, cellnote = round(pooled.rec.cites,1),
          Rowv=FALSE, Colv=FALSE, dendrogram = "none",
          key=FALSE, trace="none",
          col = my_palette, notecol="black",
          breaks = my.breaks,
          lmat=my.lmat, lhei=my.lhei, lwid=my.lwid,
          margins = my.margins,
          sepwidth=c(0.001,0.001),
          sepcolor="black",
          colsep=0:length(reports),
          rowsep=0:length(reports),
          notecex = my.notecex, cexRow = my.cexRow, cexCol = my.cexCol)
dev.off()

pdf(gsub("png", "pdf", outputfile.actualcites), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
my_palette = rev(heat.colors(256))
my.breaks = seq(1.3,4.6,length=length(my_palette)+1)
# par(cex.main = my.main.cex)
heatmap.2(x = pooled.rec.cites, cellnote = round(pooled.rec.cites,1),
          Rowv=FALSE, Colv=FALSE, dendrogram = "none",
          key=FALSE, trace="none",
          col = my_palette, notecol="black",
          breaks = my.breaks,
          lmat=my.lmat, lhei=my.lhei, lwid=my.lwid,
          margins = my.margins,
          sepwidth=c(0.001,0.001),
          sepcolor="black",
          colsep=0:length(reports),
          rowsep=0:length(reports),
          notecex = my.notecex, cexRow = my.cexRow, cexCol = my.cexCol)
dev.off()

### Heat Map for Predicted Citations
png(filename = outputfile.predcites,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
# par(cex.main = my.main.cex)
heatmap.2(x = pooled.rec.predcites, cellnote = round(pooled.rec.predcites,1),
          Rowv=FALSE, Colv=FALSE, dendrogram = "none",
          key=FALSE, trace="none",
          col = my_palette, notecol="black",
          lmat=my.lmat, lhei=my.lhei, lwid=my.lwid,
          margins = my.margins,
          breaks = my.breaks,
          sepwidth=c(0.001,0.001),
          sepcolor="black",
          colsep=0:length(reports),
          rowsep=0:length(reports),
          notecex = my.notecex, cexRow = my.cexRow, cexCol = my.cexCol)
dev.off()

pdf(gsub("png", "pdf", outputfile.predcites), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
# par(cex.main = my.main.cex)
heatmap.2(x = pooled.rec.predcites, cellnote = round(pooled.rec.predcites,1),
          Rowv=FALSE, Colv=FALSE, dendrogram = "none",
          key=FALSE, trace="none",
          col = my_palette, notecol="black",
          lmat=my.lmat, lhei=my.lhei, lwid=my.lwid,
          margins = my.margins,
          breaks = my.breaks,
          sepwidth=c(0.001,0.001),
          sepcolor="black",
          colsep=0:length(reports),
          rowsep=0:length(reports),
          notecex = my.notecex, cexRow = my.cexRow, cexCol = my.cexCol)
dev.off()

### Heat Map for Actual R&R Fractions
png(filename = outputfile.actualrr,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
my_palette = rev(heat.colors(256))
my.breaks = seq(0,1,length = length(my_palette)+1)
# par(cex.main = my.main.cex)
heatmap.2(x = pooled.rec.rr, cellnote = round(pooled.rec.rr,3),
          Rowv=FALSE, Colv=FALSE, dendrogram = "none",
          key=FALSE, trace="none",
          col = my_palette, notecol="black",
          breaks = my.breaks,
          lmat=my.lmat, lhei=my.lhei, lwid=my.lwid,
          margins = my.margins,
          sepwidth=c(0.001,0.001),
          sepcolor="black",
          colsep=0:length(reports),
          rowsep=0:length(reports),
          notecex = my.notecex, cexRow = my.cexRow, cexCol = my.cexCol)
dev.off()

pdf(gsub("png", "pdf", outputfile.actualrr), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
my_palette = rev(heat.colors(256))
my.breaks = seq(0,1,length = length(my_palette)+1)
# par(cex.main = my.main.cex)
heatmap.2(x = pooled.rec.rr, cellnote = round(pooled.rec.rr,3),
          Rowv=FALSE, Colv=FALSE, dendrogram = "none",
          key=FALSE, trace="none",
          col = my_palette, notecol="black",
          breaks = my.breaks,
          lmat=my.lmat, lhei=my.lhei, lwid=my.lwid,
          margins = my.margins,
          sepwidth=c(0.001,0.001),
          sepcolor="black",
          colsep=0:length(reports),
          rowsep=0:length(reports),
          notecex = my.notecex, cexRow = my.cexRow, cexCol = my.cexCol)
dev.off()

### Heat Map for Predicted R&R Fractions
png(filename = outputfile.predrr,
    height = landscape.a4.height*png.scale, width = landscape.a4.width*png.scale,
    units = "in", res = 300)
# par(cex.main = my.main.cex)
heatmap.2(x = pooled.rec.predrr, cellnote = round(pooled.rec.predrr,3),
          Rowv=FALSE, Colv=FALSE, dendrogram = "none",
          key=FALSE, trace="none",
          col = my_palette, notecol="black",
          lmat=my.lmat, lhei=my.lhei, lwid=my.lwid,
          margins = my.margins,
          breaks = my.breaks,
          sepwidth=c(0.001,0.001),
          sepcolor="black",
          colsep=0:length(reports),
          rowsep=0:length(reports),
          notecex = my.notecex, cexRow = my.cexRow, cexCol = my.cexCol)
dev.off()

pdf(gsub("png", "pdf", outputfile.predrr), paper = "special",
    height = landscape.a4.height*pdf.scale, width = landscape.a4.width*pdf.scale)
# par(cex.main = my.main.cex)
heatmap.2(x = pooled.rec.predrr, cellnote = round(pooled.rec.predrr,3),
          Rowv=FALSE, Colv=FALSE, dendrogram = "none",
          key=FALSE, trace="none",
          col = my_palette, notecol="black",
          lmat=my.lmat, lhei=my.lhei, lwid=my.lwid,
          margins = my.margins,
          breaks = my.breaks,
          sepwidth=c(0.001,0.001),
          sepcolor="black",
          colsep=0:length(reports),
          rowsep=0:length(reports),
          notecex = my.notecex, cexRow = my.cexRow, cexCol = my.cexCol)
dev.off()