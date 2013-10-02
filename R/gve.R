######################################################################
# eQTL genotype vs expression
######################################################################

source("colors.R")

color[1] <- bgcolor

file <- "gve.RData"
if(file.exists(file)) {
  load(file)
} else {
  attach("~/Projects/Attie/GoldStandard/Genotypes/Study_and_clean/Data/clean_cross.RData")
  f2g <- replace.map(f2g, newmap)
  detach(2)
  attach("~/Projects/Attie/GoldStandard/LiningUp/Data/genodist_i.RData")
  attach("~/Projects/Attie/GoldStandard/LiningUp/Data/F2.mlratio.islet.RData")

  phenam <- sapply(attr(dgenovi, "y"), colnames)
 
  wh1 <- which(locallod.i == max(locallod.i)) # 499541 (1259) LOD = 165.5
  e1 <- names(wh1)
  chr1 <- strsplit(names(which(sapply(phenam, function(a,b) b %in% a, e1))),"@")[[1]][1]

  wh2 <- which(sapply(phenam, length)==2)
  chr2 <- strsplit(names(wh2), "@")[[1]][1]
  e2 <- phenam[[wh2]]

  wh3 <- which(sapply(phenam, length)==3)
  chr3 <- strsplit(names(wh3), "@")[[1]][1]
  e3 <- phenam[[wh3]][2:3]

  id <- findCommonID(colnames(islet.mlratio), f2g$pheno$MouseNum)

  f2g <- calc.genoprob(subset(f2g, c(chr1,chr2,chr3), id$second), map.function="c-f", err=0.002)
  y1 <- islet.mlratio[e1,id$first]
  y2 <- t(islet.mlratio[e2,id$first])
  y3 <- t(islet.mlratio[e3,id$first])
  out1 <- scanone(f2g, phe=y1, method="hk")
  out2a <- scanone(f2g, phe=y2[,1], method="hk")
  out2b <- scanone(f2g, phe=y2[,2], method="hk")
  out3a <- scanone(f2g, phe=y3[,1], method="hk")
  out3b <- scanone(f2g, phe=y3[,2], method="hk")
  marker1 <- rownames(max(out1))
  marker2 <- rownames(max(out2a))
  marker3 <- rownames(max(out3a))
  g <- pull.geno(fill.geno(f2g, err=0.002, map.function="c-f", method="argmax"))[,c(marker1, marker2,marker3)]

  save(y1, y2, y3, g, marker1, marker2, marker3, e1, e2, e3, chr1, chr2, chr3, file=file)
  detach(2)
  detach(2)
}


set.seed(47500621)
u <- runif(length(y1), -0.1, 0.1)

pdf("../Figs/gve.pdf", width=9, height=4.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="black", col="black", col.axis="black", col.lab="black",
    bg="white", cex.axis=1.2, cex.lab=1.3, las=1)
plot(0, 0, type="n", xlab="", ylab=paste("expression  of ", e1), xaxt="n", 
     xlim=c(0.75,3.25), ylim=range(y1))
g1i <-  knn(cbind(y1), cbind(y1), g[,1], k=40, l=33)
points(g[,1]+u, y1, lwd=2, col=color[c(2,4,1)][g1i])
points((g[,1]+u)[is.na(g1i)], y1[is.na(g1i)], lwd=2, col="gray")
axis(side=1, at=1:3, c("BB", "BR", "RR"))
title(xlab=paste("Genotype  at ", marker1))
dev.off()

