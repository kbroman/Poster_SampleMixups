source("colors.R")
color[1] <- bgcolor

file <- "insulin_lod.RData"
if(file.exists(file)) {
  load(file)
} else {
  attach("~/Projects/Attie/GoldStandard/Genotypes/Study_and_clean/Data/clean_cross.RData")
  f2g <- subset(f2g, chr="-un")
  f2g <- replace.map(f2g, newmap)
  f2g.orig <- f2g
  detach(2)
  rm(f2g)

  attach("~/Projects/Attie/GoldStandard/FinalData/aligned_geno_with_pmap.RData")
  attach("~/Projects/Attie/GoldStandard/FinalData/lipomics_final.RData")
  phe <- "INSULIN (ng/ml) 10 wk"

  ids1 <- findCommonID(f2g.orig$pheno$MouseNum, lipomics$MouseNum)
  ids2 <- findCommonID(f2g$pheno$MouseNum, lipomics$MouseNum)

  f2g.orig <- f2g.orig[,ids1$first]
  f2g.orig$pheno$insulin <- log10(lipomics[ids1$second,phe])
  f2g <- f2g[,ids2$first]
  f2g$pheno$insulin <- log10(lipomics[ids2$second,phe])
  detach(2)
  detach(2)

  f2g.orig <- calc.genoprob(f2g.orig, step=1, err=0.002, map.function="c-f")
  f2g <- calc.genoprob(f2g, step=1, err=0.002, map.function="c-f")

  sex.orig <- as.numeric(f2g.orig$pheno$Sex)-1
  out.orig <- scanone(f2g.orig, phe="insulin", method="hk", addcovar=sex.orig)
  out.orig.i <- scanone(f2g.orig, phe="insulin", method="hk", addcovar=sex.orig, intcovar=sex.orig)
  sex.new <- as.numeric(f2g$pheno$Sex)-1
  out.new <- scanone(f2g, phe="insulin", method="hk", addcovar=sex.new)
  out.new.i <- scanone(f2g, phe="insulin", method="hk", addcovar=sex.new, intcovar=sex.new)

  operm.new <- scanone(f2g, phe="insulin", method="hk", addcovar=sex.new, n.perm=1000, n.cluster=8)
  operm.orig <- scanone(f2g.orig, phe="insulin", method="hk", addcovar=sex.orig, n.perm=1000, n.cluster=8)

  save(out.orig, out.new, operm.new, operm.orig, out.new.i, out.orig.i, file=file)
}

pdf("../Figs/insulin_lod.pdf", width=9, height=4.5, pointsize=12, onefile=TRUE)
par(fg="black", col="black", col.axis="black", col.lab="black", bg="white")
par(mar=c(5.1,4.1,0.1,0.1))
plot(out.new, out.orig, col=color[1:2], ylab="LOD score")
abline(h=quantile(operm.new, 0.95), lty=2, col="black")
legend("topright", lwd=2, col=color[1:2], c("after", "before"), text.col=color[1:2], cex=1.2)
dev.off()
