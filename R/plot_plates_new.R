load("final_calls.RData")
final.calls <- sub("\\*", "", final.calls)

# fullplatenum
# split.fullplatenum
# corrected.fullplatenum
# split.controlplatenum
# split.corrected.fullplatenum

source("colors.R")

######################################################################
library(B6BTBR07a)
data(topstuff)

ts.mousenum <- sapply(strsplit(rownames(topstuff), "_"), function(a) a[[2]])
ts.plate <- as.character(as.numeric(sapply(strsplit(as.character(topstuff[,3]), "[_:]"), function(a) a[2])))
ts.well <- sapply(strsplit(as.character(topstuff[,3]), "[_:]"), function(a) a[3])

ro.plate <- ts.plate[match(rownames(final.calls), ts.mousenum)]
ro.well <- ts.well[match(rownames(final.calls), ts.mousenum)]

final.calls[,"genotypes"] <- sub("dup", "", final.calls[,"genotypes"])
bad <- which(!is.na(final.calls[,"genotypes"]) & rownames(final.calls) != final.calls[,"genotypes"])
notfound <- grep("not", final.calls[,"genotypes"])
nogeno <- which(is.na(final.calls[,"genotypes"]))
noexpr <- grep("maybe", final.calls[,"genotypes"])
okay <- which(!is.na(final.calls[,"genotypes"]) & rownames(final.calls) == final.calls[,"genotypes"])

#rbind(bad=table(factor(ro.plate[bad], levels=sort(unique(ro.plate)))),
#      notfound=table(factor(ro.plate[notfound], levels=sort(unique(ro.plate)))),
#      okay=table(factor(ro.plate[okay], levels=sort(unique(ro.plate)))))

#dec <- rep(0, nrow(final.calls))
#dec[bad] <- 3
#dec[notfound] <- 2
#dec[okay] <- 1

num <- rep(1:11, rep(8,11))
num[num < 10] <- paste(0, num[num<10], sep="")
wells <- paste(LETTERS[1:8], num, sep="")

ro.well.num <- match(ro.well, wells)
ro.plate.num <- as.numeric(ro.plate) + (ro.well.num-89/2)/87
######################################################################
fullplatenum <- paste(as.numeric(ro.plate), ro.well, sep=":")
names(fullplatenum) <- rownames(final.calls)
corrected.fullplatenum <- rep("", nrow(final.calls))
names(corrected.fullplatenum) <- rownames(final.calls)
corrected.fullplatenum[-c(notfound,nogeno)] <- fullplatenum[final.calls[-c(notfound,nogeno),"genotypes"]]
corrected.fullplatenum[corrected.fullplatenum==""] <- NA

split.fullplatenum <- matrix(ncol=3, nrow=length(fullplatenum))
split.fullplatenum[,1] <- as.numeric(sapply(strsplit(fullplatenum, ":"), function(a) a[1]))
split.fullplatenum[,2] <- match(substr(sapply(strsplit(fullplatenum, ":"), function(a) a[2]), 1, 1), LETTERS[1:8])
split.fullplatenum[,3] <- as.numeric(substr(sapply(strsplit(fullplatenum, ":"), function(a) a[2]), 2, 3))

split.corrected.fullplatenum <- matrix(ncol=3, nrow=length(corrected.fullplatenum))
split.corrected.fullplatenum[,1] <- as.numeric(sapply(strsplit(corrected.fullplatenum, ":"), function(a) a[1]))
split.corrected.fullplatenum[,2] <- match(substr(sapply(strsplit(corrected.fullplatenum, ":"), function(a) a[2]), 1, 1), LETTERS[1:8])
split.corrected.fullplatenum[,3] <- as.numeric(substr(sapply(strsplit(corrected.fullplatenum, ":"), function(a) a[2]), 2, 3))

# check
#all(is.na(corrected.fullplatenum[which(apply(split.corrected.fullplatenum, 1, function(a) any(is.na(a))))]))
#all(!is.na(corrected.fullplatenum[-which(apply(split.corrected.fullplatenum, 1, function(a) any(is.na(a))))]))
######################################################################
tsid <- sapply(strsplit(rownames(topstuff), "_"), function(a) a[2])
tsid <- sapply(strsplit(rownames(topstuff), "_"), function(a) a[3])
ts.plate <- sapply(strsplit(as.character(topstuff[,3]), "[_:]"), function(a) a[2])
ts.well <- sapply(strsplit(as.character(topstuff[,3]), "[_:]"), function(a) a[3])

controlplatenum <- paste(ts.plate, ts.well, sep=":")[!is.na(tsid) & (tsid=="B6" | tsid=="F1" | tsid=="BTBR")]
names(controlplatenum) <- tsid[!is.na(tsid) & (tsid=="B6" | tsid=="F1" | tsid=="BTBR")]

split.controlplatenum <- matrix(ncol=3, nrow=length(controlplatenum))
split.controlplatenum[,1] <- as.numeric(sapply(strsplit(controlplatenum, ":"), function(a) a[1]))
split.controlplatenum[,2] <- match(substr(sapply(strsplit(controlplatenum, ":"), function(a) a[2]), 1, 1), LETTERS[1:8])
split.controlplatenum[,3] <- as.numeric(substr(sapply(strsplit(controlplatenum, ":"), function(a) a[2]), 2, 3))
rownames(split.controlplatenum) <- names(controlplatenum)
######################################################################

pdf("../Figs/errors_graphically_new.pdf", height=8, width=7.5, pointsize=10)
par(mar=rep(0,4))
xoff <- 5
yoff <- 2
plot(0,0,type="n", xlab="", ylab="", xlim=c(-xoff,100+xoff), ylim=c(55+yoff,-yoff),
     xaxt="n", yaxt="n")

plx1 <- c(0,0,0,0,55,55,55)[c(7,2,6,3,5,4,1)]
plx2 <- (plx1+45)
ply1 <- c(0,15,30,45,5,20,35)[c(7,2,6,3,5,4,1)]
ply2 <- (ply1+10)
colx <- seq(min(plx1), min(plx2), len=14)
colx <- colx[-(1:2)] - diff(colx[1:2])
rowy <- seq(max(ply1), max(ply2), len=10)-max(ply1)
rowy <- rowy[-(1:2)] - diff(rowy[1:2])
plates <- 1628:1634

rect(plx1,ply1,plx2,ply2, lwd=2)
text((plx1+plx2)/2, ply1 - diff(rowy[1:2])*1.9, plates, cex=1.3)
for(i in 1:7) {
  text(plx1[i] - diff(colx[1:2])*0.5, ply1[i] + rowy[1:8], LETTERS[1:8], cex=0.8)
  text(plx1[i] + colx[1:12], ply1[i] - diff(rowy[1:2])*0.5, 1:12, cex=0.8)
}


z <- split.fullplatenum
z <- z[!is.na(z[,1]),]
for(pl in 1:7) {
  for(i in 1:8) {
    for(j in 1:12) {
      if(any(!is.na(z[,1]) & z[,1]==plates[pl] & z[,2]==i & z[,3]==j))
        points(plx1[pl] + colx[j], ply1[pl] + rowy[i], cex=1.2)
      else
        points(plx1[pl] + colx[j], ply1[pl] + rowy[i], cex=1.2, col="gray")
    }
  }
}

z <- corrected.fullplatenum[!is.na(corrected.fullplatenum)]
z <- split.fullplatenum[-c(nogeno, noexpr),][is.na(match(fullplatenum[-c(nogeno, noexpr)], z)),]
for(i in 1:nrow(z)) 
  points(plx1[match(z[i,1], plates)] + colx[z[i,3]],
         ply1[match(z[i,1], plates)] + rowy[z[i,2]], cex=1.5, pch=4, col="red", lwd=2)

for(i in 1:nrow(split.controlplatenum)) {
  zz <- split.controlplatenum[i,]
  text(plx1[match(zz[1], plates)] + colx[zz[3]],
       ply1[match(zz[1], plates)] + rowy[zz[2]], rownames(split.controlplatenum)[i], cex=0.6)
}

arrowcol <- rep("black", length(corrected.fullplatenum))
names(arrowcol) <- names(fullplatenum)
wh <- which(!is.na(corrected.fullplatenum) & corrected.fullplatenum != fullplatenum)
z <- split.corrected.fullplatenum
z <- z[,1]*12*8 + z[,3]*8 + z[,2]
thearrows <- names(corrected.fullplatenum[wh][order(z[wh])])
suppressWarnings(arrowcol[thearrows] <- c("blue", "orange", "green"))


for(i in 1:nrow(split.corrected.fullplatenum)) {
  z <- split.corrected.fullplatenum[i,]
  if(is.na(z[1])) next

  if(fullplatenum[i] == corrected.fullplatenum[i]) 
    points(plx1[match(z[1], plates)] + colx[z[3]],
           ply1[match(z[1], plates)] + rowy[z[2]], pch=16, cex=0.8, col="black")
}

tab <- table(corrected.fullplatenum)
z <- split.corrected.fullplatenum[!is.na(match(corrected.fullplatenum,names(tab)[tab==2])),]
for(i in 1:nrow(z)) 
  points(plx1[match(z[i,1], plates)] + colx[z[i,3]],
         ply1[match(z[i,1], plates)] + rowy[z[i,2]], cex=1.2, col="hotpink")


for(i in 1:nrow(split.corrected.fullplatenum)) {
  z <- split.corrected.fullplatenum[i,]
  if(is.na(z[1])) next
  
  if(fullplatenum[i] != corrected.fullplatenum[i]) {
    zz <- split.fullplatenum[i,]
    arrows(plx1[match(z[1], plates)] + colx[z[3]],
           ply1[match(z[1], plates)] + rowy[z[2]],
           plx1[match(zz[1], plates)] + colx[zz[3]],
           ply1[match(zz[1], plates)] + rowy[zz[2]],
           col=arrowcol[i], lwd=1, len=0.08, lend=1, ljoin=1, angle=15)
    segments(plx1[match(z[1], plates)] + colx[z[3]]-diff(colx[1:2])*0.1,
           ply1[match(z[1], plates)] + rowy[z[2]],
           plx1[match(z[1], plates)] + colx[z[3]]+diff(colx[1:2])*0.1,
           ply1[match(z[1], plates)] + rowy[z[2]],
           col=arrowcol[i], lwd=1, lend=1, ljoin=1)
  }
}
           
tab <- table(corrected.fullplatenum)
z <- split.corrected.fullplatenum[!is.na(match(corrected.fullplatenum,names(tab)[tab==2])),]
for(i in 1:nrow(z)) 
  segments(plx1[match(z[i,1], plates)] + colx[z[i,3]]-diff(colx[1:2])*0.1,
           ply1[match(z[i,1], plates)] + rowy[z[i,2]],
           plx1[match(z[i,1], plates)] + colx[z[i,3]]+diff(colx[1:2])*0.1,
           ply1[match(z[i,1], plates)] + rowy[z[i,2]],
           col="hotpink", lend=1, ljoin=1)


rect(53, 48, 104, 58.5, col="gray90")

text(55, 49.5, "Tail of arrow:", adj=c(0, 0.5))
text(72, 49.5, "Where DNA should have been", adj=c(0,0.5), col=bgcolor)
text(55, 51, "Head of arrow:", adj=c(0,0.5))
text(72, 51, "Where DNA was placed", adj=c(0,0.5), col=bgcolor)

points(68, 53, cex=1.2, col="hotpink")
text(72, 53, "Sample duplicated", adj=c(0,0.5), col=bgcolor)

points(68, 54.3, cex=1.2, col="black")
points(68, 54.3, cex=0.8, pch=16)
text(72, 54.3, "DNA in correct well", adj=c(0,0.5), col=bgcolor)

points(68, 55.6, cex=1.2, col="gray40")
text(72, 55.6, "Empty well", adj=c(0,0.5), col=bgcolor)

points(68, 56.9, cex=1.2, col="black")
points(68, 56.9, cex=1.5, pch=4, col="red", lwd=2)
text(72, 56.9, "DNA lost", adj=c(0,0.5), col=bgcolor)


dev.off()
