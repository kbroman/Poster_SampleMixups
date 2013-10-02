######################################################################
# expression vs expression
######################################################################

panel.cor <- 
function(x, y, digits = 2, prefix = "", cex.cor, 
         thecolors=c("gray40", color[2]), ...) 
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x,y)
  txt <- paste(prefix, myround(r, digits), sep = "")
  if (missing(cex.cor))
    cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * 0.5, col = thecolors[(abs(r) > 0.65) + 1])
}

source("colors.R")

file <- "eve.RData"
if(file.exists(file)) {
  load(file)
} else {
  attach("~/Projects/Attie/GoldStandard/LiningUp/Data/F2.mlratio.islet.RData")
  attach("~/Projects/Attie/GoldStandard/LiningUp/Data/F2.mlratio.liver.RData")

  islet.mlratio <- fscale(t(islet.mlratio))
  liver.mlratio <- fscale(t(liver.mlratio))
  detach(2)
  detach(2)

  id <- findCommonID(rownames(islet.mlratio), rownames(liver.mlratio))

  y1 <- cbind("Mouse3280\nislet"=islet.mlratio["Mouse3280",],
              "Mouse3280\nliver"=liver.mlratio["Mouse3280",])
  
  thecor <- corbetw2mat(islet.mlratio[id$first,], liver.mlratio[id$second,])

  zisl <- islet.mlratio[id$first, thecor>0.75]
  zliv <- liver.mlratio[id$second, thecor>0.75]

  y2 <- cbind("Mouse3280\nislet"=islet.mlratio["Mouse3280",thecor>0.75],
              "Mouse3281\nislet"=islet.mlratio["Mouse3281",thecor>0.75],
              "Mouse3280\nliver"=liver.mlratio["Mouse3280",thecor>0.75],
              "Mouse3281\nliver"=liver.mlratio["Mouse3281",thecor>0.75])
  
  dil <- distee(islet.mlratio[,thecor>0.75], liver.mlratio[,thecor>0.75],
                d.method="cor", labels=c("islet", "liver"))

  y3 <- cbind("Mouse3598\nislet"=islet.mlratio["Mouse3598",thecor>0.75],
              "Mouse3599\nislet"=islet.mlratio["Mouse3599",thecor>0.75],
              "Mouse3598\nliver"=liver.mlratio["Mouse3598",thecor>0.75],
              "Mouse3599\nliver"=liver.mlratio["Mouse3599",thecor>0.75])

  y4 <- cbind("Mouse3295\nislet"=islet.mlratio["Mouse3295", thecor>0.75], 
              "Mouse3296\nislet"=islet.mlratio["Mouse3296", thecor>0.75], 
              "Mouse3295\nliver"=liver.mlratio["Mouse3295", thecor>0.75], 
              "Mouse3296\nliver"=liver.mlratio["Mouse3296", thecor>0.75]) 
              
  save(y1, y2, y3, y4, dil, zisl, zliv, file=file)
}


pdf("../Figs/eve_1.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
dev.off()

pdf("../Figs/eve_2.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,25,100,28,col=color[2],lend=1,ljoin=1)

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,25,100,28,col=color[2],lend=1,ljoin=1)
dev.off()


pdf("../Figs/eve_2.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,25,100,28,col=color[2],lend=1,ljoin=1)

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,25,100,28,col=color[2],lend=1,ljoin=1)
dev.off()


file <- "../Figs/eve_3.jpg"
if(!file.exists(file)) {
  jpeg(file, width=9, height=6.5, units="in", 
       pointsize=12, res=288)
  layout(cbind(c(1,1,2,2),c(4,3,3,5)))

  par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
  plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
       xaxs="i", yaxs="i")
  mtext(side=3,"expression in islet", line=1, cex=1.5)
  mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
  mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
  rect(0,25,100,28,col=color[2],lend=1,ljoin=1)

  plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
       xaxs="i", yaxs="i")
  mtext(side=3,"expression in liver", line=1, cex=1.5)
  mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
  mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
  rect(0,25,100,28,col=color[2],lend=1,ljoin=1)
  
  par(cex.axis=1.3, cex.lab=1.8, col.lab=color[1])
  par(mar=c(5.1,5.1,4.1,0.1))
  plot(y1, xlab="islet expression", las=1,
       ylab="liver expression", cex=0.5, cex.lab=2)
  mtext(side=3, line=1, "Mouse3280", cex=1.5)

  dev.off()
}


pdf("../Figs/eve_3b.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(40,0,41.5,100,col=color[2],lend=1,ljoin=1)

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(40,0,41.5,100,col=color[2],lend=1,ljoin=1)

par(cex.axis=1.3, cex.lab=1.8, col.lab=color[1])
par(mar=c(5.1,5.1,4.1,0.1))
plot(zisl[,1], zliv[,1], xlab="islet expression", ylab="liver expression", 
     las=1)
mtext(side=3, line=1, paste("transcript", colnames(zisl)[1]), cex=1.8)

dev.off()


pdf("../Figs/eve_3c.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(83,0,84.5,100,col=color[2],lend=1,ljoin=1)

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(83,0,84.5,100,col=color[2],lend=1,ljoin=1)

par(cex.axis=1.3, cex.lab=1.8, col.lab=color[1])
par(mar=c(5.1,5.1,4.1,0.1))
plot(zisl[,27], zliv[,27], xlab="islet expression", ylab="liver expression", 
     las=1)
mtext(side=3, line=1, paste("transcript", colnames(zisl)[27]), cex=1.8)
dev.off()


pdf("../Figs/eve_3d.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(65,0,66.5,100,col=color[2],lend=1,ljoin=1)

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(65,0,66.5,100,col=color[2],lend=1,ljoin=1)

par(cex.axis=1.3, cex.lab=1.8, col.lab=color[1])
par(mar=c(5.1,5.1,4.1,0.1))
plot(zisl[,17], zliv[,17], xlab="islet expression", ylab="liver expression", las=1)
mtext(side=3, line=1, paste("transcript", colnames(zisl)[17]), cex=1.8)
dev.off()



pdf("../Figs/eve_4.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
dev.off()


pdf("../Figs/eve_5.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,25,20,28,col=color[2],lend=1,ljoin=1)

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,25,20,28,col=color[2],lend=1,ljoin=1)

par(cex.axis=1.3, cex.lab=1.8, col.lab=color[1])
par(mar=c(5.1,5.1,4.1,0.1))
plot(y2[,c(1,3)], xlab="islet expression", las=1,
     ylab="liver expression")
mtext(side=3, line=1, "Mouse3280", cex=1.8)

dev.off()


pdf("../Figs/eve_6.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,80,20,83,col=color[2],lend=1,ljoin=1)

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,80,20,83,col=color[2],lend=1,ljoin=1)

par(cex.axis=1.3, cex.lab=1.8, col.lab=color[1])
par(mar=c(5.1,5.1,4.1,0.1))
plot(y3[,c(1,3)], xlab="islet expression", las=1,
     ylab="liver expression")
mtext(side=3, line=1, "Mouse3598", cex=1.8)

dev.off()


pdf("../Figs/eve_7.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,80,20,83,col=color[2],lend=1,ljoin=1)

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,83,20,86,col=color[4],lend=1,ljoin=1)

par(cex.axis=1.3, cex.lab=1.8, col.lab=color[1])
par(mar=c(5.1,5.1,4.1,0.1))
plot(y3[,c(1,4)], xlab="Mouse3598 islet expr", las=1,
     ylab="Mouse3599 liver expr")
mtext(side=3, line=1, "Mouse3599 liver vs Mouse3598 islet", cex=1.4)
dev.off()


pdf("../Figs/eve_8.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in islet", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,83,20,86,col=color[4],lend=1,ljoin=1)

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
rect(0,0,20,100, xpd=TRUE, col="gray40")
mtext(side=3,"expression in liver", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,80,20,83,col=color[2],lend=1,ljoin=1)

par(cex.axis=1.3, cex.lab=1.8, col.lab=color[1])
par(mar=c(5.1,5.1,4.1,0.1))
plot(y3[,c(2,3)], xlab="Mouse3599 islet expr", las=1,
     ylab="Mouse3598 liver expr")
mtext(side=3, line=1, "Mouse3598 liver vs Mouse3599 islet", cex=1.4)
dev.off()


pdf("../Figs/eve_9.pdf", width=6.5, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
par(las=1)
pairs(y3[,c(1,3,2,4)], upper.panel=panel.cor)
dev.off()

pdf("../Figs/eve_10.pdf", width=6.5, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
par(las=1)
pairs(y2[,c(1,3,2,4)], upper.panel=panel.cor)
dev.off()

pdf("../Figs/eve_11.pdf", width=6.5, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
par(las=1)
pairs(y4[,c(1,3,2,4)], upper.panel=panel.cor)
dev.off()


pdf("../Figs/eve_corr.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
par(mfrow=c(2,1), mar=c(5.1,0.1,0.1,0.1))
pd <- pulldiag(dil) 
hist(pd, breaks=seq(-1, 1, len=101), ylab="", xlab="islet : liver correlation", main="", yaxt="n")
rug(pd[pd<0.7], col=color[2], lwd=1.3)
u <- par("usr")
text(u[1]+diff(u[1:2])*0.05, mean(u[3:4]), "Self-self", col=color[1],
     adj=c(0,0.5), cex=1.5)
segments(-1, u[3], 1, u[3], xpd=TRUE, lend=1, ljoin=1)

od <- omitdiag(dil) 
hist(od, breaks=seq(-1, 1, len=101), ylab="", xlab="islet : liver correlation", main="", yaxt="n")
rug(od[od > 0.7], col=color[2], lwd=1.3)
u <- par("usr")
text(u[1]+diff(u[1:2])*0.05, mean(u[3:4]), "Self-nonself", col=color[1],
     adj=c(0,0.5), cex=1.5)
segments(-1, u[3], 1, u[3], xpd=TRUE, lend=1, ljoin=1)
dev.off()

rm(list=ls())

