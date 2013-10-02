######################################################################
# eQTL genotype vs expression
######################################################################

source("colors.R")

color[1] <- bgcolor


pdf("../Figs/gve_scheme.pdf", width=9, height=6, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="black", col="black", col.axis="black", col.lab="black", bg="white")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression traits", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"observed eQTL genotypes", line=1, cex=1.5)
mtext(side=1,"eQTL", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,24,100,29,col=color[2],lend=1,ljoin=1)


plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"inferred eQTL genotypes", line=1, cex=1.5)
mtext(side=1,"eQTL", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,64,100,69,col=color[4],lend=1,ljoin=1)
dev.off()

