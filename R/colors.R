color <- c(rgb(102,203,254,maxColorValue=255), # light blue
           rgb(254,  0,128,maxColorValue=255), # hot pink
           rgb(254,102,254,maxColorValue=255), # pink
           rgb(102,254,102,maxColorValue=255), # green
           rgb(128,  0,128,maxColorValue=255), # purple
           rgb(203,102,254,maxColorValue=255), # light purple
           rgb(254,203,102,maxColorValue=255), # yellow
           rgb(  0,128,128,maxColorValue=255)) # dark blue

#bgcolor <- rgb(0, 0, 80, maxColorValue=255)

# text
color2 <- c(rgb(255, 255, 102, maxColorValue=255), # yellow
            rgb(102, 204, 255, maxColorValue=255), # light blue
            rgb(255, 102, 255, maxColorValue=255)) # pink

#par(mar=rep(0.1,4),las=1,fg="white",col="white",col.axis="white",col.lab="white",
#    bg=bgcolor, bty="n")
#plot(0,0,type="n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(0,90), ylim=c(0,90))
#
#for(i in 1:8)
#  abline(h=i*10,col=color[i],lwd=10)
#
#text(45, 85, "This is yellow", col=color2[1], cex=1.5)
#text(45, 75, "This is blue", col=color2[2], cex=1.5)
#text(45, 65, "This is pink", col=color2[3], cex=1.5)

# redo colors for poster
bgcolor <- rgb(20, 66, 129, maxColorValue=255)
color[2] <- rgb(224, 0, 0,  maxColorValue=255)
color[4] <- rgb(98,158,31,  maxColorValue=255)
