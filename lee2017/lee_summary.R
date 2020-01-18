# source: Lee et al 2017
# https://dx.doi.org/10.1016/j.infbeh.2017.02.006

xdata <- read.csv("lee2017/lee2017_tab1.csv")
xdata

offs <- c(-0.15, 0.15)

pdf("lee2017/lee_summary.pdf", width = 7, height = 3.8)
par(mfrow = c(1, 2), family = "serif", mar = c(3, 4, 1, 1))

pdata <- droplevels(xdata[xdata$role == "infant", ])
levels(pdata$behaviour)
pdata$behaviour <- factor(pdata$behaviour, levels = c("happy", "neutral", "distressed"))
pdata$xcol <- hcl.colors(2)
pdata$xcol <- hcl.colors(2, "zissou1")

plot(0, 0, type = "n", xlim = c(0.5, 3.5), ylim = c(0, 3), axes = FALSE, xlab = "",
     ylab = "rate per minute")
title(main = "infant")
points(as.numeric(pdata$behaviour) + offs, pdata$mean, pch = 16, col = pdata$xcol, cex = 2)
segments(as.numeric(pdata$behaviour) + offs, pdata$mean - pdata$sd, 
         as.numeric(pdata$behaviour) + offs, pdata$mean + pdata$sd, col = pdata$xcol, xpd = TRUE)
axis(2, las = 1, at = 0:3)
mtext(1, line = 0.5, at = 1:3, text = c("happy", "neutral", "distressed"), padj = 0.5)
legend("top", legend = c("researcher present", "researcher absent"), cex = 0.8,
       text.col = unique(pdata$xcol), ncol = 2, bty = "n")


pdata <- droplevels(xdata[xdata$role == "mother", ])
levels(pdata$behaviour)
pdata$behaviour <- factor(pdata$behaviour, levels = c("sensitive", "neutral", "less sensitive"))
pdata$xcol <- hcl.colors(2)
pdata$xcol <- hcl.colors(2, "zissou1")

plot(0, 0, type = "n", xlim = c(0.5, 3.5), ylim = c(0, 3), axes = FALSE, xlab = "",
     ylab = "", main = "mother")
points(as.numeric(pdata$behaviour) + offs, pdata$mean, pch = 16, col = pdata$xcol, cex = 2)
segments(as.numeric(pdata$behaviour) + offs, pdata$mean - pdata$sd, 
         as.numeric(pdata$behaviour) + offs, pdata$mean + pdata$sd, col = pdata$xcol, xpd = TRUE)
axis(2, las = 1, at = 0:3)
mtext(1, line = 0.5, at = 1:3, text = c("sensitive", "neutral", "less\nsensitive"), padj = 0.5)
legend("top", legend = c("researcher present", "researcher absent"), cex = 0.8,
       text.col = unique(pdata$xcol), ncol = 2, bty = "n")




dev.off()

