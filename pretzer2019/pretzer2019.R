# data from Pretzer et al 2019
# data from http://github.com/gpretzer/WWScripts
library(lme4)
xdata <- read.csv("pretzer2019//eventcodes1sec.csv")
head(xdata)

table(xdata$InfantID, xdata$Event2T)


pdf("pretzer2019/pretzer2019_fig.pdf", width = 7, height = 3.2)
par(mfrow = c(1, 2), family = "serif", mar = c(4, 4, 0.5, 0.1))
pdata <- aggregate(list(ids = xdata$Event2T), by = list(id = xdata$InfantID, can = xdata$Event1C), mean)

plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), yaxs = "i",
     xlab = "canonical", 
     ylab = "proportion adult IDS", axes = FALSE)
axis(2, las = 1)
box()
axis(1, at = c(0.25, 0.75), labels = c("no", "yes"), lwd = 0)
points(pdata$can * 0.5 + 0.25 + runif(nrow(pdata), -0.05, 0.05), 
       pdata$ids, pch = 16, cex = 2, col = grey(0, 0.6), xpd = TRUE)
pd <- tapply(pdata$ids, pdata$can, median)
segments(c(0.25, 0.75) - 0.07, pd, c(0.25, 0.75) + 0.07, pd, col = "red", lwd = 6)


pdata <- aggregate(list(ids = xdata$Event2T), by = list(id = xdata$InfantID, refl = xdata$Event1R), mean)

plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), yaxs = "i", 
     xlab = "reflexive (laugh, cry)", 
     ylab = "", axes = FALSE)
axis(2, las = 1)
box()
axis(1, at = c(0.25, 0.75), labels = c("no", "yes"), lwd = 0)
points(pdata$refl * 0.5 + 0.25 + runif(nrow(pdata), -0.05, 0.05), 
       pdata$ids, pch = 16, cex = 2, col = grey(0, 0.6), xpd = TRUE)
pd <- tapply(pdata$ids, pdata$refl, median)
segments(c(0.25, 0.75) - 0.07, pd, c(0.25, 0.75) + 0.07, pd, col = "red", lwd = 6)

dev.off()



# Regression Models with IDS or ODS as Event 2
# res <- glmer(Event2T ~ (1|InfantID) + Event1C + Event1X + Event1R, data =xdata, family=binomial())
# summary(res)

