# population data
# Source:
# https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx
xdata <- read.csv("WEIRD publications and populations/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.csv", skip = 3)

xdata <- xdata[xdata$Reference.date..as.of.1.July. == "2010" & xdata$Type == "Region", ]
# xdata <- xdata[xdata$Reference.date..as.of.1.July. == "2010" & xdata$Type == "SDG region", ]
head(xdata)

xdata <- data.frame(region = xdata$Region..subregion..country.or.area.., below5 = xdata$X0.4, below10 = xdata$X5.9)
xdata$below5 <- as.numeric(gsub(pattern = " ", replacement = "", xdata$below5))
xdata$below10 <- as.numeric(gsub(pattern = " ", replacement = "", xdata$below10))
xdata$prop <- (xdata$below5 + xdata$below10) / sum(xdata$below5 + xdata$below10)
pop <- xdata


# subject data from Nielsen et al 2017
# https://doi.org/10.1016/j.jecp.2017.04.017
publ <- c(us = 57.65, engl = 17.95, eur = 14.92, afr = 0.63, sca = 0.7, asia = 4.36, mideast = 1.07, other = 2.4)/100
sum(publ)
# doesnt add to 1...

publ

pdata <- data.frame(area = c("North America", 
                             "Europe", 
                             "Africa", 
                             "Central/South America", 
                             "Asia"), 
                    pop = c(pop$prop[pop$region == "Northern America"], 
                            pop$prop[pop$region == "Europe"], 
                            pop$prop[pop$region == "Africa"],
                            pop$prop[pop$region == "Latin America and the Caribbean"],
                            pop$prop[pop$region == "Asia"]), 
                    pubs = c(sum(publ[c("us", "engl")]), 
                             publ["eur"], 
                             publ["afr"], 
                             publ["sca"],
                             publ["asia"]))
pdata <- pdata[c(1, 2, 5, 4, 3), ]
rownames(pdata) <- NULL
# regions don't match perfectly (e.g. UK)!

pdata$xcols <- hcl.colors(5, palette = "zissou1")


pdf(file = "WEIRD publications and populations/WEIRD pubs.pdf", 
    width = 7, height = 3.5)
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), family = "serif")
pie(pdata$pubs, col = pdata$xcols, labels = NA, border = NA, radius = 0.9)
points(0, 0, pch = 16, col = "white", cex = 20)
text(0, 0, "research\nsubjects", font = 2, cex = 1.5)
text(0, 0.7, "North America", cex = 0.8)
text(0.5, -0.5, "Europe", cex = 0.8)
pie(pdata$pop, col = pdata$xcols, labels = NA, border = NA, radius = 0.9)
points(0, 0, pch = 16, col = "white", cex = 20)
text(0, 0, "population", font = 2, cex = 1.5)
text(0, 0.7, "Asia", cex = 0.8)
text(0.5, -0.5, "Africa", cex = 0.8)
text(-0.15, -0.65, "Central/\nSouth America", cex = 0.8)
dev.off()
