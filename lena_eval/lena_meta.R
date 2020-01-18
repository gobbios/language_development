library(readxl)
xdata <- read_excel("lena_eval/data/systematic review LENA evaluations.xlsx", 
                    sheet = "data_final", na = "NA")
xdata <- data.frame(xdata)
xdata$studynum <- as.numeric(as.factor(xdata$paper_shortname))
xdata$selection <- NULL
xdata$notes <- NULL
xdata$more.info.on.comparison <- NULL
xdata$human_reliability <- NULL
xdata$more.info.on.human.coding <- NULL
xdata$more.info.on.children <- NULL
head(xdata)


# merge across all available recall metrics (per paper text)
xdata$recall <- rowMeans(xdata[, c("Adult_recall", "Child_recall", "CH_recall", "CX_recall", "FA_recall", "MA_recall")], na.rm = TRUE)
plot(xdata$age_m_m, xdata$recall)

xdata$precision <- rowMeans(xdata[, c("Adult_precision", "Child_precision", "CH_precision", "CX_precision", "FA_precision", "MA_precision")], na.rm = TRUE)
plot(xdata$age_m_m, xdata$precision)





xdata$NAE <- as.numeric(xdata$language == "NAE")

plot(xdata$NAE, xdata$recall)
plot(xdata$NAE, xdata$precision)

