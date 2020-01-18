# data from https://github.com/jsalt-coml/lena_eval
# code adapted from there too
spreadsheet <- read.csv("lena_eval/data/metadata_aclew.csv", header = TRUE, sep = ",")
spreadsheet_tsi <- read.csv("lena_eval/data/metadata_tsi.csv", header = TRUE, sep = ",")
spreadsheet_tsi <- spreadsheet_tsi[, c("id", "age_mo")]
colnames(spreadsheet_tsi) <- c("child","age")
age_id <- rbind(spreadsheet, spreadsheet_tsi)


cvtc <- read.table("lena_eval/data/key_child_voc_file_level.csv", header = TRUE, sep=" ")
cvtc$child <- substr(cvtc$filename,1,8)
cvtc$child[substr(cvtc$child, 1, 1) == "C"] <- substr(cvtc$filename[substr(cvtc$child, 1, 1) == "C"], 1, 3)
cvtc$cor <- substr(cvtc$filename, 1, 3)
cvtc$cor[substr(cvtc$child, 1, 1) == "C"] <- "TSI"
xdata <- merge(cvtc, age_id, by = "child")

awc <- read.table("lena_eval/data/LENA_AWC_rel_v1_June.txt")
colnames(awc) <- c("filename", "gold", "LENA")
awc$filename <- gsub("LUC", "ROW", awc$filename)
awc$cor <- substr(awc$filename, 1, 3)
awc$child <- substr(awc$filename, 1, 8)
awc <- merge(awc, age_id, by = "child")



cor(xdata$gold_CV_count, xdata$lena_CV_count)
xd <- xdata[xdata$cor != "WAR", ]

pdf("lena_eval/lena_eval_child_vocs.pdf", width = 7, height = 4)
par(mfrow = c(2, 2), family = "serif", mar = c(3.1, 3.1, 1.1, 0.6), mgp = c(1.3, 0.2, 0))
for (i in sort(unique(xd$cor))) {
  pdata <- xd[xd$cor == i, ]
  pdata$gold_CV_count <- pdata$gold_CV_count + runif(nrow(pdata), -0.3, 0.3)
  pdata$lena_CV_count <- pdata$lena_CV_count + runif(nrow(pdata), -0.3, 0.3)
  plot(pdata$gold_CV_count, pdata$lena_CV_count, xlim = c(0, 60), ylim = c(0, 60), 
       pch = 16, col = grey(0, 0.4), cex = 1.4, las = 1, xpd = TRUE,
       xlab = "human annotation", 
       ylab = "LENA annotation",
       axes = FALSE)
  axis(1, mgp = c(0, 0.3, 0), tcl = -0.2, cex.axis = 0.8)
  axis(2, mgp = c(0, 0.4, 0), tcl = -0.2, cex.axis = 0.8, las = 1)
  box()
  abline(0, 1, lty = 3)
  if (i == "BER") lang <- "English (US)"
  if (i == "TSI") lang <- "Tsimané"
  if (i == "ROW") lang <- "English (UK)"
  if (i == "SOD") lang <- "English (CA)"
  xcor <- sprintf("%.2f", cor(pdata$gold_CV_count, pdata$lena_CV_count, method = "s"))
  title(main = lang, line = 0.3)
  mtext(text = bquote(italic(r[s])==.(xcor)), line = -1.2, cex = 0.7)
}
dev.off()

pdf("lena_eval/lena_eval_turns.pdf", width = 7, height = 4)
par(mfrow = c(2, 2), family = "serif", mar = c(3.1, 3.1, 1.1, 0.6), mgp = c(1.3, 0.2, 0))
for (i in sort(unique(xd$cor))) {
  pdata <- xd[xd$cor == i, ]
  xcor <- sprintf("%.2f", cor(pdata$gold_CV_count, pdata$lena_CV_count, method = "s"))
  pdata$gold_CTC_count <- pdata$gold_CTC_count + runif(nrow(pdata), -0.3, 0.3)
  pdata$lena_CTC_count <- pdata$lena_CTC_count + runif(nrow(pdata), -0.3, 0.3)
  plot(pdata$gold_CTC_count, pdata$lena_CTC_count, xlim = c(0, 50), ylim = c(0, 50), 
       pch = 16, col = grey(0, 0.4), cex = 1.4, las = 1, xpd = TRUE,
       xlab = "human annotation", 
       ylab = "LENA annotation",
       axes = FALSE)
  axis(1, mgp = c(0, 0.3, 0), tcl = -0.2, cex.axis = 0.8)
  axis(2, mgp = c(0, 0.4, 0), tcl = -0.2, cex.axis = 0.8, las = 1)
  box()
  abline(0, 1, lty = 3)
  if (i == "BER") lang <- "English (US)"
  if (i == "TSI") lang <- "Tsimané"
  if (i == "ROW") lang <- "English (UK)"
  if (i == "SOD") lang <- "English (CA)"
  title(main = lang, line = 0.3)
  mtext(text = bquote(italic(r[s])==.(xcor)), line = -1.2, cex = 0.7)
}
dev.off()


xd <- awc[awc$cor != "WAR", ]

pdf("lena_eval/lena_eval_adult_words.pdf", width = 7, height = 4)
par(mfrow = c(2, 2), family = "serif", mar = c(3.1, 3.1, 1.1, 0.6), mgp = c(1.3, 0.2, 0))
for (i in sort(unique(xd$cor))) {
  pdata <- xd[xd$cor == i, ]
  xcor <- sprintf("%.2f", cor(pdata$gold, pdata$LENA, method = "s"))
  pdata$gold <- pdata$gold + runif(nrow(pdata), -0.3, 0.3)
  pdata$LENA <- pdata$LENA + runif(nrow(pdata), -0.3, 0.3)
  plot(pdata$gold, pdata$LENA, xlim = c(0, 400), ylim = c(0, 400), 
       pch = 16, col = grey(0, 0.4), cex = 1.4, las = 1, xpd = TRUE,
       xlab = "human annotation", 
       ylab = "LENA annotation",
       axes = FALSE)
  axis(1, mgp = c(0, 0.3, 0), tcl = -0.2, cex.axis = 0.8)
  axis(2, mgp = c(0, 0.4, 0), tcl = -0.2, cex.axis = 0.8, las = 1)
  box()
  abline(0, 1, lty = 3)
  if (i == "BER") lang <- "English (US)"
  if (i == "TSI") lang <- "Tsimané"
  if (i == "ROW") lang <- "English (UK)"
  if (i == "SOD") lang <- "English (CA)"
  title(main = lang, line = 0.3)
  mtext(text = bquote(italic(r[s])==.(xcor)), line = -1.2, cex = 0.7)
}
dev.off()




# library(lme4)
# res <- glmer(gold_CV_count ~ lena_CV_count + (lena_CV_count||cor/child), data = xdata, family = poisson)
# summary(res)
