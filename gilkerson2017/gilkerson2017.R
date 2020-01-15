# Gilkerson, J., Richards, J. A., Warren, S. F., Montgomery, J. K., Greenwood, C. R., Kimbrough Oller, D., ... & Paul, T. D. (2017). Mapping the early language environment using all-day recordings and automated analysis. American Journal of Speech-Language Pathology, 26(2), 248-265.
# https://doi.org/10.1044/2016_AJSLP-15-0169
# table 2


# LENA produced output averaged within age cohort

# reproduces their figure 1

xdata <- read.csv("gilkerson2017/gilkerson2017_tab2.csv")

par(family = "serif")
plot(xdata$age, xdata$adult_words, ylim = c(5000, 25000), pch = 16, 
     xlab = "age", ylab = "daily adult word count", las = 1)
segments(xdata$age, xdata$adult_words - xdata$adult_words_sd, 
         xdata$age, xdata$adult_words + xdata$adult_words_sd)
mtext(xdata$n, at = xdata$age, line = 0, cex = 0.4)

plot(xdata$age, xdata$child_vocs, ylim = c(0, 4000), pch = 16, 
     xlab = "age", ylab = "daily child vocalization count", las = 1)
segments(xdata$age, xdata$child_vocs - xdata$child_vocs_sd, 
         xdata$age, xdata$child_vocs + xdata$child_vocs_sd)
mtext(xdata$n, at = xdata$age, line = 0, cex = 0.4)

plot(xdata$age, xdata$conversational_turns, ylim = c(0, 1000), pch = 16, 
     xlab = "age", ylab = "daily conversational turns count", las = 1)
segments(xdata$age, xdata$conversational_turns - xdata$conversational_turns_sd, 
         xdata$age, xdata$conversational_turns + xdata$conversational_turns_sd)
mtext(xdata$n, at = xdata$age, line = 0, cex = 0.4)


