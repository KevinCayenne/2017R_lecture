setwd("C:/Users/acer/Desktop/R²Î­pHW/ZH's")

library(ggplot2)

try1 <- read.csv("Sleep48¤Hª©.csv")

try.df <- data.frame(try1)
try.df <- try.df[-c(10),]

lm.try <- lm(try.df$PSQITotal_1 ~ try.df$rest_NLnu_1 * try.df$rest_NHnu * try.df$rest_LFHF, data = try.df)
lm.try2 <- lm(try.df$SLPQUAL_1 ~ try.df$rest_NLnu_1 * try.df$rest_NHnu * try.df$rest_LFHF, data = try.df)

summary(lm.try)
summary(lm.try2)

ggplot(try.df, aes(PSQITotal_1, rest_NLnu_1)) + geom_point() + geom_smooth(method = "lm")
ggplot(try.df, aes(PSQITotal_1, rest_NHnu)) + geom_point() + geom_smooth(method = "lm")
ggplot(try.df, aes(PSQITotal_1, rest_LFHF)) + geom_point() + geom_smooth(method = "lm")

resNL <- cor.test(try.df$PSQITotal_1 , try.df$rest_NLnu_1 , method = "pearson")
resNH <- cor.test(try.df$PSQITotal_1 , try.df$rest_NHnu , method = "pearson")
resLFHF <- cor.test(try.df$PSQITotal_1 , try.df$rest_LFHF , method = "pearson")

ggplot(try.df, aes(try.df$SLPQUAL_1, rest_NLnu_1)) + geom_point() + geom_smooth(method = "lm")
ggplot(try.df, aes(try.df$SLPQUAL_1, rest_NHnu)) + geom_point() + geom_smooth(method = "lm")
ggplot(try.df, aes(try.df$SLPQUAL_1, rest_LFHF)) + geom_point() + geom_smooth(method = "lm")

resNL <- cor.test(try.df$SLPQUAL_1 , try.df$rest_NLnu_1 , method = "pearson")
resNH <- cor.test(try.df$SLPQUAL_1 , try.df$rest_NHnu , method = "pearson")
resLFHF <- cor.test(try.df$SLPQUAL_1 , try.df$rest_LFHF , method = "pearson")