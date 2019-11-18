# Read in the data
setwd('/Users/edward/IODS-project/')
library(ggplot2)


alc <- as.data.frame(read.table('data/alc.csv',  sep="\t", header=TRUE))
glimpse(alc)
summary(alc[c('G3','failures','absences','studytime')])
p1 <- ggplot(alc, aes(x = high_use, y = G3, col=sex)) + geom_boxplot() + ylab('final grade') + xlab('high use')
p2 <- ggplot(alc, aes(failures)) + geom_bar(aes(fill = high_use), position = "dodge", stat="count") + xlab('Failure')
p3 <- ggplot(alc, aes(x = high_use, y = absences, col=sex)) + geom_boxplot() + ylab('Absence') + xlab('high use')
p4 <- ggplot(alc, aes(studytime)) + geom_bar(aes(fill = high_use), position = "dodge", stat="count")






m <- glm(high_use ~ failures + absences + studytime, data = alc, family = "binomial")
summary(m)
or <- exp(coef(m))
or


ci <- exp(confint(m))
cbind(or, ci)

cbind(exp(coef(m)), exp(confint(m)))


# Predict the probability.
prob <- predict(m, type = "response")
# Add the probabilities to alc.
alc <- mutate(alc, probability = prob)
# Calculate a logical high use value based on probabilites.
alc <- mutate(alc, prediction = probability > 0.5)
# Tabulate the target variable versus the predictions,
# with both absolute and proportional numbers.
tbl <- table(high_use = alc$high_use, prediction = alc$prediction)
addmargins(tbl)



round(addmargins(prop.table(tbl)), 2)



high_u <- as.data.frame(prop.table(table(alc$high_use)))
predic <- as.data.frame(prop.table(table(alc$prediction)))
pp1 <- ggplot(high_u, aes(Var1, Freq)) + geom_col(aes(fill = Var1)) + scale_y_continuous(limits = 0:1) + ylab('frequency') + xlab('observed high use') + theme(legend.position = 'none')
pp2 <- ggplot(predic, aes(Var1, Freq)) + geom_col(aes(fill = Var1)) + scale_y_continuous(limits = 0:1) + ylab('frequency') + xlab('predicted high use') + theme(legend.position = 'none')
multiplot(pp1, pp2, cols = 2)


mloss <- function(obs, prob) {
  res <- ifelse(prob > 0.5, 1, 0)
  mean(res != obs)
}
round(mloss(obs = alc$high_use, prob = alc$probability), 2)
