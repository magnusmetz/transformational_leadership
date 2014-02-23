## @knitr load_data
library(foreign)
data <- read.spss("/home/magnus/Dropbox/SS13/Psychological_study_in_sport_management/DATA//DATA_PsychExperiment.sav", to.data.frame=T, use.value.labels=T)

## @knitr gender
neutral <- subset(data, group == "neutral")
positive <- subset(data, group == "positive")
gen_neut <- table(neutral$Q8.0)
gen_pos <- table(positive$Q8.0)
par(mar=c(5,0,0,0))
pie(gen_neut, col=c("white", "black"), cex.main=1.5, cex.lab=2, cex.axis=1.5, cex=2, ylim=c(0,100),xlab="Neutral group")
pie(gen_pos, col=c("white", "black"), cex.main=1.5, cex.lab=2, cex=2, cex.axis=1.5,  ylim=c(0,100),xlab="Positive group")


## @knitr emotions
neutral_emotion <- with(data = neutral, apply(cbind(Q2.1, Q2.2, Q2.3, Q2.4, Q2.5, Q2.6, Q2.7, Q2.8, Q2.9, Q2.10, Q2.11, Q2.12, Q2.13, Q2.14, Q2.15, Q2.16, Q2.17, Q2.18, Q4.0), 2, mean, na.rm=T))
positive_emotion <- with(data = positive, apply(cbind(Q2.1, Q2.2, Q2.3, Q2.4, Q2.5, Q2.6, Q2.7, Q2.8, Q2.9, Q2.10, Q2.11, Q2.12, Q2.13, Q2.14, Q2.15, Q2.16, Q2.17, Q2.18, Q4.0), 2, mean, na.rm=T))
emotions <- rbind(neutral_emotion, positive_emotion)
colnames(emotions) <- c("amusement", "anger", "anxiety", "confusion", "contempt", "disgust", "embarassment", "fear", "guilt", "happiness", "interest", "joy", "love", "pride", "sadness", "shame", "surprise", "unhappiness", "pleasantness")
par(mar=c(11,4.1,4.1,2.1))
barplot(emotions, beside=T, col=c("#FF9F00", "#0053A4"), las=3, cex.axis=1.5, cex.names=1.5, cex.main=1.5, cex.lab=1.5, legend.text=c("neutral", "positive"),  ylim=c(0,8), main="Amount of each emotion experienced, while watching the film", ylab="Value on likert scale")

## @knitr boxplots
par(mar=c(5, 4.5, 4, 2))
d2_completed <- cbind(neutral$Q11.1.1, positive$Q11.1.1)
colnames(d2_completed) <- c("neutral", "positive")
boxplot(d2_completed, col=c("orange", "#0053A4"), cex.axis=2, cex.names=2, cex.main=2, cex.lab=2, ylab="Percent", ylim=c(0,100))
d2_choice <- cbind(neutral$Q11.1.2, positive$Q11.1.2)
colnames(d2_choice) <- c("neutral", "positive")
boxplot(d2_choice, col=c("#FF9F00", "#0053A4"), cex.axis=2, cex.names=2, cex.main=2, cex.lab=2, ylab="Percent",  ylim=c(0,100))

## @knitr completed_sets
neutral_d2_completed_sets <- with(data=neutral, apply(cbind(Q11.2.1.1, Q11.2.2.1, Q11.2.3.1, Q11.2.4.1, Q11.2.5.1, Q11.2.6.1, Q11.2.7.1, Q11.2.8.1, Q11.2.9.1, Q11.2.10.1, Q11.2.11.1, Q11.2.12.1, Q11.2.13.1, Q11.2.14.1), 2, mean))
positive_d2_completed_sets <- with(data=positive, apply(cbind(Q11.2.1.1, Q11.2.2.1, Q11.2.3.1, Q11.2.4.1, Q11.2.5.1, Q11.2.6.1, Q11.2.7.1, Q11.2.8.1, Q11.2.9.1, Q11.2.10.1, Q11.2.11.1, Q11.2.12.1, Q11.2.13.1, Q11.2.14.1), 2, mean))

d2_completed_sets <- rbind(neutral_d2_completed_sets, positive_d2_completed_sets)
colnames(d2_completed_sets) <- seq(1, 14)
par(mar=c(5,4.5,4,2))
barplot(d2_completed_sets, beside=T, col=c("#FF9F00", "#0053A4"), cex.main=1.5, cex.lab=1.5, cex.axis=1.5, cex.names=1.5, ylim=c(0,100), main="Completion per sequence",xlab="Sequence in D2 test", ylab="Percent", legend.text=c("neutral", "positive"))

## @knitr choice_sets
neutral_d2_choice_sets <- with(data=neutral, apply(cbind(Q11.2.1.2, Q11.2.2.2, Q11.2.3.2, Q11.2.4.2, Q11.2.5.2, Q11.2.6.2, Q11.2.7.2, Q11.2.8.2, Q11.2.9.2, Q11.2.10.2, Q11.2.11.2, Q11.2.12.2, Q11.2.13.2, Q11.2.14.2), 2, mean))
positive_d2_choice_sets <- with(data=positive, apply(cbind(Q11.2.1.2, Q11.2.2.2, Q11.2.3.2, Q11.2.4.2, Q11.2.5.2, Q11.2.6.2, Q11.2.7.2, Q11.2.8.2, Q11.2.9.2, Q11.2.10.2, Q11.2.11.2, Q11.2.12.2, Q11.2.13.2, Q11.2.14.2), 2, mean))

d2_choice_sets <- rbind(neutral_d2_choice_sets, positive_d2_choice_sets)
colnames(d2_choice_sets) <- seq(1, 14)
par(mar=c(5,4.5,4,2))
barplot(d2_choice_sets, beside=T, col=c("#FF9F00", "#0053A4"), cex.main=1.5, cex.lab=1.5, cex.axis=1.5, cex.names=1.5, ylim=c(0,100), main="Correct choices per sequence",xlab="Sequence in D2 test", ylab="Percent")

## @knitr stat
TP_neut <- neutral$Q11.1.1
TP_pos <- positive$Q11.1.1

## @knitr stat2
mean_neut <- mean(TP_neut)
sd_neut <- sd(TP_neut)
mean_pos <- mean(TP_pos)
sd_pos <- sd(TP_pos)

## @knitr norm_density
par(mar=c(5,5,4,2))
plot(density(TP_neut), col="#FF9F00", cex.main=2, cex.lab=2, cex.axis=2, main="Density plot of both groups", ylim=c(0,0.035))
lines(density(TP_pos), col="#0053A4")

## @knitr qqplot_neut
par(mar=c(5,5,4,2))
qqnorm(TP_neut, col="#FF9F00", cex.main=2, cex.lab=2, cex.axis=2, cex=2, main="Normal Q-Q Plot of the neutral group"); qqline(TP_neut, col="black")


## @knitr qqplot_pos
par(mar=c(5,5,4,2))
qqnorm(TP_pos, col="#0053A4", cex.main=2, cex.lab=2, cex.axis=2, cex=2,  main="Normal Q-Q Plot of the positive group"); qqline(TP_pos, col="black")

## @knitr t_test
t.test(TP_neut, TP_pos, alternative="less")
