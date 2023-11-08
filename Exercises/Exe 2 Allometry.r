birds = read.csv("Exercises/Dataset_BIOS/bird_allometry.csv")
head(birds)

males = birds[birds$Sex=="m",]
females = birds[birds$Sex=="f",]

mm = lm(log(brain_mass)~log(body_mass), data=males)


xx = seq(min(log(males$body_mass)), max(log(males$body_mass)), 
         length.out=100)
yy = mm$coef[1] + mm$coef[2]*xx
par(mfrow=c(1,2))
plot(log(males$body_mass), log(males$brain_mass), 
     xlab="Body mass (log g)",
     ylab="Brain mass (log g)",
     las=1, pch=21, col="black", bg="lightblue")
lines(xx, yy, lwd=2)

summary(mm)

predvals = cf[1] + cf[2]*log(males$body_mass)

#segments(log(males$body_mass), log(males$brain_mass), log(males$body_mass), predvals)
hist(residuals(mm), xlab="", las=1)