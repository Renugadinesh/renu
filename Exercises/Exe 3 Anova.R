dev.new(width=5, height=5)
dat = read.csv("Exercises/Dataset_BIOS/butterflies.csv")
names(dat)
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")

means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
means

ses = tapply(dat$DevelopmentTime, 
             list(dat$MaternalHost, dat$LarvalHost), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))
ses

#Define layout of plot
dev.new(width=5, height=5)

#Plot the developmental time 
plot(c(0.97, 1.03), means[,1], ylim=c(18, 40), xlim=c(0.8, 2.2),  
     xlab="Larval host", 
     ylab="Developmental time (days)", 
     main = "Larval developmental time depending on larval and maternal host plant", 
     cex.main = 1, 
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))

#Plot the residuals/standard error
arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03), 
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03), 
       means[,2]+ses[,2], length=0.05, angle=90, code=3)

#Plot the lines
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])

points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")

#Create legends
legend("topleft", c("Maternal host", "Barbarea", "Berteroa"), 
       bty="n", pch=c(NA,21,16))

names(dat)
m = lm(DevelopmentTime~MaternalHost*LarvalHost, data=dat)
anova(m)

colMeans(means)
rowMeans(means)

#--------------------------------------------------------------------
dev.new(width=5, height=5)
dat = read.csv("Exercises/Dataset_BIOS/butterflies.csv")
names(dat)

dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")

means.gr = tapply(dat$GrowthRate, list(dat$MaternalHost, dat$LarvalHost), mean)
means.gr

ses.gr = tapply(dat$GrowthRate, 
             list(dat$MaternalHost, dat$LarvalHost), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))
ses.gr

plot(c(0.97, 1.03), means.gr[,1], ylim=c(0.02, 0.1), xlim=c(0.8, 2.2),  
     xlab="Larval host", 
     ylab="Growth rate (cm/day)", 
     main = "Larvae growth rate depending on larval and maternal host plant", 
     cex.main = 1, 
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))

#Plot the residuals/standard error
arrows(c(0.97,1.03), means.gr[,1]-ses.gr[,1], c(0.97,1.03), 
       means.gr[,1]+ses.gr[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means.gr[,2]-ses.gr[,2], c(1.97,2.03), 
       means.gr[,2]+ses.gr[,2], length=0.05, angle=90, code=3)

#Plot the lines
segments(0.97, means.gr[1,1], 1.97, means.gr[1,2], lty=2)
segments(1.03, means.gr[2,1], 2.03, means.gr[2,2])

points(c(0.97, 1.03), means.gr[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means.gr[,2], pch=c(21, 16), bg="white")

#Create legends
legend("topleft", c("Maternal host", "Barbarea", "Berteroa"), 
       bty="n", pch=c(NA,21,16))
names(dat)
mg = lm(GrowthRate~MaternalHost*LarvalHost, data=dat)
anova(mg)

colMeans(means.gr)
rowMeans(means.gr)


#----------------------------------------------------------------
# Plot for adult weight
dev.new(width=5, height=5)
dat = read.csv("Exercises/Dataset_BIOS/butterflies.csv")
names(dat)

dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")

means.aw = tapply(dat$AdultWeight, list(dat$MaternalHost, dat$LarvalHost), mean)
means.aw

ses.aw = tapply(dat$AdultWeight, 
                list(dat$MaternalHost, dat$LarvalHost), 
                function(x) sd(x)/sqrt(sum(!is.na(x))))
ses.aw

plot(c(0.97, 1.03), means.aw[,1], ylim=c(50, 70), xlim=c(0.8, 2.2),  
     xlab="Larval host", 
     ylab="Adult weight (g)", 
     main = "Adult weight depending on larval and maternal host plant", 
     cex.main = 1, 
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))

#Plot the residuals/standard error
arrows(c(0.97,1.03), means.aw[,1]-ses.aw[,1], c(0.97,1.03), 
       means.aw[,1]+ses.aw[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means.aw[,2]-ses.aw[,2], c(1.97,2.03), 
       means.aw[,2]+ses.aw[,2], length=0.05, angle=90, code=3)

#Plot the lines
segments(0.97, means.aw[1,1], 1.97, means.aw[1,2], lty=2)
segments(1.03, means.aw[2,1], 2.03, means.aw[2,2])

points(c(0.97, 1.03), means.aw[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means.aw[,2], pch=c(21, 16), bg="white")

#Create legends
legend("topleft", c("Maternal host", "Barbarea", "Berteroa"), 
       bty="n", pch=c(NA,21,16))

names(dat)
ma = lm(AdultWeight~MaternalHost*LarvalHost, data=dat)
anova(ma)

colMeans(means.aw)
rowMeans(means.aw)

