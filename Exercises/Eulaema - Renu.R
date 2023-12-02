library(MASS)
library(MuMIn)
dat = read.csv("Exercises/Dataset_BIOS/Eulaema.csv")
head(dat)

 x = dat$forest.
 y = dat$Eulaema_nigrita
eta = glm.nb(y~x,data = dat )
summary(eta)


plot(x*100,eta$fit, xlab = "Forest Cover (%)", ylab = "Eulaema nigrita abundance (log)")

dat<-na.omit(dat)
newForest <- runif(n=178,min=0,max=1)
predvals = predict(eta, newdata=list(forest.=newForest), type="response")

m2 = glm.nb(y~dat$MAP, data = dat)
summary(m2)
plot(dat$MAP,m2$fit, xlab = "MAP (mm)", ylab = "Eulaema nigrita abundance (log)")

m3 = glm.nb(y~dat$MAT, data = dat)
summary(m3)
plot(dat$MAT,m3$fit, xlab = "MAT(degree celcius) x 10", ylab = "Eulaema nigrita abundance (log)")

r.squaredGLMM(eta)
r.squaredGLMM(m2)
r.squaredGLMM(m3)