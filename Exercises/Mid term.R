dat = read.table("Exercises/Dataset_BIOS/tephritis.txt", header=T)
dat$Patry = as.factor(dat$Patry)
dat$Hostplant = as.factor(dat$Hostplant)
dat$Sex = as.factor(dat$Sex)
dat$Baltic = as.factor(dat$Baltic)
head(dat)

anova(lm(OL ~ Hostplant, data = dat))

anova(lm(Wing_length ~ Sex + Hostplant, data = dat))


# Fit multiple linear models with different combinations of predictors

--------------------------------------------------------
m11 = lm(OL ~ Patry * Hostplant, data = dat)
m12 = lm(OL ~ Patry + Hostplant, data = dat)
m13 = lm(OL ~ Wing_length + Hostplant, data = dat)


mlist = list(m11, m12, m13)
AICTab = AIC(m11, m12, m13)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab
#-------------------------------------------------------------
m22 = lm(BL ~ Wing_length + Hostplant, data = dat)
m23 = lm(BL ~ Wing_length * Hostplant, data = dat)
m25 = lm(BL ~ Sex, data = dat)
m26 = lm(BL ~ Sex + Hostplant, data = dat)
m27 = lm(BL ~ Sex * Hostplant, data = dat)
m28 = lm(BL ~ 1, data = dat)
m29 = lm(BL ~ Patry, data = dat)
m221 = lm(BL ~ Patry + Hostplant, data = dat)


mlist = list(m22, m23, m25, m26, m27, m28, m29, m221)
AICTab = AIC(m22, m23, m25, m26, m27, m28, m29, m221)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

#-------------------------------------------------------------
m31 = lm(Wing_length ~ Patry, data = dat)
m32 = lm(Wing_length ~ Patry + Hostplant, data = dat)
m33 = lm(Wing_length ~ Patry * Hostplant, data = dat)
m34 = lm(Wing_length ~ Patry + Sex, data = dat)
m35 = lm(Wing_length ~ Patry * Sex, data = dat)


mlist = list(m31, m32, m33, m34, m35)
AICTab = AIC(m31, m32, m33, m34, m35)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab
