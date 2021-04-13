###################################################
### Adatok és függvények beolvasása.
###################################################
lepke = read.table("lepke.txt",header=T)
###################################################
### 339. oldal 
###################################################
lepke[1:5,]

###################################################
### 339. oldal 
###################################################
library(tables)
library(sciplot)
tabular( (HOM * TAP) ~ (n=1) + Format(digits=2)*
(BABTOMEG)*(mean + median+ sd + se), data=lepke )

###################################################
### 340. oldal, 11.1. ábra 
###################################################
with(lepke,boxplot(BABTOMEG~TAP:HOM, xlab="Kezelési csoportok",
ylab="Bábtömeg (g)",names=c("AH","LH","AM","LM","AS","LS")))

###################################################
### 340. oldal; 341. oldal, 11.2. ábra 
###################################################
library(sciplot)
lineplot.CI(HOM, BABTOMEG, group = TAP, data = lepke, cex = 1,bty="l",
ylab = "Bábtömeg (g; átlag ± SE)", xlab = "Hőmérsékleti kezelés",
cex.lab = 1, x.leg = 2.3, y.leg=0.36,las=1, leg.lab=c("ad libitum", "limitált"),
xaxt="n", trace.label="Táplálék", fixed=F,type="p",
pch = c(16,17),xlim=c(0.7,3.2),ylim=c(0.18,0.38))
axis(1,at=1:3,labels=c("Hűtött","Melegített","Szobahő"))

bargraph.CI(HOM, BABTOMEG, group = TAP, data = lepke, cex = 1,bty="l", split=F,
ylab = "Bábtömeg (g; átlag ± SE)", xlab = "Hőmérsékleti kezelés", col = "black", angle = 45,
cex.lab = 1, x.leg = 5.5, y.leg=0.42,las=1, leg.lab=c("ad libitum", "limitált"),
ylim=c(0,0.45),density = c(0,20), legend = TRUE,xaxt="n")
axis(1,at=c(2,5,8),labels=c("Hűtött","Melegített","Szobahő"))

###################################################
### 341. oldal; 342. oldal 11.3. ábra 
##################################################
with(lepke,interaction.plot(HOM,TAP,BABTOMEG,
xlab="Hőmérsékleti kezelés", ylab="Bábtömeg (g; átlag)",
trace.label="", xaxt="n",legend=F,ylim=c(0.17,0.33)))
legend(2.5,0.28,lty=1:2,legend=c("ad libitum", "limitált"),
bty="n",title="Táplálék")
axis(1,at=1:3,labels=c("Hűtött","Melegített","Szobahő"))

###################################################
### 341. oldal; 342. oldal, 11.4. ábra 
###################################################
with(lepke,boxplot(TOMEG0~TAP:HOM,
names=c("AH","LH","AM","LM","AS","LS"),
xlab="Kezelési csoportok",
ylab="Kezdeti hernyótömeg (g)"))

###################################################
### 342. oldal; 343. oldal 11.5. ábra
###################################################
with(lepke,coplot(BABTOMEG~TOMEG0|TAP*HOM, las=0,
xlab=c("Kezdeti hernyótömeg (g)"
,"Táplálékkezelés"), ylab=c("Bábtömeg (g)",
paste("Hőmérsékleti kezelés"))))

###################################################
### 342. oldal; 343. oldal 11.6. ábra
###################################################
library(car)
scatterplot(BABTOMEG~TOMEG0|TAP,data=lepke,by.groups=T,
smooth=T,col=rep(1,2),xlab="Kezdeti hernyótömeg (g)", 
ylab="Bábtömeg (g)", legend.plot=F)
legend("topright",legend=c("ad libitum", "limitált"), 
title="Táplálék", bty="n")

###################################################
### 351. oldal, 11.1. példa
###################################################
(lmmod1 = lm(BABTOMEG~HOM,data=lepke))

(lmmod2 = lm(BABTOMEG~HOM-1,data=lepke))

###################################################
### 352. oldal, 11.2. példa
###################################################
(lmmod3a = lm(BABTOMEG~TAP+HOM,data=lepke))

(lmmod3b = lm(BABTOMEG~TAP+HOM-1,data=lepke))

###################################################
### 352-353. oldal, 11.3. példa
###################################################
(lmmod4 = lm(BABTOMEG~TAP+HOM+TAP:HOM,data=lepke))

(lmmod5 = lm(BABTOMEG~TAP:HOM-1,data=lepke))

###################################################
### 354. oldal, 11.4. példa, 355. oldal 11.8. ábra
###################################################
(lmmod6 = lm(BABTOMEG~TOMEG0+TAP,data=lepke))

(lmmod7 = lm(BABTOMEG~TOMEG0+TAP-1,data=lepke))

with(lepke,plot(BABTOMEG~TOMEG0,pch=as.numeric(TAP),
xlab="Kezdeti hernyótömeg (g)", ylab="Bábtömeg (g)"))
legend(0.05,0.3,pch=1:2,lty=1:2,title="Táplálék",
legend=c("ad libitum","limitált"),bty="n")
abline(coef(lmmod7)[[2]],coef(lmmod7)[[1]])
abline(coef(lmmod7)[[3]],coef(lmmod7)[[1]],lty=2)

###################################################
### 355-356. oldal, 11.5. példa, 356. oldal, 11.9. ábra
###################################################
(lmmod8 = lm(BABTOMEG~TOMEG0*TAP,data=lepke))

(lmmod9 = lm(BABTOMEG~TOMEG0:TAP+TAP-1,data=lepke))

with(lepke,plot(BABTOMEG~TOMEG0,pch=as.numeric(TAP),
xlab="Kezdeti hernyótömeg (g)", ylab="Bábtömeg (g)"))
legend(0.05,0.3,pch=1:2,lty=1:2,title="Táplálék",
legend=c("ad libitum","limitált"),bty="n")
abline(coef(lmmod9)[[1]],coef(lmmod9)[[3]])
abline(coef(lmmod9)[[2]],coef(lmmod9)[[4]],lty=2)

###################################################
### 357. oldal, 11.6. példa
###################################################
str(lepke)

###################################################
### 362-363. oldal, 11.7. példa
###################################################
teljesmod = lm(BABTOMEG~TOMEG0*TAP*HOM,data=lepke)
summary(teljesmod)

nullmod = lm(BABTOMEG~1,data=lepke)
anova(teljesmod,nullmod)

###################################################
### 363.-364. oldal, 11.8. példa 
###################################################
teljesmod = lm(BABTOMEG~TOMEG0*TAP*HOM,data=lepke)
anova(teljesmod)

###################################################
### 364. oldal, 11.9. példa
###################################################
teljesmod = lm(BABTOMEG~TOMEG0*TAP*HOM,data=lepke)
reszmod = lm(BABTOMEG~TOMEG0*TAP,data=lepke)
anova(teljesmod,reszmod)

###################################################
### 365.-366. oldal, 11.10. példa
###################################################
lmmod10 = lm(BABTOMEG~TOMEG0+TAP*HOM,data=lepke)
confint(lmmod10)

lmmod11 = lm(BABTOMEG~TOMEG0+TAP:HOM-1,data=lepke)
confint(lmmod11)

###################################################
### 366. oldal, 11.11. példa; 367. oldal, 11.12. ábra
###################################################
redmod = lm(BABTOMEG~TOMEG0*TAP,data=lepke)
par (mfrow=c(2,2))
plot(redmod)
par(mfrow=c(1,1))

###################################################
### 368. oldal, 11.12. példa; 369. oldal, 11.13. ábra
###################################################
oz = read.table("oz.csv",sep=";",header=T)
ozmod = lm(TOMEG~SEX+TESTH,data=oz)
anova(ozmod)
plot(ozmod,1,pch=20)

oz$TESTHfaktor = cut(oz$TESTH,breaks=c(50,80,110,130),
labels=c("kicsi","közepes","nagy"))
ozmodkat = lm(TOMEG~SEX+TESTH+TESTHfaktor,data=oz)
anova(ozmodkat)
plot(ozmodkat,1,pch=20)

###################################################
### 372-373. oldal, 11.13. példa 
###################################################
lmmod14 = lm(BABTOMEG~TOMEG0*TAP,data=lepke)
lmmod15 = lm(BABTOMEG~TOMEG0:TAP+TAP-1,data=lepke)
lmmod3 = lm(BABTOMEG~TOMEG0+TAP,data=lepke)

extractAIC(lmmod14)
extractAIC(lmmod15)
extractAIC(lmmod3)

(BICmod14 = extractAIC(lmmod14, k=log(nrow(lepke))))
(BICmod15 = extractAIC(lmmod15, k=log(nrow(lepke))))
(BICmod3 = extractAIC(lmmod3, k=log(nrow(lepke))))

anova(lmmod14,lmmod3)

###################################################
### 374-375. oldal, 11.14. példa
###################################################
teljesmod = lm(BABTOMEG~TOMEG0*TAP*HOM,data=lepke)
(redmod = step(teljesmod,trace=0))
anova(teljesmod,redmod)

drop1(teljesmod,test="F")

###################################################
### 377-379. oldal, 11.15. példa 
###################################################
magassag = c(60, 58, 65, 64, 57, 55, 57, 58, 54, 46, 50, 48,
62, 57, 60, 61, 55, 54, 52, 60, 45, 44, 42, 48)
tapoldat = rep(rep(c("tomeny","hig","viz"),rep(4,3)),2)
fajta = rep(1:2,rep(12,2))
fajta = factor(fajta)
adat = data.frame(fajta,tapoldat,magassag)

tapmod1 = lm(magassag~tapoldat+fajta,data=adat)
anova(tapmod1)

adat$tapoldat = relevel(adat$tapoldat,ref="viz")
tapmod2 = lm(magassag~tapoldat+fajta,data=adat)

library(multcomp)
confint(glht(tapmod2,linfct=mcp(tapoldat="Dunnett")))
summary(glht(tapmod2,linfct=mcp(tapoldat="Dunnett")))

confint(glht(tapmod1,linfct=mcp(tapoldat="Tukey")))
summary(glht(tapmod1,linfct=mcp(tapoldat="Tukey")))

###################################################
### Adatok betöltése
###################################################
lile = read.table("lilemeret.txt",header=T,sep=";")

###################################################
### 381-382. oldal, 11.16. példa, 11.14. ábra
###################################################
library(sciplot)
library(tables)
library(car)

lineplot.CI(hely, tomeg, group = ivar, data = lile,
ylab = "Tömeg (g; átlag ± SE)", xlab = "Hely",
x.leg = 2.6, y.leg = 43.5, leg.lab = c("Tojó", "Hím"),
trace.label="Ivar", fixed=F,type="p",err.lty = 1:2,
pch = 1:2, xlim=c(0.7,3.2),ylim=c(38,44))

scatterplot(tomeg ~ jszarny|ivar,
smooth=T,col=rep(1,2), xlab = "Jobb szárny hossza (mm)",
ylab = "Tömeg (g)", legend.plot = F)
legend("topleft", legend = c("Tojó", "Hím"), title = "Ivar",
bty = "n"))

tabular( (hely*ivar) ~ (n=1) + Format(digits=2)*
(tomeg)*(mean +

###################################################
### 383. oldal, 11.17. példa; 384. oldal, 11.15. ábra
###################################################
lilemod = lm(tomeg~jszarny*hely+ivar,data=lile)
anova(lilemod)

par (mfrow=c(2,2))
plot(lilemod)
par(mfrow=c(1,1))


###################################################
### 383-386. oldal, 11.18. példa, 
### 387. oldal, 11.16. ábra
###################################################
(mod1 = lm(tomeg~hely+ivar,data=lile))

Kontmat1 = matrix(0,3,4,dimnames=list(c("B","C","D"),
c("(Intercept)", "helyC", "helyD", "ivarT")))
Kontmat1[1,]=c(1,0,0,1/2)
Kontmat1[2,]=c(1,1,0,1/2)
Kontmat1[3,]=c(1,0,1,1/2)
Kontmat1

library(gmodels)
(estmod1 = estimable(mod1,Kontmat1,conf.int=.95))
estmod1[, c(1, 2, 6, 7)]

(mod2 = lm(tomeg~hely+ivar-1,data=lile))

Kontmat2 = matrix(0,3,4,dimnames=list(c("B","C","D"),
c("helyB", "helyC", "helyD", "ivarT")))
Kontmat2[1,]=c(1,0,0,1/2)
Kontmat2[2,]=c(0,1,0,1/2)
Kontmat2[3,]=c(0,0,1,1/2)

(estmod2 = estimable(mod2,Kontmat2,conf.int=.95))

library(gplots)
plotCI(estmod2[,1],uiw=estmod2[,7]-estmod2[,1],
xlim=c(0,4),xaxt="n",
bty="l",ylab="Átlagbecslések és konfidencia-intervallumok",
xlab="Helyek",ylim=c(37,43))
axis(side=1, at=1:3, labels=c("B","C","D"), cex=0.7)


###################################################
### 386-387. oldal, 1.19. példa
###################################################
Kontmat3 = matrix(0,3,4,dimnames=list(c("C-B","D-B","C-D"),
c("helyB", "helyC", "helyD", "ivarT")))

Kontmat3[1,]=c(-1,1,0,0)
Kontmat3[2,]=c(-1,0,1,0)
Kontmat3[3,]=c(0,1,-1,0)

(estmod3 = estimable(mod2,Kontmat3,conf.int=.95))

p.adjust(estmod3[,5], method = c("fdr"))

source("tk.adjust.r")
tk.adjust(estmod3[, c(1, 2, 4)])

tk.adjust(estmod3[, c(1, 2, 4)], conf.int = 0.95)
cbind(estmod3[, c(1, 2)], tk.adjust(estmod3[, c(1, 2, 4)], 
conf.int = 0.95))

###################################################
### 388. oldal, 11.20. példa 
###################################################
(mod4 = lm(tomeg~hely+ivar+jszarny-1,data=lile))

atlagJszarny = mean(lile$jszarny)

Kontmat4 = matrix(0,3,5,dimnames=list(c("B","C","D"),
c("helyB", "helyC", "helyD", "ivarT", "jszarny")))
Kontmat4[1,]=c(1,0,0,1/2,atlagJszarny)
Kontmat4[2,]=c(0,1,0,1/2,atlagJszarny)
Kontmat4[3,]=c(0,0,1,1/2,atlagJszarny)

(estmod4 = estimable(mod4,Kontmat4,conf.lev=.95))

###################################################
### 388-389. oldal, 11.21. példa
###################################################
mod5 = lm(tomeg~hely:ivar-1+jszarny,data=lile)
ci(mod5)

(mod6 = lm(tomeg~hely+ivar+hely:jszarny-1,data=lile))

Kontmat6 = matrix(0,3,7,dimnames=list(c("B","C","D"),
c("helyB", "helyC", "helyD", "ivarT", "helyB:jszarny",
"helyC:jszarny", "helyD:jszarny" )))
Kontmat6[1,]=c(1,0,0,1/2,100,0,0)
Kontmat6[2,]=c(0,1,0,1/2,0,100,0)
Kontmat6[3,]=c(0,0,1,1/2,0,0,100)

(estmod6 = estimable(mod6,Kontmat6,conf.lev=.95))

Kontmat7 = matrix(0,3,7,dimnames=list(c("C-B","D-B","D-C"),
c("helyB", "helyC", "helyD", "ivarT", "helyB:jszarny",
"helyC:jszarny", "helyD:jszarny" )))
Kontmat7[1,]=c(-1,1,0,0,-100,100,0)
Kontmat7[2,]=c(-1,0,1,0,-100,0,100)
Kontmat7[3,]=c(0,-1,1,0,0,-100,100)

(estmod7 = estimable(mod6,Kontmat7,conf.lev=.95))

###################################################
### 390. oldal, 11.22. példa
###################################################

> Kontmat8 = matrix(0, 3, 7, dimnames = list(c("C-B", "D-B", "D-C"), c("helyB", "helyC", "helyD", "ivarT",
"helyB:jszarny", "helyC:jszarny", "helyD:jszarny")))
Kontmat8[1, ] = c(0, 0, 0, 0, -1, 1, 0)
Kontmat8[2, ] = c(0, 0, 0, 0, -1, 0, 1)
Kontmat8[3, ] = c(0, 0, 0, 0, 0, -1, 1)
(estmod8 = estimable(mod6, Kontmat8, conf.lev = 0.95))








