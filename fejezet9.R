setwd("/home/andrea/work/biostat130730/biostat-R")
###################################################
### Adatbeolvasás
###################################################
regr.kurz = read.table("regr.kurz.csv",sep=";",header=T)
regr.kurz = regr.kurz[,1:5]

###################################################
### 255. oldal, 9.2. példa, 256.oldal 9.2. ábra
###################################################
(regmod1 = lm(TOMEG~SZULTOMEG,data=regr.kurz))

with(regr.kurz,plot(TOMEG~SZULTOMEG,pch=20, 
xlab="Születéskori testtömeg (g)",ylab="Testtömeg (kg)"))
abline(regmod1,lty=2)

###################################################
### 260. oldal, 9.3. példa
###################################################
summary(regmod1)

###################################################
### 261. oldal
###################################################
pred.frame  =  data.frame(SZULTOMEG = 4000)
predict(regmod1, newdata = pred.frame)

confint(regmod1)

###################################################
### 262. oldal
###################################################
x  =  data.frame(SZULTOMEG = 4000)
predict(regmod1, newdata = x, int = "confidence")
predict(regmod1, newdata = x, int = "prediction")

###################################################
### 263. oldal, 9.4. példa, 9.4. ábra
###################################################
x = data.frame(SZULTOMEG=2500:4500)
konf.sav = predict(regmod1,int="confidence",newdata=x)
pred.sav = predict(regmod1,int="prediction",newdata=x)
with(regr.kurz,plot(TOMEG~SZULTOMEG,xlim=c(2500,5000),
ylim=c(40,100),
xlab="Születéskori testtömeg (g)",ylab="Testtömeg (kg)"))
matlines(x$SZULTOMEG,konf.sav,lty=c(1,2,2),col=1,lwd=2)
matlines(x$SZULTOMEG,pred.sav,lty=c(1,3,3),col=1,lwd=2)

###################################################
### 264-265. oldal, 9.5. példa
###################################################
(regorigo = lm(TOMEG~SZULTOMEG-1,data=regr.kurz))
summary(regorigo)$r.squared

###################################################
### 266-268. oldal, 9.6. példa, 9.6. ábra
###################################################
lile = read.csv2("lile.csv",dec=".",header=T)

library(smatr)
OLSreg = with(lile, line.cis(jszarny, jcsud, method = "OLS"))

MAreg = with(lile, line.cis(jszarny, jcsud, method = "MA"))
SMAreg = with(lile, line.cis(jszarny, jcsud, method = "SMA"))

with(lile,plot(jcsud,jszarny, main="",xlab="Jobb csüdhossz (mm)",
ylab="Jobb szárnyhossz (mm)"))
abline(OLSreg$coef, lwd=2)
abline(MAreg$coef, lty=2, lwd=2)
abline(SMAreg$coef, lty=3,lwd=2)
legend(30,102, lty=1:3,legend=c("OLS","MA","SMA"),bty="n")

###################################################
### 269. oldal, 9.7. péda 
###################################################
tabla = with(regr.kurz,data.frame(TOMEG,MAGASSAG,SZULTOMEG))
plot(tabla)

###################################################
### 270-271. oldal, 9.8. példa
###################################################
(regmod2 = lm(TOMEG~MAGASSAG+SZULTOMEG,data=regr.kurz))

###################################################
### 271. oldal, 9.8. ábra
###################################################
library(scatterplot3d)
s3d = with(regr.kurz,scatterplot3d(MAGASSAG,SZULTOMEG,
TOMEG, type = "p",angle = 55, scale.y = 0.7, pch = 16, 
main ="",box=F, xlab="Magasság (cm)",
ylab="Születéskori testtömeg (g)        ", 
zlab="Testtömeg (kg)" ))
s3d$plane3d(regmod2, lty.box = "solid")
orig  =  with(regr.kurz,s3d$xyz.convert(MAGASSAG,
SZULTOMEG,TOMEG))
plane  =  with(regr.kurz,s3d$xyz.convert(MAGASSAG,
SZULTOMEG, fitted(regmod2)))
segments(orig$x, orig$y, plane$x, plane$y)

###################################################
### 274. oldal, 9.9. példa
###################################################
regmod2 = lm(TOMEG ~ MAGASSAG + SZULTOMEG, data = regr.kurz)
summary(regmod2)

###################################################
### 275. oldal, 9.10. példa; 276. oldal, 9.9. ábra
###################################################
oz = read.table("ozmeret.csv",sep=";",header=T)

plot(oz,pch=20)

(korr = cor(oz,use="complete.obs"))

library(corpcor)
cor2pcor(korr)

###################################################
### 277-278. oldal, 9.11. példa
###################################################
regmod3 = lm(OVMERET~TOMEG+ZSIGTOMEG+TESTH+MARMAG,
data=oz)
summary(regmod3)

library(faraway)
vif(regmod3)

regmod4 = lm(OVMERET~ZSIGTOMEG+TESTH+MARMAG,data=oz)
summary(regmod4)

vif(regmod4)

###################################################
### 279. oldal, 9.10. ábra
###################################################
x = c(1:10)
y = c(3,3,3,4,6,5,5.5,7,6,7)

plot(x,y,main="",xlab="x",ylab="y",pch=19,bty="l")
line =  lm(y~x)
abline(line)
becsult = fitted(line)
segments(x,becsult,x,y)

plot(x,resid(line),main="",xlab="Sorszám",ylab="Reziduum",
pch=19,bty="l")
abline(h=0)
segments(x,0,x,resid(line))

###################################################
### 280. oldal, 9.11. ábra
###################################################
x = c(1:10)
y = c(3,4,5,6,7,10,11,17,19,23)

plot(x,y,main="",xlab="x",ylab="y",pch=19)

line =  lm(y~x)
abline(line)
becsult = fitted(line)
segments(x,becsult,x,y)

plot(x,resid(line),main="",pch=19,xlab="Sorszám",ylab="Reziduum")
abline(h=0)
segments(x,0,x,resid(line))

###################################################
### 281. oldal, 9.12. ábra
###################################################
x = c(1:10)
y = c(2,3,10,10,13,20,10,27,15,30)

plot(x,y,main="",xlab="x",ylab="y",pch=19,bty="l")

line =  lm(y~x)
abline(line)
becsult = fitted(line)
segments(x,becsult,x,y)

plot(x,resid(line),main="",xlab="Sorszám",ylab="Reziduum",
pch=19,bty="l")
abline(h=0)
segments(x,0,x,resid(line))

###################################################
### 280-281. oldal, 9.12. példa; 280. oldal, 9.13. ábra
###################################################
x = c(1:10)
y = c(2,3,10,10,13,20,10,27,15,30)

WLSmod = lm(y~x,weights=1/x^2)
summary(WLSmod)    

OLSmod = lm(y~x)
summary(OLSmod)

###################################################
### 282. oldal; 282. oldal, 9.13. ábra
###################################################
plot(OLSmod, 3)
plot(regmod2, 3)


###################################################
### 282. oldal; 283. oldal, 9.14. ábra
###################################################
qqnorm(residuals(OLSmod), pch = 16, main = "")
qqline(residuals(OLSmod))

qqnorm(residuals(regmod2), pch = 16, main = "")
qqline(residuals(regmod2))

###################################################
### 284. oldal, 9.15. ábra
###################################################
x = c(1:10)
y = c(0,2,2,4,6,0.5,5.5,7,6,7)

line =  lm(y~x)

x1 = c(1:5,7:10)
y1 = c(0,2,2,4,6,5.5,7,6,7)

plot(x1,y1,main="",xlab="x",ylab="y",ylim=c(-1,8),bty="l")
abline(line)

line1 =  lm(y1~x1)
abline(line1,lty=2)
points(6,0.5,pch=19)

x = c(1:10)
y = c(0,2,2,4,6,5,5.5,7,6,1)

line =  lm(y~x)

x1 = c(1:10)
y1 = c(0,2,2,4,6,5,5.5,7,6,7)

plot(x1,y1,main="",xlab="x",ylab="y",ylim=c(-1,8),bty="l")
abline(line)

line1 =  lm(y1~x1)
abline(line1,lty=2)
points(10,1,pch=19)

###################################################
### 286. oldal, 9.16. ábra
###################################################
x = c(1:10,15)
y = c(0,2,2,4,6,5,5.5,7,6,7,10)

line =  lm(y~x)

x1 = c(1:10)
y1 = c(0,2,2,4,6,5,5.5,7,6,7)

plot(x1,y1,main="",xlab="x",ylab="y",xlim=c(0,15),
ylim=c(-1,20),bty="l", cex=0.9)
abline(line)

line1 =  lm(y1~x1)
abline(line1,lty=2)
points(15,10,pch=19)

x = c(1:10,15)
y = c(0,2,2,4,6,5,5.5,7,6,7,20)

line =  lm(y~x)

x1 = c(1:10)
y1 = c(0,2,2,4,6,5,5.5,7,6,7)

plot(x1,y1,main="",xlab="x",ylab="y",xlim=c(0,15),
ylim=c(-1,20),bty="l", cex=0.9)
abline(line)

line1 =  lm(y1~x1)
abline(line1,lty=2)
points(15,20,pch=19)

###################################################
### 289. oldal, 9.13. példa
###################################################
library(MASS)
x = c(1:10,15)
y = c(0,2,2,4,6,5,5.5,7,6,7,20)

mod = lm(y~x)
which(abs(studres(mod)) > qt(p = 0.99,df = 8))

###################################################
### 290. oldal, 9.18. ábra
###################################################
x = c(1:10)
y = c(0,2,2,4,6,0.5,5.5,7,6,7)

line =  lm(y~x)

plot(dffits(line),pch=16,ylim=c(-4.5,1),ylab="DFFIT")

x = c(1:10)
y = c(0,2,2,4,6,5,5.5,7,6,1)

line =  lm(y~x)

plot(dffits(line),pch=16,ylim=c(-4.5,1),ylab="DFFIT")

###################################################
### 291. oldal, 9.19. ábra
###################################################
x = c(1:10)
y = c(0,2,2,4,6,0.5,5.5,7,6,7)

line =  lm(y~x)

plot(line,5,pch=16)

x = c(1:10)
y = c(0,2,2,4,6,5,5.5,7,6,1)

line =  lm(y~x)

plot(line,5,pch=16)

###################################################
### 291. oldal, 9.14. példa
###################################################
x = c(1:10)
y = c(0,2,2,4,6,5,5.5,7,6,1)

mod2 =  lm(y~x)

influence.measures(mod2)

###################################################
### 292. oldal, 9.20. ábra
###################################################
regmod2 = lm(TOMEG~MAGASSAG+SZULTOMEG,data=regr.kurz)

par(mfrow = c(2, 2))
plot(regmod2)
par(mfrow = c(1, 1))

###################################################
### 293. oldal, 9.21. ábra 
###################################################
mag3 = regr.kurz$MAGASSAG^3

regmod2 = lm(TOMEG~mag3,data=regr.kurz)

with(regr.kurz,plot(TOMEG~MAGASSAG,pch=20, 
xlab="Magasság (cm)", ylab="Testtömeg (kg)", 
xlim=c(100,200),ylim=c(20,90))) 	

curve(coef(regmod2)[1]+coef(regmod2)[2]*x^3,add=T)
abline(lm(TOMEG~MAGASSAG,data=regr.kurz),lty=2)

###################################################
### 296. oldal, 9.22. ábra
###################################################
x = runif(100,0.15,1)
h = rnorm(100,0,.15)
y = log(x)+h
plot(x,y,pch=20,xlab="",ylab="",xaxt="n",yaxt="n")
mtext('x',side=1,line=0.5)
mtext('y',side=2,line=0.5,las=3)

plot(log(x),y,pch=20,xlab="",ylab="",xaxt="n",yaxt="n")
mtext('log(x)',side=1,line=0.5)
mtext('y',side=2,line=0.5,las=3)

x = runif(100,2,5)
h = rnorm(100,0,.15)
y = exp(x)*exp(h)
plot(x,y,pch=20,xlab="",ylab="",xaxt="n",yaxt="n")
mtext('x',side=1,line=0.5)
mtext('y',side=2,line=0.5,las=3)

y = log(y)
plot(x,y,pch=20,xlab="",ylab="",xaxt="n",yaxt="n")
mtext('x',side=1,line=0.5)
mtext('log(y)',side=2,line=0.5,las=3)

x = runif(100,2,5)
h = rnorm(100,0,.15)
y = 2*x^3*exp(h)
plot(x,y,pch=20,xlab="",ylab="",xaxt="n",yaxt="n")
mtext('x',side=1,line=0.5)
mtext('y',side=2,line=0.5,las=3)

plot(log(x),log(y),pch=20,xlab="",ylab="",xaxt="n",yaxt="n")
mtext('log(x)',side=1,line=0.5)
mtext('log(y)',side=2,line=0.5,las=3)

###################################################
### 297. oldal 9.15. példa, 298. oldal 9.23. ábra 
###################################################
terulet = c(253 , 7400 , 53 , 37 , 2887 , 822 , 61 , 
10770 , 11800 , 56)
fajszam = c(26 , 63 , 1 , 21 , 30 , 50 , 0 , 44 , 79 , 8)

(logreg = lm(fajszam~log(terulet)))

plot(fajszam~terulet,pch=20,
xlab=expression(paste("Terület (",km^2,")",
sep='')),ylab="Fajszám (db)")

b0 = coef(logreg)[1]
b1 = coef(logreg)[2]
curve(b0+b1*log(x),add=T)

plot(fajszam~log(terulet),xlab="log(Terület)",
ylab="Fajszám",pch=20)
abline(logreg)

###################################################
### 297-298. oldal 9.16. példa, 299. oldal 9.24. ábra
###################################################
csibe = read.csv2("csibe.csv",dec=".")
csibe = csibe[csibe$nap<11,]


with(csibe,plot(tomeg~nap,pch=20,xlab="Kor (nap)",
ylab="Tömeg (g)"))
(expreg = lm(log(tomeg)~nap,data=csibe))
curve(exp(coef(expreg)[1])*exp(coef(expreg)[2]*x),add=T)

with(csibe,plot(log(tomeg)~nap,pch=20,xlab="Kor (nap)",
ylab="log(Tömeg)"))
abline(lm(log(tomeg)~nap,data=csibe))

###################################################
### 299-300. oldal, 9.17. példa
###################################################
csibe = read.csv2("csibe.csv",dec=".")
csibe = csibe[csibe$nap<11,]
id = rep(1:4,rep(11,4))
csibe = data.frame(csibe, id)

csibe[1:5,]

library(nlme)
expreg.gls = gls(log(tomeg)~nap,data=csibe, 
correlation = corAR1(form = ~ nap | id))

summary(expreg.gls)

expreg = lm(log(tomeg)~nap,data=csibe)
summary(expreg)

###################################################
### 301-303. oldal, 9.18. példa, 9.25. és 9.26. ábra 
###################################################
oz = read.csv2("oz1.csv",dec=".")

with(oz,plot(TOMEG~TESTH,pch=20,xlab="Testhossz (cm)",
ylab="Teljes tömeg (kg)"))

with(oz,plot(log(TOMEG)~log(TESTH),pch=20,
xlab="log(testhossz)",ylab="log(teljes tömeg)"))

(hatvmod = lm(log(TOMEG)~log(TESTH),data=oz))

x = data.frame(TESTH=55:135)
konf.sav = predict(hatvmod,int="confidence",newdata=x)
pred.sav = predict(hatvmod,int="prediction",newdata=x)
with(oz,plot(TOMEG~TESTH,pch=20,xlab="Testhossz (cm)",
ylab="Teljes tömeg (kg)"))
matlines(55:135,exp(konf.sav),lty=c(1,2,2), col=1,lwd=2)
matlines(55:135,exp(pred.sav),lty=c(1,3,3), col=1,lwd=2)

###################################################
### 304. oldal, 9.27. ábra
###################################################
x = 1:6
plot(x,x^2,main="",xlab=expression(x[1]),
ylab=expression(x[2]),pch=19)
text(2,35,expression(italic(x[2]==x[1]^2)))

plot(x,(x-mean(x))^2,
main="",xlab=expression(x[1]),ylab=expression(x[2]),pch=19)
text(3.5,4,expression(italic(x[2]==(x[1]-bar(x)[1])^2)))

###################################################
### 305-306. oldal, 9.19. példa, 9.29. ábra
###################################################
oz = read.csv2("oz1.csv",dec=".")

with(oz,plot(TESTH~KOR,pch=20,ylab="Testhossz (cm)",
xlab="Becsült kor (év)"))

(telmod = nls(TESTH~p1+p2*(1-exp(-p3*KOR)),
start=list(p1=50,p2=60,p3=2),data=oz))

p1 = coef(telmod)[1]
p2 = coef(telmod)[2]
p3 = coef(telmod)[3]
with(oz,plot(TESTH~KOR,pch=20,ylab="Testhossz (cm)",xlab="Becsült kor (év)"))
curve(p1+p2*(1-exp(-p3*x)),add=T)

###################################################
### 307. oldal, 9.30. ábra
###################################################
plot(telmod,col=1)
qqnorm(resid(telmod),main="")

###################################################
### 307-309. oldal, 9.20. példa; 
### 308. oldal, 9.32. ábra;
### 310. oldal, 9.33. ábra
###################################################
csibe = read.csv2("csibe.csv",dec=".")
id = rep(1:4,rep(22,4))
csibe2 = data.frame(csibe, id)

with(csibe,plot(tomeg~nap,pch=20,xlab="Kor (nap)",
ylab="Tömeg (g)"))

library(nlme)
(logismod = gnls(tomeg~p1+p2/(1+exp(-p3*(nap-p4))),
start=list(p1=0,p2=300,p3=.1,p4=15),data=csibe2,
correlation = corAR1(form = ~ nap | id),weights=varPower()))

p1 = coef(logismod)[1]
p2 = coef(logismod)[2]
p3 = coef(logismod)[3]
p4 = coef(logismod)[4]
with(csibe,plot(tomeg~nap,pch=20,xlab="Kor (nap)",ylab="Tömeg (g)"))
curve(p1+p2/(1+exp(-p3*(x-p4))),add=T)

plot(logismod,col=1)
qqnorm(logismod,col=1)

###################################################
### 311. oldal, 9.21. példa, 9.35. ábra 
###################################################
oz = read.csv2("oz1.csv",dec=".")

(trigmod = nls(SZORH~p1+p2*cos(2*pi/12*(HO-p4)),
start=list(p1=3.5,p2=1,p4=3),data=oz))

p1 = coef(trigmod)[1]
p2 = coef(trigmod)[2]
p4 = coef(trigmod)[3]

with(oz,plot(SZORH~HO,pch=20,ylab="Szõrhossz (cm)",
xlab="Hónap"))
curve(p1+p2*cos(2*pi/12*(x-p4)),add=T)
