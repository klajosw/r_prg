###################################################
### 316. oldal
###################################################
(x = rep(c("t","v","h"),c(4,4,4)))
sample(x,12)

###################################################
### 317. oldal, 10.1. ábra
###################################################
magassag = c(56, 48, 66,54,57,50,47,58,54,46,60,48)
tapoldat = rep(c("tomeny","hig","viz"),rep(4,3))

stripchart(magassag~tapoldat,vertical=T,xlim=c(0.5,3.5),
ylim=c(30,80),pch=1, group.names=c("híg","tömény","víz"),
ylab="Magasság (cm)", xlab="Kezelés")
abline(h=mean(magassag), lty=2)
points(tapply(magassag,tapoldat,mean),pch=16)

###################################################
### 320-321. oldal, 10.1. példa
###################################################
magassag = c(56, 48, 66,54,57,50,47,58,54,46,60,48)
tapoldat = rep(c("tomeny","hig","viz"),rep(4,3))
(adat = data.frame(magassag,tapoldat))

aovmod1 = aov(magassag~tapoldat,data=adat)
anova(aovmod1)

power.anova.test(groups=3, n=4, between.var=17.33, 
within.var= 41.55)

model.tables(aovmod1,type="means",se=T)

###################################################
### 323-324. oldal, 10.2. példa
###################################################
magassag = c(60, 58, 65, 64, 57, 55, 57, 58, 54, 46, 50, 48)
tapoldat = rep(c("tomeny","hig","viz"),rep(4,3))
(adat = data.frame(magassag,tapoldat))

aovmod2 = aov(magassag~tapoldat,data=adat)
anova(aovmod2)

TukeyHSD(aovmod2)

###################################################
### 326-327. oldal, 10.3. példa 
###################################################
magassag = c(60, 58, 65, 64, 57, 55, 57, 58, 54, 46, 50, 48,
62, 57, 60, 61, 55, 54, 52, 60, 45, 44, 42, 48)
tapoldat = rep(rep(c("tomeny","hig","viz"),rep(4,3)),2)
fajta = rep(1:2,rep(12,2))
fajta = factor(fajta)
(adat = data.frame(fajta,tapoldat,magassag))

aovmod3 = aov(magassag~tapoldat*fajta,data=adat)
anova(aovmod3)
aovmod4 = aov(magassag~tapoldat+fajta,data=adat)
anova(aovmod4)

TukeyHSD(aovmod4, which="tapoldat")

###################################################
### 328. oldal, 10.2. ábra
###################################################
with(adat, interaction.plot(tapoldat,fajta,magassag,
xlab="Kezelés",ylab="Magasság átlagok (cm)",xaxt="n"))
axis(1,at=1:3,labels=c("híg","tömény","víz"))

magassag = c(50, 48, 55, 54, 57, 55, 57, 58, 54, 46, 50, 48,
62, 57, 60, 61, 55, 54, 52, 60, 45, 44, 42, 48)
tapoldat = rep(rep(c("tomeny","hig","viz"),rep(4,3)),2)
fajta = rep(1:2,rep(12,2))
fajta = factor(fajta)
adat2 = data.frame(fajta,tapoldat,magassag)

with(adat2, interaction.plot(tapoldat,fajta,magassag,
xlab="Kezel?s", ylab="Magasság átlagok (cm)",xaxt="n",xpd=T))
axis(1,at=1:3,labels=c("híg","tömény","víz"))

###################################################
### 333. oldal, 10.3. ábra
###################################################
par(mfrow = c(2, 2))
plot(aovmod3)
par(mfrow = c(1, 1))









