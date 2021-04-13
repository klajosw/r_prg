###################################################
### Adatbeolvasás
###################################################
pop  =  read.table("elsoeves.txt",header=TRUE)
lepke  =  read.table("lepke.txt",header=TRUE)
regr.kurz = read.table("regr.kurz.csv",header=T,sep=";")

###################################################
### 89. oldal, 4.1. ábra
###################################################
alom  =  c(rep(6,3),rep(7,7),rep(8,12),rep(9,20),rep(10,31),
rep(11,17),rep(12,8),rep(13,5),rep(14,2))

barplot(table(alom), xlab='Alomszám', ylab='Darab')

barplot(prop.table(table(alom)), xlab='Alomszám', 
ylab='Relatív gyakoriság')

###################################################
### 89-90. oldal, 4.2. példa
###################################################
inszem = c(rep(1,7),rep(2,9),rep(3,5),rep(4,3),
rep(5,2),rep(6,1))

table(inszem)

prop.table(table(inszem))  

cumsum(table(inszem))  

cumsum(prop.table(table(inszem)))  

###################################################
### 91. oldal, 4.2. ábra 
###################################################
barplot(table(inszem),xlab='Inszeminálások száma', 
ylab='Gyakoriság',ylim=c(0,10))

barplot(prop.table(table(inszem)),xlab='Inszeminálások száma', 
ylab='Relatív gyakoriság',ylim=c(0,0.35))

barplot(cumsum(table(inszem)),xlab='Inszeminálások száma', 
ylab='Gyakoriság',ylim=c(0,30))

barplot(cumsum(prop.table(table(inszem))),ylim=c(0,1.05),
xlab='Inszeminálások száma', ylab='Relatív gyakoriság')


###################################################
### 92. oldal, 4.3. ábra
###################################################
faj =  c("kutya", "macska", "nyest", "vörös róka",
"szarvasmarha")
case = c(47, 102, 5, 692, 29)
pie(case, faj, col = gray(seq(0.4,1.0,length=5)))

###################################################
### 92. oldal, 4.4. ábra
###################################################
with(lepke,stripchart(BABTOMEG~TAP,at=c(1.2,1.7),
pch=1,group.names=c("1", "2"),xlab="Bábtömeg (g)",
ylab="Csoport"))

###################################################
### 93. oldal, 4.5. ábra
###################################################

par(mfrow=c(3,1))
stripchart(pop$magas[1:400]~pop$matek.kat[1:400],
ylim=c(0,3),xlim=c(150,200),ylab="Csoport", bty="l",
xlab="Testmagasság (cm)", group.names=c("1","2"),pch=1)

with(pop[1:400,],hist(magas[matek.kat=="rossz"],
xlab="Testmagasság (cm)",ylab="Gyakoriság",main="",
xlim=c(150,200),breaks=15,ylim=c(0,45)))

with(pop[1:400,],hist(magas[matek.kat=="jo"],
xlab="Testmagasság (cm)",ylab="Gyakoriság",
main="",xlim=c(150,200),breaks=15,ylim=c(0,45)))

###################################################
### 94. oldal, 4.6. ábra
###################################################
hist(pop$magas, main='',xlab="Testmagasság (cm)",
xlim=c(150,200),ylab="Sûrûség",freq=F,ylim=c(0,0.07))

hist(pop$magas,breaks=c(155,165,170,180,185,190,200), 
main='',xlim=c(150,200), xlab="Testmagasság (cm)",
ylab="Sûrûség",ylim=c(0,0.07))


###################################################
### 94-95. oldal; 95. oldal, 4.7. ábra
###################################################
hist(pop$magas[1:60],xlab="Testmagasság (cm)",
ylab="Sûrûség",main="",freq=F,xlim=c(160,200))
rug(pop$magas[1:60])

plot(density(pop$magas[1:60]),xlab="Testmagasság (cm)",
ylab="Sûrûség",main="",xlim=c(160,200))
rug(pop$magas[1:60])


###################################################
### 96. oldal, 4.8. ábra 
###################################################
hist(pop$magas[1:80],breaks=3,xlab="Testmagasság (cm)",
ylab="Gyakoriság",main="",ylim=c(0,45))

hist(pop$magas[1:80], xlab="Testmagasság (cm)",ylim=c(0,45),
ylab="Gyakoriság",main="",xlim=c(150,200))

hist(pop$magas[1:80],breaks=30,xlab="Testmagasság (cm)",ylim=c(0,45),
ylab="Gyakoriság",main="",xlim=c(150,200))

###################################################
### 96. oldal, 97. oldal, 4.9. ábra 
###################################################
with(lepke,boxplot(BABTOMEG~HOM,xlab="Hõmérsékleti kezelés",
 ylab="Bábtömeg (g)", names=c("hûtött","meleg","szobahõ")))

library(vioplot)
with(lepke,vioplot(BABTOMEG[HOM=="hutott"],
BABTOMEG[HOM=="melegitett"],BABTOMEG[HOM=="szobahom"], col='white',names=c("hûtött","meleg","szobahõ")))
title(ylab="Bábtömeg (g)",xlab="Hõmérsékleti kezelés")


###################################################
### 97. oldal; 98. oldal, 4.10. ábra 
###################################################
library(sciplot)

lineplot.CI(KEZELES, BABTOMEG, data = lepke, 
ylab = "Bábtömeg (g; átlag ± SD)", xlab = "Kezelés",
ylim=c(0.15,0.35), type="p",
fun = function(x) mean(x, na.rm=TRUE),
ci.fun= function(x) c(mean(x, na.rm=TRUE)-sd(x), mean(x, na.rm=TRUE)+sd(x)))
            
       
bargraph.CI(KEZELES, BABTOMEG, data = lepke, ylab = "Bábtömeg (g; átlag ± SD)", 
xlab = "Kezelés", col = "black", angle = 45,
fun = function(x) mean(x, na.rm=TRUE),
ci.fun= function(x) c(mean(x, na.rm=TRUE)-sd(x), mean(x, na.rm=TRUE) + sd(x)),
ylim=c(0,0.4),density = c(0,10,20,30,40,50))

###################################################
### 98. oldal, 4.3. példa; 99.oldal, 4.11. ábra
###################################################
table(pop$matek,pop$biol)

prop.table(table(pop$matek,pop$biol))

mosaicplot(table(pop$matek,pop$biol), 
xlab = "Matematika jegyek", ylab = "Biológia jegyek",
main = "")


###################################################
### 100-102. oldal, 4.4. példa, 4.12-14. ábrák 
###################################################
regr.kurz[1:5,]

with(regr.kurz,plot(MAGASSAG~SZULHOSSZ,pch=as.numeric(NEM),
main="", xlab= "Születési hossz (cm)", ylab= "Magasság (cm)"))
with(regr.kurz, legend(50,190,pch=1:2,legend=c("férfi","nõ"),bty="n"))

with(regr.kurz,coplot(MAGASSAG~SZULHOSSZ|NEM,main="", 
xlab= c("Születési hossz (cm)","Nem"), ylab= "Magasság (cm)"))

pairs(regr.kurz[,2:5])


###################################################
### 102-103. oldal; 4.5. példa, 4.15. ábra 
###################################################
with(regr.kurz, stripchart(MAGASSAG~NEM,xlab="nem",
ylab="testmagasság (cm)",vertical=T,pch=16,
group.names=c("férfi","n? "),at=c(1.2,1.8)))

with(regr.kurz,boxplot(MAGASSAG~NEM,xlab="nem",
ylab="Testmagasság (cm)",names=c("férfi","n? ")))


library(sciplot)

lineplot.CI(NEM, MAGASSAG, data = regr.kurz, 
ylab = "Testmagasság (cm) átlag±SD", xlab = "Nem",
ylim = c(160,190), type = "p", xaxt = "n",
fun = function(x) mean(x, na.rm = TRUE),
ci.fun = function(x) c(mean(x, na.rm = TRUE) - sd(x), 
  mean(x, na.rm = TRUE) + sd(x)))
axis(1, at = 1:2, labels = c("férfi", "nõ "))
           
###################################################
### 105. oldal, 4.6. példa 
###################################################
minta = c(1,5,8,12,17,20)
mean(minta)

###################################################
### 106. oldal, 4.7. példa 
###################################################
egyedszam = c(300,250,500,400,550)
tejterm = c(4500,6000,5500,4000,5000)
weighted.mean(tejterm,egyedszam)


###################################################
### 108. oldal, 4.8. példa 
###################################################
minta = c(1.2,1.7,1.2,1.4,1.6,1.5,1.1,20.1,1.3,2.0,0.9,1.6)
mean(minta)
mean(minta,trim=0.1)

###################################################
### 112. oldal, 4.10. példa 
###################################################
minta = c(2,3,1,4,5,10)
mad(minta, constant=1)


###################################################
### 113. oldal, 4.11. példa 
###################################################
atlag = c(50,150,500)
szoras = c(5,18,75)
(cv = szoras/atlag*100)


###################################################
### 114. oldal, 4.12. példa 
###################################################
quantile(pop$magas)

quantile(pop$magas,probs=c(0.1,0.9))


###################################################
### 122. oldal, 4.15. példa
###################################################
V1 = c(1,3,2,4,5,3,8)
V2 = c(4,5,6,7,7,5,11)

(V1.lintr = 2*V1+3)
(V2.lintr = 2*V2+3)
(V1.gyok = sqrt(V1))
(V2.gyok = sqrt(V2))

cor(V1,V2)
cor(V1.lintr,V2.lintr)
cor(V1.gyok,V2.gyok)

cor(V1,V2,method="spearman")
cor(V1.lintr,V2.lintr,method="spearman")
cor(V1.gyok,V2.gyok,method="spearman")




