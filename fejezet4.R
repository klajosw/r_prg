###################################################
### Adatbeolvas�s
###################################################
pop  =  read.table("elsoeves.txt",header=TRUE)
lepke  =  read.table("lepke.txt",header=TRUE)
regr.kurz = read.table("regr.kurz.csv",header=T,sep=";")

###################################################
### 89. oldal, 4.1. �bra
###################################################
alom  =  c(rep(6,3),rep(7,7),rep(8,12),rep(9,20),rep(10,31),
rep(11,17),rep(12,8),rep(13,5),rep(14,2))

barplot(table(alom), xlab='Alomsz�m', ylab='Darab')

barplot(prop.table(table(alom)), xlab='Alomsz�m', 
ylab='Relat�v gyakoris�g')

###################################################
### 89-90. oldal, 4.2. p�lda
###################################################
inszem = c(rep(1,7),rep(2,9),rep(3,5),rep(4,3),
rep(5,2),rep(6,1))

table(inszem)

prop.table(table(inszem))  

cumsum(table(inszem))  

cumsum(prop.table(table(inszem)))  

###################################################
### 91. oldal, 4.2. �bra 
###################################################
barplot(table(inszem),xlab='Inszemin�l�sok sz�ma', 
ylab='Gyakoris�g',ylim=c(0,10))

barplot(prop.table(table(inszem)),xlab='Inszemin�l�sok sz�ma', 
ylab='Relat�v gyakoris�g',ylim=c(0,0.35))

barplot(cumsum(table(inszem)),xlab='Inszemin�l�sok sz�ma', 
ylab='Gyakoris�g',ylim=c(0,30))

barplot(cumsum(prop.table(table(inszem))),ylim=c(0,1.05),
xlab='Inszemin�l�sok sz�ma', ylab='Relat�v gyakoris�g')


###################################################
### 92. oldal, 4.3. �bra
###################################################
faj =  c("kutya", "macska", "nyest", "v�r�s r�ka",
"szarvasmarha")
case = c(47, 102, 5, 692, 29)
pie(case, faj, col = gray(seq(0.4,1.0,length=5)))

###################################################
### 92. oldal, 4.4. �bra
###################################################
with(lepke,stripchart(BABTOMEG~TAP,at=c(1.2,1.7),
pch=1,group.names=c("1", "2"),xlab="B�bt�meg (g)",
ylab="Csoport"))

###################################################
### 93. oldal, 4.5. �bra
###################################################

par(mfrow=c(3,1))
stripchart(pop$magas[1:400]~pop$matek.kat[1:400],
ylim=c(0,3),xlim=c(150,200),ylab="Csoport", bty="l",
xlab="Testmagass�g (cm)", group.names=c("1","2"),pch=1)

with(pop[1:400,],hist(magas[matek.kat=="rossz"],
xlab="Testmagass�g (cm)",ylab="Gyakoris�g",main="",
xlim=c(150,200),breaks=15,ylim=c(0,45)))

with(pop[1:400,],hist(magas[matek.kat=="jo"],
xlab="Testmagass�g (cm)",ylab="Gyakoris�g",
main="",xlim=c(150,200),breaks=15,ylim=c(0,45)))

###################################################
### 94. oldal, 4.6. �bra
###################################################
hist(pop$magas, main='',xlab="Testmagass�g (cm)",
xlim=c(150,200),ylab="S�r�s�g",freq=F,ylim=c(0,0.07))

hist(pop$magas,breaks=c(155,165,170,180,185,190,200), 
main='',xlim=c(150,200), xlab="Testmagass�g (cm)",
ylab="S�r�s�g",ylim=c(0,0.07))


###################################################
### 94-95. oldal; 95. oldal, 4.7. �bra
###################################################
hist(pop$magas[1:60],xlab="Testmagass�g (cm)",
ylab="S�r�s�g",main="",freq=F,xlim=c(160,200))
rug(pop$magas[1:60])

plot(density(pop$magas[1:60]),xlab="Testmagass�g (cm)",
ylab="S�r�s�g",main="",xlim=c(160,200))
rug(pop$magas[1:60])


###################################################
### 96. oldal, 4.8. �bra 
###################################################
hist(pop$magas[1:80],breaks=3,xlab="Testmagass�g (cm)",
ylab="Gyakoris�g",main="",ylim=c(0,45))

hist(pop$magas[1:80], xlab="Testmagass�g (cm)",ylim=c(0,45),
ylab="Gyakoris�g",main="",xlim=c(150,200))

hist(pop$magas[1:80],breaks=30,xlab="Testmagass�g (cm)",ylim=c(0,45),
ylab="Gyakoris�g",main="",xlim=c(150,200))

###################################################
### 96. oldal, 97. oldal, 4.9. �bra 
###################################################
with(lepke,boxplot(BABTOMEG~HOM,xlab="H�m�rs�kleti kezel�s",
 ylab="B�bt�meg (g)", names=c("h�t�tt","meleg","szobah�")))

library(vioplot)
with(lepke,vioplot(BABTOMEG[HOM=="hutott"],
BABTOMEG[HOM=="melegitett"],BABTOMEG[HOM=="szobahom"], col='white',names=c("h�t�tt","meleg","szobah�")))
title(ylab="B�bt�meg (g)",xlab="H�m�rs�kleti kezel�s")


###################################################
### 97. oldal; 98. oldal, 4.10. �bra 
###################################################
library(sciplot)

lineplot.CI(KEZELES, BABTOMEG, data = lepke, 
ylab = "B�bt�meg (g; �tlag � SD)", xlab = "Kezel�s",
ylim=c(0.15,0.35), type="p",
fun = function(x) mean(x, na.rm=TRUE),
ci.fun= function(x) c(mean(x, na.rm=TRUE)-sd(x), mean(x, na.rm=TRUE)+sd(x)))
            
       
bargraph.CI(KEZELES, BABTOMEG, data = lepke, ylab = "B�bt�meg (g; �tlag � SD)", 
xlab = "Kezel�s", col = "black", angle = 45,
fun = function(x) mean(x, na.rm=TRUE),
ci.fun= function(x) c(mean(x, na.rm=TRUE)-sd(x), mean(x, na.rm=TRUE) + sd(x)),
ylim=c(0,0.4),density = c(0,10,20,30,40,50))

###################################################
### 98. oldal, 4.3. p�lda; 99.oldal, 4.11. �bra
###################################################
table(pop$matek,pop$biol)

prop.table(table(pop$matek,pop$biol))

mosaicplot(table(pop$matek,pop$biol), 
xlab = "Matematika jegyek", ylab = "Biol�gia jegyek",
main = "")


###################################################
### 100-102. oldal, 4.4. p�lda, 4.12-14. �br�k 
###################################################
regr.kurz[1:5,]

with(regr.kurz,plot(MAGASSAG~SZULHOSSZ,pch=as.numeric(NEM),
main="", xlab= "Sz�let�si hossz (cm)", ylab= "Magass�g (cm)"))
with(regr.kurz, legend(50,190,pch=1:2,legend=c("f�rfi","n�"),bty="n"))

with(regr.kurz,coplot(MAGASSAG~SZULHOSSZ|NEM,main="", 
xlab= c("Sz�let�si hossz (cm)","Nem"), ylab= "Magass�g (cm)"))

pairs(regr.kurz[,2:5])


###################################################
### 102-103. oldal; 4.5. p�lda, 4.15. �bra 
###################################################
with(regr.kurz, stripchart(MAGASSAG~NEM,xlab="nem",
ylab="testmagass�g (cm)",vertical=T,pch=16,
group.names=c("f�rfi","n? "),at=c(1.2,1.8)))

with(regr.kurz,boxplot(MAGASSAG~NEM,xlab="nem",
ylab="Testmagass�g (cm)",names=c("f�rfi","n? ")))


library(sciplot)

lineplot.CI(NEM, MAGASSAG, data = regr.kurz, 
ylab = "Testmagass�g (cm) �tlag�SD", xlab = "Nem",
ylim = c(160,190), type = "p", xaxt = "n",
fun = function(x) mean(x, na.rm = TRUE),
ci.fun = function(x) c(mean(x, na.rm = TRUE) - sd(x), 
  mean(x, na.rm = TRUE) + sd(x)))
axis(1, at = 1:2, labels = c("f�rfi", "n� "))
           
###################################################
### 105. oldal, 4.6. p�lda 
###################################################
minta = c(1,5,8,12,17,20)
mean(minta)

###################################################
### 106. oldal, 4.7. p�lda 
###################################################
egyedszam = c(300,250,500,400,550)
tejterm = c(4500,6000,5500,4000,5000)
weighted.mean(tejterm,egyedszam)


###################################################
### 108. oldal, 4.8. p�lda 
###################################################
minta = c(1.2,1.7,1.2,1.4,1.6,1.5,1.1,20.1,1.3,2.0,0.9,1.6)
mean(minta)
mean(minta,trim=0.1)

###################################################
### 112. oldal, 4.10. p�lda 
###################################################
minta = c(2,3,1,4,5,10)
mad(minta, constant=1)


###################################################
### 113. oldal, 4.11. p�lda 
###################################################
atlag = c(50,150,500)
szoras = c(5,18,75)
(cv = szoras/atlag*100)


###################################################
### 114. oldal, 4.12. p�lda 
###################################################
quantile(pop$magas)

quantile(pop$magas,probs=c(0.1,0.9))


###################################################
### 122. oldal, 4.15. p�lda
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




