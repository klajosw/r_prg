###################################################
### Adatbeolvas�s. A regr.kurz t�bl�zatnak csak az
### els� 5 oszlop�t haszn�juk (a k�nyvb�l ez a k�d-
### r�szlet hi�nyzik).
###################################################
regr.kurz = read.table("regr.kurz.csv",sep=";",header=T)
regr.kurz = regr.kurz[,1:5]

####################################################
### 245. oldal, 8.1. p�lda 
###################################################
regr.kurz[1:5,]
with(regr.kurz,cor(TOMEG,SZULTOMEG))

##################################################
### 246. oldal, 8.2. �bra
###################################################
with(regr.kurz, plot(TOMEG~SZULTOMEG,pch=20, 
ylim=c(40,100), xlab="Sz�let�skori testt�meg (g)",
ylab="Feln�ttkori testt�meg (kg)"))

###################################################
### 247. oldal, 8.2. p�lda
###################################################
with(regr.kurz,cor.test(SZULTOMEG,TOMEG))

###################################################
### 248. oldal, 8.3. p�lda
###################################################
elsoeves = read.table("elsoeves.txt",header=T)
with(elsoeves,cor(matek,biol,method="spearman"))
with(elsoeves,cor(matek,biol,method="kendall"))

###################################################
### 249. oldal, 8.4. p�lda
### Az egyforma �rt�kek miatt csak k�zel�t� p-�rt�ket
### sz�mol az R (nem baj).
###################################################
with(regr.kurz,cor.test(SZULTOMEG,TOMEG,method="spearman"))
with(regr.kurz,cor.test(SZULTOMEG,TOMEG,method="kendall"))


