###################################################
### Adatbeolvasás. A regr.kurz táblázatnak csak az
### elsõ 5 oszlopát hasznájuk (a könyvbõl ez a kód-
### részlet hiányzik).
###################################################
regr.kurz = read.table("regr.kurz.csv",sep=";",header=T)
regr.kurz = regr.kurz[,1:5]

####################################################
### 245. oldal, 8.1. példa 
###################################################
regr.kurz[1:5,]
with(regr.kurz,cor(TOMEG,SZULTOMEG))

##################################################
### 246. oldal, 8.2. ábra
###################################################
with(regr.kurz, plot(TOMEG~SZULTOMEG,pch=20, 
ylim=c(40,100), xlab="Születéskori testtömeg (g)",
ylab="Felnõttkori testtömeg (kg)"))

###################################################
### 247. oldal, 8.2. példa
###################################################
with(regr.kurz,cor.test(SZULTOMEG,TOMEG))

###################################################
### 248. oldal, 8.3. példa
###################################################
elsoeves = read.table("elsoeves.txt",header=T)
with(elsoeves,cor(matek,biol,method="spearman"))
with(elsoeves,cor(matek,biol,method="kendall"))

###################################################
### 249. oldal, 8.4. példa
### Az egyforma értékek miatt csak közelítõ p-értéket
### számol az R (nem baj).
###################################################
with(regr.kurz,cor.test(SZULTOMEG,TOMEG,method="spearman"))
with(regr.kurz,cor.test(SZULTOMEG,TOMEG,method="kendall"))


