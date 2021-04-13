###################################################
### Adatbeolvasás
###################################################
men  =  read.table("men.txt",header=TRUE)
minta  =  read.table("minta.txt",header=TRUE)$height


###################################################
### 125. oldal, 5.1. ábra 
###################################################
a=c(179.0273, 176.9141, 177.5521, 178.8648, 178.8195,
179.1127, 179.3808, 175.8195, 180.4640, 178.5046)
b=c(177.2599, 178.8735, 178.8594, 178.5458, 176.9382,
178.9306, 177.9731, 177.3444,177.5396, 176.9480)

plot(c(175,181),c(0,0.8),type='n',yaxt='n',ylab='',
xlab='Testmagasság (cm)')
points(a,rep(0.6,10),pch=20)
points(b,rep(0.25,10),pch=20)
points(mean(a),0.6,pch=15,cex=1.5)
points(mean(b),0.25,pch=15,cex=1.5)
abline(v=178)
text(175,0.6,'a')
text(175,0.25,'b')

###################################################
### 125. oldal 
###################################################
mean(minta,na.rm=T)  
s = sd(minta,na.rm=T) 
(n = length(na.omit(minta)))   
(SE = s/sqrt(n))	

###################################################
### 126. oldal, 5.2. ábra 
###################################################
x  =  seq(158.257, 197.743, length=100)
plot(x, dnorm(x, mean=178, sd=6), xlab="Testmagasság (cm)",
xlim=c(160,200), ylab="Sûrûség",  type="l",ylim=c(0,0.5))
abline(h=0, col="grey")
points(x, dnorm(x, mean=178, sd=6/sqrt(10)), type="l")
points(x, dnorm(x, mean=178, sd=6/sqrt(50)), type="l")
axis(1, seq(150,210,by=10),seq(150,210,by=10))
text (187.5, 0.05, labels = 'n = 1')
text (182.5, 0.1, labels = 'n = 10')
text (181, 0.3, labels = 'n = 50')

###################################################
### 127. oldal 
###################################################
library(gmodels) 
ci(minta) 


###################################################
### 128. oldal, 5.4. ábra 
###################################################
n = 30
konf.int = matrix(0,n,3)
for (i in 1:n)
konf.int[i,] = ci(sample(men$height,300))[1:3]
matplot(x=konf.int,y=1:n,type="n",col=1, 
xlab="Paraméter és konfidencia-intervallumok",
 ylab="Minta sorszáma")
abline(v=178)
segments(konf.int[,3],1:n,konf.int[,1],1:n)
segments(konf.int[,2],1:n,konf.int[,1],1:n)

###################################################
### 131. oldal, 5.2. példa
###################################################
library(gmodels)
ci(minta,confidence=0.9)

###################################################
### 141. oldal, 5.5. példa
###################################################
dbinom(18, size = 30, prob = seq(0, 1, 0.05))
