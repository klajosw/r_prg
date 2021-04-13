###################################################
### 62. oldal, 3.2. ábra 
###################################################
plot(rep(1/6,6),type="h",xlab="Kockadobás",
ylab="Valószínûség", bty="l", lwd=4)

###################################################
### 63. oldal, 3.3. ábra  
###################################################
val = c(1/2, 1/4, 1/8, 1/16, 1/32, 1/64, 1/128, 1/256, 
1/512, 1/1024, 1/2048, 1/4096)
hat  =  1:20
val  =  1/(2^hat)
plot(val,type="h",xlab="Dobások száma",
ylab="Valószínûség", bty="l", lwd=4)

###################################################
### 70. oldal, 3.6. ábra 
###################################################
x  =  0:10
plot(x, dhyper(x, m=15, n=105, k=10), xlab="Érték", 
ylab="Valószínûség", type="h",bty="l",ylim=c(0,0.42),lwd=4)
abline(h=0, col="gray")

plot(x, dhyper(x, m=60, k=10, n=60), xlab="Érték", 
ylab="Valószínûség", type="h",bty="l",lwd=4)
abline(h=0, col="gray")

plot(x, dhyper(x, m=90, k=10, n=30), xlab="Érték", 
ylab="Valószínûség", type="h",bty="l", ylim=c(0,0.32),lwd=4)
abline(h=0, col="gray")

###################################################
### 71. oldal, 3.5. példa 
###################################################
dhyper(x=0:10, m=15, n=105, k=10)


###################################################
### 73. oldal, 3.7. ábra 
###################################################
x  =  0:10
plot(x, dbinom(x, 10,0.08), xlab="Érték", 
ylab="Valószínûség", type="h",bty="l",lwd=4)
abline(h=0, col="gray")

plot(x, dbinom(x, 10,0.5), xlab="Érték", 
ylab="Valószínûség", type="h",bty="l",lwd=4)
abline(h=0, col="gray")

plot(x, dbinom(x, 10, 0.75), xlab="Érték", 
ylab="Valószínûség", type="h",bty="l",lwd=4)
abline(h=0, col="gray")

###################################################
### 74. oldal, 3.7. példa 
###################################################
dbinom(0:10, size=10, prob=0.08)

###################################################
### 76-77. oldal, 3.9. példa 
###################################################
dpois(x=3, lambda=0.5)

###################################################
### 78. oldal, 3.8. ábra 
###################################################
x  =  0:20
y  =  dpois(x,0.5)
plot(x,y,type='h',xlab='Gyakoriság', ylab='Valószínûség',lwd=4)

y  =  dpois(x,2.5)
plot(x,y,type='h',xlab='Gyakoriság', ylab='Valószínûség',lwd=4)


y  =  dpois(x,10)
plot(x,y,type='h',xlab='Gyakoriság', ylab='Valószínûség',lwd=4)

###################################################
### 79. oldal, 3.9. ábra 
###################################################
x  =  seq(-10, 10, length=300)
plot(x, dnorm(x, mean=0, sd=3),ylim=c(0,.45),
xlab="Érték",ylab="Sûrûség", type="l")
points(x, dnorm(x, mean=2, sd=2),xlab="",ylab="", type="l")
points(x, dnorm(x, mean=0, sd=1),xlab="",ylab="", type="l")
abline(h=0, col="gray")
text(c(2,5,-4),c(0.3,0.15,0.1),c('N(0,1)','N(2,2)','N(0,3)'))

###################################################
### 79. oldal, 3.10. ábra 
###################################################
x  =  seq(-5.291, 5.291, length=200)
plot(x, dnorm(x, mean=0, sd=1), xlab="Érték", ylab="Sûrûség", type="l")
abline(h=0, col="gray")

plot(x, pnorm(x, mean=0, sd=1), xlab="Érték", ylab="Valószínûség",  type="l")
abline(h=0, col="gray")


###################################################
### 81-82. oldal, 3.10. példa
###################################################
pnorm(10,mean=12,sd=2)

1-pnorm(15,mean=12,sd=2)

1-pnorm(3)

qt(0.05,df=10)

qt(0.95,df=10)

qnorm(0.025)
qnorm(0.975)


###################################################
### 83. oldal
###################################################
dbinom(4,size=50,prob=0.2)


###################################################
### 84. oldal, 3.12. ábra
###################################################
x =  seq(0, 0.3, length=100)
plot(x, dbinom(4, 50, x), type="l", 
xlab='Betegek aránya a populációban',
ylab='Annak a valószínû sége, hogy egy 50 elemû
mintában 4 beteg van')
abline(v=0.08)



