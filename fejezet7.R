###################################################
### 198-200. oldal, 7.2. példa 
###################################################
b = c(46, 37, 39, 37, 33, 48, 35)
u = c(27, 37, 35, 41, 35, 34, 43, 38, 40)
t.test(b,u,alternative="greater")
t.test(b,u,alternative="greater",var.equal=T)

###################################################
### 201-202. oldal, 7.3. példa 
###################################################
elso  =  c(3490, 3440, 3300, 3170, 3260, 3580, 3250, 
2870, 3020, 3030)
masodik  =  c(3840, 3520, 3420, 3480, 3030, 4030, 
3020, 3230, 3010, 3100)
t.test(elso, masodik, alternative = "less", paired = T)

###################################################
### 204-205. oldal, 7.4. példa
###################################################
hom36C = c(35.9, 36.2, 35.3, 36.3, 36.2, 35.6, 35.7, 
36.1, 35.9, 36.1)
hom44C = c(44.3, 43.9, 44.9, 43.5, 44.6, 43.2, 44.6, 
43.3, 43.2, 44.3)
var.test(hom36C,hom44C,alternative="two.sided")

###################################################
### 208-209. oldal, 7.5. példa
###################################################
megfigy  =  c(6, 5, 13, 11, 5, 5)
valosz  =  rep(1/6, 6)
chisq.test(x = megfigy, p = valosz)

###################################################
### 212.oldal, 7.3. ábra
###################################################
minta = rnorm(30,mean=0.5,sd=1)
qqnorm(minta,pch=20,main="")
qqline(minta)

minta = rnorm(100,mean=0.5,sd=1)
qqnorm(minta,pch=20,main="")
qqline(minta)
###################################################
### 216.oldal, 7.8. példa
###################################################
(x  =  matrix(c(40, 22, 18 ,26 ,5, 11), nrow = 2))
chisq.test(x)

###################################################
### 218.oldal, 7.10. példa
###################################################
(x  =  matrix(c(40, 22, 18 ,26 ,5, 11), nrow = 2))
fisher.test(x)

###################################################
### 221-222. oldal, 7.11. példa
###################################################
binom.test(x=41,n=50,p=.5)
prop.test(x=41,n=50,p=.5,correct=F)

###################################################
### 224-225. oldal, 7.12. példa
###################################################
beteg  =  c(212, 126)
osszes  =  c(670, 520)
prop.test(beteg, osszes, correct = F)
(x  =  matrix(c(212, 458, 126, 394), nrow = 2))
chisq.test(x, correct = F)
fisher.test(x)

###################################################
### 226. oldal, 7.13. példa
###################################################
(x  =  matrix(c(151, 145, 192, 112), nrow = 2))
mcnemar.test(x)

###################################################
### 228-229. oldal, 7.14. példa
###################################################
binom.test(14, n = 21, p = 0.5)

###################################################
### 230. oldal, 7.15. példa
###################################################
(x  =  matrix(c(11, 10, 7, 14), nrow = 2))
fisher.test(x)

###################################################
### 232-233. oldal, 7.16. példa 
###################################################
x = c(1.4,3.3,5.0,5.0,6.2,7.5,10.1,10.5,13.0,18.1)
wilcox.test(x,mu=9,alternative="less")
wilcox.test(x,mu=9,alternative="less",correct=F)
x = c(1.4 , 3.3 , 5.0 , 5.0 , 6.2 , 7.5 , 10.1 , 10.5 , 13.0 , 18.1)
library(coin)
# ismételjük a hipotetikus értéket annyiszor, ahány elemû a minta
mu = rep(9, length(x))
wilcoxsign_test(x ~ mu, alternative="less", distribution="exact")

###################################################
### 234-235. oldal, 7.17. példa  
###################################################
zajos = c(0.24,0.36,0.20,0.30,0.40,0.34,0.20,0.44,0.38,0.47)
csendes = c(0.24,0.11,0.27,0.36,0.19,0.14,0.25,0.37,0.08,0.10)
wilcox.test(zajos, csendes, paired=T, alternative="greater", correct=F)
wilcox.test(zajos, csendes, paired=T, alternative="greater")
library(coin)
wilcoxsign_test(zajos~csendes, alternative="greater",
distribution="exact")
###################################################
### 237-238. oldal, 7.18. példa
###################################################
kezelt = c(9.1, 10.3, 11.0, 11.5, 11.9, 9.5, 10.6, 9.3, 11.0, 9.8)
kontroll = c(8.1, 8.4, 9.2, 9.4, 8.8, 9.8, 8.2, 10.3, 9.5)
wilcox.test(kezelt,kontroll,alternative="greater",correct=F)
mind = c(kezelt, kontroll)
csoport = factor(rep(c(1,2), c(10,9)))
library(coin)
wilcox_test(mind ~ csoport,alternative="greater", distribution="exact")

###################################################
###  240-241. oldal, 7.19. példa, 7.7. ábra
###################################################
megfigy = c(37,14, 8,18, 7,36,18,28,51,22,17, 0, 3,41, 
8,44,62,81,48,39)
terulet = c( 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 
3, 4, 4, 4, 4, 4)
kruskal.test(megfigy,terulet)
kruskal.test(megfigy~terulet)
stripchart(megfigy~terulet,xlab='Pipacsok száma',ylab='Terület', 
group.names=c('A','B','C','D'),pch=1)
library(coin)
kruskal_test(megfigy ~ factor(terulet))
kruskal_test(megfigy ~ factor(terulet),
distribution=approximate(B=50000))




