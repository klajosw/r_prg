########################################################
# The Tukey-Kramer correction of p-values and confidence limits.
# The tk.adjust() function requires as argument a matrix with 3 columns. 
# The first column should contain the contrast estimates, 
# the second the estimated standard deviation (of the estimated contrast) 
# and the third the degrees of freedom.
# Adjusted confidence limits can also be obtained specifying the option conf.int.

# PREPARED BY THE STATISTICS GROUP, KVL, Denmark


tk.adjust<-function(estMat,conf.int)
{
noCI<-F
if (missing(conf.int)) {noCI<-T}
n1<-nrow(estMat)
n<-(1+sqrt(1+8*n1))/2
confWidth<-rep(0,n1)
pValue<-rep(0,n1)
for (i in 1:n1)
{
dev<-estMat[i,2]/sqrt(2)
if (!noCI) {confWidth[i]<-qtukey(conf.int,n,estMat[i,3])*dev}
pValue[i]<-ptukey(abs(estMat[i,1])/dev,n,estMat[i,3],lower.tail=F)
}
dimNames<-attr(estMat,"dimnames")
dimNames[[2]]<-c("Adj Pr(>|t|)","Adj Lower CI","Adj Upper CI")
pdiffMat<-matrix(0,n1,3,dimnames=dimNames)
pdiffMat[,2]<-estMat[,1]-confWidth
pdiffMat[,3]<-estMat[,1]+confWidth
pdiffMat[,1]<-pValue
if (noCI) {return(pdiffMat[,1])}
else {return(pdiffMat)}
}
