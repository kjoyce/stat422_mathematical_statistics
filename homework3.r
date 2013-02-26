# 10. Homework Supplement (6?)
# The following codes demonstrates the central limit theorem
# for chi squared with a sample size of M.  We take repeat 
# the sample N times.  The density of N(1,sqrt(2/M)) is 
# plotted on top of the histogram
N = 100  # replications
M = 100  # sample size
sig2 = 3

X = matrix(data=rnorm(N*M,mean=0,sd=sqrt(sig2)),nrow=M,ncol=M)
Xi = colSums(X^2)/M

hist(Xi,breaks=N/8,freq=FALSE)
f = function (x) dnorm(x,mean=sig2,sd=sqrt(2/M)*sig2)
curve(f,add=TRUE,col='red')

# Perform transformation
dev.new()
Xj = Xi^(-1) 
hist(Xj,breaks=N/10,freq=FALSE)
f = function (x) dnorm(x,mean=sig2^-1,sd=sqrt(2/M)*sig2^-1)
curve(f,add=TRUE,col='red')
