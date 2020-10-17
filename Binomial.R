set.seed(seed=123)
N=20
p=function(t,a,b)
{
 m=rbeta(t,a,b)
 M=matrix(m,nrow=t)
 M
}
A=p(50,2,9)
A
E=function(n,i)
{
 x=rbinom(n,N,A[i,])
 x_bar=mean(x)
 x_bar/n
}
CP=function(n,i)
{
 x=rbinom(N,n,A[i,])
 p_star=(sum(x)+1/2)/((N*n)+1/2)
 p_star
}
n=c(10:20)
E(n,1)
CP(n,1)
plot(n,E(n,1),col="red",type="o",pch="+",xlab="Sample Size",ylab="Bayesian and ML Estimates")
points(n,CP(n,1),col="dark green",pch="*")
