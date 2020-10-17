set.seed(seed=1234)
l=function(t,a,b)
{
 m=rgamma(t,a,b)
 M=matrix(m,nrow=t)
 M
}
A=l(50,2,9)
A
E=function(n,i)
{
 x=rpois(n,A[i,])
 x_bar=sum(x)/n
 x_bar
}
J=function(n,i)
{
 x=rpois(n,A[i,])
 x_bar=mean(x)
 x_bar+(1/(2*n))
}
n=c(10:50)
E(n,1)
J(n,1)
plot(n,E(n,1),col="red",type="o",pch="+",xlab="Sample Size",ylab="Bayesian and ML Estimates")
points(n,J(n,1),col="dark green",pch="*")
lines(n,J(n,1),col="dark green",lty=2)