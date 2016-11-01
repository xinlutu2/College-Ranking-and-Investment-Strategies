library(lpSolve)

rm(list=ls())
data=read.csv("ExcellentData.csv",na.strings = ".")
inputs=data.frame(data[2])
outputs=data.frame(data[c(3,4,5,6,7)])
N=dim(data)[1] #number of DMUs
s=dim(inputs)[2] #number of input variable s= 1 
m=dim(outputs)[2] #number of outputs m=4

for (i in 1:N)
{
  f.rhs <- c(rep(0,1,N),1)
  f.dir <- c(rep("<=",1,N),"=")
  aux <- cbind(-1*inputs,outputs)
  
  f.obj <- c(0*rep(1,s),as.numeric(outputs[i,]))
  f.con <- rbind(aux ,c(as.numeric(inputs[i,]), rep(0,1,m)))
  results <- lp ("max",as.numeric(f.obj), f.con, f.dir, f.rhs,scale=0, compute.sens=TRUE)
  if(i==1)
  {
    weights <- results$solution
    effcrs <- results$objval
    lambdas <- results$duals[seq(1,N)]
  }
  else
  {
    weights <- rbind(weights, results$solution)
    effcrs <- rbind(effcrs , results$objval)
    lambdas <- rbind(lambdas, results$duals[seq(1,N)] )
  }
}



