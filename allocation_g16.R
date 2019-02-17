library(lpSolve)
allocation<-function(ROI=ROI_vec[10], upper_bound=NULL, budget)
{ if (is.null(upper_bound))
  {c=ROI
  A=matrix(0,3,10)
  A[1,]=1
  A[2,c(5,10)]=-1
  A[2,c(1,2)]=1
  A[3,c(3,4)]=2
  A[3,seq(from=5,to=10,by=1)]=-1
  b=c(budget,0,0)
  dir=c('=',rep("<=",2))
  s=lp("max",c,A,dir,b)
  ls<-list('objval'=s$objval,'sol'=s$solution)}
  else
  {c=ROI
  A=matrix(0,13,10)
  A[1,]=1
  A[2,c(5,10)]=-1
  A[2,c(1,2)]=1
  A[3,c(3,4)]=2
  A[3,seq(from=5,to=10,by=1)]=-1
  A[4:13,]=diag(10)
  b=c(budget,0,0,rep(upper_bound,10))
  dir=c('=',rep("<=",12))
  s=lp("max",c,A,dir,b)
  ls<-list('objval'=s$objval,'sol'=s$solution)}
  return(ls)
}

