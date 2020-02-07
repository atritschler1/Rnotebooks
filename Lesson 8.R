#Constrained Optimizaion#

install.packages("NlcOptim")
library(MASS);library(NlcOptim)

### Number 1 Lesson 7 Board sheet ###

obj=function(x){

  return((-16*x[2]^2+(x[3]-32)*x[1]*x[2]+1/2*(x[3]-32)*x[1]^2)*(-1)) # Problem 1b

}

x = c(5.49,1.33,42.67) #solution for part a
print(obj(x))

#part b
con = function(x){
  f = NULL
  f = rbind(f,x[3]^2*x[1]-10000)
  f = rbind(f,-32*x[2]+(x[3]-32)*x[1])
  return(list(ceq=f,c=NULL))
}

solnl(x,objfun = obj,confun = con)
##########################################
Outer = function(f,x){
  n1 = length(x[[1]])
  n2 = length(x[[2]])
  res = matrix(0,nrow=n1,ncol=n2)
  rownames(res) = x[[1]]
  colnames(res) = x[[2]]
  for (i in 1:n1){
    for (j in 1:n2){
      res[i,j]=f(c(x[[1]][[i]],x[[2]][[j]]))
    }
  }
  return(res)
}
##########################################
obj1=function(x){
  ((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])*(-1)
}
x0 = c(1,1)
optim(x0,obj1)
X = list(x=seq(100,500),y=seq(100,500))
Z = Outer(obj1,X)
contour(x=X$x,y=X$y,z=-Z)

con = function(x){
  f = NULL
  f = rbind(f,(x[1]-300)^2+(x[2]-300)^2-200^2)
  return(list(ceq=f,c=NULL))
}





