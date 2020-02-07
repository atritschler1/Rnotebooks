#StepFunction Plotting#
## Define the function using boolean statements such as >, <=, ==
r = 5/7
fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}
x = seq(0,50)
plot(x,fT(x))

fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}
bisection = function(f,a,b,tol=0.0001){
  if (f(a)*f(b) > 0){
    return ("Boundary Conditions Not Met")
  }
  else{
    middle = a
    while (abs(f(middle))>tol){
      middle = (a+b)/2
      if (f(middle)*f(a)>0) (a = middle)
      else (b = middle)
      x=middle
      y=f(middle)
      ## if you want to "see" what happens at every step, take off the # of the next line ##
      #cat(sprintf("x-Val: %.4f ; f(x-val): %.4f\n",x,y))
    }
    return (middle)
  }
}

#library(ma391laporte)
dT = function(x){fprime(fT,x)}
bisection(dT,0,20)

cost = fT(x)
ans = data.frame(crews=x, cost=cost)
which(ans$cost==min(ans$cost))
#12 is the index (which gives the index of answer)

ans[which(ans$cost==min(ans$cost)),]
#this gives the answer at the index


curr.fine = 10000;curr.crews=11.28;curr.cost=508333.3
ans.crew=0;ans.cost=0;ans.days=0
r=5/7
fineValues = seq(5000,15000,1000)
for (i in 1:length(fineValues)){
  fine = fineValues[i]
  fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}
  dT = function(x){fprime(fT,x)}
  ans.crew[i] = bisection(dT,0,50)
  ans.cost[i] = fT(ans.crew[i])
  ans.days[i] = 200/(r*(ans.crew[i]+1))
}
result = data.frame(fine=fineValues,crew=ans.crew,cost=ans.cost,
                    days=ans.days)
print(result)
