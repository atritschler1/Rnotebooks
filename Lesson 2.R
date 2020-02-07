profit = function (x){
  return((0.65-0.01*x)*(200+5*x)-.45*x)
}
x = seq(0,20,1)
plot(x,profit(x),type="o")
print(profit(8))

x=seq(0,20)
fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}
plot(x,fprime(profit,x))

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
#Example of a finding a root of a function
f = function (x){x^3-5} #sinlge line function definition
zero=bisection (f,-10,20)
x=seq(-1,2,0.01)
plot(x,f(x),"l")
abline(h=0,col="red")
abline(v=zero,col="blue")
print(zero)

dProfit = function(x){fprime(profit,x)}
bisection(dProfit,0,20)


#############################################

x = seq(0,20)
profit = function(t){(0.65-0.01*t)*(200+5*t)-0.45*t}
dProfit = function(t){fprime(profit,t)}
plot(x,profit(x),"l")

ans = bisection(dProfit,0,20)
print(ans)
print(profit(ans))

ans.time = 0
ans.prof=0
r = seq(.008,.012,.001)
for (i in 1:length(r)){
  profit = function(t){(0.65-r[i]*t)*(200+5*t)-0.45*t}
  dProfit = function(t){fprime(profit,t)}
  ans.time[i] = bisection(dProfit,0,20)
  ans.prof[i] = profit(ans.time[i])
}
results = data.frame(price=r,time=ans.time,profit=ans.prof)
print(results)
