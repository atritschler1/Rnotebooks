#library(ma391laporte)

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


p = function(x){(0.65-0.01*t)*(200+5*t-t^2/60)-0.45*t}
dp = function(t){fprime(p,t)}
t = seq(0,20)
ans = bisection(dp,10)
print(ans)
print(p(ans))



months = seq(1,10)
ans.time = 0
ans.profit = 0
for (i in 1:length(months)){
  m = months[i]
  p = function(t){(0.65-0.01*t)*((5/m)*(m*t-t^2/60)+200)-0.45*t}
  dp = function(t){fprime(p,t)}
  ans.time[i] = newton(dp,10)
  ans.profit[i] = p(ans.time[i])
}

result = data.frame(months = months,time = ans.time,profit = ans.profit)
print(result)


