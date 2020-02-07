#MV Optimization Plotting#

Units = function(p){c((600-3*p[1]+p[2]),(800-2*p[2]+p[1]))}

Profit = function(p){(p[1]*(600-3*p[1]+p[2])+p[2]*(800-2*p[2]+p[1])-
          (200*(600-3*p[1]+p[2])+300*(800-2*p[2]+p[1])))*(-1)}
#multiply by -1 to auto do minimization#

x = c(100,100)
ans = optim(x,Profit)
print(ans)
Units(ans$par)


P = function(c1){
  Profit = function(p){(p[1]*(600-3*p[1]+p[2])+p[2]*(800-2*p[2]+p[1])-
                          (200*(600-3*p[1]+p[2])+300*(800-2*p[2]+p[1])))*(-1)}
  x = c(100,100)
  ans = optim(x,Profit)
  return(ans)
}

cost1 = seq(200,600,20)
ans.profit = 0
