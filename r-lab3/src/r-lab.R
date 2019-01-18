False = 0
num = 1000
M = 100
a = 1

if(False){
    note = '1 2'
}
myFun1 <- function(num, M, a){
    e_error = numeric(M)
    m_error = numeric(M)
    index = numeric(M)
    for(i in 1:M){
        index[i] = i
        sim = rcauchy(num,1,1)
        x = mean(sim)
        y = median(sim)
        m_error[i] = x-a
        e_error[i] = y-a
    }
    res <- c(sum(e_error)/M, sum(m_error)/M)
    return (res)
}

if(False){
    note = '3 4'
}

Newton <-function(fun, x,eps = 0.5){
  k <- 0
  repeat{
  k <- k+1
    x1 <- x
    obj <- fun(x)
    x <- x - solve(obj$jac,obj$f)
    if((x-x1)%*%(x-x1)<eps){
      return(list(root=x,iter=k))
      break
    }
  }
}

myFun2 <- function(num, M, a){
    e3_error = numeric(M)
    e4_error = numeric(M)
    index = numeric(M)
    for(i in 1:M){
        index[i] = i
        sim = rcauchy(num,1,1)
        f1 <- function(a){
            res = 1
            for(j in 1:num){
                res = res/(pi*((sim[j]-a)*(sim[j]-a)+1))
            }
            return(res)
        }
        s = optimize(f1,c(-20,20),maximum=TRUE)
        e3_error[i] = s$maximum - 1

        f2 <- function(a){
            res = 0
            j = 0
            for(j in 1:num){
                res = res + ((2*(sim[j]-a))/((sim[j]-a)*(sim[j]-a)+1))
                j = j+ (2*(sim[j]-a)*(sim[j]-a)-2)/(((sim[j]-a)*(sim[j]-a)+1)*((sim[j]-a)*(sim[j]-a)+1))
            }

            jac <- matrix(c(j),nr=1)
            list(f=res,jac=jac)
        }
        s = Newton(f2,c(0))
        e4_error[i] = s$root - 1
    }
    res <- c(sum(e3_error)/M, sum(e4_error)/M)
    return(res)
}

res = myFun1(num, M, a)
s = myFun2(num, M, a)
