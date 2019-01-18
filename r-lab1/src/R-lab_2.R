False = 0
if(False){
    note = 'For Question Two'
}

if(False){
    note = 'for mySim
    u, r are for rnorm
    n, m are number for each sim. and times for sim'
}
mySim <- function(u, r, n, m){
    x_sim = numeric(m)
    for(i in 1:m){
        data <- rnorm(n, u, r)
        x_sim[i] = sum(data)
        x_sim[i] = x_sim[i]/n
    }

    if(False){
        note = 'feature for E(E(sim data)),'
    }

    return (x_sim)
}

if(False){
    note = 'draw'
}
mat <- matrix(1:4, 2, 2)
layout(mat)
data = mySim(0,1,50,50000)
hist(data)
par(new=TRUE)
curve(dnorm(x,mean=0,sd=sqrt(1/50))*50000,from=-0.5,to=0.5)
par(new=FALSE)
data = mySim(0,1,100,50000)
hist(data)
par(new=TRUE)
curve(dnorm(x,mean=0,sd=sqrt(1/100))*50000,from=-0.5,to=0.5)
par(new=FALSE)
data = mySim(0,1,200,50000)
hist(data)
par(new=TRUE)
curve(dnorm(x,mean=0,sd=sqrt(1/200))*50000,from=-0.5,to=0.5)
par(new=FALSE)
data = mySim(0,1,400,50000)
hist(data)
par(new=TRUE)
curve(dnorm(x,mean=0,sd=sqrt(1/400))*50000,from=-0.5,to=0.5)
par(new=FALSE)
