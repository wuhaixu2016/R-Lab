
False = 0
if(False){
    note = 'second error'
}
n = 30
num = 5
err1List = numeric(num)
err2List = numeric(num)
for(mu in 2:5){
    err1 = 0
    for(i in 1:1000){
        data <- rnorm(n, mu, sqrt(30))
        avg = sum(data)/30
        if(avg<1.645){
            err1 = err1+1
        }
    }
    err1List[mu] = err1
    err2 = 0
    for(i in 1:1000){
        data <- rnorm(n, mu, sqrt(30))
        avg = sum(data)/30
        if(avg<1.96){
            if(avg> -1.96){
                err2 = err2+1
            }
        }    
    }
    err2List[mu] = err2
}

paste("first ",err1List)
paste("second ",err2List)  
