
False = 0
if(False){
    note = 'first error'
}
n = 30

err1 = 0
for(i in 1:1000){
    data <- rnorm(n, 0, sqrt(30))
    avg = sum(data)/30
    if(avg>1.645){
        err1 = err1+1
    }
}

err2 = 0
for(i in 1:1000){
    data <- rnorm(n, 0, sqrt(30))
    avg = sum(data)/30
    if(avg>1.96){
        err2 = err2+1
    }    
    if(avg< -1.96){
        err2 = err2+1
    }
}

paste("first ",err1)
paste("second ",err2)