False = 0
if(False){
    note = 'test generation'
}

if(False){
    note = 'design my own function
            x is the var
            return the value of f(x)'
}
myFun <- function(x){
    if(x < 0){
        return (0)
    }
    else{
        if(x > 4){
            return (0)
        }
        else{
             if(x <=2){
                tmp = sqrt(x*(2-x))
                y = tmp/(pi/2 + 2)
                return (y)
            }
            else{
                y = (x-2)/(pi/2 + 2)
                return (y)
            }
        }
    }
}

begin = 0
end = 4
num = 100000
count_x = 0
for(j in 1:5){
    sim = numeric(num)
    for(i in 1:num){
        count_x = count_x + 1
        sim[i] = runif(1,begin,end)
        y = runif(1,0,1)
        while(y > myFun(sim[i])){
            count_x = count_x + 1
            sim[i] = runif(1,begin,end)
            y = runif(1,0,1)
        }
    }
}
paste("total num=100000, count_x = ",count_x/10)