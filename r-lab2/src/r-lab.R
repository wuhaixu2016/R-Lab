False = 0
if(False){
    note = 'generate var for special function'
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


if(False){
    note = 'sim generate var for myFun
            begin: x min
            end : x max
            num : num of points'
}

number = 100000

mySim <- function(begin, end, num){
    count_x = 0
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
    paste("total num=100000, count_x = ",count_x)
    return (sim)
}

if(False){
    note = 'acl e_sim'
}

e_sim = sum(sim)/number
paste("sum(sim)/n = ",e_sim)
if(False){
    note = 'draw hist'
}

sim = mySim(0,4,number)
hist(sim, freq = F)
par(new=TRUE)
x1 = seq(0, 2, 0.1)
f1 = sqrt(x1*(2-x1))/(pi/2+2)
plot(x1, f1, type="l", col="red", xaxt="n", yaxt ="n", bty = "n", xlab="", ylab="", xlim=c(0,4), ylim=c(0,0.5))
par(new=TRUE)
x2 = seq(2, 4, 0.1)
f2 = (x2-2)/(pi/2+2)
plot(x2, f2, type="l", col="red", xaxt="n", yaxt ="n", bty = "n", xlab="", ylab="", xlim=c(0,4), ylim=c(0,0.5))
par(new=False)