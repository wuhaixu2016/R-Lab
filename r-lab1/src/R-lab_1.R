
False = 0
if(False){
    note = 'For Question One'
}


if(False){
    note = 'for mySim
    u, r are for rnorm
    n, m are number for each sim. and times for sim'
}
mySim <- function(u, r, n, m){
    x_sim = numeric(m)
    r_sim = numeric(m)
    for(i in 1:m){
        data <- rnorm(n, u, r)
        x_sim[i] = sum(data)
        r_sim[i] = sd(data)
        r_sim[i] = (r_sim[i]*r_sim[i])
        r_sim[i] = (r_sim[i])*n/(n-1)
        x_sim[i] = x_sim[i]/n
    }

    if(False){
        note = 'feature for E(E(sim data)),'
    }
    e_e_sim = sum(x_sim)/m

    if(False){
        note = 'feature for r(r(sim data)),'
    }
    r_r_sim = sum(r_sim)/m

    res <- c(e_e_sim, r_r_sim)
    return (res)
}

if(False){
    note = 'repeat up work for several times and cal sd'
    note = 'for mySim
    u, r are for rnorm
    n, m are number for each sim. and times for sim'
}

mySimRepeat <- function(num, u, r, n, m){
    e_record = numeric(num)
    r_record = numeric(num)
    for(i in 1:num){
        tmp = mySim(u,r,n,m)
        e_record[i] = tmp[1]
        r_record[i] = tmp[2]
    }
    e = sum(e_record)/num
    r_e = sd(e_record)

    r = sum(r_record)/num
    r_r = sd(r_record)
    final_res <- c(e, r_e, r, r_r)
    return(final_res)
}

mat <- matrix(1:4, 2, 2)
layout(mat)
if(False){
    note = 'draw x'
    note = 'adjust n'
}
x = numeric(10)
y = numeric(10)
z = numeric(10)
for(i in 1:10){
    m = mySimRepeat(10, 0, 1, i*20, 500)
    y[i] = m[1]
    z[i] = m[2]
    x[i] = i
}
plot(x,y,type='b',lty=6,col='red')
plot(x,z,type='b')

if(False){
    note = 'draw r'
    note = 'adjust n'
}

x = numeric(10)
y = numeric(10)
z = numeric(10)
for(i in 1:10){
    m = mySimRepeat(10, 0, 1, i*20, 500)
    y[i] = m[3]
    z[i] = m[4]
    x[i] = i
}
plot(x,y,type='b',lty=6,col='red')
plot(x,z,type='b')