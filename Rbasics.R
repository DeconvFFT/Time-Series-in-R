library(WRS)
x <- c(1,20,30,234,5,6,7)
tmean(x,tr=0.2)

idealf(x) # using h,j,k method for ideal forths
summary(x) # summary stats with min,q1,median,mean,q2,max

var(x) # variance
sd(x) #standard deviation

winvar(x,tr=0.2)
winse(x,tr=0.2)

mad(x) # computes MADN = MAD/0.6745

outms(x)
outbox(x)
outpro(x)
