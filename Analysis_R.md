DATA=read.csv2("C://Users//Tay Shin//Desktop//Last Semester//Stat 157//proj1//quest.csv",header=TRUE,sep=',')


mod1=lm(y~x[,1])
summary(mod1)
plot(y,x[,1],main="lang vs visual")
abline(mod1,col='red')
# not significant

mod2=lm(y~x[,2])
plot(y,x[,2],main="lang vs aural")
summary(mod2)
abline(mod1,col='red')
# not significant


mod3=lm(y~x[,3])
plot(y,x[,3],main="lang vs read.write")
summary(mod3)
abline(mod1,col='red')
# not significant


mod4=lm(y~x[,4])
plot(y,x[,4],main="lang vs kinesthetic")
summary(mod4)
abline(mod1,col='red')
# not significant


mod5=lm(y~x[,1]+x[,2]+x[,3]+x[,4])
summary(mod5)

# not significant
