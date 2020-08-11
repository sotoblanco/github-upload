#One way ANOVA
growth=c(1.45,0.76,1.11,1.71,0.97,1.24,1.93,1.96,2.20,
         3.93,2.24,3.71,2.92,3.01,6.33,1.18,0.59,0.52,-0.74,-0.99)

substrate=c('sand','sand','sand','sand','sand','silt',
            'silt', 'silt','silt','silt','pebbles',
            'pebbles','pebbles','pebbles', 'pebbles',
            'glass','glass','glass','glass','glass')

MyData=data.frame('growth'=growth, 'substrate'=substrate)

boxplot(growth~substrate, ylab='Algal Growth Rate (/d)',xlab='Substrate Type')
MyFit=lm(growth~substrate, data=MyData)
summary(MyFit)
summary(aov(MyFit))
qf(0.95,3,16)

#ANOVA ASSUMPTIONS
hist(residuals(MyFit),main="",xlab="Residuals", freq=FALSE)
MyRes=residuals(MyFit)
xfit=seq(min(MyRes),max(MyRes),length=100) #new x variable
yfit=dnorm(xfit,0,sd(MyRes)) #predicted Normal
Lines(xfit, yfit, col="blue") #add a blue line.
shapiro.test(residuals(MyFit))
plot(MyFit,1)
bartlett.test(growth~substrate, data=MyData)
TukeyHSD(aov(MyFit))

library("contrast")
contrast(MyFit,list(substrate="pebbles"),list(substrate="glass"))
contrast(MyFit,list(substrate="pebbles"),list(substrate="sand"))
contrast(MyFit,list(substrate="pebbles"),list(substrate="silt"))
alphaF=0.05
alphaC=1-(1-alphaF)^(1/3)
alphaC