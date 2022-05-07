data<-read.csv(file.choose(), header=T , sep="")

data=regresyon_veriler
names(data)
names(data)<-c("y","x1","x2","x3","x4")
x4<-as.factor(x4)
attach(data)
qqnorm(data$y)
shapiro.test(data$y)

shapiro.test(x1)
shapiro.test(x2)
shapiro.test(x3)
sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)
pairs(data)

lny<-log(y)
qqnorm(lny)
shapiro.test(lny)

inf<-ls.diag(sonuc)

inf

par(mfrow=c(2,2)) 

plot(predict(sonuc), inf$stud.res, ylab="Studentized Residuals", xlab="Predicted Value")



cooksd <- cooks.distance(sonuc)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1),names(cooksd),""), col="red")

which(cooksd > 0.04)


hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*length(data)/length(y) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*length(data)/length(y),index(hat),""), col="red")
which( hat > (2*6)/100)


stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
which(stud > 3) 


std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")
which(std > 2) 


data = regresyonveriyeni
names(data)
names(data)<- c("y","x1","x2","x3","x4")

x4<-as.factor(x4)

attach(data)

qqnorm(data$y)

shapiro.test(data$y)

shapiro.test(x1)
shapiro.test(x2)
shapiro.test(x3)

sonuc<-lm(y~x1+x2+x3+x4)
summary(sonuc)

confint(sonuc, level= .99)

summary(lm(abs(residuals(sonuc)) ~ fitted(sonuc)))
library(lmtest)
bptest(sonuc)
dwtest(sonuc)
inf<-ls.diag(sonuc)
inf
par(mfrow=c(2,2))
plot(predict(sonuc), inf$stud.res, ylab="Studentized Residuals", xlab="Predicted Value")

ols_coll_diag(sonuc)

plot(predict(sonuc), sonuc$stud.res, ylab="Studentized Residuals", xlab="Predicted Value")
library(fastDummies)
dummy<-dummy_cols(x4)
x41<-dummy$.data_1
x42<-dummy$.data_2
x43<-dummy$.data_3

x = data.frame(x1=4.585003, x2=1.06304509,x3=2.798820,x4="1")

predict(sonuc, data = x,interval = 'confidence')
x$x4 <- as.numeric(x$x4)

x = data.frame(x1=10, x2=5,x3=2,x4=1)

x$x4 <- as.factor(x$x4)

distPred <- predict(sonuc, x,interval = 'confidence')  
distPred


library(stats)

lm.null <- lm(y ~ 1)

forward <- step(lm.null,y~x1+x2+x3+x4, direction = "forward")

forward

summary(forward)
ols_step_all_possible(resnew)


backward<-step(sonuc,direction="backward")

summary(backward)

ols_step_backward_p(sonuc)
sonuc


library(MASS)

step.model <- stepAIC(sonuc, direction = "both", trace = FALSE)

summary(step.model)



ridge <- lm.ridge(y~x1+x2+x3+x4 ,lambda = seq(0,1,0.05))

matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),
        
        ylab=expression(hat(beta)))

abline(h=0,lwd=2)

ridge$coef
select(ridge)
ridge$coef[,ridge$lam == 0.4]
