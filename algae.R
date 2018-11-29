library(DMwR)
data(algae)

##描述分析##
summary(algae.a5)

hist(algae$a5,prob=T,xlab='',main='Histogram of a5')

boxplot(algae$a5~algae$size,xlab='River Size',ylab='Algae a5')

algae.a5 <- algae[,c(1:11,16)]


##################################################
### 1 Multiple Linear Regression
###################################################
library(DMwR)
data(algae)
algae <- algae[-manyNAs(algae), ]
algae_a5 <- algae[,c(1:11,16)]
clean.algae <- knnImputation(algae, k = 10)
clean.algae_a5 <- clean.algae[,c(1:11,16)]
 
#多元线性回归模型
lm.a5 <- lm(a5 ~ .,data=clean.algae_a5)
summary(lm.a5)

#精简回归模型
anova(lm.a5)
lm2.a5 <- update(lm.a5, . ~ . - mxPH)
summary(lm2.a5)


anova(lm.a5,lm2.a5)
#对两个模型进行比较，两个模型并无显著不同

#对初始模型用向后消元法
final.lm <- step(lm.a5)
summary(final.lm)
#这个模型所解释的方差比例(R^2)仍然不是很可观，这样的R^2表明对海藻案例应用假定的线性模型是不合适的


#利用此模型进行预测
lm.predictions.a5 <- predict(final.lm,clean.algae_a5)
mean((lm.predictions.a5-algae[,'a5'])^2)/
                mean((mean(algae[,'a5'])-algae[,'a5'])^2)


# 0.7304913


###################################################
### 2 Regression Trees
###################################################
library(rpart)
rt.a5 <- rpart(a5 ~ .,data=algae_a5)
rt.a5

printcp(rt.a5)
prettyTree(rt.a5)


#利用此模型进行预测
rt.predictions.a5 <- predict(rt.a5,algae_a5)

mean((rt.predictions.a5-algae[,'a5'])^2)/
                mean((mean(algae[,'a5'])-algae[,'a5'])^2)

# 0.5221316


plot(rt.predictions.a5,algae_a5$a5,main="rpart model",
xlab="Predicitons",ylab="True Values")
abline(0,1,lty=2)

#剪枝#
rt2.a5 <- prune(rt.a5,cp=0.048)
rt2.predictions.a5 <- predict(rt2.a5,algae_a5)

mean((rt2.predictions.a5-algae[,'a5'])^2)/
                mean((mean(algae[,'a5'])-algae[,'a5'])^2)

# 0.6115449

###################################################
### 3 nnet
###################################################
library(nnet)
data_scale <- scale(clean.algae_a5[,4:12])
norm.data <- cbind(clean.algae_a5[,1:3],data_scale)

#使用nnet命令，参数规定隐层单元个数为10，权重调整速度为0.01，最大迭代次数为1000次，线性输入
nn <- nnet(a5~., norm.data,size=10,decay=0.01,maxit=1000,linout=T,trace=F)

#利用模型进行预测 
norm.preds.nn <- predict(nn,norm.data)

preds.nn <- unscale(norm.preds.nn,data_scale)


plot(norm.preds.nn,scale(clean.algae_a5$a5),main="nnet model",
xlab="Predicitons",ylab="True Values")
abline(0,1,lty=2)

#计算相对误差 
(nmse2 <- mean((norm.preds.nn-scale(clean.algae_a5$a5))^2)/ 
   mean((mean( scale(clean.algae_a5$a5))- scale(clean.algae_a5$a5))^2)) 




(nmse.a5.nn <- mean((preds.nn-clean.algae_a5[,'a5'])^2)/
                mean((mean(clean.algae_a5[,'a5'])-clean.algae_a5[,'a5'])^2))







###################################################
### 4 svm
###################################################
library(e1071)
sv <- svm(a5~., clean.algae_a5)

s.preds <- predict(sv,clean.algae_a5)


plot(s.preds,clean.algae_a5$a5,main="svm model",
xlab="Predicitons",ylab="True Values")
abline(0,1,lty=2)

mean((s.preds-algae[,'a5'])^2)/
                mean((mean(algae[,'a5'])-algae[,'a5'])^2)






###################################################
### 5 randomForest
###################################################
library(randomForest)
rf.a5 <- randomForest(a5~.,data=clean.algae_a5)
importance(rf.a5,type=2)
rf.preds <- predict(rf.a5,clean.algae_a5)

plot(rf.preds,clean.algae_a5$a5,main="randomForest model",
xlab="Predicitons",ylab="True Values")
abline(0,1,lty=2)


(nmse.a5.rf <- mean((rf.preds-algae[,'a5'])^2)/
                mean((mean(algae[,'a5'])-algae[,'a5'])^2))









###################################################
### 2.7 Model Evaluation and Selection
###################################################
regr.eval(algae[,'a5'],lm.predictions.a5,train.y=clean.algae[,'a5'])
regr.eval(algae[,'a5'],rt.predictions.a5,train.y=algae[,'a5'])
regr.eval(algae[,'a5'],norm.preds.nn,train.y=scale(clean.algae[,'a5']))
regr.eval(algae[,'a5'],s.preds,train.y=clean.algae[,'a5'])
regr.eval(algae[,'a5'],rf.preds,train.y=clean.algae[,'a5'])



#对回归模型的改进
sensible.lm.predictions.a5 <- ifelse(lm.predictions.a5 < 0,0,lm.predictions.a5)
regr.eval(algae[,'a5'],lm.predictions.a5,stats=c('mae','mse'))
regr.eval(algae[,'a5'],sensible.lm.predictions.a5,stats=c('mae','mse'))


#####交叉验证
cv.rpart <- function(form,train,test,...) {
  m <- rpartXse(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}


cv.lm <- function(form,train,test,...) {
  m <- lm(form,train,...)
  p <- predict(m,test)
  p <- ifelse(p < 0,0,p)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}


cv.rf <- function(form,train,test,...) {
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}


cv.nn <- function(form,train,test,...){
  m <- nnet(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}


cv.svm <- function(form,train,test,...){
  m <- svm(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}



res.all <- experimentalComparison(
            c(dataset(a5 ~ .,clean.algae[,c(1:11,16)],'a5')),
            c(variants('cv.lm'), 
              variants('cv.rpart',se=c(0,0.5,1)),
		  variants('cv.rf',ntree=c(200,300,400),mtry=c(1,2,3)),
		  variants('cv.nn',size=c(5,10,15),decay=c(0.01,0.1)),
		  variants('cv.svm',gamma=c(0.01,0.001),cost=c(1,10))
             ),
            cvSettings(5,10,1234))

summary(res_all)
#最优模型
bestScores(res_all)
plot(res.all)
getVariant("cv.rf.v3",res_all)@pars

compAnalysis(res_all,against='cv.rf.v3')




### 预测a5的频率 ###
bestModelsName <- sapply(bestScores(res_all),
                          function(x) x['nmse','system'])
learner <- c(rf='randomForest',rpart='rpartXse',svm='svm') 
func <- learner[sapply(strsplit(bestModelsName,'\\.'),
                        function(x) x[2])]
parSetts <- getVariant("cv.rf.v2",res_all)@pars
form <- as.formula(paste(names(clean.algae)[16], '~ .'))
Model <- do.call(func,c(list(form,clean.algae[,c(1:11,16)]),parSetts))


clean.test.algae <- knnImputation(test.algae, k = 10, distData = algae[,1:11])
preds <- predict(Model, clean.test.algae)
avg.preds <- mean(algae[,'a5'])

mean((algae.sols$a5 - preds)^2) / mean(scale(algae.sols$a5, avg.preds,F)^2)







