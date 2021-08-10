datahat<-read.csv(file.choose())
summary(datahat)
view(datahat)
datax9<-datahat[-c(1,9,11:18)]
datax9_train<-datax9[1:80,]
datax9_test<-datax9[81:100,]
datax10<-datahat[-c(1,9,10,12:18)]
datax10_train<-datax10[1:80,]
datax10_test<-datax10[81:100,]

plot(datax9_train$X1 ~ datax9_train$X9, main = "Asumsi linearitas x1~x9") + abline(lm(datax9_train$X1 ~ datax9_train$X9))
cor.test(datax9_train$X1,datax9_train$X9)
plot(datax9_train$X2 ~ datax9_train$X9, main = "Asumsi linearitas x1~x9") + abline(lm(datax9_train$X2 ~ datax9_train$X9))
cor.test(datax9_train$X2,datax9_train$X9)
plot(datax9_train$X3 ~ datax9_train$X9, main = "Asumsi linearitas x3~x9") + abline(lm(datax9_train$X3 ~ datax9_train$X9))
cor.test(datax9_train$X3,datax9_train$X9)
plot(datax9_train$X4 ~ datax9_train$X9, main = "Asumsi linearitas x4~x9") + abline(lm(datax9_train$X4 ~ datax9_train$X9))
cor.test(datax9_train$X4,datax9_train$X9)
plot(datax9_train$X5 ~ datax9_train$X9, main = "Asumsi linearitas x5~x9") + abline(lm(datax9_train$X5 ~ datax9_train$X9))
cor.test(datax9_train$X5,datax9_train$X9)
plot(datax9_train$X6 ~ datax9_train$X9, main = "Asumsi linearitas x6~x9") + abline(lm(datax9_train$X6 ~ datax9_train$X9))
cor.test(datax9_train$X6,datax9_train$X9)
plot(datax9_train$X7 ~ datax9_train$X9, main = "Asumsi linearitas x7~x9") + abline(lm(datax9_train$X7 ~ datax9_train$X9))
cor.test(datax9_train$X7,datax9_train$X9)

full.modelx9 <- lm(X9~., data=datax9_train)
summary(full.modelX9)
step.modelX9 <- stepAIC(full.modelX9, direction="both", trace=FALSE)
summary(step.modelX9)

full.modelX9.studres <- studres(full.modelX9)
estimated_x9_full <- predict(full.modelX9,datax9_train)
plot(full.modelX9.studres ~ estimated_x9_full, main = "Asumsi homoskedatisitas full model") + abline(lm(full.modelX9.studres ~ estimated_x9_full))
bptest(full.modelX9)
step.modelX9.studres <- studres(step.modelX9)
estimated_x9_step <- predict(step.modelX9,datax9_train)
plot(step.modelX9.studres ~ estimated_x9_step, main = "Asumsi homoskedastisitas step model") + abline(lm(step.modelX9.studres ~ estimated_x9_step))
bptest(step.modelX9)

full.modelX9.res <- resid(full.modelX9)
plot(full.modelX9.res ~ datax9_train$X3, main = "Asumsi indepedensi residu x9~x3") + abline(lm(full.modelX9.res ~ datax9_train$X3))
vif(full.modelX9)
step.modelX9.res <- resid(full.modelX9)
plot(full.modelX9.res ~ datax9_train$X3, main = "Asumsi indepedensi residu X9~x3") + abline(lm(step.modelX9.res ~ datax9_train$X3))
vif(step.modelX9)

hist(full.modelX9.res, prob=TRUE, xlim=c(-15,15), ylim=c(0,0.15))
curve(dnorm(x, mean=mean(full.modelX9.res), sd=sd(full.modelX9.res)), add=TRUE)
ks.test(full.modelX9.res, "pnorm", mean=mean(full.modelX9.res), sd=sd(full.modelX9.res))
hist(step.modelX9.res, prob=TRUE, xlim=c(-15,15), ylim=c(0,0.15))
curve(dnorm(x, mean=mean(step.modelX9.res), sd=sd(step.modelX9.res)), add=TRUE)
ks.test(step.modelX9.res, "pnorm", mean=mean(step.modelX9.res), sd=sd(full.modelX9.res))

postResample(predict(full.modelX9, newdata=datax9_test), obs=datax9_test$X9)
postResample(predict(step.modelX9, newdata=datax9_test), obs=datax9_test$X9)
