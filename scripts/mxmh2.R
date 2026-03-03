#DATOS COMPENSADOS

library(readr)
mxmh2 <- read_csv("mxmh2.csv")
View(mxmh2)

mxmh2<-mxmh2[,-c(1,33)]

names(mxmh2) <- c("age","stream","hours","work","instr","compose","favgenre",
                 "explore","lang","bpm","classical","country","edm","folk","gospel",
                 "hiphop","jazz","kpop","latin","lofi","metal","pop","rnb","rap","rock",
                 "videogame","anxiety","depression","insomnia","ocd","effect")

table(mxmh2$effect)

mxmh2[,c(11:26)][mxmh2[,c(11:26)] == "Never"] <- '0'
mxmh2[,c(11:26)][mxmh2[,c(11:26)] == "Rarely"] <- '1'
mxmh2[,c(11:26)][mxmh2[,c(11:26)] == "Sometimes"] <- '2'
mxmh2[,c(11:26)][mxmh2[,c(11:26)] == "Very frequently"] <- '3'

mxmh2$hours<-as.numeric(mxmh2$hours)
mxmh2$anxiety<-as.numeric(mxmh2$anxiety)
mxmh2$depression<-as.numeric(mxmh2$depression)
mxmh2$insomnia<-as.numeric(mxmh2$insomnia)
mxmh2$ocd<-as.numeric(mxmh2$ocd)

mxmh2$classical<-as.numeric(mxmh2$classical)
mxmh2$country<-as.numeric(mxmh2$country)
mxmh2$edm<-as.numeric(mxmh2$edm)
mxmh2$folk<-as.numeric(mxmh2$folk)
mxmh2$gospel<-as.numeric(mxmh2$gospel)
mxmh2$hiphop<-as.numeric(mxmh2$hiphop)
mxmh2$jazz<-as.numeric(mxmh2$jazz)
mxmh2$kpop<-as.numeric(mxmh2$kpop)
mxmh2$latin<-as.numeric(mxmh2$latin)
mxmh2$lofi<-as.numeric(mxmh2$lofi)
mxmh2$metal<-as.numeric(mxmh2$metal)
mxmh2$pop<-as.numeric(mxmh2$pop)
mxmh2$rnb<-as.numeric(mxmh2$rnb)
mxmh2$rap<-as.numeric(mxmh2$rap)
mxmh2$rock<-as.numeric(mxmh2$rock)
mxmh2$videogame<-as.numeric(mxmh2$videogame)
View(mxmh2)

mxmh2[,c(4,5,6,8,9)][mxmh2[,c(4,5,6,8,9)] == "No"] <- '0'
mxmh2[,c(4,5,6,8,9)][mxmh2[,c(4,5,6,8,9)] == "Yes"] <- '1'

mxmh2$work<-as.numeric(mxmh2$work)
mxmh2$instr<-as.numeric(mxmh2$instr)
mxmh2$compose<-as.numeric(mxmh2$compose)
mxmh2$explore<-as.numeric(mxmh2$explore)
mxmh2$lang<-as.numeric(mxmh2$lang)

genres <- c("classical","country","edm","folk","gospel",
            "hiphop","jazz","kpop","latin","lofi","metal","pop","rnb","rap","rock",
            "videogame")

mxmh2$favgenre[mxmh2$favgenre == "Classical"] <- 'classical'
mxmh2$favgenre[mxmh2$favgenre == "Country"] <- 'country'
mxmh2$favgenre[mxmh2$favgenre == "EDM"] <- 'edm'
mxmh2$favgenre[mxmh2$favgenre == "Folk"] <- 'folk'
mxmh2$favgenre[mxmh2$favgenre == "Gospel"] <- 'gospel'
mxmh2$favgenre[mxmh2$favgenre == "Hip hop"] <- 'hiphop'
mxmh2$favgenre[mxmh2$favgenre == "Jazz"] <- 'jazz'
mxmh2$favgenre[mxmh2$favgenre == "K pop"] <- 'kpop'
mxmh2$favgenre[mxmh2$favgenre == "Latin"] <- 'latin'
mxmh2$favgenre[mxmh2$favgenre == "Lofi"] <- 'lofi'
mxmh2$favgenre[mxmh2$favgenre == "Metal"] <- 'metal'
mxmh2$favgenre[mxmh2$favgenre == "Pop"] <- 'pop'
mxmh2$favgenre[mxmh2$favgenre == "R&B"] <- 'rnb'
mxmh2$favgenre[mxmh2$favgenre == "Rap"] <- 'rap'
mxmh2$favgenre[mxmh2$favgenre == "Rock"] <- 'rock'
mxmh2$favgenre[mxmh2$favgenre == "Video game music"] <- 'videogame'

mxmh2[is.na(mxmh2$instr),]$instr<-0
mxmh2[is.na(mxmh2$work),]$work<-1

mxmh2[is.na(mxmh2$stream),]$stream<-"I do not use a streaming service."

mxmh2[is.na(mxmh2$lang),]$lang<-1
View(mxmh2)
mxmh2<-mxmh2[,-10]

which(is.na(mxmh2$age))
mxmh2<-mxmh2[-9,]
View(mxmh2)

which(is.na(mxmh2$effect))
mxmh2 <- mxmh2[-c(1083:1090),]

mxmh2$effect <- ifelse(mxmh2$effect=="Improve",1,0)
View(mxmh2)

bin2 <- mxmh2 #Utilizar bin2 para árboles

mxmh2$apple <- ifelse(mxmh2$stream=="Apple Music",1,0)
mxmh2$other <- ifelse(mxmh2$stream=="Other streaming service",1,0)
mxmh2$pandora <- ifelse(mxmh2$stream=="Pandora",1,0)
mxmh2$spotify <- ifelse(mxmh2$stream=="Spotify",1,0)
mxmh2$youtube <- ifelse(mxmh2$stream=="YouTube Music",1,0)
mxmh2<-mxmh2[,-c(2,7)]
View(mxmh2)

nrow(mxmh2)
table(mxmh2$effect)

# Random Forest

install.packages("randomForest")
library(randomForest)
View(bin2)
ncol(bin2)

set.seed(86)
mtrybd <- tuneRF(bin2[-30], bin2$effect, stepFactor=1.5, 
               improve=0.00001, ntreeTry=500, trace=TRUE, plot=TRUE)
print(mtrybd) 

set.seed(86)
trows <- sample(1:nrow(bin2),nrow(bin2)*0.7)
train <- bin2[trows,]
test <- bin2[-trows,]

set.seed(86)
rfbd <- randomForest(effect~., data=train,
                    mtry=13, ntree=500, importance=TRUE)

rfbdpred <- round(predict(rfbd, newdata=test),0)
table(rfbdpred, test$effect)
(175+125)/(175+128+7+18)

treesizebd <- treesize(rfbd) 
hist(treesizebd)
summary(treesizebd)

round(importance(rfbd),2)
varImpPlot(rfbd)


# SVM

install.packages('e1071')
library(e1071)

set.seed(659)
trows <- sample(1:nrow(mxmh2),nrow(mxmh2)*0.75)
train <- mxmh2[trows,]
test <- mxmh2[-trows,]

set.seed(392)
tunesvmlin<-tune(svm,factor(effect)~.,data=train,kernel="linear",
                 ranges=list(cost=c(0.001,0.01,0.1,0.5,1,5,10,20)),scale=TRUE)
svmlin <- tunesvmlin$best.model
summary(svmlin)
predlin <- predict(svmlin, test)
table(predlin,test$effect)
(85+85)/(85+85+47+54)

set.seed(253)
tunesvmpol<-tune(svm,factor(effect)~.,data=train,kernel="polynomial",
                 ranges=list(cost=c(0.001,0.01,0.1,0.5,1,5,10,20)),scale=TRUE)
svmpol <- tunesvmpol$best.model
summary(svmpol)
predpol <- predict(svmpol, test)
table(predpol,test$effect)
(127+111)/(127+111+5+28)

set.seed(992)
tunesvmrad<-tune(svm,factor(effect)~.,data=train,kernel="radial",
                 ranges=list(cost=c(0.001,0.01,0.1,0.5,1,5,10,20)),scale=TRUE)
svmrad <- tunesvmrad$best.model
summary(svmrad)
predrad <- predict(svmrad, test)
table(predrad,test$effect)
(127+114)/(127+114+25+5)


# Regresión Logística

logr1 <- glm(effect ~., data=mxmh2, family="binomial")
summary(logr1)
1-pchisq(logr1$deviance,logr1$df.residual)

logpr1 <- predict(logr1, newdata=mxmh2, type="response")
logpr1 <- round(logpr1, 0)

table(logpr1, mxmh2$effect)
(356+364)/(356+364+177+185)

AICr <- step(object=logr1, direction="both", trace=1)
AICr

logr2 <- glm(effect ~ work + instr + compose + explore + classical + country + 
               folk + gospel + rnb + rock + anxiety + insomnia + pandora, 
             data=mxmh2, family="binomial")
summary(logr2)
1-pchisq(logr2$deviance,logr2$df.residual)

logpr2 <- predict(logr2, newdata=mxmh2, type="response")
logpr2 <- round(logpr2, 0)

table(logpr2, mxmh2$effect)
(358+356)/(358+356+185+183)

logr3 <- glm(effect ~ work + instr + compose + explore + classical + country + 
               folk + gospel + rnb + rock + anxiety + insomnia, 
             data=mxmh2, family="binomial")
summary(logr3)
1-pchisq(logr3$deviance,logr3$df.residual)

logpr3 <- predict(logr3, newdata=mxmh2, type="response")
logpr3 <- round(logpr3, 0)

table(logpr3, mxmh2$effect)
(350+356)/(350+356+185+191)


# Bootstrapping (int. confianza)

library(caret)
set.seed(22)
boottrain <- trainControl(method = "boot", number = 999)
boot3 <- train(effect ~ work + instr + compose + explore + classical + country + 
                folk + gospel + rnb + rock + anxiety + insomnia, data = mxmh2, 
              method = "glm", family="binomial", 
              trControl = boottrain)

summary(boot3)
print(boot3)


# NUEVA ENCUESTA

#Media y SD para datos centrados

library(readr)
mhnew <- read_csv("mhnew.csv")
View(mhnew)

mhnew<-mhnew[,-c(1,32)]

mhnew[,c(10:25)][mhnew[,c(10:25)] == "Nunca"] <- '0'
mhnew[,c(10:25)][mhnew[,c(10:25)] == "Rara vez"] <- '1'
mhnew[,c(10:25)][mhnew[,c(10:25)] == "A veces"] <- '2'
mhnew[,c(10:25)][mhnew[,c(10:25)] == "Con frecuencia"] <- '3'

mhnew$hours<-as.numeric(mhnew$hours)
mhnew$anxiety<-as.numeric(mhnew$anxiety)
mhnew$depression<-as.numeric(mhnew$depression)
mhnew$insomnia<-as.numeric(mhnew$insomnia)
mhnew$ocd<-as.numeric(mhnew$ocd)

mhnew$classical<-as.numeric(mhnew$classical)
mhnew$country<-as.numeric(mhnew$country)
mhnew$edm<-as.numeric(mhnew$edm)
mhnew$folk<-as.numeric(mhnew$folk)
mhnew$gospel<-as.numeric(mhnew$gospel)
mhnew$hiphop<-as.numeric(mhnew$hiphop)
mhnew$jazz<-as.numeric(mhnew$jazz)
mhnew$kpop<-as.numeric(mhnew$kpop)
mhnew$latin<-as.numeric(mhnew$latin)
mhnew$lofi<-as.numeric(mhnew$lofi)
mhnew$metal<-as.numeric(mhnew$metal)
mhnew$pop<-as.numeric(mhnew$pop)
mhnew$rnb<-as.numeric(mhnew$rnb)
mhnew$rap<-as.numeric(mhnew$rap)
mhnew$rock<-as.numeric(mhnew$rock)
mhnew$videogame<-as.numeric(mhnew$videogame)
View(mhnew)

mhnew[,c(4,5,6,8,9)][mhnew[,c(4,5,6,8,9)] == "No"] <- '0'
mhnew[,c(4,5,6,8,9)][mhnew[,c(4,5,6,8,9)] == "Sí"] <- '1'

mhnew$work<-as.numeric(mhnew$work)
mhnew$instr<-as.numeric(mhnew$instr)
mhnew$compose<-as.numeric(mhnew$compose)
mhnew$explore<-as.numeric(mhnew$explore)
mhnew$lang<-as.numeric(mhnew$lang)

mhnew$favgenre[mhnew$favgenre == "Clásica"] <- 'classical'
mhnew$favgenre[mhnew$favgenre == "Country"] <- 'country'
mhnew$favgenre[mhnew$favgenre == "EDM"] <- 'edm'
mhnew$favgenre[mhnew$favgenre == "Folk"] <- 'folk'
mhnew$favgenre[mhnew$favgenre == "Gospel"] <- 'gospel'
mhnew$favgenre[mhnew$favgenre == "Hip hop"] <- 'hiphop'
mhnew$favgenre[mhnew$favgenre == "Jazz"] <- 'jazz'
mhnew$favgenre[mhnew$favgenre == "K-pop"] <- 'kpop'
mhnew$favgenre[mhnew$favgenre == "Latina"] <- 'latin'
mhnew$favgenre[mhnew$favgenre == "Lo-fi"] <- 'lofi'
mhnew$favgenre[mhnew$favgenre == "Metal"] <- 'metal'
mhnew$favgenre[mhnew$favgenre == "Pop"] <- 'pop'
mhnew$favgenre[mhnew$favgenre == "R&B"] <- 'rnb'
mhnew$favgenre[mhnew$favgenre == "Rap"] <- 'rap'
mhnew$favgenre[mhnew$favgenre == "Rock"] <- 'rock'
mhnew$favgenre[mhnew$favgenre == "Música de videojuegos"] <- 'videogame'
View(mhnew)

na_rows2 <- rowSums(is.na(mhnew))
table(na_rows2)

mhnew$effect <- ifelse(mhnew$effect=="Mejoran",1,0)

table(mxmh2$stream)
table(mhnew$stream)
mhnew$stream[mhnew$stream == "No suelo utilizar servicios de streaming"] <- 
  'I do not use a streaming service.'
mhnew$stream[mhnew$stream == "Youtube Music"] <- 'YouTube Music'
mhnew$stream[mhnew$stream == "Otros"] <- 'Other streaming service'
View(mhnew)

newcent <- mhnew
newcent$spotify <- ifelse(newcent$stream=="Spotify",1,0)
newcent$youtube <- ifelse(newcent$stream=="YouTube Music",1,0)
newcent$other <- ifelse(newcent$stream=="Other streaming service",1,0)
newcent$apple <- 0
newcent$pandora <- 0
newcent<-newcent[,-c(2,7)]
View(newcent)
newreg<-newcent
newcent2<-newcent

newcent$age<-(newcent$age-mean(mxmh$age))/sd(mxmh$age)
newcent$hours<-(newcent$hours-mean(mxmh$hours))/sd(mxmh$hours)
newcent$classical<-(newcent$classical-mean(mxmh$classical))/sd(mxmh$classical)
newcent$country<-(newcent$country-mean(mxmh$country))/sd(mxmh$country)
newcent$edm<-(newcent$edm-mean(mxmh$edm))/sd(mxmh$edm)
newcent$folk<-(newcent$folk-mean(mxmh$folk))/sd(mxmh$folk)
newcent$gospel<-(newcent$gospel-mean(mxmh$gospel))/sd(mxmh$gospel)
newcent$hiphop<-(newcent$hiphop-mean(mxmh$hiphop))/sd(mxmh$hiphop)
newcent$jazz<-(newcent$jazz-mean(mxmh$jazz))/sd(mxmh$jazz)
newcent$kpop<-(newcent$kpop-mean(mxmh$kpop))/sd(mxmh$kpop)
newcent$latin<-(newcent$latin-mean(mxmh$latin))/sd(mxmh$latin)
newcent$lofi<-(newcent$lofi-mean(mxmh$lofi))/sd(mxmh$lofi)
newcent$metal<-(newcent$metal-mean(mxmh$metal))/sd(mxmh$metal)
newcent$pop<-(newcent$pop-mean(mxmh$pop))/sd(mxmh$pop)
newcent$rnb<-(newcent$rnb-mean(mxmh$rnb))/sd(mxmh$rnb)
newcent$rock<-(newcent$rock-mean(mxmh$rock))/sd(mxmh$rock)
newcent$videogame<-(newcent$videogame-mean(mxmh$videogame))/sd(mxmh$videogame)
newcent$anxiety<-(newcent$anxiety-mean(mxmh$anxiety))/sd(mxmh$anxiety)
newcent$depression<-(newcent$depression-mean(mxmh$depression))/sd(mxmh$depression)
newcent$insomnia<-(newcent$insomnia-mean(mxmh$insomnia))/sd(mxmh$insomnia)
newcent$ocd<-(newcent$ocd-mean(mxmh$ocd))/sd(mxmh$ocd)
View(newcent)

newcent2$age<-(newcent2$age-mean(mxmh2$age))/sd(mxmh2$age)
newcent2$hours<-(newcent2$hours-mean(mxmh2$hours))/sd(mxmh2$hours)
newcent2$classical<-(newcent2$classical-mean(mxmh2$classical))/sd(mxmh2$classical)
newcent2$country<-(newcent2$country-mean(mxmh2$country))/sd(mxmh2$country)
newcent2$edm<-(newcent2$edm-mean(mxmh2$edm))/sd(mxmh2$edm)
newcent2$folk<-(newcent2$folk-mean(mxmh2$folk))/sd(mxmh2$folk)
newcent2$gospel<-(newcent2$gospel-mean(mxmh$gospel))/sd(mxmh2$gospel)
newcent2$hiphop<-(newcent2$hiphop-mean(mxmh2$hiphop))/sd(mxmh2$hiphop)
newcent2$jazz<-(newcent2$jazz-mean(mxmh2$jazz))/sd(mxmh2$jazz)
newcent2$kpop<-(newcent2$kpop-mean(mxmh2$kpop))/sd(mxmh2$kpop)
newcent2$latin<-(newcent2$latin-mean(mxmh2$latin))/sd(mxmh2$latin)
newcent2$lofi<-(newcent2$lofi-mean(mxmh2$lofi))/sd(mxmh2$lofi)
newcent2$metal<-(newcent2$metal-mean(mxmh2$metal))/sd(mxmh2$metal)
newcent2$pop<-(newcent2$pop-mean(mxmh2$pop))/sd(mxmh2$pop)
newcent2$rnb<-(newcent2$rnb-mean(mxmh2$rnb))/sd(mxmh2$rnb)
newcent2$rock<-(newcent2$rock-mean(mxmh2$rock))/sd(mxmh2$rock)
newcent2$videogame<-(newcent2$videogame-mean(mxmh2$videogame))/sd(mxmh2$videogame)
newcent2$anxiety<-(newcent2$anxiety-mean(mxmh2$anxiety))/sd(mxmh2$anxiety)
newcent2$depression<-(newcent2$depression-mean(mxmh2$depression))/sd(mxmh2$depression)
newcent2$insomnia<-(newcent2$insomnia-mean(mxmh2$insomnia))/sd(mxmh2$insomnia)
newcent2$ocd<-(newcent2$ocd-mean(mxmh2$ocd))/sd(mxmh2$ocd)
View(newcent2)

#Análisis previo

table(mhnew$effect)
121/172
1-121/172

summary(mhnew$age)
summary(mhnew$hours)

for(i in c(4,5,6,8,9)) {
  print(100*prop.table(table(mhnew[,i])))
}

apply(mhnew[,c(26:29)],2,mean)
apply(mhnew[,c(26:29)],2,median)

table(mhnew$stream)
prop.table(table(mhnew$stream))*100

table(mhnew$favgenre)
prop.table(table(mhnew$favgenre))*100

apply(mhnew[,c(10:25)],2,mean)

library(GGally)
ggpairs(mhnew[,c(26:29)], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#Comparación de las poblaciones
#Variables categóricas como binarias dummies, todas las variables sin escalar

p1<-mxmh
p1$effect <- ifelse(p1$effect=="Improve",1,0)
p1$apple <- ifelse(p1$stream=="Apple Music",1,0)
p1$other <- ifelse(p1$stream=="Other streaming service",1,0)
p1$pandora <- ifelse(p1$stream=="Pandora",1,0)
p1$spotify <- ifelse(p1$stream=="Spotify",1,0)
p1$youtube <- ifelse(p1$stream=="YouTube Music",1,0)
p1<-p1[,-c(2,7)]

p2<-mhnew
p2$apple <- ifelse(p2$stream=="Apple Music",1,0)
p2$other <- ifelse(p2$stream=="Other streaming service",1,0)
p2$pandora <- ifelse(p2$stream=="Pandora",1,0)
p2$spotify <- ifelse(p2$stream=="Spotify",1,0)
p2$youtube <- ifelse(p2$stream=="YouTube Music",1,0)
p2<-p2[,-c(2,7)]

install.packages("Hotelling")
library(Hotelling)
print(hotelling.test(p1, p2))


#Aplicación de los modelos predictivos (DATOS ORIGINALES)

library(tree)
library(caret)
library(randomForest)
library(gbm)
library(class)


tree1newpred <- predict(tree1, newdata=mhnew, type = "class") 
table(tree1newpred, mhnew$effect)

treeb1newpred <- predict(treeb1, newdata=mhnew, type = "class") 
table(treeb1newpred, mhnew$effect)

rf1newpred <- predict(rf1, newdata=mhnew)
table(rf1newpred, mhnew$effect)
(109+10)/172

rf2newpred <- predict(rf2, newdata=mhnew)
table(rf2newpred, mhnew$effect)
122/172

lr1newpred <- predict(logreg, newdata=newcent, type='response')
lr1newpred <- round(lr1newpred, 0)
table(lr1newpred, newcent$effect)
(87+25)/172

lr3newpred <- predict(logreg3, newdata=newcent, type='response')
lr3newpred <- round(lr3newpred, 0)
table(lr3newpred, newcent$effect)
(19+94)/172

btreenewpred <- predict(BTree, newdata=newcent, n.trees=5000, type="response")
btreenewpred <- round(btreenewpred, 0)
table(btreenewpred, newcent$effect)
(18+84)/172


set.seed(492)
trows <- sample(1:nrow(regdata),nrow(regdata)*0.75)
train <- regdata[trows,]

knn3model <- knn(train[,-28],newcent[,-28],train$effect,k=3)
table(knn3model, newcent$effect)
(18+95)/172

knn10model <- knn(train[,-28],newcent[,-28],train$effect,k=10)
table(knn10model, newcent$effect)
(10+114)/172

#PROGRAMAR kNN 

set.seed(515)
for(i in 1:30){
  n<-paste("Knn",i,sep="")
  z<-paste("K",i,sep="")
  a<-knn(regdata[,-28],newcent[,-28],regdata$effect,k=i)
  assign(n,a)
  assign(z,sum(a==newcent$effect)/172)
}

print(c(K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,
        K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25
        ,K26,K27,K28,K29,K30))

table(Knn9,newcent$effect)

length(Knn11)
table(Knn11,newcent$effect)

#Aplicación de los modelos predictivos (DATOS COMPENSADOS)

rfbdnewpred <- predict(rfbd, newdata=mhnew)
rfbdnewpred <- round(rfbdnewpred, 0)
table(rfbdnewpred, mhnew$effect)
(75+26)/172

library(e1071)

svmlnewpred <- predict(svmlin, newdata=newcent2)
table(svmlnewpred, newcent2$effect)
(42+14)/172

svmpnewpred <- predict(svmpol, newdata=newcent2)
table(svmpnewpred, newcent2$effect)
(51+27)/172

svmrnewpred <- predict(svmrad, newdata=newcent2)
table(svmrnewpred, newcent2$effect)
(90+9)/172

logr1newpred <- predict(logr1, newdata=newreg, type='response')
logr1newpred <- round(logr1newpred, 0)
table(logr1newpred, newreg$effect)
(38+41)/172

logr3newpred <- predict(logr3, newdata=newreg, type='response')
logr3newpred <- round(logr3newpred, 0)
table(logr3newpred, newreg$effect)
(38+36)/172




