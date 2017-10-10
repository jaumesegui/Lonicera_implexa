
#source('d:/cosas nuevas a guardar en H/BBbehaviourFIELD/Centaureapaper/analisisRcorregidos/scriptscorr/Variance_Inflation_Factors.R')

data<-read.table('d:/cosas nuevas a guardar en H/Lonicera/Lonicera2012/paperherko/HSA/HSAINTERPOPconVol.csv',sep=';',head=T)

head(data)

names(data)

class (data$seedsflo)
class (data$fruitset)



#center the dependent variable
data$seedsfloC <- data$seedsflo - mean(data$seedsflo)
data$fruitsetC <- data$fruitset - mean(data$fruitset)

#center the predictors
data$INTcorC <- data$INTcor - mean(data$INTcor)

data$IefficC <- data$Ieffic - mean(data$Ieffic)
data$IattracCorrC <- data$IattracCorr - mean(data$IattracCorr)
data$IaccesCorrC <- data$IaccesCorr - mean(data$IaccesCorr)

data$longcorC <- data$longcor - mean(data$longcor)
data$longpetC <- data$longpet - mean(data$longpet)
data$anchopetC <- data$anchopet - mean(data$anchopet)
data$antherslengthC <- data$antherslength - mean(data$antherslength)
data$stylelengthC <- data$stylelength - mean(data$stylelength)
data$tubewidthC <- data$tubewidth - mean(data$tubewidth)
data$styleexertionC <- data$styleexertion - mean(data$styleexertion)
data$herkoRelaC <- data$herkoRela - mean(data$herkoRela)

data$totfloresC <- data$totflores - mean(data$totflores)

data$INTcorC2 <- data$INTcorC*data$INTcorC

data$IefficC2 <- data$IefficC*data$IefficC 
data$IattracCorrC2 <- data$IattracCorrC*data$IattracCorrC
data$IaccesCorrC2 <- data$IaccesCorrC*data$IaccesCorrC

data$longcorC2 <- data$longcorC*data$longcorC
data$longpetC2 <- data$longpetC*data$longpetC
data$anchopetC2 <- data$anchopetC*data$anchopetC
data$antherslengthC2 <- data$antherslengthC*data$antherslengthC
data$stylelengthC2 <- data$stylelengthC*data$stylelengthC
data$tubewidthC2 <- data$tubewidthC*data$tubewidthC
data$styleexertionC2 <- data$styleexertionC*data$styleexertionC
data$herkoRelaC2 <- data$herkoRelaC*data$herkoRelaC

#corvif(cbind(data$INTcorC, data$IefficC,data$IattracCorrC,data$IaccesCorrC,data$longcorC,data$longpetC,data$anchopetC,data$totfloresC,data$stylelengthC,data$antherslengthC,data$tubewidthC))

corvif(cbind(data$longcorC,data$styleexertionC,data$herkoRelaC,data$tubewidthC))

corvif(cbind(data$longcorC,data$herkoRelaC,data$tubewidthC))

#####para hcerlo por separado
B<-data[data$population=="B", ]
E<-data[data$population=="E", ]
ST<-data[data$population=="ST", ]

#center the dependent variable
B$fruitsetC <- B$fruitset - mean(B$fruitset)
B$seedsfloC <- B$seedsflo - mean(B$seedsflo)
E$seedsfloC <- E$seedsflo - mean(E$seedsflo)
E$fruitsetC <- E$fruitset - mean(E$fruitset)
ST$seedsfloC <- ST$seedsflo - mean(ST$seedsflo)

#center the predictors
B$longcorC <- B$longcor - mean(B$longcor)
B$tubewidthC <- B$tubewidth - mean(B$tubewidth)
B$styleexertionC <- B$styleexertion - mean(B$styleexertion)
B$herkoRelaC <- B$herkoRela - mean(B$herkoRela)

B$longcorC2 <- B$longcorC*B$longcorC
B$tubewidthC2 <- B$tubewidthC*B$tubewidthC
B$styleexertionC2 <- B$styleexertionC*B$styleexertionC
B$herkoRelaC2 <- B$herkoRelaC*B$herkoRelaC

E$longcorC <- E$longcor - mean(E$longcor)
E$tubewidthC <- E$tubewidth - mean(E$tubewidth)
E$styleexertionC <-E$styleexertion - mean(E$styleexertion)
E$herkoRelaC <- E$herkoRela - mean(E$herkoRela)

E$longcorC2 <- E$longcorC*E$longcorC
E$tubewidthC2 <- E$tubewidthC*E$tubewidthC
E$styleexertionC2 <- E$styleexertionC*E$styleexertionC
E$herkoRelaC2 <- E$herkoRelaC*E$herkoRelaC

ST$longcorC <- ST$longcor - mean(ST$longcor)
ST$tubewidthC <- ST$tubewidth - mean(ST$tubewidth)
ST$styleexertionC <-ST$styleexertion - mean(ST$styleexertion)
ST$herkoRelaC <- ST$herkoRela - mean(ST$herkoRela)

ST$longcorC2 <- ST$longcorC*ST$longcorC
ST$tubewidthC2 <- ST$tubewidthC*ST$tubewidthC
ST$styleexertionC2 <- ST$styleexertionC*ST$styleexertionC
ST$herkoRelaC2 <- ST$herkoRelaC*ST$herkoRelaC

#####TODO JUNTO con SELECCION AUTOMATICA

library(nlme)

M01<-lm(seedsflo~ nectarvolume*population + longcor*population+tubewidth*population+herkoRela*population, na.action=na.omit, data=data)
summary(M01)
step(M01)

M02<-lm(seedsflo~ herkoRela+population, na.action=na.omit, data=data)
summary(M02)
anova(M02)

M02<-lm(seedsflo~ herkoRela, na.action=na.omit, data=data)
summary(M02)
anova(M02)


M02<-lm(seedsflo~ herkoRela*population, na.action=na.omit, data=data)
summary(M02)
anova(M02)

###con exercion del estilo

M01<-lm(seedsflo~ nectarvolume*population + longcor*population+tubewidth*population+styleexertion*population, na.action=na.omit, data=data)
summary(M01)
step(M01)

M03.1<-lm(seedsflo~ styleexertion, na.action=na.omit, data=data)
summary(M03.1)


###POR POBLAICONE
####banyalbufar
M01<-lm(seedsflo~ longcor +herkoRela*longcor, na.action=na.omit, data=B)
summary(M01)
anova(M01)
step(M01)

M01<-lm(seedsflo~ herkoRela, na.action=na.omit, data=B)
summary(M01)




###establiments

M01<-lm(seedsflo~ longcor +herkoRela*longcor, na.action=na.omit, data=E)
summary(M01)
anova(M01)
step(M01)


M01<-lm(seedsflo~ herkoRela, na.action=na.omit, data=E)
summary(M01)


M01<-lm(seedsflo~ longcor, na.action=na.omit, data=E)
summary(M01)


###son tries

M01<-lm(seedsflo~ longcor +herkoRela*longcor, na.action=na.omit, data=ST)
summary(M01)
anova(M01)
step(M01)

M01<-lm(seedsflo~herkoRela, na.action=na.omit, data=ST)
summary(M01)


M01<-lm(seedsfloC~herkoRelaC, na.action=na.omit, data=ST)
summary(M01)

M01<-lm(fruitsetC~herkoRelaC2+herkoRelaC, na.action=na.omit, data=ST)
summary(M01)

M01<-lm(fruitsetC~herkoRelaC, na.action=na.omit, data=ST)
summary(M01)

M01<-lm(seedsfloC~herkoRelaC,  family=Gamma, na.action=na.omit, data=ST)
summary(M01)

M01<-lm(fruitset~longcor*herkoRelaC, family=binomial, weights=Nflores,na.action=na.omit, data=ST)
summary(M01)

library(rJava)
library(glmulti)

resultado1<-glmulti(M01, level = 1, method = "h")

######con fruit set y seedsperfruitR y distribuciones no noramles
library(nlme)
Model100 <- glm(fruitset~herkoRela*population+longcor*population+herkoRela*longcor*population, family=binomial, weights= Nflores, na.action=na.omit, data=data)
summary(Model100)
drop1(Model100, test="Chisq")###herkoRela:population:longcor  2   631.72 885.39 6.1228  0.04682 *



Model100.1 <- glm(fruitset~herkoRela*population+longcor*population, family=binomial, weights= Nflores, na.action=na.omit, data=data)
summary(Model100.1)
anova(Model100, Model100.1, test="Chisq")####+herkoRela*longcor*population p= o.o62??
drop1(Model100.1, test="Chisq")#########herkoRela:population  2   671.45 919.12 38.535 4.288e-09 ***
						#####population:longcor    2   646.86 894.54 13.952  0.000934 ***
add1(Model100.1,scope=~herkoRela*population*longcor, test="Chisq")

data$longcor
####POP-herko 

###BANYALBUFAR
curve (exp(-2.25522+
0.50320*x+
-0.02919*11+
-0.01844*11*x)/1+exp(-2.25522+
0.50320*x+
-0.02919*11+
-0.01844*11*x)


,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Herkogamy",
ylab="Fruit set",
ylim=c(-0.5,1.5),
xlim=c(-7, 7)) #para 11 mm corolla

curve  (exp(-2.25522+
0.50320*x+
-0.02919*22+
-0.01844*22*x)/1+exp(-2.25522+
0.50320*x+
-0.02919*22+
-0.01844*22*x)

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 22 mm corolla


curve   (exp(-2.25522+
0.50320*x+
-0.02919*33+
-0.01844*33*x)/1+exp(-2.25522+
0.50320*x+
-0.02919*33+
-0.01844*33*x)

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para B

legend(0,0,lwd=3,lty=c(1,2,3),c("corolla length: 11 mm","corolla length: 22 mm","corolla length: 33 mm"))

head(data)
data[is.na(data),]


data$residuals100<-residuals(Model100)

estimalongcor11<-(exp(-2.25522+
0.50320*data[data$population=="B",]$herkoRela+
-0.02919*11+
-0.01844*11*data[data$population=="B",]$herkoRela)/1+exp(-2.25522+
0.50320*data[data$population=="B",]$herkoRela+
-0.02919*11+
-0.01844*11*data[data$population=="B",]$herkoRela))

resparlongcor11<-data[data$population=="B",]$residuals100+estimalongcor11


estimalongcor22<-(exp(-2.25522+
0.50320*data[data$population=="B",]$herkoRela+
-0.02919*22+
-0.01844*22*data[data$population=="B",]$herkoRela)/1+exp(-2.25522+
0.50320*data[data$population=="B",]$herkoRela+
-0.02919*22+
-0.01844*22*data[data$population=="B",]$herkoRela))

resparlongcor22<-data[data$population=="B",]$residuals100+estimalongcor22


estimalongcor33<-(exp(-2.25522+
0.50320*data[data$population=="B",]$herkoRela+
-0.02919*33+
-0.01844*33*data[data$population=="B",]$herkoRela)/1+exp(-2.25522+
0.50320*data[data$population=="B",]$herkoRela+
-0.02919*33+
-0.01844*33*data[data$population=="B",]$herkoRela))

resparlongcor33<-data[data$population=="B",]$residuals100+estimalongcor33


points(data[data$population=="B",]$herkoRela,resparlongcor11, pch=17)
points(data[data$population=="B",]$herkoRela,resparlongcor22, pch=19)
points(data[data$population=="B",]$herkoRela,resparlongcor33, pch=21)


###ESTABLIMENTS
curve (exp(-2.25522+
0.50320*x+
1.96548+
-0.02919*11+
-0.73688*x+
-0.02963*11+
-0.01844*11*x+
0.02678*11*x)/1+exp(-2.25522+
0.50320*x+
1.96548+
-0.02919*11+
-0.73688*x+
-0.02963*11+
-0.01844*11*x+
0.02678*11*x)


,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Herkogamy",
ylab="Fruit set",
ylim=c(-0.5,1.5),
xlim=c(-7, 7)) #para 11 mm corolla

curve  (exp(-2.25522+
0.50320*x+
1.96548+
-0.02919*22+
-0.73688*x+
-0.02963*22+
-0.01844*22*x+
0.02678*22*x)/1+exp(-2.25522+
0.50320*x+
1.96548+
-0.02919*22+
-0.73688*x+
-0.02963*22+
-0.01844*22*x+
0.02678*22*x)

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 22 mm corolla


curve   (exp(-2.25522+
0.50320*x+
1.96548+
-0.02919*33+
-0.73688*x+
-0.02963*33+
-0.01844*33*x+
0.02678*33*x)/1+exp(-2.25522+
0.50320*x+
1.96548+
-0.02919*33+
-0.73688*x+
-0.02963*33+
-0.01844*33*x+
0.02678*33*x)

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 33

legend(0,0,lwd=3,lty=c(1,2,3),c("11 mm","22 mm","33 mm"))

head(data)
data[is.na(data),]


data$residuals100<-residuals(Model100)

estimalongcor11<-(exp(-2.25522+
0.50320*data[data$population=="E",]$herkoRela+
1.96548+
-0.02919*11+
-0.73688*data[data$population=="E",]$herkoRela+
-0.02963*11+
-0.01844*11*data[data$population=="E",]$herkoRela+
0.02678*11*data[data$population=="E",]$herkoRela)/1+exp(-2.25522+
0.50320*data[data$population=="E",]$herkoRela+
1.96548+
-0.02919*11+
-0.73688*data[data$population=="E",]$herkoRela+
-0.02963*11+
-0.01844*11*data[data$population=="E",]$herkoRela+
0.02678*11*data[data$population=="E",]$herkoRela))


resparlongcor11<-data[data$population=="E",]$residuals100+estimalongcor11


estimalongcor22<-(exp(-2.25522+
0.50320*data[data$population=="E",]$herkoRela+
1.96548+
-0.02919*22+
-0.73688*data[data$population=="E",]$herkoRela+
-0.02963*22+
-0.01844*22*data[data$population=="E",]$herkoRela+
0.02678*22*data[data$population=="E",]$herkoRela)/1+exp(-2.25522+
0.50320*data[data$population=="E",]$herkoRela+
1.96548+
-0.02919*22+
-0.73688*data[data$population=="E",]$herkoRela+
-0.02963*22+
-0.01844*22*data[data$population=="E",]$herkoRela+
0.02678*22*data[data$population=="E",]$herkoRela))

resparlongcor22<-data[data$population=="E",]$residuals100+estimalongcor22


estimalongcor33<-(exp(-2.25522+
0.50320*data[data$population=="E",]$herkoRela+
1.96548+
-0.02919*33+
-0.73688*data[data$population=="E",]$herkoRela+
-0.02963*33+
-0.01844*33*data[data$population=="E",]$herkoRela+
0.02678*33*data[data$population=="E",]$herkoRela)/1+exp(-2.25522+
0.50320*data[data$population=="E",]$herkoRela+
1.96548+
-0.02919*33+
-0.73688*data[data$population=="E",]$herkoRela+
-0.02963*33+
-0.01844*33*data[data$population=="E",]$herkoRela+
0.02678*33*data[data$population=="E",]$herkoRela))

resparlongcor33<-data[data$population=="E",]$residuals100+estimalongcor33


points(data[data$population=="E",]$herkoRela,resparlongcor11, pch=17)
points(data[data$population=="E",]$herkoRela,resparlongcor22, pch=19)
points(data[data$population=="E",]$herkoRela,resparlongcor33, pch=21)




###SON TRIES
curve (exp(-2.25522+
0.50320*x+
-0.46145+
-0.02919*11+
-0.48264*x+
0.02521*11+
-0.01844*11*x+
0.03153*11*x)/1+exp(-2.25522+
0.50320*x+
-0.46145+
-0.02919*11+
-0.48264*x+
0.02521*11+
-0.01844*11*x+
0.03153*11*x)


,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Herkogamy",
ylab="Fruit set",
ylim=c(-0.5,1.5),
xlim=c(-7, 7)) #para 11 mm corolla

curve   (exp(-2.25522+
0.50320*x+
-0.46145+
-0.02919*22+
-0.48264*x+
0.02521*22+
-0.01844*22*x+
0.03153*22*x)/1+exp(-2.25522+
0.50320*x+
-0.46145+
-0.02919*22+
-0.48264*x+
0.02521*22+
-0.01844*22*x+
0.03153*22*x)

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 22 mm corolla


curve   (exp(-2.25522+
0.50320*x+
-0.46145+
-0.02919*33+
-0.48264*x+
0.02521*33+
-0.01844*33*x+
0.03153*33*x)/1+exp(-2.25522+
0.50320*x+
-0.46145+
-0.02919*33+
-0.48264*x+
0.02521*33+
-0.01844*33*x+
0.03153*33*x)

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 33

legend(0,0,lwd=3,lty=c(1,2,3),c("11 mm","22 mm","33 mm"))

head(data)
data[is.na(data),]


data$residuals100<-residuals(Model100)

estimalongcor11<-(exp(-2.25522+
0.50320*data[data$population=="ST",]$herkoRela+
-0.46145+
-0.02919*11+
-0.48264*data[data$population=="ST",]$herkoRela+
0.02521*11+
-0.01844*11*data[data$population=="ST",]$herkoRela+
0.03153*11*data[data$population=="ST",]$herkoRela)/1+exp(-2.25522+
0.50320*data[data$population=="ST",]$herkoRela+
-0.46145+
-0.02919*11+
-0.48264*data[data$population=="ST",]$herkoRela+
0.02521*11+
-0.01844*11*data[data$population=="ST",]$herkoRela+
0.03153*11*data[data$population=="ST",]$herkoRela))

resparlongcor11<-data[data$population=="ST",]$residuals100+estimalongcor11


estimalongcor22<- (exp(-2.25522+
0.50320*data[data$population=="ST",]$herkoRela+
-0.46145+
-0.02919*22+
-0.48264*data[data$population=="ST",]$herkoRela+
0.02521*22+
-0.01844*22*data[data$population=="ST",]$herkoRela+
0.03153*22*data[data$population=="ST",]$herkoRela)/1+exp(-2.25522+
0.50320*data[data$population=="ST",]$herkoRela+
-0.46145+
-0.02919*22+
-0.48264*data[data$population=="ST",]$herkoRela+
0.02521*22+
-0.01844*22*data[data$population=="ST",]$herkoRela+
0.03153*22*data[data$population=="ST",]$herkoRela))


resparlongcor22<-data[data$population=="ST",]$residuals100+estimalongcor22


estimalongcor33<- (exp(-2.25522+
0.50320*data[data$population=="ST",]$herkoRela+
-0.46145+
-0.02919*33+
-0.48264*data[data$population=="ST",]$herkoRela+
0.02521*33+
-0.01844*33*data[data$population=="ST",]$herkoRela+
0.03153*33*data[data$population=="ST",]$herkoRela)/1+exp(-2.25522+
0.50320*data[data$population=="ST",]$herkoRela+
-0.46145+
-0.02919*33+
-0.48264*data[data$population=="ST",]$herkoRela+
0.02521*33+
-0.01844*33*data[data$population=="ST",]$herkoRela+
0.03153*33*data[data$population=="ST",]$herkoRela))

resparlongcor33<-data[data$population=="ST",]$residuals100+estimalongcor33


points(data[data$population=="ST",]$herkoRela,resparlongcor11, pch=17)
points(data[data$population=="ST",]$herkoRela,resparlongcor22, pch=19)
points(data[data$population=="ST",]$herkoRela,resparlongcor33, pch=21)





Model200 <- glm(semillasporfrutoR~herkoRela*population+longcor*population+herkoRela*longcor*population, family=poisson, na.action=na.omit, data=data)
summary(Model200)
drop1(Model200, test="Chisq")## 2   39.513 174.76 0.9651   0.6172

Model200.1 <- glm(semillasporfrutoR~herkoRela*population+longcor*population, family=poisson, na.action=na.omit, data=data)
summary(Model200.1)
drop1(Model200.1, test="Chisq")###erkoRela:population  2   42.166 171.41 2.4992   0.2866
					####population:longcor    2   41.348 170.60 1.6813   0.4314
Model200.2 <- glm(semillasporfrutoR~herkoRela*population+longcor, family=poisson, na.action=na.omit, data=data)
summary(Model200.2)
drop1(Model200.2, test="Chisq")######longcor  1   41.706 168.96 0.35855   0.5493
					########herkoRela:population  2   42.776 168.03 1.42821   0.4896

Model200.3 <- glm(semillasporfrutoR~herkoRela+population+longcor, family=poisson, na.action=na.omit, data=data)
summary(Model200.3)
drop1(Model200.3, test="Chisq")###herkoRela   1   42.866 166.12 0.0901   0.7640  
					###population  2   49.140 170.39 6.3641   0.0415 *
					###longcor     1   43.299 166.55 0.5228   0.4697 

####style exertion
library(nlme)
Model1000 <- glm(fruitset~styleexertion*population+longcor*population+styleexertion*longcor*population, family=binomial, weights= Nflores, na.action=na.omit, data=data)
summary(Model1000)
drop1 (Model1000, test="Chisq")

Model1000.1 <- glm(fruitset~styleexertion*population+longcor*population, family=binomial, weights= Nflores, na.action=na.omit, data=data)
summary(Model1000.1)
anova(Model1000, Model1000.1, test="Chisq")
drop1 (Model1000.1, test="Chisq")###styleexertion:population  2   668.15 915.82 30.5409 2.334e-07 ***
						####population:longcor        2   647.17 894.85  9.5631  0.008383 **

data$styleexertion
####POP-styleexertion 

###BANYALBUFAR
curve (exp(-5.11005+
1.08558*x+
0.07394*11+
-0.04049*11*x)/1+exp(-5.11005+
1.08558*x+
0.07394*11+
-0.04049*11*x)

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Style exertion",
ylab="Fruit set",
ylim=c(-0.5,1.5),
xlim=c(-4, 11)) #para 11 mm corolla

curve  (exp(-5.11005+
1.08558*x+
0.07394*22+
-0.04049*22*x)/1+exp(-5.11005+
1.08558*x+
0.07394*22+
-0.04049*22*x)


,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 22 mm corolla


curve   (exp(-5.11005+
1.08558*x+
0.07394*33+
-0.04049*33*x)/1+exp(-5.11005+
1.08558*x+
0.07394*33+
-0.04049*33*x)


,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para B

legend(0,0,lwd=3,lty=c(1,2,3),c("corolla length: 11 mm","corolla length: 22 mm","corolla length: 33 mm"))

head(data)
data[is.na(data),]


data$residuals1000<-residuals(Model1000)

estimalongcor11<-(exp(-5.11005+
1.08558*data[data$population=="B",]$styleexertion+
0.07394*11+
-0.04049*11*data[data$population=="B",]$styleexertion)/1+exp(-5.11005+
1.08558*data[data$population=="B",]$styleexertion+
0.07394*11+
-0.04049*11*data[data$population=="B",]$styleexertion))

resparlongcor11<-data[data$population=="B",]$residuals1000+estimalongcor11


estimalongcor22<-(exp(-5.11005+
1.08558*data[data$population=="B",]$styleexertion+
0.07394*22+
-0.04049*22*data[data$population=="B",]$styleexertion)/1+exp(-5.11005+
1.08558*data[data$population=="B",]$styleexertion+
0.07394*22+
-0.04049*22*data[data$population=="B",]$styleexertion))

resparlongcor22<-data[data$population=="B",]$residuals1000+estimalongcor22


estimalongcor33<-(exp(-5.11005+
1.08558*data[data$population=="B",]$styleexertion+
0.07394*33+
-0.04049*33*data[data$population=="B",]$styleexertion)/1+exp(-5.11005+
1.08558*data[data$population=="B",]$styleexertion+
0.07394*33+
-0.04049*33*data[data$population=="B",]$styleexertion))

resparlongcor33<-data[data$population=="B",]$residuals1000+estimalongcor33


points(data[data$population=="B",]$styleexertion,resparlongcor11, pch=17)
points(data[data$population=="B",]$styleexertion,resparlongcor22, pch=19)
points(data[data$population=="B",]$styleexertion,resparlongcor33, pch=21)


###ESTABLIMENTS
curve (exp(-5.11005+
1.08558*x+
6.66704+
0.07394*11+
-1.53920*x+
-0.20452*11+
-0.04049*11*x+
0.05835*11*x)/1+exp(-5.11005+
1.08558*x+
6.66704+
0.07394*11+
-1.53920*x+
-0.20452*11+
-0.04049*11*x+
0.05835*11*x)

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Style exertion",
ylab="Fruit set",
ylim=c(-0.5,1.5),
xlim=c(-4, 11)) #para 11 mm corolla

curve  (exp(-5.11005+
1.08558*x+
6.66704+
0.07394*22+
-1.53920*x+
-0.20452*22+
-0.04049*22*x+
0.05835*22*x)/1+exp(-5.11005+
1.08558*x+
6.66704+
0.07394*22+
-1.53920*x+
-0.20452*22+
-0.04049*22*x+
0.05835*22*x)

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 22 mm corolla


curve   (exp(-5.11005+
1.08558*x+
6.66704+
0.07394*33+
-1.53920*x+
-0.20452*33+
-0.04049*33*x+
0.05835*33*x)/1+exp(-5.11005+
1.08558*x+
6.66704+
0.07394*33+
-1.53920*x+
-0.20452*33+
-0.04049*33*x+
0.05835*33*x)


,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 33

legend(0,0,lwd=3,lty=c(1,2,3),c("11 mm","22 mm","33 mm"))

head(data)
data[is.na(data),]


data$residuals1000<-residuals(Model1000)

estimalongcor11<-(exp(-5.11005+
1.08558*data[data$population=="E",]$styleexertion+
6.66704+
0.07394*11+
-1.53920*data[data$population=="E",]$styleexertion+
-0.20452*11+
-0.04049*11*data[data$population=="E",]$styleexertion+
0.05835*11*data[data$population=="E",]$styleexertion)/1+exp(-5.11005+
1.08558*data[data$population=="E",]$styleexertion+
6.66704+
0.07394*11+
-1.53920*data[data$population=="E",]$styleexertion+
-0.20452*11+
-0.04049*11*data[data$population=="E",]$styleexertion+
0.05835*11*data[data$population=="E",]$styleexertion))


resparlongcor11<-data[data$population=="E",]$residuals1000+estimalongcor11


estimalongcor22<-(exp(-5.11005+
1.08558*data[data$population=="E",]$styleexertion+
6.66704+
0.07394*22+
-1.53920*data[data$population=="E",]$styleexertion+
-0.20452*22+
-0.04049*22*data[data$population=="E",]$styleexertion+
0.05835*22*data[data$population=="E",]$styleexertion)/1+exp(-5.11005+
1.08558*data[data$population=="E",]$styleexertion+
6.66704+
0.07394*22+
-1.53920*data[data$population=="E",]$styleexertion+
-0.20452*22+
-0.04049*22*data[data$population=="E",]$styleexertion+
0.05835*22*data[data$population=="E",]$styleexertion))

resparlongcor22<-data[data$population=="E",]$residuals1000+estimalongcor22


estimalongcor33<-(exp(-5.11005+
1.08558*data[data$population=="E",]$styleexertion+
6.66704+
0.07394*33+
-1.53920*data[data$population=="E",]$styleexertion+
-0.20452*33+
-0.04049*33*data[data$population=="E",]$styleexertion+
0.05835*33*data[data$population=="E",]$styleexertion)/1+exp(-5.11005+
1.08558*data[data$population=="E",]$styleexertion+
6.66704+
0.07394*33+
-1.53920*data[data$population=="E",]$styleexertion+
-0.20452*33+
-0.04049*33*data[data$population=="E",]$styleexertion+
0.05835*33*data[data$population=="E",]$styleexertion))

resparlongcor33<-data[data$population=="E",]$residuals1000+estimalongcor33


points(data[data$population=="E",]$styleexertion,resparlongcor11, pch=17)
points(data[data$population=="E",]$styleexertion,resparlongcor22, pch=19)
points(data[data$population=="E",]$styleexertion,resparlongcor33, pch=21)


###SON TRIES
curve (exp(-5.11005+
1.08558*x+
5.46577+
0.07394*11+
-1.48139*x+
-0.26117*11+
-0.04049*11*x+
0.07025*11*x)/1+exp(-5.11005+
1.08558*x+
5.46577+
0.07394*11+
-1.48139*x+
-0.26117*11+
-0.04049*11*x+
0.07025*11*x)

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Style exertion",
ylab="Fruit set",
ylim=c(-0.5,1.5),
xlim=c(-4, 11)) #para 11 mm corolla

curve  (exp(-5.11005+
1.08558*x+
5.46577+
0.07394*22+
-1.48139*x+
-0.26117*22+
-0.04049*22*x+
0.07025*22*x)/1+exp(-5.11005+
1.08558*x+
5.46577+
0.07394*22+
-1.48139*x+
-0.26117*22+
-0.04049*22*x+
0.07025*22*x)

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 22 mm corolla


curve  (exp(-5.11005+
1.08558*x+
5.46577+
0.07394*33+
-1.48139*x+
-0.26117*33+
-0.04049*33*x+
0.07025*33*x)/1+exp(-5.11005+
1.08558*x+
5.46577+
0.07394*33+
-1.48139*x+
-0.26117*33+
-0.04049*33*x+
0.07025*33*x)

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para 33

legend(0,0,lwd=3,lty=c(1,2,3),c("11 mm","22 mm","33 mm"))

head(data)
data[is.na(data),]


data$residuals1000<-residuals(Model1000)

estimalongcor11<-(exp(-5.11005+
1.08558*data[data$population=="ST",]$styleexertion+
5.46577+
0.07394*11+
-1.48139*data[data$population=="ST",]$styleexertion+
-0.26117*11+
-0.04049*11*data[data$population=="ST",]$styleexertion+
0.07025*11*data[data$population=="ST",]$styleexertion)/1+exp(-5.11005+
1.08558*data[data$population=="ST",]$styleexertion+
5.46577+
0.07394*11+
-1.48139*data[data$population=="ST",]$styleexertion+
-0.26117*11+
-0.04049*11*data[data$population=="ST",]$styleexertion+
0.07025*11*data[data$population=="ST",]$styleexertion))

resparlongcor11<-data[data$population=="ST",]$residuals1000+estimalongcor11


estimalongcor22<-(exp(-5.11005+
1.08558*data[data$population=="ST",]$styleexertion+
5.46577+
0.07394*22+
-1.48139*data[data$population=="ST",]$styleexertion+
-0.26117*22+
-0.04049*22*data[data$population=="ST",]$styleexertion+
0.07025*22*data[data$population=="ST",]$styleexertion)/1+exp(-5.11005+
1.08558*data[data$population=="ST",]$styleexertion+
5.46577+
0.07394*22+
-1.48139*data[data$population=="ST",]$styleexertion+
-0.26117*22+
-0.04049*22*data[data$population=="ST",]$styleexertion+
0.07025*22*data[data$population=="ST",]$styleexertion))


resparlongcor22<-data[data$population=="ST",]$residuals1000+estimalongcor22


estimalongcor33<-(exp(-5.11005+
1.08558*data[data$population=="ST",]$styleexertion+
5.46577+
0.07394*33+
-1.48139*data[data$population=="ST",]$styleexertion+
-0.26117*33+
-0.04049*33*data[data$population=="ST",]$styleexertion+
0.07025*33*data[data$population=="ST",]$styleexertion)/1+exp(-5.11005+
1.08558*data[data$population=="ST",]$styleexertion+
5.46577+
0.07394*33+
-1.48139*data[data$population=="ST",]$styleexertion+
-0.26117*33+
-0.04049*33*data[data$population=="ST",]$styleexertion+
0.07025*33*data[data$population=="ST",]$styleexertion))

resparlongcor33<-data[data$population=="ST",]$residuals1000+estimalongcor33


points(data[data$population=="ST",]$styleexertion,resparlongcor11, pch=17)
points(data[data$population=="ST",]$styleexertion,resparlongcor22, pch=19)
points(data[data$population=="ST",]$styleexertion,resparlongcor33, pch=21)





Model2000 <- glm(semillasporfrutoR~styleexertion*population+longcor*population+styleexertion*longcor*population, family=poisson, na.action=na.omit, data=data)
summary(Model2000)
drop1 (Model2000, test="Chisq")###styleexertion:population:longcor  2   38.705 173.96 0.24749   0.8836

Model2000.1 <- glm(semillasporfrutoR~styleexertion*population+longcor*population, family=poisson, na.action=na.omit, data=data)
summary(Model2000.1)
drop1 (Model2000.1, test="Chisq")###styleexertion:population  2   42.141 171.39 3.4358   0.1794
						##population:longcor        2   39.969 169.22 1.2632   0.5317

Model2000.2 <- glm(semillasporfrutoR~styleexertion*population+longcor, family=poisson, na.action=na.omit, data=data)
summary(Model2000.2)
drop1 (Model2000.2, test="Chisq")#longcor                   1   40.079 167.33 0.11067   0.7394
						###styleexertion:population  2   42.857 168.11 2.88845   0.2359

Model2000.3 <- glm(semillasporfrutoR~styleexertion+population+longcor, family=poisson, na.action=na.omit, data=data)
summary(Model2000.3)
drop1 (Model2000.3, test="Chisq")###styleexertion  1   42.866 166.12 0.0089  0.92476  
						###population     2   48.633 169.88 5.7763  0.05568 .
						###longcor        1   43.277 166.53 0.4198  0.51703  

