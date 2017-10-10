
source('d:/cosas nuevas a guardar en H/BBbehaviourFIELD/Centaureapaper/analisisRcorregidos/scriptscorr/Variance_Inflation_Factors.R')

data<-read.table('d:/cosas nuevas a guardar en H/Lonicera/Lonicera2012/paperherko/HSA/HSAST.csv',sep=';',head=T)

head(data)

names(data)

class (data$seedsflo)
class (data$fruitset)

#center the dependent variable
data$seedsfloC <- data$seedsflo / mean(data$seedsflo)
data$fruitsetC <- data$fruitset / mean(data$fruitset)

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
data$herkoRelaC2 <- data$herkoRelaC*data$herkoRelaC


#corvif(cbind(data$INTcorC, data$IefficC,data$IattracCorrC,data$IaccesCorrC,data$longcorC,data$longpetC,data$anchopetC,data$totfloresC,data$antherslengthC,data$tubewidthC))

#corvif(cbind(data$INTcorC, data$IefficC,data$IaccesCorrC,data$longcorC,data$totfloresC,data$antherslengthC))

corvif(cbind(data$longcorC,data$longpetC,data$anchopetC,data$styleexertionC,data$herkoRelaC,data$tubewidthC))
corvif(cbind(data$longcorC,data$longpetC,data$anchopetC,data$herkoRelaC,data$tubewidthC))



###FLORAL TRAITS 
M01<-lm(seedsfloC~longcorC + longcorC2+tubewidthC+tubewidthC2+herkoRelaC+herkoRelaC2, na.action=na.omit, data=data)
summary(M01)
step(M01)

M02<-lm(seedsfloC~herkoRelaC*tubewidthC, na.action=na.omit, data=data)
summary(M02)

M03<-lm(seedsfloC ~ tubewidthC + tubewidthC2 + herkoRelaC, na.action=na.omit, data=data)
summary(M03)
anova(M03)

M03<-lm(seedsfloC~longcorC*herkoRelaC, na.action=na.omit, data=data)
summary(M03)

M010<-lm(fruitsetC~longcorC + longcorC2+tubewidthC+tubewidthC2+herkoRelaC+herkoRelaC2, na.action=na.omit, data=data)
summary(M010)
step(M010)

M011<-lm(fruitsetC~herkoRelaC, na.action=na.omit, data=data)
summary(M011)
step(M011)

M012<-lm(fruitsetC~longcorC*herkoRelaC, na.action=na.omit, data=data)
summary(M012)




M011<-lm(fruitsetC ~ data$longcorC2 + longpetC + data$longpetC2 + 
    data$anchopetC2 + antherslengthC + data$antherslengthC2 + 
    stylelengthC + data$stylelengthC2 + data$tubewidthC + data$tubewidthC2, 
    data = data, na.action = na.omit)

summary(M011)

M012<-lm(fruitsetC ~ data$longcorC2 + longpetC + data$longpetC2 + 
    data$anchopetC2 + antherslengthC + data$antherslengthC2 + 
    stylelengthC + data$stylelengthC2 + data$tubewidthC + data$tubewidthC2, 
    data = data, na.action = na.omit)

summary(M012)


M013<-lm(fruitsetC ~ data$longcorC2 + data$longpetC2 + 
    data$anchopetC2 + antherslengthC + data$antherslengthC2 + 
    stylelengthC + data$stylelengthC2 + data$tubewidthC + data$tubewidthC2, 
    data = data, na.action = na.omit)

summary(M013)
###este es el mejor de fruitset
M014<-lm(fruitsetC ~ data$longcorC2 +
    data$anchopetC2 + antherslengthC + data$antherslengthC2 + 
    stylelengthC + data$stylelengthC2 + data$tubewidthC + data$tubewidthC2, 
    data = data, na.action = na.omit)

summary(M014)


### FLORAL TRAITS SIN STYLELENGHT
M020<-lm(seedsfloC~longcorC + data$longcorC2+longpetC+data$longpetC2+anchopetC+data$anchopetC2+antherslengthC+data$antherslengthC2+data$tubewidthC+data$tubewidthC2+data$totfloresC, na.action=na.omit, data=data)
summary(M020)
step(M020)

M021<-lm(seedsfloC ~ data$longpetC2 + data$anchopetC2 + antherslengthC + 
    data$antherslengthC2, data = data, na.action = na.omit)
summary(M021)

M022<-lm(seedsfloC ~ data$longpetC2 + data$anchopetC2 + antherslengthC, data = data, na.action = na.omit)
summary(M022)


M023<-lm(seedsfloC ~ data$longpetC2 + data$anchopetC2, data = data, na.action = na.omit)
summary(M023)

###mejor de seedsflo
M024<-lm(seedsfloC ~ data$anchopetC2, data = data, na.action = na.omit)
summary(M024)


##no sale nada
M025<-lm(fruitsetC~longcorC + data$longcorC2+longpetC+data$longpetC2+anchopetC+data$anchopetC2+antherslengthC+data$antherslengthC2+data$tubewidthC+data$tubewidthC2+data$totfloresC, na.action=na.omit, data=data)
summary(M025)
step(M025)

M026<-lm(fruitsetC ~ data$longpetC2 + data$anchopetC2 + antherslengthC, 
    data = data, na.action = na.omit)
summary(M026)

M027<-lm(fruitsetC ~ data$longpetC2 + data$anchopetC2, 
    data = data, na.action = na.omit)
summary(M027)

M028<-lm(fruitsetC ~ data$longpetC2, 
    data = data, na.action = na.omit)
summary(M028)



###INDICES (no sale nada)

M030<-lm(seedsfloC~data$IefficC + data$IefficC2+data$IattracCorrC+data$IattracCorrC2+data$IaccesCorrC+data$IaccesCorrC2+data$totfloresC, na.action=na.omit, data=data)
summary(M030)
step(M030)


M032<-lm(fruitsetC~data$IefficC + data$IefficC2+data$IattracCorrC+data$IattracCorrC2+data$IaccesCorrC+data$IaccesCorrC2+data$totfloresC, na.action=na.omit, data=data)
summary(M032)
step(M032)




###INT (no sale nada)

M040<-lm(seedsfloC~data$INTcorC +data$INTcorC2 +data$totfloresC, na.action=na.omit, data=data)
summary(M040)
step(M040)

###este es el mejor con seedsflo
M041<-lm(seedsfloC~data$INTcorC +data$INTcorC2, na.action=na.omit, data=data)
summary(M041)


M042<-lm(fruitsetC~data$INTcorC +data$INTcorC2 +data$totfloresC, na.action=na.omit, data=data)
summary(M042)
step(M042)

###el mejor con fruitset
M043<-lm(fruitsetC~data$INTcorC +data$INTcorC2, na.action=na.omit, data=data)
summary(M043)





#####TODO JUNTO

M050<-lm(seedsfloC~longcorC + data$longcorC2+longpetC+data$longpetC2+anchopetC+data$anchopetC2+antherslengthC+data$antherslengthC2+data$tubewidthC+data$tubewidthC2+data$totfloresC+data$IefficC + data$IefficC2+data$IattracCorrC+data$IattracCorrC2+data$IaccesCorrC+data$IaccesCorrC2+data$INTcorC +data$INTcorC2, na.action=na.omit, data=data)
summary(M050)
step(M050)

M051<-lm(seedsfloC ~ longpetC + anchopetC + data$anchopetC2 + 
    antherslengthC + data$antherslengthC2 + data$tubewidthC + 
    data$IefficC + data$IefficC2 + data$IattracCorrC + data$INTcorC + 
    data$INTcorC2, data = data, na.action = na.omit)

summary(M051)

M052<-lm(seedsfloC ~ longpetC + anchopetC + data$anchopetC2 + 
    antherslengthC + data$tubewidthC + 
    data$IefficC + data$IefficC2 + data$IattracCorrC + data$INTcorC + 
    data$INTcorC2, data = data, na.action = na.omit)

summary(M052)

###este esta muy bien con seedsflo
M053<-lm(seedsfloC ~ longpetC + anchopetC + data$anchopetC2 + 
    antherslengthC + data$tubewidthC + 
    data$IefficC + data$IattracCorrC + data$INTcorC + 
    data$INTcorC2, data = data, na.action = na.omit)

summary(M053)
aic(M053)


M054<-lm(seedsfloC ~ longpetC + anchopetC + data$antherslengthC + 
    data$tubewidthC + 
    data$IefficC + data$IattracCorrC + data$INTcorC + 
    data$INTcorC2, data = data, na.action = na.omit)

summary(M054)


M055<-lm(seedsfloC ~ longpetC + anchopetC + 
    data$tubewidthC + 
    data$IefficC + data$IattracCorrC + data$INTcorC + 
    data$INTcorC2, data = data, na.action = na.omit)

summary(M055)

###este es el mejor con seedflo
M056<-lm(seedsfloC ~ longpetC +
    data$tubewidthC + 
    data$IefficC + data$IattracCorrC + data$INTcorC + 
    data$INTcorC2, data = data, na.action = na.omit)

summary(M056)
step (M056)




M070<-lm(fruitsetC~longcorC + data$longcorC2+longpetC+data$longpetC2+anchopetC+data$anchopetC2+antherslengthC+data$antherslengthC2+data$tubewidthC+data$tubewidthC2+data$totfloresC+data$IefficC + data$IefficC2+data$IattracCorrC+data$IattracCorrC2+data$IaccesCorrC+data$IaccesCorrC2+data$INTcorC +data$INTcorC2, na.action=na.omit, data=data)
summary(M070)
step(M070)


M071<-lm(fruitsetC ~ data$longcorC2 + longpetC + anchopetC + 
    data$anchopetC2 + antherslengthC + data$antherslengthC2 + 
    data$tubewidthC + data$IefficC + data$IefficC2 + data$IattracCorrC + 
    data$IattracCorrC2 + data$IaccesCorrC2 + data$INTcorC + data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M071)

M072<-lm(fruitsetC ~ data$longcorC2 + longpetC + anchopetC + 
    data$anchopetC2 + antherslengthC +
    data$tubewidthC + data$IefficC + data$IefficC2 + data$IattracCorrC + 
    data$IattracCorrC2 + data$IaccesCorrC2 + data$INTcorC + data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M072)

M073<-lm(fruitsetC ~ data$longcorC2 + longpetC + anchopetC + 
    data$anchopetC2 + antherslengthC +
    data$tubewidthC + data$IefficC + data$IefficC2 + data$IattracCorrC + 
    data$IattracCorrC2 + data$IaccesCorrC2 + data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M073)


M074<-lm(fruitsetC ~ data$longcorC2 + longpetC + anchopetC + 
    data$anchopetC2 + antherslengthC +
    data$tubewidthC + data$IefficC + data$IefficC2 + data$IattracCorrC + 
    data$IaccesCorrC2 + data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M074)

M075<-lm(fruitsetC ~ longpetC + anchopetC + 
    data$anchopetC2 + antherslengthC +
    data$tubewidthC + data$IefficC + data$IefficC2 + data$IattracCorrC + 
    data$IaccesCorrC2 + data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M075)

M076<-lm(fruitsetC ~ longpetC + anchopetC + 
    data$anchopetC2 + antherslengthC +
    data$tubewidthC + data$IefficC + data$IattracCorrC + 
    data$IaccesCorrC2 + data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M076)

M077<-lm(fruitsetC ~ longpetC + anchopetC + 
    data$anchopetC2 + antherslengthC +
    data$tubewidthC + data$IefficC + data$IattracCorrC + 
    data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M077)

M078<-lm(fruitsetC ~ longpetC + anchopetC + 
     antherslengthC +
    data$tubewidthC + data$IefficC + data$IattracCorrC + 
    data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M078)

M079<-lm(fruitsetC ~ longpetC +
     antherslengthC +
    data$tubewidthC + data$IefficC + data$IattracCorrC + 
    data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M079)

M080<-lm(fruitsetC ~ longpetC +
   
    data$tubewidthC + data$IefficC + data$IattracCorrC + 
    data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M080)


M081<-lm(fruitsetC ~ longpetC +
   
     data$IefficC + data$IattracCorrC + 
    data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M081)

M082<-lm(fruitsetC ~    
     data$IefficC + data$IattracCorrC + 
    data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M082)


M083<-lm(fruitsetC ~    
     data$IattracCorrC + 
    data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M083)

M084<-lm(fruitsetC ~     
    data$INTcorC2, 
    data = data, na.action = na.omit)

summary(M084)


#####TODO JUNTO con SELECCION AUTOMATICA
library(nlme)
M0100<-glm(seedsfloC~longcorC + longcorC2+longpetC+longpetC2+anchopetC+anchopetC2+antherslengthC+antherslengthC2+tubewidthC+tubewidthC2+totfloresC+IefficC + IefficC2+IattracCorrC+IattracCorrC2+IaccesCorrC+IaccesCorrC2+INTcorC +INTcorC2, na.action=na.omit, data=data)
summary(M0100)
step(M0100)

M0101<-glm(seedsfloC ~ longcorC + longpetC + longpetC2 + anchopetC + 
    anchopetC2 + antherslengthC + antherslengthC2 + tubewidthC + 
    tubewidthC2 + IefficC + IattracCorrC + IaccesCorrC + INTcorC + 
    INTcorC2, data = data, na.action = na.omit)
summary(M0101)
step(M0101)


library(rJava)
library(glmulti)

resultado101<-glmulti(M0101, level = 1, method = "h")

####este es el mejro seedsflo
mediaslongpetC <- mean(data$longpetC, na.rm = TRUE)
mediasanchopetC <- mean(data$anchopetC, na.rm = TRUE)

M0102<-glm(seedsfloC~1+longpetC+anchopetC+INTcorC+INTcorC2, data = data, na.action = na.omit)

summary(M0102)
anova(M0102)
aic(M0102)

###INT
residuals (M0102)

data$RES<-residuals (M0102)

par(mfrow=c(1,1))

curve (  -0.3759 +
-1.7191*x+
 4.54*x^2

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Floral integration (INTc)",
ylab="Seeds/flower",
ylim=c(-4,12),
xlim=c(-1.5,1.5))

estimaINT<-(  -0.3759 +
-1.7191*data$INTcorC+
 4.54*data$INTcorC2)


resparINT<-data$RES+estimaINT

points(data$INTcorC,resparINT, pch=19)

###longpet

residuals (M0102)

data$RES<-residuals (M0102)

par(mfrow=c(1,1))

curve (  -0.3759 +
-0.6391*x

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Upper lip length",
ylab="Seeds/flower",
ylim=c(-4,12),
xlim=c(-5,5))

estimalongpet<-(  -0.3759 +
-0.6391*data$longpetC)



resparlongpet<-data$RES+estimalongpet

points(data$longpetC,resparlongpet, pch=19)


###anchopet

residuals (M0102)

data$RES<-residuals (M0102)

par(mfrow=c(1,1))

curve (  -0.3759 +
 1.3548*x

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Upper lip width",
ylab="Seeds/flower",
ylim=c(-4,12),
xlim=c(-5,5))

estimaanchopet<-(  -0.3759 +
1.3548*data$anchopetC)

resparanchopet<-data$RES+estimaanchopet

points(data$anchopetC,resparanchopet, pch=19)


####alternativos
M0103<-glm(seedsfloC~1+anchopetC+INTcorC+INTcorC2, data = data, na.action = na.omit)

summary(M0103)

data$INTcorC
data$seedsfloC

M0105<-glm(seedsfloC~anchopetC2, data = data, na.action = na.omit)

summary(M0105)

M0106<-glm(seedsfloC~1+longpetC+anchopetC+INTcorC, data = data, na.action = na.omit)

summary(M0106)

M0104<-glm(seedsfloC~INTcorC+INTcorC2, data = data, na.action = na.omit)

summary(M0104)

residuals (M0104)

data$RES<-residuals (M0104)

par(mfrow=c(1,1))

curve ( -0.1790 +
-1.4978*x+
 3.90*x^2

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Floral integration (INTc)",
ylab="Seeds/flower",
ylim=c(-1,4),
xlim=c(-1.5,1.5))

estimaINT<-( -0.1790 +
-1.4978*data$INTcorC+
 3.90*data$INTcorC2)


resparINT<-data$RES+estimaINT

points(data$INTcorC,resparINT, pch=19)



#####TODO JUNTO con SELECCION AUTOMATICA SIN VARIABLES QUE SALIERON EN VIF
library(nlme)
M0400<-glm(seedsfloC~longcorC + longcorC2+antherslengthC+antherslengthC2+totfloresC+IefficC + IefficC2+IaccesCorrC+IaccesCorrC2+INTcorC +INTcorC2, na.action=na.omit, data=data)
summary(M0400)
step(M0400)


library(rJava)
library(glmulti)

resultado400<-glmulti(M0400, level = 1, method = "h")


M0401<-glm(seedsfloC~1+IaccesCorrC+IaccesCorrC2+INTcorC+INTcorC2, na.action=na.omit, data=data)
summary(M0401)


####IaccesCorrC
residuals (M0401)

data$RES<-residuals (M0401)

par(mfrow=c(1,1))

curve (  -0.1543 +
 1.6115*x+
 -0.74*x^2

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
xlab="Accessibility index",
ylab="Seeds/flower",
ylim=c(-7,2),
xlim=c(-1.5,1.5))

estimaACC<-(  -0.1543 +
 1.6115*data$IaccesCorrC+
 -0.74*data$IaccesCorrC2)


resparACC<-data$RES+estimaACC

points(data$IaccesCorrC,resparACC, pch=19)









M0200<-glm(fruitsetC~longcorC + longcorC2+longpetC+longpetC2+anchopetC+anchopetC2+antherslengthC+antherslengthC2+tubewidthC+tubewidthC2+totfloresC+IefficC + IefficC2+IattracCorrC+IattracCorrC2+IaccesCorrC+IaccesCorrC2+INTcorC +INTcorC2, na.action=na.omit, data=data)
summary(M0200)
step(M0200)

resultado200<-glmulti(M0200, level = 1, method = "h")

##el mejor con fruitset
M0201<-glm(fruitsetC~data$INTcorC+data$INTcorC2, data = data, na.action = na.omit)
summary(M0201)
aic(M0201)

M0202<-glm(fruitsetC~anchopetC+data$INTcorC+data$INTcorC2, data = data, na.action = na.omit)
summary(M0202)
aic(M0202)

