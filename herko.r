
#read data
data <- read.table("Lonicera.txt", h=T,sep='\t',dec='.')
#explore data
str(data)
fix(data)
data[1:5,]
summary(data)
hist(data$Longestigma)
hist(data$styleexertion)
hist(data$herkoRela,breaks=20)
plot(data$herkoRela,data$Longestigma)
plot(data$Longestigma)

#Distribución de la herkogamia entre individuos, y entre poblaciones
dev.new(height=20,width=20)
boxplot(data$herkoRela~data$plantID)
boxplot(data$herkoRela~data$population,ylim=c(-12,10))
boxplot(data$herkoRela~data$plantID:data$population)


#preliminar analysis

m<-lm(data$herkoRela~data$population)
summary(m)
library(lme4)
glm1<-lmer(herkoRela~population+(1|plantID/infloID),na.action=na.omit,data=data)
glm1<-lmer(herkoRela~population+(1|plantID),na.action=na.omit,data=data)
summary(glm1)
plot(glm1)
confint(glm1)
library(effects)
plot(allEffects(glm1))
library(visreg)
visreg(glm1,scale = "response")
##Antes del modelo, mirar linearidad (distribuciÃ³n), residuos (que sean homogÃ©neos, no en forma de embudo), como comprobarlos?

hist(resid(glm1))
library(MuMIn)
library(DHARMa)#permite analizar los residuos
r.squaredGLM(glm1)
coef(mixed)
##el modelo va mejor con la variable controlada
##el summary nos da el SD del random, i de los residuos (del modelo entero)
##tenemos una sola pendiente, un intercept medio, despues tengo un intercept
##para cada random, y el summary me da la varianza entre esos intercepts
library(sjPlot)
sjp.lmer(glm1)
sjp.lmer(glm1,type = "ri.slope")####recta para cada random
sjp.lmer(glm1, type = "eff", show.ci = TRUE)##recta general del modelo

library(nlme)
M02 <- lme(herkoRela~ longcor*population+anchcor*population, random= ~1|plantID/infloID, na.action=na.omit, data=data)
summary(M02)
anova(M02)

mediasanchcor <- mean(data$anchcor, na.rm = TRUE)###2.090952

####POP-LONGCOR 

curve (-0.721588+
-0.026814*x+
7.546652+
-0.698053*2.090952+
-0.233662*x+
0.724247*2.090952


,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
xlab="Corolla length",
ylab="Herkogamy",
ylim=c(-20,20),
xlim=c(0, 50)) #para E

curve  (-0.721588+
-0.026814*x+
6.571178+
-0.698053*2.090952+
-0.167285*x+
0.658166*2.090952

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para ST


curve  (-0.721588+
-0.026814*x+
-0.698053*2.090952

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para B

legend(30,20,lwd=3,lty=c(1,2,3),c("Banyalbufar","Establiments","Son Tríes"))

head(data)
data[is.na(data),]


data$residuals02<-residuals(M02)

estimalongcorE<-(-0.721588+
-0.026814*data[data$population=="E",]$longcor+
7.546652+
-0.698053*2.090952+
-0.233662*data[data$population=="E",]$longcor+
0.724247*2.090952)


resparlongcorE<-data[data$population=="E",]$residuals02+estimalongcorE


estimalongcorST<-(-0.721588+
-0.026814*data[data$population=="ST",]$longcor+
6.571178+
-0.698053*2.090952+
-0.167285*data[data$population=="ST",]$longcor+
0.658166*2.090952)


resparlongcorST<-data[data$population=="ST",]$residuals02+estimalongcorST


estimalongcorB<-(-0.721588+
-0.026814*data[data$population=="B",]$longcor+
-0.698053*2.090952)


resparlongcorB<-data[data$population=="B",]$residuals02+estimalongcorB


points(data[data$population=="B",]$longcor,resparlongcorB, pch=17)
points(data[data$population=="E",]$longcor,resparlongcorE, pch=19)
points(data[data$population=="ST",]$longcor,resparlongcorST, pch=21)





library(nlme)
M03 <- lme(styleexertion~ longcor*population+anchcor*population, random= ~1|plantID/infloID, na.action=na.omit, data=data)
summary(M03)
anova(M03)


mediasanchcor <- mean(data$anchcor, na.rm = TRUE)###2.090952

####POP-LONGCOR 

curve (3.155826+
-0.016938*x+
4.883148+
-0.488500*2.090952+
-0.116396*x+
0.603311*2.090952


,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
xlab="Corolla length",
ylab="Style exertion",
ylim=c(-10,20),
xlim=c(0, 50)) #para E

curve  (3.155826+
-0.016938*x+
4.356227+
-0.488500*2.090952+
-0.062258*x+
0.434276*2.090952

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para ST


curve  (3.155826+
-0.016938*x+
-0.488500*2.090952

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para B

legend(30,20,lwd=3,lty=c(1,2,3),c("Banyalbufar","Establiments","Son Tríes"))

head(data)
data[is.na(data),]


data$residuals03<-residuals(M03)

estimalongcorE<-(3.155826+
-0.016938*data[data$population=="E",]$longcor+
4.883148+
-0.488500*2.090952+
-0.116396*data[data$population=="E",]$longcor+
0.603311*2.090952)

resparlongcorE<-data[data$population=="E",]$residuals03+estimalongcorE


estimalongcorST<-(3.155826+
-0.016938*data[data$population=="ST",]$longcor+
4.356227+
-0.488500*2.090952+
-0.062258*data[data$population=="ST",]$longcor+
0.434276*2.090952)


resparlongcorST<-data[data$population=="ST",]$residuals03+estimalongcorST


estimalongcorB<-(3.155826+
-0.016938*data[data$population=="B",]$longcor+
-0.488500*2.090952)


resparlongcorB<-data[data$population=="B",]$residuals03+estimalongcorB


points(data[data$population=="B",]$longcor,resparlongcorB, pch=17)
points(data[data$population=="E",]$longcor,resparlongcorE, pch=19)
points(data[data$population=="ST",]$longcor,resparlongcorST, pch=21)





library(nlme)
M05 <- lme(herkoRela~ styleexertion*population, random= ~1|plantID/infloID, na.action=na.omit, data=data)
summary(M05)
anova(M05)


####POP-LONGCOR - 2011

curve (22.621568 +
-3.962796+
 3.165782*x+
-1.258822*x

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
xlab="Corolla length",
ylab="Upper lip area",
ylim=c(0,200),
xlim=c(0, 50)) #para E

curve (22.621568 +
-19.028834+
 3.165782*x+
-0.008419*x


,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para ST


curve (22.621568 +
 3.165782*x

,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para B

legend(0,200,lwd=3,lty=c(1,2,3),c("Banyalbufar","Establiments","Son Tríes"))

head(data)
data[is.na(data),]


data$residuals02<-residuals(M02)

estimalongcorE<-(22.621568 +
-3.962796+
 3.165782*data[data$population=="E",]$longcor+
-1.258822*data[data$population=="E",]$longcor)

resparlongcorE<-data[data$population=="E",]$residuals02+estimalongcorE


estimalongcorST<-(22.621568 +
-19.028834+
 3.165782*data[data$population=="ST",]$longcor+
-0.008419*data[data$population=="ST",]$longcor)


resparlongcorST<-data[data$population=="ST",]$residuals02+estimalongcorST


estimalongcorB<-(22.621568 +
 3.165782*data[data$population=="B",]$longcor)

resparlongcorB<-data[data$population=="B",]$residuals02+estimalongcorB


points(data[data$population=="B",]$longcor,resparlongcorB, pch=17)
points(data[data$population=="E",]$longcor,resparlongcorE, pch=19)
points(data[data$population=="ST",]$longcor,resparlongcorST, pch=21)


####POP-LONGCOR - 2012

curve (22.621568 +
-14.845747+
-3.962796+
 3.165782*x+
-1.258822*x+
-0.413580*x+
-5.030772+
2.213989*x

,add=F, #esto lo que hace es decir si añade o no a un gráfico existente
lty=2,
lwd=3,
xlab="Corolla length",
ylab="Upper lip area",
ylim=c(0,200),
xlim=c(0, 50)) #para E

curve (22.621568 +
-14.845747+
-19.028834+
 3.165782*x+
-0.008419*x+
-0.413580*x+
4.989330+
 0.885456*x


,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=3,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para ST


curve (22.621568 +
-14.845747+
 3.165782*x+
-0.413580*x


,add=T, #esto lo que hace es decir si añade o no a un gráfico existente
lty=1,
lwd=3,
#xlab="titulo de x",
#ylab="titulo de y"
) #para B

##legend(0,200,lwd=3,lty=c(1,2,3),c("Banyalbufar","Establiments","Son Tríes"))

head(data)
data[is.na(data),]


data$residuals02<-residuals(M02)

estimalongcorE<-(22.621568 +
-14.845747+
-3.962796+
 3.165782*data[data$population=="E",]$longcor+
-1.258822*data[data$population=="E",]$longcor+
-0.413580*data[data$population=="E",]$longcor+
-5.030772+
2.213989*data[data$population=="E",]$longcor)


resparlongcorE<-data[data$population=="E",]$residuals02+estimalongcorE


estimalongcorST<-(22.621568 +
-14.845747+
-19.028834+
 3.165782*data[data$population=="ST",]$longcor+
-0.008419*data[data$population=="ST",]$longcor+
-0.413580*data[data$population=="ST",]$longcor+
4.989330+
 0.885456*data[data$population=="ST",]$longcor)


resparlongcorST<-data[data$population=="ST",]$residuals02+estimalongcorST


estimalongcorB<- (22.621568 +
-14.845747+
 3.165782*data[data$population=="B",]$longcor+
-0.413580*data[data$population=="B",]$longcor)

resparlongcorB<-data[data$population=="B",]$residuals02+estimalongcorB


points(data[data$population=="B",]$longcor,resparlongcorB, pch=17)
points(data[data$population=="E",]$longcor,resparlongcorE, pch=19)
points(data[data$population=="ST",]$longcor,resparlongcorST, pch=21)







