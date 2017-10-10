
#source('d:/cosas nuevas a guardar en H/BBbehaviourFIELD/Centaureapaper/analisisRcorregidos/scriptscorr/Variance_Inflation_Factors.R')

data<-read.table('c:/R/Lonicera/HSAINTERPOP2.csv',sep=';',head=T)

head(data)
fix(data)
names(data)

class (data$seedsflo)
class (data$fruitset)

hist(data$seedsflo)

#####para hcerlo por separado
B<-data[data$population=="B", ]
E<-data[data$population=="E", ]
ST<-data[data$population=="ST", ]

#center the dependent variable
B$seedsfloC <- B$seedsflo/mean(B$seedsflo)
E$seedsfloC <- E$seedsflo/mean(E$seedsflo)
ST$seedsfloC <-ST$seedsflo/mean(ST$seedsflo)

hist(B$seedsfloC)
hist(E$seedsfloC)
hist(ST$seedsfloC)

#center the predictors

B$longcorC <- B$longcor - mean(B$longcor)
B$tubewidthC <- B$tubewidth - mean(B$tubewidth)
B$styleexertionC <- B$styleexertion - mean(B$styleexertion)
B$herkoRelaC <- B$herkoRela - mean(B$herkoRela)

B$totfloresC <- B$totflowers - mean(B$totflowers)
head(data)

ST$longcorC <- ST$longcor - mean(ST$longcor)
ST$tubewidthC <-ST$tubewidth - mean(ST$tubewidth)
ST$styleexertionC <-ST$styleexertion - mean(ST$styleexertion)
ST$herkoRelaC <-ST$herkoRela - mean(ST$herkoRela)

ST$totfloresC <-ST$totflowers - mean(ST$totflowers)

E$longcorC <- E$longcor - mean(E$longcor)
E$tubewidthC <-E$tubewidth - mean(E$tubewidth)
E$styleexertionC <-E$styleexertion - mean(E$styleexertion)
E$herkoRelaC <-E$herkoRela - mean(E$herkoRela)

E$totfloresC <-E$totflowers - mean(E$totflowers)

###quadratic terms
##data$longcorC2 <- data$longcorC*data$longcorC
##data$tubewidthC2 <- data$tubewidthC*data$tubewidthC

B$styleexertionC2 <- B$styleexertionC*B$styleexertionC
B$herkoRelaC2 <- B$herkoRelaC*B$herkoRelaC

E$styleexertionC2 <- E$styleexertionC*E$styleexertionC
E$herkoRelaC2 <- E$herkoRelaC*E$herkoRelaC

ST$styleexertionC2 <- ST$styleexertionC*ST$styleexertionC
ST$herkoRelaC2 <- ST$herkoRelaC*ST$herkoRelaC

#VIF
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}

corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  #correlation part
  cat("Correlations of the variables\n\n")
  tmp_cor <- cor(dataz,use="complete.obs")
  print(tmp_cor)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}
corvif(cbind(B$longcorC,B$styleexertionC,B$herkoRelaC,B$tubewidthC))


corvif(cbind(E$longcorC,E$styleexertionC,E$herkoRelaC,E$tubewidthC))


corvif(cbind(ST$longcorC,ST$styleexertionC,ST$herkoRelaC,ST$tubewidthC))


#####TODO JUNTO con SELECCION AUTOMATICA

library(nlme)

M01<-lm(seedsflo~ population+ longcor*population + tubewidth*population+herkoRela*population, na.action=na.omit, data=data)
M02<-lm(seedsflo~ population+ longcor+tubewidth+herkoRela+population, na.action=na.omit, data=data)
M03<-lm(seedsflo~ population+ tubewidth*population+herkoRela*population, na.action=na.omit, data=data)
M04<-lm(seedsflo~ population+ longcor*population+herkoRela*population, na.action=na.omit, data=data)
M05<-lm(seedsflo~ population+ longcor*population + tubewidth*population, na.action=na.omit, data=data)
M06<-lm(seedsflo~ herkoRela+population, na.action=na.omit, data=data)
M07<-lm(seedsflo~ population, na.action=na.omit, data=data)
anova(M01,M02,M03,M04,M05)
AIC(M01,M02,M03,M04,M05,M06,M07)
summary(M01)
drop1(M01)
anova(M01)

library(MuMIn)
ms.area.lm<-dredge(M01,rank="AICc",extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
dev.new(height=18,width=20)
plot(ms.area.lm, xlab=c("dns","plt", "dns:plt")) # Gr?fico ?til para ver que variables salen significativas en cada modelo
delta2=subset(ms.area.lm, subset = delta < 2) #Te devuelve los modelos con delta < 2
summary(model.avg(ms.area.lm, subset = delta < 2))

library(MuMIn)
options(na.action = "na.fail") # prevent fitting models to different datasets
ddHsubop<- dredge(M01, fixed = "population") #m.min=1 is to always keep one variable in the model and fixed = is a variale to always have in the model
subset(ddHsubop, delta < 4)
summary(get.models(ddHsubop, 1)[[1]])


M02<-lm(log(seedsflo+1)~ herkoRela+population, na.action=na.omit, data=data)
summary(M02)
anova(M02)

data$logseedsflo<-log(data$seedsflo+1)
hist(data$logseedsflo)

####con gamma
M01<-glm(seedsflo+1~ population+ longcor*population + longcor*population+tubewidth*population+herkoRela*population, family=Gamma(link=log), na.action=na.omit, data=data)
M01<-glm(seedsflo+1~ longcor*population + longcor*population+tubewidth*population+herkoRela*population, family=Gamma(link=log), na.action=na.omit, data=data)
M01<-glm(seedsflo+1~ population+ longcor*population + longcor*population+tubewidth*population+herkoRela*population, family=Gamma(link=log), na.action=na.omit, data=data)
M01<-glm(seedsflo+1~ population+ longcor*population + longcor*population+tubewidth*population+herkoRela*population, family=Gamma(link=log), na.action=na.omit, data=data)
M01<-glm(seedsflo+1~ population+ longcor*population + longcor*population+tubewidth*population+herkoRela*population, family=Gamma(link=log), na.action=na.omit, data=data)
M01<-glm(seedsflo+1~ population+ longcor*population + longcor*population+tubewidth*population+herkoRela*population, family=Gamma(link=log), na.action=na.omit, data=data)
M01<-glm(seedsflo+1~ population+ longcor*population + longcor*population+tubewidth*population+herkoRela*population, family=Gamma(link=log), na.action=na.omit, data=data)




summary(M01)
step(M01)


M01<-glm(seedsflo+1~ population+ herkoRela, family=Gamma(link=log), na.action=na.omit, data=data)
summary(M01)
anova.glm(M01, test="Chisq")
drop1(M01)

M01<-glm(seedsflo+1~ population*herkoRela, family=Gamma(link=log), na.action=na.omit, data=data)
summary(M01)###no sale sign

library(rJava)
library(glmulti)

glmulti(M01, family=Gamma)
resultado01<-glmulti(M01)



###style exertion
library(nlme)

M01<-lm(seedsflo~ population+ longcor*population + longcor*population+tubewidth*population+styleexertion*population, na.action=na.omit, data=data)
summary(M01)
step(M01)

M01<-lm(seedsflo~ population+ styleexertion, na.action=na.omit, data=data)
summary(M01)

M01<-lm(seedsflo~ population*styleexertion, na.action=na.omit, data=data)
summary(M01)###nosale


###con gamma
M01<-glm(seedsflo+1~ population+ longcor*population + longcor*population+tubewidth*population+styleexertion*population, family=Gamma(link=log), na.action=na.omit, data=data)
summary(M01)
step(M01)


M01<-glm(seedsflo+1~ population+ styleexertion, family=Gamma(link=log), na.action=na.omit, data=data)
summary(M01)


###POR POBLAICONE
####banyalbufar

###centradas
M01<-lm(seedsfloC~ longcorC +herkoRelaC*longcorC+herkoRelaC2, na.action=na.omit, data=B)
summary(M01)
anova(M01)
step(M01)

M01<-lm(seedsfloC~ herkoRelaC, na.action=na.omit, data=B)
summary(M01)



M01<-glm(seedsfloC+1~ longcorC +herkoRelaC*longcorC+herkoRelaC2, family=Gamma(link=log),na.action=na.omit, data=B)
summary(M01)
step(M01)

M01<-glm(seedsfloC+1~ herkoRelaC, family=Gamma(link=log),na.action=na.omit, data=B)
summary(M01)####no sale nada
step(M01)

####sincentrar
M01<-glm(seedsflo+1~ longcor +herkoRela*longcor, family=Gamma(link=log),na.action=na.omit, data=B)
summary(M01)
step(M01)

M01<-glm(seedsflo+1~ herkoRela, family=Gamma(link=log),na.action=na.omit, data=B)
summary(M01)
step(M01)####no sale nada


###establiments
###centradas
M01<-lm(seedsfloC~ longcorC +herkoRelaC*longcorC+herkoRelaC2, na.action=na.omit, data=E)
summary(M01)
anova(M01)
step(M01)

M01<-lm(seedsfloC~herkoRelaC2+herkoRelaC, na.action=na.omit, data=E)
summary(M01)

M01<-lm(seedsfloC~longcorC, na.action=na.omit, data=E)
summary(M01)##es el mejor, pero no es signif

###con gamma
M01<-glm(seedsfloC+1~ longcorC +herkoRelaC+herkoRelaC2, family=Gamma(link=log), na.action=na.omit, data=E)
summary(M01)
anova(M01)
step(M01)

M01<-glm(seedsfloC+1~ longcorC , family=Gamma(link=log), na.action=na.omit, data=E)
summary(M01)##es el mejor, pero no es signif


####sincentrar
M01<-glm(seedsflo+1~ longcor +herkoRela*longcor, family=Gamma(link=log),na.action=na.omit, data=E)
summary(M01)
step(M01)

M01<-glm(seedsflo+1~ longcor , family=Gamma(link=log),na.action=na.omit, data=E)
summary(M01)##pero no es signi

###son tries
###centradas
M01<-lm(seedsfloC~ longcorC +herkoRelaC*longcorC+herkoRelaC2*longcorC, na.action=na.omit, data=ST)
summary(M01)
anova(M01)
step(M01)

M01<-lm(seedsfloC~herkoRelaC, na.action=na.omit, data=ST)
summary(M01)###pero no es signifi

###congamma
M01<-glm(seedsfloC+1~ longcorC +herkoRelaC*longcorC+herkoRelaC2*longcorC, family=Gamma(link=log), na.action=na.omit, data=ST)
summary(M01)
anova(M01)
step(M01)



M01<-glm(seedsfloC+1~herkoRelaC,  family=Gamma(link=log), na.action=na.omit, data=ST)
summary(M01)###si sale!


library(rJava)
library(glmulti)

resultado1<-glmulti(M01, level = 1, method = "h")


####sincentrar
M01<-glm(seedsflo+1~ longcor +herkoRela*longcor, family=Gamma(link=log),na.action=na.omit, data=ST)
summary(M01)
step(M01)

M01<-glm(seedsflo+1~ herkoRela, family=Gamma(link=log),na.action=na.omit, data=ST)
summary(M01)###marginal





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

