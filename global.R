library(survival)
library(knitr)
library(dplyr)
library(gridExtra)
library(openintro)
library(plotrix)
library(formattable)
library(KMsurv)
library(MASS)
library(ggplot2)
library(plotly)
library(survminer)
library(ggsankey)
library(circlize)
library(readxl)
library(pillar)
library(kableExtra)
library(survival)
library(VSURF)
library(KMsurv)
library(randomForestSRC)
library(lubridate)
library(nlme)
library(ggRandomForests)
library(fastDummies)
library(caret)
library(parallel)
load("www/RF_door1.RData")
#load("/Users/carlosmartinperez/Desktop/MENINGITIS/RF/RF_door1.RData")
load("/Users/carlosmartinperez/Desktop/MENINGITIS/RF/RF_door2.RData")
load("/Users/carlosmartinperez/Desktop/MENINGITIS/RF/RF_door3.RData")
load("/Users/carlosmartinperez/Desktop/MENINGITIS/RF/RF_door_todos.RData")
load("/Users/carlosmartinperez/Desktop/MENINGITIS/RF/RF_ED0.RData")

#Algoritmos para obtener todos los modelos posibles con su correspondiente error OOB
# y así seleccionar el modelo que menos error de predicción cometa

#_____funcion primera
data_select<-function(df,Yvar,Xvar=NULL){
  df<-df[,c(Yvar,Xvar)]
  Xvar2<-2:(length(Xvar)+1)
  return(df)
}

#_____funcion segunda (combinaciones tomandas de n en n)
combinar<-function(df,Xvar=NULL,numXVar=1){
  
  comb_Var<-t(combn(Xvar,numXVar));combinaciones<-comb_Var
  
  colnames(combinaciones)<-NULL
  
  options(warn=-1)
  
  get_names<-function(df,x){
    outcome<-matrix(data=NA,nrow=nrow(x),ncol=ncol(x))
    for(i in 1:nrow(x)){
      outcome[i,1:ncol(x)]<-colnames(df)[x[i,1:ncol(x)]]
    }
    return(outcome)
  }
  combn_names<-get_names(df,combinaciones)
  return(combn_names)
}

#_____funcion tercera (obtención de los modelos y su error)
Mconf<-function(df, x)
{
  filas<-nrow(ccc)
  materr2<-matrix(nrow=filas,ncol=2)
  colnames(materr2)<-c( "Variables", "Error")
  rownames(materr2)<-paste("Modelo",c(1:filas))
  for (i in 1:nrow(ccc)){
    xnam <- as.matrix(ccc[i,])
    fmla <- as.formula(paste("Surv(",colnames(b_datos[1]),",",colnames(b_datos[2]),") ~", paste(xnam, collapse= "+")))
    
    mod <- rfsrc(fmla, data = b_datos,ntree = 500)
    df <- mod$data
    prob = predict(mod,type = c("response"))
    df$prob = prob
    
    fmla<-as.character(fmla)
    #coeficiente<-summary(mod)$coefficients[2]
    #pv<-summary(mod)$coefficients[8]
    
    #materr2[i,1]<-nrow(xnam)
    materr2[i,1]<-fmla[3]
    materr2[i,2]<-get.cindex(mod$yvar[,1], mod$yvar[,2], mod$predicted.oob)
    #materr2[i,3]<-round(pv,4)
    
  }
  #return(materr2)
}

  

