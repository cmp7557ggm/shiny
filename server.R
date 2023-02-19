shinyServer(function(input, output) {
  
  dataframe<-reactive({
    if(input$meur=="DOORMAT1"){
      dataframe<-RF_door1
    }
    else{
      if(input$meur=="DOORMAT2"){
        dataframe<-RF_door2
      }
      else{
        if(input$meur=="DOORMAT3"){
          dataframe<-RF_door3
        }
        else{
          if(input$meur=="Todos"){
            dataframe<-RF_door_todos
          }
          else{
            if(input$meur=="Espectro DOORMAT0"){
              dataframe<-RF_ED0
            }
          }}}}
  })
 
  output$titulin=renderText({
    paste("Modelo inicial con todas las variables incluyendo ",input$meur )
  })
  output$titulin2=renderText({
    paste("Importancia por permutación de las variables " )
  })
  output$titulin3=renderText({
    paste("Variables con VIMP > 0" )
  })
  output$titulin4=renderText({
    paste("Clasificacion según profundidad mínima" )
  })
  output$titulin5=renderText({
    paste("Variables top según profundidad mínima" )
  })
  output$titulin6=renderText({
    paste("Comparando VIMP y Profundidad mínima" )
  })
  output$titulin7=renderText({
    paste("Selección de  modelos con menor error" )
  })
  output$titulin8=renderText({
    paste("Modelo con menor error de predicción" )
  })
  
  mod1<-reactive({ 
    set.seed(123)
    if(input$meur=="DOORMAT1"){
      mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_door1,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
    }
    else{
      if(input$meur=="DOORMAT2"){
        mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_door2,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
      }
      else{
        if(input$meur=="DOORMAT3"){
          mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_door3,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
        }
        else{
          if(input$meur=="Todos"){
            mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_door_todos,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
          }
          else{
            if(input$meur=="Espectro DOORMAT0"){
              mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_ED0,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
            }
          }}}}
  })
  output$mod_1<-renderPrint({
    set.seed(123)
    if(input$meur=="DOORMAT1"){
      mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_door1,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
    }
    else{
      if(input$meur=="DOORMAT2"){
        mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_door2,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
      }
      else{
        if(input$meur=="DOORMAT3"){
          mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_door3,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
        }
        else{
          if(input$meur=="Todos"){
            mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_door_todos,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
          }
          else{
            if(input$meur=="Espectro DOORMAT0"){
              mod1<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=RF_ED0,ntree = 500, block.size = 1, nodesize = 1, mtry=18 ,importance = T,na.action = c( "na.impute"))
            }
          }}}}
    mod1
  })
  output$textomod_D1=renderText({
    #if(input$meur=="DOORMAT1"){
      .n1<-dim(RF_door1)[1]
      .c1<-ncol(RF_door1)
      .m1<-table(RF_door1$Muerte_atribuible)[2]
      .ntree<-mod1()$ntree
      nvar<-data.frame(mod1()$xvar.names)
      nvar<-dim(nvar)[1]
      err<-get.cindex(mod1()$yvar[,1], mod1()$yvar[,2], mod1()$predicted.oob)
      err<-round(err,3)
      err<-err*100
      #t1<-paste("El modelo se ajusta con ",nvar, " Variables")
      #.cindex<-
      paste(h4(strong("Modelo inicial")),
            p(paste("El modelo se ajusta con ",.c1-2, "variables correspondientes a  ",.n1, "pacientes",
                    "de los que fallecen",.m1 )),
            #p(paste("Correspondientes a  ",.n1, "pacientes")),
            #p(paste("de los que fallecen",.m1 )),
            p(paste("Se cultivan ", .ntree, "arboles")),
            p(paste("Se comete un error OOB de predicción del ",err,"%"))
            )
    #}
  })
  output$gvimp=renderPlot({ 
    plot(gg_vimp(mod1()))
})
  VIMP<-reactive({
    imperm<-as.data.frame(mod1()$importance)
    colnames(imperm)<-c("vimp")
    imperm$variable<-rownames(imperm)
    imperm = imperm [ , c(2,1)]
    VIMP<-arrange(imperm,desc(vimp))
    #VIMP<-dplyr::select(VIMP,vimp)
    
  })
  
  output$tvimp=renderTable({ 
  #imperm<-as.data.frame(mod1()$importance)
  #colnames(imperm)<-c("vimp")
  #imperm$variable<-rownames(imperm)
  #imperm = imperm [ , c(2,1)]
  #imperm<-arrange(imperm,desc(vimp))
  imperm<-dplyr::filter(VIMP(),vimp>0)
  imperm
  })
  pmin<-reactive({
    pmin<-var.select(mod1(), conservative = "medium")
  })
  output$gpm=renderPlot({ 
    gg_pmin<-gg_minimal_depth(pmin())
    plot(gg_pmin)
  })
  output$tpm=renderPrint({ 
    gg_pmin<-gg_minimal_depth(pmin())
    print(gg_pmin)
  })
  output$comp=renderPlot({ 
    gg_pmin<-gg_minimal_depth(pmin())
  plot (gg_minimal_vimp (gg_pmin))+ 
    theme (legend.position = c (0.8, 0.2)) 
  })
  output$mm=renderTable({ 
    VIMP10<-dplyr::slice(VIMP(),1:10)
    b_datos = dplyr::select(dataframe(),tiempo,Muerte_atribuible,row.names(VIMP10))
    ccc2<-combinar(b_datos,c(3:12),2)
    ccc3<-combinar(b_datos,c(3:12),3)
    ccc4<-combinar(b_datos,c(3:12),4)
    ccc5<-combinar(b_datos,c(3:12),5)
    ccc6<-combinar(b_datos,c(3:12),6)
    
    filas2<-nrow(ccc2)
    materr2<-matrix(nrow=filas2,ncol=2)
    colnames(materr2)<-c( "Variables", "Error")
    rownames(materr2)<-paste("Modelo",c(1:filas2))
    
    filas3<-nrow(ccc3)
    materr3<-matrix(nrow=filas3,ncol=2)
    colnames(materr3)<-c( "Variables", "Error")
    rownames(materr3)<-paste("Modelo",c(1:filas3))
    
    filas4<-nrow(ccc4)
    materr4<-matrix(nrow=filas4,ncol=2)
    colnames(materr4)<-c( "Variables", "Error")
    rownames(materr4)<-paste("Modelo",c(1:filas4))
    
    filas5<-nrow(ccc5)
    materr5<-matrix(nrow=filas5,ncol=2)
    colnames(materr5)<-c( "Variables", "Error")
    rownames(materr5)<-paste("Modelo",c(1:filas5))
    
    filas6<-nrow(ccc6)
    materr6<-matrix(nrow=filas6,ncol=2)
    colnames(materr6)<-c( "Variables", "Error")
    rownames(materr6)<-paste("Modelo",c(1:filas6))
    
    for (i in 1:nrow(ccc2)){
      xnam <- as.matrix(ccc2[i,])
      fmla <- as.formula(paste("Surv(",colnames(b_datos[1]),",",colnames(b_datos[2]),") ~", paste(xnam, collapse= "+")))
      
      mod <- rfsrc(fmla, data = b_datos,ntree = 500)
      df <- mod$data
      prob = predict(mod,type = c("response"))
      df$prob = prob
      
      fmla<-as.character(fmla)
      
      materr2[i,1]<-fmla[3]
      materr2[i,2]<-get.cindex(mod$yvar[,1], mod$yvar[,2], mod$predicted.oob)
    }
    
    for (i in 1:nrow(ccc3)){
      xnam <- as.matrix(ccc3[i,])
      fmla <- as.formula(paste("Surv(",colnames(b_datos[1]),",",colnames(b_datos[2]),") ~", paste(xnam, collapse= "+")))
      
      mod <- rfsrc(fmla, data = b_datos,ntree = 500)
      df <- mod$data
      prob = predict(mod,type = c("response"))
      df$prob = prob
      
      fmla<-as.character(fmla)
      
      materr3[i,1]<-fmla[3]
      materr3[i,2]<-get.cindex(mod$yvar[,1], mod$yvar[,2], mod$predicted.oob)
    }
    
    #return(materr3)
    #d1<-Mconf(b_datos,ccc1)
    for (i in 1:nrow(ccc4)){
      xnam <- as.matrix(ccc4[i,])
      fmla <- as.formula(paste("Surv(",colnames(b_datos[1]),",",colnames(b_datos[2]),") ~", paste(xnam, collapse= "+")))
      
      mod <- rfsrc(fmla, data = b_datos,ntree = 500)
      df <- mod$data
      prob = predict(mod,type = c("response"))
      df$prob = prob
      
      fmla<-as.character(fmla)
      
      materr4[i,1]<-fmla[3]
      materr4[i,2]<-get.cindex(mod$yvar[,1], mod$yvar[,2], mod$predicted.oob)
    }
    #return(materr4)
    
    for (i in 1:nrow(ccc5)){
      xnam <- as.matrix(ccc5[i,])
      fmla <- as.formula(paste("Surv(",colnames(b_datos[1]),",",colnames(b_datos[2]),") ~", paste(xnam, collapse= "+")))
      
      mod <- rfsrc(fmla, data = b_datos,ntree = 500)
      df <- mod$data
      prob = predict(mod,type = c("response"))
      df$prob = prob
      
      fmla<-as.character(fmla)
      
      materr5[i,1]<-fmla[3]
      materr5[i,2]<-get.cindex(mod$yvar[,1], mod$yvar[,2], mod$predicted.oob)
    }
    #return(materr5)
    
    for (i in 1:nrow(ccc6)){
      xnam <- as.matrix(ccc6[i,])
      fmla <- as.formula(paste("Surv(",colnames(b_datos[1]),",",colnames(b_datos[2]),") ~", paste(xnam, collapse= "+")))
      
      mod <- rfsrc(fmla, data = b_datos,ntree = 500)
      df <- mod$data
      prob = predict(mod,type = c("response"))
      df$prob = prob
      
      fmla<-as.character(fmla)
      
      materr6[i,1]<-fmla[3]
      materr6[i,2]<-get.cindex(mod$yvar[,1], mod$yvar[,2], mod$predicted.oob)
    }
    dfinal<-rbind(  materr2, materr3, materr4, materr5, materr6)
    dfinal<-as.data.frame(dfinal)
    dfinal<-arrange(dfinal,Error)
    dfinalr<-slice(dfinal,1:10)
    
    dfinalr
  })
  output$mmm=renderPrint({ 
  })
  
  mf1<-reactive({
    fmla <- as.formula(paste("Surv( tiempo,Muerte_atribuible) ~", paste(input$var,collapse = "+")))
    set.seed(123)
    mf1 <- rfsrc(fmla, data =dataframe(),ntree = 500)
    mf1
  })
  output$modelomio<-renderPrint({
    mf1()
  })
  output$textomodelofin1<-renderPrint({
    paste("base de datos de meningitis")
  }) 
output$textomodelofin2<-renderPrint({ 
  nvar<-data.frame(mf1()$xvar.names)
  nvar<-dim(nvar)[1]
    paste("Modelo con ",nvar, "variables")
}) 
output$textomodelofin3<-renderPrint({ 
  err<-get.cindex(mf1()$yvar[,1], mf1()$yvar[,2], mf1()$predicted.oob)
  err<-round(err,3)
  err<-err*100
    paste("Error del ",err,"%")
  })
output$var<-renderUI({
  mod2<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=dataframe(),ntree =500, block.size = 1,nodesize = 4,mtry = 10,
              importance = T,na.action = c( "na.impute"))
  checkboxGroupInput("var","Seleccionar las variables pra construir el modelo",
                     choices = mod2$xvar.names)
})
output$var1<-renderUI({
  nvar<-data.frame(mf1()$xvar.names)
  nvar<-dim(nvar)[1]
  paste(nvar,"variables en el modelo")
})
output$var2<-renderUI({
  #lv<-as.data.frame(mf1()$xvar.names)
          #colnames(lv)<-c("Variables:")
          #lv
          radioButtons("var2","Seleccionar variable para gráfico parcial",
                             choices = mf1()$xvar.names)
})
output$tipo<-renderUI({
  radioButtons("tipo","Seleccionar resultado ",
               choices = c("Superviencia en un tiempo","Mortalidad"))
})
output$parcial<-renderPlot({
  if(input$tipo=="Superviencia en un tiempo"){
  plot.variable(mf1(), partial=T,plots.per.page = 1, xvar.names = c(input$var2),
                notch=FALSE,
                surv.type = "surv",
                time=input$tiempos)}
  else{
    if(input$tipo=="Mortalidad"){
      plot.variable(mf1(), partial=T,plots.per.page = 1, xvar.names = c(input$var2),
                    notch=FALSE,
                    surv.type = "mort")
                    }
  }
  
})
output$textgraf<-renderText({
  paste("Gráfico de efectos parciales. Variable ",input$var2)
})
})