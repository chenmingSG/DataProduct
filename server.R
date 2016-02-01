# library(UsingR)
library(AppliedPredictiveModeling)
library(caret)
library(car)
data(FuelEconomy)
cars2010 <- cars2010[order(cars2010$EngDispl),]
cars2011 <- cars2011[order(cars2011$EngDispl),]
carsTrain<-cars2010
carsTest<-cars2011
varNames<-colnames(carsTrain)
response<-"FE"
fit<-lm(FE~.,data=carsTrain)
IsFactor<-sapply(carsTrain, is.factor)
PredictorsFactor<-varNames[IsFactor]
PredictorsNum<-varNames[-match(c(response,PredictorsFactor),varNames)]
correlationMatrix <- cor(carsTrain[,PredictorsNum])
##print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75,exact=FALSE)
PredictorsNum<-PredictorsNum[-highlyCorrelated]
# predictors<-c(response,PredictorsNum,PredictorsFactor)
predictors<-c(response,PredictorsFactor,PredictorsNum)
carsTrain<-carsTrain[,predictors]
varNames<-colnames(carsTrain)
fit<-lm(FE~.,data=carsTrain)
Pvalue<-summary(fit)$coef[,"Pr(>|t|)"]
VarInsignif<-rownames(summary(fit)$coef)[Pvalue>0.01]
VarInsignifIndex<-match(VarInsignif,varNames)
VarInsignifIndex<-VarInsignifIndex[!is.na(VarInsignifIndex)]
varNamesNew<-varNames[-VarInsignifIndex]
carsTrainNew<-carsTrain[,varNamesNew]
# varNamesNew<-colnames(carsTrainNew)
IsFactor<-sapply(carsTrainNew, is.factor)
varNamesFix<-varNamesNew[!IsFactor]

PredictorsFactorSignificant<-sapply(carsTrainNew[,PredictorsFactor], is.factor)
PredictorsFactorSignificant<-!PredictorsFactorSignificant
carsTrainNewWithoutFactor<-carsTrainNew[,varNamesFix]
fitWithoutFactor<-lm(FE~.,data=carsTrainNewWithoutFactor)
L0<-logLik(fitWithoutFactor)
for (i in 1:length(PredictorsFactorSignificant)){
  ##likehod for standard model
  ##likelihood test for appended new model
  carsTrainNewWith<- carsTrainNew[,c(varNamesFix,PredictorsFactor[i])]
  fitWithFactori<-lm(FE~.,data=carsTrainNewWith)
  L1i<-logLik(fitWithFactori)
  Lambdai<-2*(L1i-L0)
  PValuei<-1-pchisq(Lambdai,df=(attr(L1i, "df")-1))
  ##PredictorsFactorSignificant[i]<-(PValuei<0.05) 
  ##varNamesFix<-c(varNamesFix, PredictorsFactor[PredictorsFactorSignificant])
  if(PValuei<0.05){
    L0<-L1i
    varNamesFix<-c(varNamesFix,PredictorsFactor[i])
    #varNamesFix<-c(PredictorsFactor[i],varNamesFix)
  }
}

##varNamesFix<-c(varNamesFix,PredictorsFactor[PredictorsFactorSignificant])
carsTrainNew<-carsTrain[,varNamesFix]
fitNew<-lm(FE~.,data=carsTrainNew)
vifV<-vif(fitNew)
sortindex<-match(vifV[,"GVIF"],sort(vifV[,"GVIF"]))
vifV<-vifV[sortindex,]
predictors<-rownames(vifV[vifV[,"GVIF"]<10,])
#browser(predictors)
carsTrain$VarValveLift<-factor(carsTrain$VarValveLift)
carsTrainFilter<-carsTrain[,c(response,predictors)]
# carsTrainFilter$VarValveLift<-factor(carsTrainFilter$VarValveLift)
IsFactor<-sapply(carsTrainFilter[,predictors], is.factor)
fitmodel<-lm(FE~.,data=carsTrainFilter)
FEPred<-predict(fitmodel)
plotnamelist<-c("Analysis","Prediction")



shinyServer(  
  function(input, output) {    
    # Drop-down selection box for which data set
    output$choose_predictor <- renderUI({
      selectInput("predictor", "Choose Predictor", as.list(predictors))
#       browser(predictorname)
    })

## re-model based on user input
#     NumofPredictors<-input$NumofPredictors
#     predictors<-predictors[1:NumofPredictors]
#     carsTrainFilter<-carsTrain[,c(response,predictors)]
#     fitmodel<-lm(FE~.,data=carsTrainFilter)
#     FEPred<-predict(fitmodel)

    output$Silders_Drops <- renderUI({
      if(is.null(input$NumofPredictors))
        return()
      NumofPredictors<-input$NumofPredictors
      predictors<-predictors[1:NumofPredictors]
      carsTrainFilter<-carsTrain[,c(response,predictors)]
      fitmodel<-lm(FE~.,data=carsTrainFilter)
      FEPred<-predict(fitmodel)
      #browser(predictors)
      #browser(carsTrainFilter)
      numIndividuals <- NumofPredictors
      
      if (numIndividuals>1){
        lapply(1:numIndividuals, function(i) {
          if (IsFactor[i]){
            #browser(carsTrainFilter)
            selectInput(predictors[i], paste("Input for Predictor ",predictors[i]), as.list(levels(carsTrainFilter[,predictors[i]])))
          } else {
            Predictori<-carsTrainFilter[,predictors[i]]
            sliderInput(inputId=predictors[i], label=paste("Input for Predictor ",predictors[i]),value = median(Predictori), min = min(Predictori), max = max(Predictori), step = (max(Predictori)-min(Predictori))/50)
          }
      })
      } else{
        i<-1
        if (IsFactor[i]){
          #browser(carsTrainFilter)
          selectInput(predictors[i], paste("Input for Predictor ",predictors[i]), as.list(levels(carsTrainFilter[,predictors[i]])))
        } else {
          Predictori<-carsTrainFilter[,predictors[i]]
          sliderInput(inputId=predictors[i], label=paste("Input for Predictor ",predictors[i]),value = median(Predictori), min = min(Predictori), max = max(Predictori), step = (max(Predictori)-min(Predictori))/50)
        }
      }
        
        
    #  })
    })
    

  # Insert the right number of plot output objects into the web page
  output$plots <- renderUI({
    plot_output_list <- lapply(1:2, function(i) {
      plotname <- plotnamelist[i]
      #browser(plotname)
      plotOutput(plotname, height = 380, width = 500)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })

  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in 1:2) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      #browser(my_i)
      #plotname <- paste("plot", my_i, sep="")
      plotname <- plotnamelist[my_i]
      
      output[[plotname]] <- renderPlot({
        #use [] instead of [[]]
        if (my_i==1){
          if(is.null(input$predictor))
            return()
          predictorname<-input$predictor
          #browser(predictorname)
          predictorSel<-carsTrainFilter[,input$predictor]
          plot(carsTrainFilter[,input$predictor],carsTrainFilter$FE,xlab=predictorname,ylab="Fuel Efficiency",
               main="Explatory Analysis of Impact of Predictors")
        } else {    
          NumofPredictors<-input$NumofPredictors
          predictors<-predictors[1:NumofPredictors]
          carsTrainFilter<-carsTrain[,c(response,predictors)]
          fitmodel<-lm(FE~.,data=carsTrainFilter)
          FEPred<-predict(fitmodel)
          
          
          
          xymin<-min(min(FEPred),min(carsTrainFilter$FE))
          xymax<-max(max(FEPred),max(carsTrainFilter$FE))
          
          plot(FEPred,carsTrainFilter$FE,xlab="Predicted Fuel Efficiency",ylab="Actual Fuel Efficiency"
               ,main="Modeling and Prediction",xlim=c(xymin,xymax),ylim=c(xymin,xymax))
          
          PredictorsUser<-data.frame(carsTrainFilter[1,predictors])
          colnames(PredictorsUser)<-predictors
          #browser(PredictorsUser)
          for (i in 1:NumofPredictors){
            if(is.null(input[[predictors[i]]]))
              return()
            #browser(input)
            if (NumofPredictors>1){
              PredictorsUser[1,i]<-input[[predictors[i]]]
            }else{
              PredictorsUser[i]<-as.factor(input[[predictors[i]]])
            }
            
            
          }
          
          PredictedValue<-predict(fitmodel,newdata=PredictorsUser)
          #browser(PredictedValue)

          #browser(PredictorsUser)
          points(PredictedValue,PredictedValue,col="red")
          abline(h = PredictedValue, v = PredictedValue, col = "gray60")
          text(20, 65, paste("Predicted FE = ", round(PredictedValue,1))) 
        }     
        
      })
    })
  }


  
  }
)