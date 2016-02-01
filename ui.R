## use fluid layout
shinyUI(fluidPage(  
  titlePanel("Car Fuel Efficiency Explanatory Study, Modeling and Prediction"), 
  fluidRow(
    column(12,
           includeHTML("Readme.html")
      )
    ),
  sidebarLayout(
    sidebarPanel(
      h2("Below Drop Down is for Explanatory Analysis"),
      uiOutput("choose_predictor"),
      h2("Below Knobs are for Fuel Effciency Prediction"),
      sliderInput("NumofPredictors","Number of Predictors Used",value=3,min=1,max=4,step=1,),
#       sliderInput("test","test",value=1,min=1,max=3,step=1,),
      uiOutput("Silders_Drops")
    ),
    mainPanel(    
      uiOutput('plots')
    )
    
    )

))