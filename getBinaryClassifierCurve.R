
getBinaryClassifierCurve<- function(actuals, probs, tick = 1000, type ="ROC", plot=F){
  #produces ROC curve or precision recall call
  #accepst type: ROC, PR
  
  
  #takes two arrays, a control and an actual... 
  step = 1/tick
  rtVal1<-array()
  rtVal2<-array()
  
  
  #create a dataframe from the actuals
  df<- data.frame(actuals=actuals, probs=probs)
  
  counter = 1
  
  
  for (i in 1:tick+1){
    decisionBoundary<-(i-1)*step
    print(decisionBoundary)
    df["predictions"]=0
    df[df$probs>decisionBoundary, "predictions"]=1
    
    #don't use table function here.... much easier to do manually as table doesn't have fixed dims... 
    TP = nrow(df[df["predictions"]==1 & df["actuals"]==1,] )
    FP = nrow(df[df["predictions"]==1 & df["actuals"]==0,] )
    TN = nrow(df[df["predictions"]==0 & df["actuals"]==0,] )
    FN = nrow(df[df["predictions"]==0 & df["actuals"]==1,] )
    
    
    if (type=="ROC"){
      rtVal1[counter]= TP/(TP+FN)
      rtVal2[counter]= FP/(TN+FP)
    }else if (type == "PR"){
      rtVal1[counter]= TP/(TP+FP)
      rtVal2[counter]= TP/(FN+TP)
    }else{
      print("error: unspecified type")
      return()
    }
    
    
    counter=counter+1
  }
  
  if (plot==T){
    plot(rtVal1, rtVal2)
  }
  
  
  list (v1=rtVal1, v2=rtVal2)
}





