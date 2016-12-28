library(shiny)
t=c(1.01,2:160)
modelAFT=list(summary=1)
modelAFT$summary=read.csv("data\\modelAFTSPTuncorPRED3brEDU.csv")[,-1]
u=modelAFT$summary[3:66,1]
map=read.csv("data\\LAcountyORDER.csv")

shinyServer(function(input,output){
  output$text <- renderText({
    paste("The plot below displays the survival probability curve for breast cancer related mortality 
          following breast cancer diagnosis for a ",input$agedx," year old ",
          ifelse(input$race==TRUE,"African American","non-African American"),
          " woman that resided in ",input$spt, " Parish",
          ifelse(input$married=="Single"," and was single",
          ifelse(input$married=="Currently Married"," and was currently married",
          ifelse(input$married=="Previously Married"," and was previously married",""))),
          " at the time of diagnosis. Her cancer was considered ",
          ifelse(input$grade==TRUE,"high",
            "low"),"grade, ",
          ifelse(input$er=="Positive","ER positive,",
          ifelse(input$er=="Unknown","ER status unkown,","ER negative/borderline,"))," ",
          ifelse(input$pr=="Positive","PR positive,",
          ifelse(input$pr=="Unknown","PR status unkown,","PR negative/borderline,")),
          " and ",
          ifelse(input$surg=="Yes","surgery was performed.",ifelse(input$surg=="No",
          "surgery was not performed.","it is unknown if surgery was performed.")))
  })
  output$survplot <- renderPlot({
    sptnum=map$order[which(map$NAME==input$spt)]
    edu=map$EDU[which(map$NAME==input$spt)]
        survprob=1/(1+(exp(-(modelAFT$summary[68,1]+
            modelAFT$summary[69,1]*(input$agedx-61.45055)/13.6418+
            ifelse(input$race==TRUE,modelAFT$summary[70,1],0)+
            ifelse(input$grade==TRUE,modelAFT$summary[71,1],0)+
            ifelse(input$surg=="Yes",modelAFT$summary[72,1],
                   ifelse(input$surg=="No",0,modelAFT$summary[73,1]))+
            ifelse(input$married=="Single",0,
                   ifelse(input$married=="Currently Married",modelAFT$summary[74,1],
                   ifelse(input$married=="Previously Married",
                          modelAFT$summary[75,1],modelAFT$summary[76,1])))+
            ifelse(input$er=="Positive",0,ifelse(input$er=="Unknown",
                          modelAFT$summary[78,1],modelAFT$summary[77,1]))+
            ifelse(input$pr=="Positive",0,ifelse(input$pr=="Unknown",
                          modelAFT$summary[80,1],modelAFT$summary[79,1]))+
            modelAFT$summary[81,1]*(edu-21.17882)/7.224876+
            u[sptnum]))*t)^(1/modelAFT$summary[2,1]))
    plot(survprob~t,ylab="Survival Probability",type="l",xlab="Months",
         ylim=c(0,1),main="")
  })
  output$tab <- renderTable({
    sptnum=map$order[which(map$NAME==input$spt)]
    edu=map$EDU[which(map$NAME==input$spt)]
    survprob=1/(1+(exp(-(modelAFT$summary[68,1]+
            modelAFT$summary[69,1]*(input$agedx-61.45055)/13.6418+
            ifelse(input$race==TRUE,modelAFT$summary[70,1],0)+
            ifelse(input$grade==TRUE,modelAFT$summary[71,1],0)+
            ifelse(input$surg=="Yes",modelAFT$summary[72,1],
                   ifelse(input$surg=="No",0,modelAFT$summary[73,1]))+
            ifelse(input$married=="Single",0,
                   ifelse(input$married=="Currently Married",modelAFT$summary[74,1],
                   ifelse(input$married=="Previously Married",
                          modelAFT$summary[75,1],modelAFT$summary[76,1])))+
            ifelse(input$er=="Positive",0,ifelse(input$er=="Unknown",
                          modelAFT$summary[78,1],modelAFT$summary[77,1]))+
            ifelse(input$pr=="Positive",0,ifelse(input$pr=="Unknown",
                          modelAFT$summary[80,1],modelAFT$summary[79,1]))+
              modelAFT$summary[81,1]*(edu-21.17882)/7.224876+
              u[sptnum]))*c(12,24,60))^(1/modelAFT$summary[2,1]))
    data <- matrix(survprob,nrow=1,ncol=3,dimnames=list(list(),list("Survival at 1 year",
                      "Survival at 2 years","Survival at 5 years")))
  })
})