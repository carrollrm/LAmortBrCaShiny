library(shiny)
t=c(1.01,2:160)
modelAFT=list(summary=1)
modelAFT$summary=read.csv("data\\modelAFTM3Uerprrad2.csv")[,-1]
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
          ifelse(input$grade==TRUE,"high","low"),
          "grade with ",
          ifelse(input$erpr=="+/+","+/+",
          ifelse(input$erpr=="+/-","+/-",
          ifelse(input$erpr=="-/+","-/+",
          ifelse(input$erpr=="-/-","-/-","unknown")))),
          " ER/PR status. For treatment, ",
          ifelse(input$rad=="Yes","radiation therapy was performed",
                 ifelse(input$rad=="No","radiation therapy was performed",
                        "it is unknown if radiation therapy was performed")),
          " and ",
          ifelse(input$surg=="Yes","surgery was performed.",
                 ifelse(input$surg=="No","surgery was not performed.",
                        "it is unknown if surgery was performed.")))
  })
  output$survplot <- renderPlot({
    sptnum=map$order[which(map$NAME==input$spt)]
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
            ifelse(input$erpr=="+/+",0,
                   ifelse(input$erpr=="+/-",modelAFT$summary[78,1],
                   ifelse(input$erpr=="-/+",modelAFT$summary[77,1],
                   ifelse(input$erpr=="-/-",modelAFT$summary[79,1],modelAFT$summary[80,1]))))+
            ifelse(input$rad=="Yes",modelAFT$summary[81,1],
                   ifelse(input$rad=="No",0,modelAFT$summary[82,1]))+
            u[sptnum]))*t)^(1/modelAFT$summary[2,1]))
    plot(survprob~t,ylab="Survival Probability",type="l",xlab="Months",
         ylim=c(0,1),main="")
  })
  output$tab <- renderTable({
    sptnum=map$order[which(map$NAME==input$spt)]
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
            ifelse(input$erpr=="+/+",0,
                   ifelse(input$erpr=="+/-",modelAFT$summary[78,1],
                   ifelse(input$erpr=="-/+",modelAFT$summary[77,1],
                   ifelse(input$erpr=="-/-",modelAFT$summary[79,1],modelAFT$summary[80,1]))))+
            ifelse(input$rad=="Yes",modelAFT$summary[81,1],
                   ifelse(input$rad=="No",0,modelAFT$summary[82,1]))+
            u[sptnum]))*c(12,24,60))^(1/modelAFT$summary[2,1]))
    data <- matrix(survprob,nrow=1,ncol=3,dimnames=list(list(),list("Survival Probabilty at 1 year",
                      "Survival Probability at 2 years","Survival Probability at 5 years")))
  })
})