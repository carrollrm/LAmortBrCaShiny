library(shiny)
t=1:160
modelAFT=list(summary=1)
modelAFT$summary=read.csv("data\\modelAFTSPTuncorPRED3br.csv")[,-1]
u=modelAFT$summary[3:66,1]
v=modelAFT$summary[68:131,1]
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
          " at the time of diagnosis. Her cancer was ",
          ifelse(input$grade==TRUE,"not",
            ""),"considered well or moderately defined, ",
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
    survprob=1/(1+exp((log(t)+0.01)-
            (modelAFT$summary[133,1]+
            modelAFT$summary[134,1]*(input$agedx-61.41453)/13.62831+
            ifelse(input$race==TRUE,modelAFT$summary[135,1],0)+
            ifelse(input$grade==TRUE,modelAFT$summary[136,1],0)+
            ifelse(input$surg=="Yes",modelAFT$summary[137,1],
                   ifelse(input$surg=="No",0,modelAFT$summary[138,1]))+
            ifelse(input$married=="Single",0,
                   ifelse(input$married=="Currently Married",modelAFT$summary[139,1],
                   ifelse(input$married=="Previously Married",
                          modelAFT$summary[140,1],modelAFT$summary[141,1])))+
            ifelse(input$er=="Positive",0,ifelse(input$er=="Unknown",
                          modelAFT$summary[143,1],modelAFT$summary[142,1]))+
            ifelse(input$pr=="Positive",0,ifelse(input$pr=="Unknown",
                          modelAFT$summary[145,1],modelAFT$summary[144,1]))+
            u[sptnum]+v[sptnum])/modelAFT$summary[2,1]))
    plot(survprob~t,ylab="Survival Probability",type="l",xlab="Months",
         ylim=c(0,1),main="")
  })
  output$tab <- renderTable({
    sptnum=map$order[which(map$NAME==input$spt)]
    survprob=1/(1+exp((log(c(12,24,60))+0.01)-
            (modelAFT$summary[133,1]+
                 modelAFT$summary[134,1]*(input$agedx-61.41453)/13.62831+
                 ifelse(input$race==TRUE,modelAFT$summary[135,1],0)+
                 ifelse(input$grade==TRUE,modelAFT$summary[136,1],0)+
                 ifelse(input$surg=="Yes",modelAFT$summary[137,1],
                    ifelse(input$surg=="No",0,modelAFT$summary[138,1]))+
                 ifelse(input$married=="Single",0,
                    ifelse(input$married=="Married",modelAFT$summary[139,1],
                        ifelse(input$married=="Separated"|input$married=="Divorced",
                            modelAFT$summary[140,1],modelAFT$summary[141,1])))+
                 ifelse(input$er=="Positive",0,ifelse(input$er=="Unknown",
                        modelAFT$summary[143,1],modelAFT$summary[142,1]))+
                 ifelse(input$pr=="Positive",0,ifelse(input$pr=="Unknown",
                        modelAFT$summary[145,1],modelAFT$summary[144,1]))+
                 u[sptnum]+v[sptnum])/modelAFT$summary[2,1]))
    data <- matrix(survprob,nrow=1,ncol=3,dimnames=list(list(),list("Survival at 1 year",
                      "Survival at 2 years","Survival at 5 years")))
  })
})