library(shiny)

shinyUI(fluidPage(
  titlePanel("Mortality in LA following Breast Cancer Diagnosis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Display survival probability curves based on user input."),
      selectInput("spt",label="Parish of Residence",
                  choices=sort(c("Calcasieu","Union","Tangipahoa","Caldwell","Tensas","Jackson",
                               "Grant","Lincoln","Jefferson Davis","Lafayette","Vermilion",
                               "East Carroll","East Feliciana","St. Bernard","Iberville","Richland",
                               "St. Martin","Claiborne","Evangeline","St. Landry","Pointe Coupee",
                               "LaSalle","Webster","St. James","Plaquemines","Morehouse","Rapides",
                               "Avoyelles","Winn","Vernon","Catahoula","Assumption","De Soto",
                               "Caddo","Red River","Washington","Sabine","Jefferson","St. Tammany",
                               "Cameron","East Baton Rouge","Iberia","Natchitoches","Terrebonne",
                               "Bienville","Bossier","Allen","Ouachita","St. John the Baptist",
                               "St. Helena","West Feliciana","St. Mary","Lafourche","West Carroll",
                               "Concordia","Livingston","West Baton Rouge","Madison","Orleans",
                               "Ascension","Acadia","St. Charles","Beauregard","Franklin"))),
      numericInput("agedx",label="Enter your age at diagnosis",value=50),
      checkboxInput("race",label="Select if you are of African American descent.",value=FALSE),
      checkboxInput("grade",
                    label="Select if your cancer was considered high grade",
                    value=FALSE),
      selectInput("surg",label="Was surgery performed?",
                  choices=list("Yes","No","Unknown"),selected="Yes"),
      selectInput("rad",label="Was radiation therapy performed?",
                   choices=list("Yes","No","Unknown")),
      selectInput("married",label="Marital Status at Diagnosis",
                  choices=list("Single","Currently Married","Previously Married","Other"),selected="Single"),
      helpText("Note: 'currently married' includes life or domestic partners."),
      selectInput("erpr",label="Estrogen recpetor (ER)/Progesteron recpetor (PR) test result",
                   choices=list("+/+","+/-","-/+","-/-","Unknown")),
      helpText("Select 'unknown' if either test result is unknown"),
      submitButton("Submit")
    ),
    mainPanel(
      textOutput("text"),
      plotOutput("survplot"),
      tableOutput("tab")
     )
  )
))