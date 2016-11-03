library(shiny)

shinyUI(fluidPage(
  titlePanel("Mortality in LA following Breast Cancer Diagnosis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Display survival probability curves based on user input."),
      selectInput("spt",label="Parish of Residence",
                  choices=list("Calcasieu","Union","Tangipahoa","Caldwell","Tensas","Jackson",
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
                               "Ascension","Acadia","St. Charles","Beauregard","Franklin")),
      numericInput("agedx",label="Enter your age at diagnosis",value=50),
      checkboxInput("race",label="Select if you are of African American descent.",value=FALSE),
      checkboxInput("grade",
                    label="Select if your cancer was considered well or moderately defined",
                    value=FALSE),
      selectInput("surg",label="Was surgery performed?",
                  choices=list("Yes","No","Unknown"),selected="Yes"),
      selectInput("married",label="Marital Status at Diagnosis",
                  choices=list("Single","Married","separated","Divorced","Other"),selected="Single"),
      helpText("Note: married includes life or domestic partners."),
      radioButtons("er",label="Estrogen recpetor (ER) test result",
                   choices=list("Positive","Negative/Borderline","Unknown")),
      radioButtons("pr",label="Progesteron recpetor (PR) test result",
                   choices=list("Positive","Negative/Borderline","Unknown")),
      submitButton("Submit")
    ),
    mainPanel(
      textOutput("text"),
      plotOutput("survplot"),
      tableOutput("tab")
     )
  )
))