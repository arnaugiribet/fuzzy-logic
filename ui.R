#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  #TITLE
  
  #tags$style("body {background-color: black;}"),
  
  
  h2("Fuzzy Tennis"),

  hr(),
  
  #TABLE OF STORED MATCHES
  uiOutput("selectTournament"),
  
  DT:: dataTableOutput('fuzzyMatches'),
  
  br(),
  
  #ACTIONS
  fluidRow(column(2,actionButton(inputId = "new.match", label = "Add New Match", icon = icon("plus"))),
           column(2,downloadButton("download.matches", "Download"))),
  
  ###############
  #ADD MATCH DATA
  ###############
  
  conditionalPanel(condition = "output.showaddmatchdata", hr()),
  conditionalPanel(condition = "output.showaddmatchdata", 
                   fluidRow(column(5, br()),
                            column(3, h4("Add New Match")))),
  br(),
  #match data
  conditionalPanel(condition = "output.showaddmatchdata", fluidRow(
    column(6,uiOutput("selectizeTournament")),
    column(6,uiOutput("selectdate"))
  )),
  #players data
  #player 1 panel
  conditionalPanel(condition = "output.showaddmatchdata", wellPanel(fluidRow(
    
    column(2,uiOutput("selectizePlayer1")),
    column(2,uiOutput("Technique1")),
    column(2,uiOutput("Physique1")),
    column(2,uiOutput("Psicology1")),
    column(2,uiOutput("Regularity1")),
    column(2,uiOutput("Experience1")),
    column(2,uiOutput("Implication1")),
    column(2,uiOutput("Surface1")),
    column(2,uiOutput("Rest1")),
    column(2,uiOutput("Confidence1")),
    column(2,uiOutput("H2H1"))
  ))),
  #player 2 panel
  conditionalPanel(condition = "output.showaddmatchdata", wellPanel(fluidRow(
    
    column(2,uiOutput("selectizePlayer2")),
    column(2,uiOutput("Technique2")),
    column(2,uiOutput("Physique2")),
    column(2,uiOutput("Psicology2")),
    column(2,uiOutput("Regularity2")),
    column(2,uiOutput("Experience2")),
    column(2,uiOutput("Implication2")),
    column(2,uiOutput("Surface2")),
    column(2,uiOutput("Rest2")),
    column(2,uiOutput("Confidence2"))
    
  ))),

  #results and bookie
  
  conditionalPanel(condition = "output.showaddmatchdata", 
                   wellPanel(fluidRow(
                     column(2,uiOutput("Status")),
                     column(4,conditionalPanel(condition = "output.played", 
                                               wellPanel(fluidRow(
                                                 column(12,uiOutput("Winner")
                                                        )
                                                 )
                                                 )
                                               )
                            ),
                     column(6,conditionalPanel(condition = "output.finished", 
                                               wellPanel(fluidRow(
                                                 column(6,uiOutput("NSets")),
                                                 column(6,uiOutput("GamesDif"))))))
                     ))),
  conditionalPanel(condition = "output.showaddmatchdata", 
                   wellPanel(fluidRow(
                       column(2,br()),
                       column(4,uiOutput("Bookie1")),
                       column(4,uiOutput("Bookie2"))
                       ),
                     fluidRow(
                       column(5,br()),
                       column(3,h4('Handicaps'))
                     ),
                     fluidRow(
                       column(5,br()),
                       column(3,uiOutput("HAP_Highest"))
                     ),
                     fluidRow(
                       column(2,br()),
                       column(4,uiOutput("HAP1_4")),
                       column(4,uiOutput("HAP2_4"))
                     ))),
  conditionalPanel(condition = "output.showaddmatchdata", 
                   wellPanel(
                     
                     fluidRow(
                       column(2,br()),
                       column(4,uiOutput("HAP1_1")),
                       column(4,uiOutput("HAP2_1"))
                     ),
                     fluidRow(
                       column(2,br()),
                       column(4,uiOutput("HAP1_2")),
                       column(4,uiOutput("HAP2_2"))
                     ),
                     fluidRow(
                       column(2,br()),
                       column(4,uiOutput("HAP1_3")),
                       column(4,uiOutput("HAP2_3"))
                     ),
                     fluidRow(
                       column(2,br()),
                       column(4,uiOutput("HAP1_5")),
                       column(4,uiOutput("HAP2_5"))
                     ),
                     fluidRow(
                       column(2,br()),
                       column(4,uiOutput("HAP1_6")),
                       column(4,uiOutput("HAP2_6"))
                     ),
                     fluidRow(
                       column(2,br()),
                       column(4,uiOutput("HAP1_7")),
                       column(4,uiOutput("HAP2_7"))
                     )
                     )),
  #save
  
  conditionalPanel(condition = "output.showaddmatchdata", fluidRow(
    column(5,br()),
    column(3,actionButton(inputId = "save.match",
                          label = "Save New Match",
                          icon = icon("save")))
  )),
  
  ##################
  #SHOW DATA
  ##################
  
  hr(),
      #titol
  conditionalPanel(condition = "output.showselecteddata", 
                   fluidRow(column(5, br()),
                            column(3, h4("Match Data")))),
  br(),
  
      #taula de partits
  conditionalPanel(condition = "output.showselecteddata", DT:: dataTableOutput('matchdata')),
  conditionalPanel(condition = "output.showselecteddata", br()),
  
      #taula jugadors
  conditionalPanel(condition = "output.showselecteddata", 
                   fluidRow(column(2, DT:: dataTableOutput('fuzzyselecteddataId')),
                            column(10, DT:: dataTableOutput('fuzzyselecteddataFeatures')))),
  conditionalPanel(condition = "output.showselecteddata", br()),
  
      #taula handicaps
  conditionalPanel(condition = "output.showselecteddata", 
                   fluidRow(column(3, br()),
                            column(6, DT:: dataTableOutput('oddsBet')))),
  conditionalPanel(condition = "output.showselecteddata", br()),

                            #,column(10, DT:: dataTableOutput('fuzzyselecteddataFeatures'))
  
  conditionalPanel(condition = "output.showselecteddata", fluidRow(
    column(2,actionButton(inputId = "editdata",label = "Edit Data",icon = icon("user-edit"))),
    column(2,actionButton(inputId = "delete.match", label = "Delete Match", icon = icon("trash"))),
    column(2,downloadButton("download.players", "Download"))
  )),
  conditionalPanel(condition = "output.showselecteddata", hr()),
  
  ##################
  #EDIT DATA
  ##################

  conditionalPanel(condition = "output.editdata",
                   fluidRow(column(5, br()),
                            column(3, h4("Edit Match")))),
  br(),

  #match data
  conditionalPanel(condition = "output.editdata", fluidRow(
    column(6,uiOutput("MODselectizeTournament")),
    column(6,uiOutput("MODselectdate"))
  )),
  #players data
  #player 1 panel
  conditionalPanel(condition = "output.editdata", wellPanel(fluidRow(
    
    column(2,uiOutput("MODselectizePlayer1")),
    column(2,uiOutput("MODTechnique1")),
    column(2,uiOutput("MODPhysique1")),
    column(2,uiOutput("MODPsicology1")),
    column(2,uiOutput("MODRegularity1")),
    column(2,uiOutput("MODExperience1")),
    column(2,uiOutput("MODImplication1")),
    column(2,uiOutput("MODSurface1")),
    column(2,uiOutput("MODRest1")),
    column(2,uiOutput("MODConfidence1")),
    column(2,uiOutput("MODH2H1"))
  ))),
  #player 2 panel
  conditionalPanel(condition = "output.editdata", wellPanel(fluidRow(
    
    column(2,uiOutput("MODselectizePlayer2")),
    column(2,uiOutput("MODTechnique2")),
    column(2,uiOutput("MODPhysique2")),
    column(2,uiOutput("MODPsicology2")),
    column(2,uiOutput("MODRegularity2")),
    column(2,uiOutput("MODExperience2")),
    column(2,uiOutput("MODImplication2")),
    column(2,uiOutput("MODSurface2")),
    column(2,uiOutput("MODRest2")),
    column(2,uiOutput("MODConfidence2"))
    
  ))),
  
  #results
  
  conditionalPanel(condition = "output.editdata", wellPanel(fluidRow(
    
    column(2,uiOutput("MODStatus")),
    column(4,conditionalPanel(condition = "output.playedMOD", wellPanel(fluidRow(
      column(12,uiOutput("MODWinner")))))),
    column(6,conditionalPanel(condition = "output.finishedMOD", wellPanel(fluidRow(
      column(6,uiOutput("MODNSets")),
      column(6,uiOutput("MODGamesDif"))))))
  ))),
  
  #Bookmakers
  
  conditionalPanel(condition = "output.editdata", 
                   wellPanel(fluidRow(
                     column(2,br()),
                     column(4,uiOutput("MODBookie1")),
                     column(4,uiOutput("MODBookie2"))
                   ),
                   fluidRow(
                     column(5,br()),
                     column(3,h4('Handicaps'))
                   ),
                   fluidRow(
                     column(5,br()),
                     column(3,uiOutput("MODHAP_Highest"))
                   ),
                   fluidRow(
                     column(2,br()),
                     column(4,uiOutput("MODHAP1_4")),
                     column(4,uiOutput("MODHAP2_4"))
                   ))),
  conditionalPanel(condition = "output.editdata", 
                   wellPanel(fluidRow(
                     column(2,br()),
                     column(4,uiOutput("MODHAP1_1")),
                     column(4,uiOutput("MODHAP2_1"))
                   ),
                   fluidRow(
                     column(2,br()),
                     column(4,uiOutput("MODHAP1_2")),
                     column(4,uiOutput("MODHAP2_2"))
                   ),
                   fluidRow(
                     column(2,br()),
                     column(4,uiOutput("MODHAP1_3")),
                     column(4,uiOutput("MODHAP2_3"))
                   ),
                   fluidRow(
                     column(2,br()),
                     column(4,uiOutput("MODHAP1_5")),
                     column(4,uiOutput("MODHAP2_5"))
                   ),
                   fluidRow(
                     column(2,br()),
                     column(4,uiOutput("MODHAP1_6")),
                     column(4,uiOutput("MODHAP2_6"))
                   ),
                   fluidRow(
                     column(2,br()),
                     column(4,uiOutput("MODHAP1_7")),
                     column(4,uiOutput("MODHAP2_7"))
                   )
                   )),
  
  #save
  
  conditionalPanel(condition = "output.editdata", fluidRow(
    column(5,br()),
    column(3,actionButton(inputId = "save.edits",
                          label = "Save Changes",
                          icon = icon("save")))
  )),
  
  conditionalPanel(condition = "output.editdata", hr()),
  
  ##################
  #CLOSE APP
  ##################
  

  #Close app when clicking the close button
  useShinyjs(),
  fluidRow(column(5,br()),
           column(1,extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }", functions = c("closeWindow")),
                  actionButton("close", "Close App", icon=icon('window-close')))),
  br()
))
