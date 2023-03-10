#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

shinyServer(function(input, output) {
  
  library(dplyr)
  library(rdrop2)
  library(rsconnect)
  library(shiny)
  library(ggplot2)
  library(DT)

  drop_auth(rdstoken = "droptoken.rds")
  
  loadDropbox<-source('functions/loaddropbox.R')$value
  saveDropbox<-source('functions/savedropbox.R',local=T)$value
  selectedData_Id<-source("functions/selecteddataid.R",local=T)$value
  selectedData_Features<-source("functions/selecteddatafeatures.R",local=T)$value
  fuzzyLogic<-source("functions/fuzzylogic.R",local=T)$value
  fuzzyLogic2<-source("functions/fuzzylogic2.R",local=T)$value
  getProbs<-source("functions/getprobs.R",local=T)$value
  initialValues<-source("functions/initialValues.R",local=T)$value
  getPlayersNames<-source("functions/getPlayersNames.R",local=T)$value
  saveResults<-source("functions/saveResults.R",local=T)$value
  mergeMatches<-source('functions/mergeMatches.R',local=T)$value
  selectTournament<-source('functions/selectTournament.R',local=T)$value
  whichTournament<-source('functions/whichTournament.R',local=T)$value
  previousValues<-source('functions/previousValues.R',local=T)$value
  matchData<-source('functions/matchData.R',local=T)$value
  handicapVector<-source('functions/handicapVector.R',local=T)$value
  handicapShow<-source('functions/handicapShow.R',local=T)$value
  hideColumns<-source('functions/hideColumns.R',local=T)$value
  fuzzyProbs<-source('functions/fuzzyProbs.R')$value
  
  rheight20<-JS("function(r,d) {$(r).attr('height', '40px')}")
  
  dataDropbox<-loadDropbox()
  
  values <- reactiveValues()
  values$fuzzyMatches <- dataDropbox[[1]]
  values$fuzzyData <- dataDropbox[[2]]
  values$fuzzyProbs <- dataDropbox[[3]]
  values$Results <- dataDropbox[[4]]
  values$bookies <- dataDropbox[[5]]
  values$MatchesResults <- mergeMatches(dataDropbox[[1]],dataDropbox[[4]],dataDropbox[[3]],dataDropbox[[2]])
  
  rm(dataDropbox)
  
  #output fuzzy matches

  output$selectTournament <- renderUI({
      selectInput("TournamentSel", "Tournament",
                  whichTournament(values$fuzzyMatches))
  })    
  
  
  output$fuzzyMatches <- 
    DT::renderDataTable(DT::datatable(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                             input$TournamentSel),],
                                      selection = 'single',
                                      rownames = FALSE,
                                      options=list(columnDefs = list(list(visible=FALSE, 
                                                                          targets=hideColumns(values$MatchesResults))),
                                                   scrollX = TRUE)) 
                        %>%
                          formatStyle(c('Player1'), 'WonPlayer1',backgroundColor= styleEqual(c(1), c('#E8EAF6')))
                        %>%
                          formatStyle(c('Player2'), 'WonPlayer1',backgroundColor= styleEqual(c(0), c('#E8EAF6')))
                        %>%
                          formatStyle(c('ValueF1P1','ValueF1P2'),
                                      backgroundColor= styleInterval(c(#-100,-19.99,-14.99,-9.99,-4.99,
                                                                       -0.01,0.01),
                                                                       #4.99,9.99,14.99,19.99,100),
                                                                     c(#'#F44336','#EF5350','#E57373','#EF9A9A','#FFCDD2','#FFEBEE',
                                                                       '#FFCDD2','#FFFFFF','#E8F5E9')
                                                                       # '#E8F5E9','#C8E6C9','#A5D6A7','#81C784','#66BB6A','#4CAF50')
                                                                     ))
                        %>%
                          formatStyle(c('ValueF2P1','ValueF2P2'),
                                      backgroundColor= styleInterval(c(#-100,-19.99,-14.99,-9.99,-4.99,
                                        -0.01,0.01),
                                        #4.99,9.99,14.99,19.99,100),
                                        c(#'#F44336','#EF5350','#E57373','#EF9A9A','#FFCDD2','#FFEBEE',
                                          '#FFCDD2','#FFFFFF','#E8F5E9')
                                        # '#E8F5E9','#C8E6C9','#A5D6A7','#81C784','#66BB6A','#4CAF50')
                                      ))
                        )

  
  #save new match
  observeEvent(input$save.match,{
    #add in matches dataset
    
    newRow <- data.frame(input$Tournament, input$Date,
                         input$Player1, input$Player2)
    MatchId <- length(values$fuzzyMatches$Tournament[which(values$fuzzyMatches$Tournament==input$Tournament)])
    MatchId <- paste(substr(input$Tournament,1,3),'_',round(as.numeric(Sys.time())),'_',MatchId+1,sep='')
    newRow<-cbind(MatchId,newRow)
    colnames(newRow)<-colnames(values$fuzzyMatches)
    values$fuzzyMatches <- rbind(values$fuzzyMatches,newRow)
    
    #add in players dataset
    newRows <- data.frame(MatchId=rep(MatchId,2),
                          Player=c(input$Player1,input$Player2),
                          Technique=c(input$Technique1,input$Technique2),
                          Physique=c(input$Physique1,input$Physique2),
                          Psicology=c(input$Psicology1,input$Psicology2),
                          Regularity=c(input$Regularity1,input$Regularity2),
                          Experience=c(input$Experience1,input$Experience2),
                          Implication=c(input$Implication1,input$Implication2),
                          Surface=c(input$Surface1,input$Surface2),
                          Rest=c(input$Rest1,input$Rest2),
                          Confidence=c(input$Confidence1,input$Confidence2),
                          H2H=c(input$H2H1,1-input$H2H1))
    values$fuzzyData <- rbind(values$fuzzyData,newRows)
    
    #add in fuzzyprobs dataset
    fuzzyProbsPrev <- fuzzyLogic(values$fuzzyData[(nrow(values$fuzzyData)-1):nrow(values$fuzzyData),])
    bp1<-input$Bookie2/(input$Bookie1+input$Bookie2)
    BookieProb<-c(round(bp1,4), round(1-bp1,4))
    values$fuzzyProbs <- rbind(values$fuzzyProbs, cbind(fuzzyProbsPrev,BookieProb))

    #add in results dataset
    newRowResult<-saveResults(MatchId,input$Winner, input$NSets, input$GamesDif, input$Status)
    
    
    values$Results<-rbind(values$Results, newRowResult)
    
    #add in bookies dataset
    newbookie<- data.frame(MatchId=MatchId,
                           OddsP1=input$Bookie1,
                           OddsP2=input$Bookie2,
                           HAP_Highest=input$HAP_Highest,
                           HAP1_1=input$HAP1_1,HAP1_2=input$HAP1_2,HAP1_3=input$HAP1_3,
                           HAP1_4=input$HAP1_4,HAP1_5=input$HAP1_5,HAP1_6=input$HAP1_6,HAP1_7=input$HAP1_7,
                           HAP2_1=input$HAP2_1,HAP2_2=input$HAP2_2,HAP2_3=input$HAP2_3,
                           HAP2_4=input$HAP2_4,HAP2_5=input$HAP2_5,HAP2_6=input$HAP2_6,HAP2_7=input$HAP2_7
                           )
    values$bookies <- rbind(values$bookies,newbookie)
    
    #save and upload
    saveDropbox(values$fuzzyMatches, values$fuzzyData, values$fuzzyProbs, values$Results, values$bookies)
    showNotification('Match Saved', duration = 3, type='message')
    
    #add in values$MatchesResults dataset
    
    dataDropbox<-loadDropbox()
    values$MatchesResults <- mergeMatches(dataDropbox[[1]],dataDropbox[[4]],dataDropbox[[3]],dataDropbox[[2]])
    
  })
  
  #output fuzzy data selected players
  output$fuzzyselecteddataId<-
    DT::renderDataTable(selectedData_Id(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                               input$TournamentSel),],
                                        input$fuzzyMatches_rows_selected),
                        selection = 'single',rownames = FALSE,
                        options = list(dom = 't',scrollX = TRUE,ordering=F,rowCallback = rheight20))
  
  
  output$fuzzyselecteddataFeatures<-
    DT::renderDataTable(selectedData_Features(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                                     input$TournamentSel),],
                                              values$fuzzyData,
                                              input$fuzzyMatches_rows_selected),
                        selection = 'single',editable=F,rownames = FALSE,
                        options = list(dom = 't',
                                       scrollX = TRUE,
                                       ordering=F,
                                       rowCallback = rheight20))
  
  output$matchdata<-
    DT::renderDataTable(matchData(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                         input$TournamentSel),],
                                  input$TournamentSel,
                                  values$fuzzyData,
                                  input$fuzzyMatches_rows_selected,
                                  values$bookies),
                        
                        selection = 'single',rownames = FALSE,
                        options = list(dom = 't',scrollX = TRUE,ordering=F,rowCallback = rheight20))
  
  #output odds i handicaps
  
  output$oddsBet<-
    DT::renderDataTable(handicapShow(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                            input$TournamentSel),],
                                     values$fuzzyData,
                                     input$fuzzyMatches_rows_selected,
                                     values$bookies),
                        
                        selection = 'single',rownames = FALSE,
                        options = list(dom = 't',scrollX = TRUE,ordering=F,rowCallback = rheight20))
  
  
  #condicio per ensenyar players i probs dels seleccionats
  output$showselecteddata <- reactive({
    !is.null(input$fuzzyMatches_rows_selected)
  })
  outputOptions(output, 'showselecteddata', suspendWhenHidden=FALSE)
  
  #condicio per ensenyar panel d'afegir partits
  output$showaddmatchdata <- reactive({
    (!input$new.match %% 2 == 0 & input$save.match %% 2 == 0) | 
      (!input$new.match %% 2 != 0 & input$save.match %% 2 != 0)
  })
  outputOptions(output, 'showaddmatchdata', suspendWhenHidden=FALSE)
  
  
  #condicio per ensenyar panel d'editar partits
  output$editdata <- reactive({
    (!input$editdata %% 2 == 0 & input$save.edits %% 2 == 0) |
      (!input$editdata %% 2 != 0 & input$save.edits %% 2 != 0)
  })
  
  outputOptions(output, 'editdata', suspendWhenHidden=FALSE)
  
  #condicio per ensenyar i editar panel de resultats de partits
  output$played <- reactive({
    input$Status != 2
  })
  
  output$playedMOD <- reactive({
    input$MODStatus != 2
  })
  
  output$finished <- reactive({
    input$Status == 1
  })
  
  output$finishedMOD <- reactive({
    input$MODStatus == 1
  })

  outputOptions(output, 'played', suspendWhenHidden=FALSE)
  outputOptions(output, 'playedMOD', suspendWhenHidden=FALSE)
  outputOptions(output, 'finished', suspendWhenHidden=FALSE)
  outputOptions(output, 'finishedMOD', suspendWhenHidden=FALSE)
  
  ############################
  #AFEGIR DADES NOU PARTITS
  ############################
  
  #selectize tournament
  
  output$selectizeTournament <- renderUI({
    selectizeInput("Tournament", "Tournament",
                   unique(values$fuzzyMatches$Tournament)[length(unique(values$fuzzyMatches$Tournament)):1],
                   options=list(create = TRUE)) 
  })
  
  #select last added date
  
  output$selectdate <- renderUI({
    dateInput("Date","Date",
              unique(values$fuzzyMatches$Date)[length(unique(values$fuzzyMatches$Date))]
    )
  })
  
  #selectize player1
  output$selectizePlayer1 <- renderUI({
    selectizeInput("Player1",
                   "Player1",
                   sort(unique(c(as.character(values$fuzzyMatches$Player1),
                                 as.character(values$fuzzyMatches$Player2)))),
                   options=list(create = TRUE))
  })
  #selectize player2
  output$selectizePlayer2 <- renderUI({
    selectizeInput("Player2",
                   "Player2",
                   sort(unique(c(as.character(values$fuzzyMatches$Player1),
                                 as.character(values$fuzzyMatches$Player2)))),
                   options=list(create = TRUE))
  })  
  
  #jugador1
  output$Technique1 <- renderUI({
    numericInput("Technique1", "Technique",
                 initialValues(input$Player1,values$fuzzyData)[1], 0,1,0.1,'60%')
  })
  output$Physique1 <- renderUI({
    numericInput("Physique1", "Physique",
                 initialValues(input$Player1,values$fuzzyData)[2], 0,1,0.1,'60%')
  })
  output$Psicology1 <- renderUI({
    numericInput("Psicology1", "Psicology",
                 initialValues(input$Player1,values$fuzzyData)[3], 0,1,0.1,'60%')
  })
  output$Regularity1 <- renderUI({
    numericInput("Regularity1", "Regularity",
                 initialValues(input$Player1,values$fuzzyData)[4], 0,1,0.1,'60%')
  })
  output$Experience1 <- renderUI({
    numericInput("Experience1", "Experience",
                 initialValues(input$Player1,values$fuzzyData)[5], 0,1,0.1,'60%')
  })
  output$Implication1 <- renderUI({
    numericInput("Implication1", "Implication",
                 initialValues(input$Player1,values$fuzzyData)[6], 0,1,0.1,'60%')
  })
  output$Surface1 <- renderUI({
    numericInput("Surface1", "Surface",
                 initialValues(input$Player1,values$fuzzyData)[7], 0,1,0.1,'60%')
  })
  output$Rest1 <- renderUI({
    numericInput("Rest1", "Rest",
                 initialValues(input$Player1,values$fuzzyData)[8], 0,1,0.1,'60%')
  })
  output$Confidence1 <- renderUI({
    numericInput("Confidence1", "Confidence",
                 initialValues(input$Player1,values$fuzzyData)[9], 0,1,0.1,'60%')
  })
  output$H2H1 <- renderUI({
    numericInput("H2H1", "H2H",
                 0.5,0,1,0.1,'60%')
  })
  
  #jugador2
  output$Technique2 <- renderUI({
    numericInput("Technique2", "Technique",
                 initialValues(input$Player2,values$fuzzyData)[1], 0,1,0.1,'60%')
  })
  output$Physique2 <- renderUI({
    numericInput("Physique2", "Physique",
                 initialValues(input$Player2,values$fuzzyData)[2], 0,1,0.1,'60%')
  })
  output$Psicology2 <- renderUI({
    numericInput("Psicology2", "Psicology",
                 initialValues(input$Player2,values$fuzzyData)[3], 0,1,0.1,'60%')
  })
  output$Regularity2 <- renderUI({
    numericInput("Regularity2", "Regularity",
                 initialValues(input$Player2,values$fuzzyData)[4], 0,1,0.1,'60%')
  })
  output$Experience2 <- renderUI({
    numericInput("Experience2", "Experience",
                 initialValues(input$Player2,values$fuzzyData)[5], 0,1,0.1,'60%')
  })
  output$Implication2 <- renderUI({
    numericInput("Implication2", "Implication",
                 initialValues(input$Player2,values$fuzzyData)[6], 0,1,0.1,'60%')
  })
  output$Surface2 <- renderUI({
    numericInput("Surface2", "Surface",
                 initialValues(input$Player2,values$fuzzyData)[7], 0,1,0.1,'60%')
  })
  output$Rest2 <- renderUI({
    numericInput("Rest2", "Rest",
                 initialValues(input$Player2,values$fuzzyData)[8], 0,1,0.1,'60%')
  })
  output$Confidence2 <- renderUI({
    numericInput("Confidence2", "Confidence",
                 initialValues(input$Player2,values$fuzzyData)[9], 0,1,0.1,'60%')
  })
  
  output$Winner <- renderUI({
    radioButtons("Winner", "Winner",
                 getPlayersNames(input$Player1,input$Player2), inline=T)
  })
  
  output$NSets <- renderUI({
    sliderInput("NSets", "Number of sets",
                2,5,2,1)
  })
  
  output$GamesDif <- renderUI({
    numericInput("GamesDif", "Games Diference",
                 0,-10,30,1,'60%')
  })
  
  output$Status <- renderUI({
    radioButtons("Status", "Status",
                 list('Played' = 1,
                      'Scheduled' = 2,
                      'Abandoned' = 3), inline=F)
  })
  
  #bookies
  
  output$Bookie1 <- renderUI({
    
    numericInput("Bookie1", paste(input$Player1,"odds"),
                 1.50,1,100,0.01,'60%')
    
  })
  
  output$Bookie2 <- renderUI({
    
    numericInput("Bookie2", paste(input$Player2,"odds"),
                 1.51,1,100,0.01,'60%')
    
  })
  
  output$HAP_Highest <- renderUI({
    
    numericInput("HAP_Highest", "Highest Handicap",
                 4.5,-10.5,10.5,1,'60%')
    
  })
  
  output$HAP1_1 <- renderUI({
    
    numericInput("HAP1_1", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[1],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP1_2 <- renderUI({
    
    numericInput("HAP1_2", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[2],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP1_3 <- renderUI({
    
    numericInput("HAP1_3", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[3],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP1_4 <- renderUI({
    
    numericInput("HAP1_4", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[4],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP1_5 <- renderUI({
    
    numericInput("HAP1_5", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[5],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP1_6 <- renderUI({
    
    numericInput("HAP1_6", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[6],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP1_7 <- renderUI({
    
    numericInput("HAP1_7", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[7],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP2_1 <- renderUI({
    
    numericInput("HAP2_1", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[8],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP2_2 <- renderUI({
    
    numericInput("HAP2_2", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[9],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP2_3 <- renderUI({
    
    numericInput("HAP2_3", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[10],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP2_4 <- renderUI({
    
    numericInput("HAP2_4", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[11],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP2_5 <- renderUI({
    
    numericInput("HAP2_5", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[12],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP2_6 <- renderUI({
    
    numericInput("HAP2_6", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[13],
                 1,1,20,0.01,'60%')
    
  })
  
  output$HAP2_7 <- renderUI({
    
    numericInput("HAP2_7", handicapVector(input$Bookie1, input$Bookie2, input$HAP_Highest)[14],
                 1,1,20,0.01,'60%')
    
  })
  
  ##############
  #EDIT DATA
  ##############

  #selectize tournament
  
  output$MODselectizeTournament <- renderUI({
    selectizeInput("MODTournament", "Tournament",
                   unique(values$fuzzyMatches$Tournament)[length(unique(values$fuzzyMatches$Tournament)):1],
                   options=list(create = TRUE),
                   selected=previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                                  input$TournamentSel),],
                                           values$fuzzyData,
                                           values$bookies,
                                           input$fuzzyMatches_rows_selected)[[22]])
  })
  
  #select date
  
  output$MODselectdate <- renderUI({
    dateInput("MODDate","Date",
              previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                    input$TournamentSel),],
                             values$fuzzyData,
                             values$bookies,
                             input$fuzzyMatches_rows_selected)[[23]]
    )
  })
  
  #selectize player1
  output$MODselectizePlayer1 <- renderUI({
    selectizeInput("MODPlayer1",
                   "Player1",
                   sort(unique(c(as.character(values$fuzzyMatches$Player1),
                                 as.character(values$fuzzyMatches$Player2)))),
                   options=list(create = TRUE),
                   selected=previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                                  input$TournamentSel),],
                                           values$fuzzyData,
                                           values$bookies,
                                           input$fuzzyMatches_rows_selected)[[1]])
  })
  #selectize player2
  output$MODselectizePlayer2 <- renderUI({
    selectizeInput("MODPlayer2",
                   "Player2",
                   sort(unique(c(as.character(values$fuzzyMatches$Player1),
                                 as.character(values$fuzzyMatches$Player2)))),
                   options=list(create = TRUE),
                   selected=previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                                  input$TournamentSel),],
                                           values$fuzzyData,
                                           values$bookies,
                                           input$fuzzyMatches_rows_selected)[[12]])
  })  
  
  #jugador1
  output$MODTechnique1 <- renderUI({
    numericInput("MODTechnique1", "Technique",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                                input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[2]], 0,1,0.1,'60%')
  })
  output$MODPhysique1 <- renderUI({
    numericInput("MODPhysique1", "Physique",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                                input$TournamentSel),],
                                         values$fuzzyData,
                                values$bookies,
                                         input$fuzzyMatches_rows_selected)[[3]], 0,1,0.1,'60%')
  })
  output$MODPsicology1 <- renderUI({
    numericInput("MODPsicology1", "Psicology",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[4]], 0,1,0.1,'60%')
  })
  output$MODRegularity1 <- renderUI({
    numericInput("MODRegularity1", "Regularity",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[5]], 0,1,0.1,'60%')
  })
  output$MODExperience1 <- renderUI({
    numericInput("MODExperience1", "Experience",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[6]], 0,1,0.1,'60%')
  })
  output$MODImplication1 <- renderUI({
    numericInput("MODImplication1", "Implication",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[7]], 0,1,0.1,'60%')
  })
  output$MODSurface1 <- renderUI({
    numericInput("MODSurface1", "Surface",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[8]], 0,1,0.1,'60%')
  })
  output$MODRest1 <- renderUI({
    numericInput("MODRest1", "Rest",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[9]], 0,1,0.1,'60%')
  })
  output$MODConfidence1 <- renderUI({
    numericInput("MODConfidence1", "Confidence",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[10]], 0,1,0.1,'60%')
  })
  output$MODH2H1 <- renderUI({
    numericInput("MODH2H1", "H2H",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[11]],0,1,0.1,'60%')
  })
  
  #jugador2
  output$MODTechnique2 <- renderUI({
    numericInput("MODTechnique2", "Technique",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[13]], 0,1,0.1,'60%')
  })
  output$MODPhysique2 <- renderUI({
    numericInput("MODPhysique2", "Physique",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[14]], 0,1,0.1,'60%')
  })
  output$MODPsicology2 <- renderUI({
    numericInput("MODPsicology2", "Psicology",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[15]], 0,1,0.1,'60%')
  })
  output$MODRegularity2 <- renderUI({
    numericInput("MODRegularity2", "Regularity",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[16]], 0,1,0.1,'60%')
  })
  output$MODExperience2 <- renderUI({
    numericInput("MODExperience2", "Experience",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[17]], 0,1,0.1,'60%')
  })
  output$MODImplication2 <- renderUI({
    numericInput("MODImplication2", "Implication",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[18]], 0,1,0.1,'60%')
  })
  output$MODSurface2 <- renderUI({
    numericInput("MODSurface2", "Surface",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[19]], 0,1,0.1,'60%')
  })
  output$MODRest2 <- renderUI({
    numericInput("MODRest2", "Rest",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[20]], 0,1,0.1,'60%')
  })
  output$MODConfidence2 <- renderUI({
    numericInput("MODConfidence2", "Confidence",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[21]], 0,1,0.1,'60%')
  })
  
  output$MODWinner <- renderUI({
    radioButtons("MODWinner", "Winner",
                 getPlayersNames(input$MODPlayer1,input$MODPlayer2), inline=T,
                 selected=previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                                input$TournamentSel),],
                                         values$fuzzyData,
                                         values$bookies,
                                         input$fuzzyMatches_rows_selected)[[25]])
  })
  
  output$MODNSets <- renderUI({
    sliderInput("MODNSets", "Number of sets",
                2,5,previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                          input$TournamentSel),],
                                   values$fuzzyData,
                                   values$bookies,
                                   input$fuzzyMatches_rows_selected)[[26]],1)
  })
  
  output$MODGamesDif <- renderUI({
    numericInput("MODGamesDif", "Games Diference",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[27]],-10,30,1,'60%')
  })
  
  output$MODStatus <- renderUI({
    radioButtons("MODStatus", "Status",
                 list('Played' = 1,
                      'Scheduled' = 2,
                      'Abandoned' = 3), inline=F,
                 selected=previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                                input$TournamentSel),],
                                         values$fuzzyData,
                                         values$bookies,
                                         input$fuzzyMatches_rows_selected)[[28]])
  })
  
  #bookies
  
  output$MODBookie1 <- renderUI({
    
    numericInput("MODBookie1", paste(input$MODPlayer1,"odds"),
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[30]],
                 1,100,0.01,'60%')
    
  })
  
  output$MODBookie2 <- renderUI({
    
    numericInput("MODBookie2", paste(input$MODPlayer2,"odds"),
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[31]],
                 1,100,0.01,'60%')
    
  })
  
  output$MODHAP_Highest <- renderUI({
    
    numericInput("MODHAP_Highest", "Highest Handicap",
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[32]],
                 -10.5,10.5,1,'60%')
    
  })
  
  
  output$MODHAP1_1 <- renderUI({
    
    numericInput("MODHAP1_1", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[1],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[33]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP1_2 <- renderUI({
    
    numericInput("MODHAP1_2", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[2],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[34]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP1_3 <- renderUI({
    
    numericInput("MODHAP1_3", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[3],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[35]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP1_4 <- renderUI({
    
    numericInput("MODHAP1_4", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[4],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[36]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP1_5 <- renderUI({
    
    numericInput("MODHAP1_5", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[5],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[37]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP1_6 <- renderUI({
    
    numericInput("MODHAP1_6", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[6],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[38]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP1_7 <- renderUI({
    
    numericInput("MODHAP1_7", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[7],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[39]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP2_1 <- renderUI({
    
    numericInput("MODHAP2_1", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[8],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[40]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP2_2 <- renderUI({
    
    numericInput("MODHAP2_2", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[9],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[41]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP2_3 <- renderUI({
    
    numericInput("MODHAP2_3", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[10],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[42]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP2_4 <- renderUI({
    
    numericInput("MODHAP2_4", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[11],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[43]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP2_5 <- renderUI({
    
    numericInput("MODHAP2_5", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[12],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[44]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP2_6 <- renderUI({
    
    numericInput("MODHAP2_6", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[13],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[45]],1,20,0.01,'60%')
    
  })
  
  output$MODHAP2_7 <- renderUI({
    
    numericInput("MODHAP2_7", handicapVector(input$MODBookie1, input$MODBookie2, input$MODHAP_Highest)[14],
                 previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                       input$TournamentSel),],
                                values$fuzzyData,
                                values$bookies,
                                input$fuzzyMatches_rows_selected)[[46]],1,20,0.01,'60%')
    
  })
  
  #save match edits
  observeEvent(input$save.edits,{
    
    MatchId<-previousValues(values$MatchesResults[selectTournament(values$MatchesResults,
                                                                   input$TournamentSel),],
                            values$fuzzyData,
                            values$bookies,
                            input$fuzzyMatches_rows_selected)[[29]]
    
    #edit matches dataset
    
    MODRow <- data.frame(MatchId,input$MODTournament, input$MODDate,input$MODPlayer1, input$MODPlayer2)
    
    MODRow[,2]<-as.character(MODRow[,2])
    MODRow[,4]<-as.character(MODRow[,4])
    MODRow[,5]<-as.character(MODRow[,5])
    
    values$fuzzyMatches$Tournament<-as.character(values$fuzzyMatches$Tournament)
    values$fuzzyMatches$Player1<-as.character(values$fuzzyMatches$Player1)
    values$fuzzyMatches$Player2<-as.character(values$fuzzyMatches$Player2)
    
    values$fuzzyMatches[which(values$fuzzyMatches$MatchId == MatchId),] <- MODRow
    
    #edit players dataset
    
    MODRows <- data.frame(MatchId=rep(MatchId,2),
                          Player=c(input$MODPlayer1,input$MODPlayer2),
                          Technique=c(input$MODTechnique1,input$MODTechnique2),
                          Physique=c(input$MODPhysique1,input$MODPhysique2),
                          Psicology=c(input$MODPsicology1,input$MODPsicology2),
                          Regularity=c(input$MODRegularity1,input$MODRegularity2),
                          Experience=c(input$MODExperience1,input$MODExperience2),
                          Implication=c(input$MODImplication1,input$MODImplication2),
                          Surface=c(input$MODSurface1,input$MODSurface2),
                          Rest=c(input$MODRest1,input$MODRest2),
                          Confidence=c(input$MODConfidence1,input$MODConfidence2),
                          H2H=c(input$MODH2H1,1-input$MODH2H1))
    
    MODRows$MatchId<-as.character(MODRows$MatchId)
    MODRows$Player<-as.character(MODRows$Player)
    
    values$fuzzyData$Player<-as.character(values$fuzzyData$Player)
    
    values$fuzzyData[which(values$fuzzyData$MatchId == MatchId),] <- MODRows

    #edit fuzzyprobs dataset
    
    fuzzyProbsPrev <- fuzzyLogic(values$fuzzyData[which(values$fuzzyData$MatchId == MatchId),])
    
    MODbp1<-input$MODBookie2/(input$MODBookie1+input$MODBookie2)
    BookieProb<-c(round(MODbp1,4), round(1-MODbp1,4))
    newProb<-cbind(fuzzyProbsPrev,BookieProb)

    newProb$MatchId<-as.character(newProb$MatchId)
    newProb$Player<-as.character(newProb$Player)

    values$fuzzyProbs$Player<-as.character(values$fuzzyProbs$Player)

    values$fuzzyProbs[which(values$fuzzyProbs$MatchId==MatchId),] <- newProb

    #edit results dataset
    
    MODResult<-saveResults(MatchId,input$MODWinner, input$MODNSets, input$MODGamesDif, input$MODStatus)
    values$Results[which(values$Results$MatchId == MatchId),]<-MODResult
    
    #edit bookies dataset
    
    MODbook<- data.frame(MatchId=MatchId,
                         OddsP1=input$MODBookie1,
                         OddsP2=input$MODBookie2,
                         HAP_Highest=input$MODHAP_Highest,
                         HAP1_1=input$MODHAP1_1,HAP1_2=input$MODHAP1_2,HAP1_3=input$MODHAP1_3,
                         HAP1_4=input$MODHAP1_4,HAP1_5=input$MODHAP1_5,HAP1_6=input$MODHAP1_6, HAP1_7=input$MODHAP1_7, 
                         HAP2_1=input$MODHAP2_1,HAP2_2=input$MODHAP2_2,HAP2_3=input$MODHAP2_3,
                         HAP2_4=input$MODHAP2_4,HAP2_5=input$MODHAP2_5,HAP2_6=input$MODHAP2_6, HAP2_7=input$MODHAP2_7
    )
    
    MODbook$MatchId<-as.character(MODbook$MatchId)
    
    values$bookies[which(values$bookies$MatchId == MatchId),] <- MODbook
    
    #save and upload
    saveDropbox(values$fuzzyMatches, values$fuzzyData, values$fuzzyProbs, values$Results, values$bookies)
    showNotification('Match Saved', duration = 3, type='message')
    
    #add in values$MatchesResults dataset
    
    dataDropbox<-loadDropbox()
    values$MatchesResults <- mergeMatches(dataDropbox[[1]],dataDropbox[[4]],dataDropbox[[3]],dataDropbox[[2]])
    
  })
  
  
  
  
  
  
  ###########################
  # descarregar dades matches
  ###########################
  
  # descarregar dades matches
  output$download.matches <- downloadHandler(
    filename = function() {
      paste('matches_',strftime(Sys.time(),format='%Y%m%d'),'.csv',sep='')
    },
    content = function(file) {
      write.csv(values$fuzzyMatches, file, row.names = FALSE)
    }
  )
  
  # descarregar dades players
  output$download.players <- downloadHandler(
    filename = function() {
      paste('players_',strftime(Sys.time(),format='%Y%m%d'),'.csv',sep='')
    },
    content = function(file) {
      write.csv(values$fuzzyData, file, row.names = FALSE)
    }
  )
  # descarregar dades probs
  output$download.probs <- downloadHandler(
    filename = function() {
      paste('probs_',strftime(Sys.time(),format='%Y%m%d'),'.csv',sep='')
    },
    content = function(file) {
      write.csv(values$fuzzyProbs, file, row.names = FALSE)
    }
  )
  

  
  
  #show selected fuzzy probs
  # output$fuzzyselectedProbs <-
  #   DT::renderDataTable(getProbs(values$MatchesResults[selectTournament(values$MatchesResults,
  #                                                                       input$TournamentSel),],
  #                                input$fuzzyMatches_rows_selected)[,-1],
  #                       selection = 'single',rownames = FALSE,
  #                       options = list(dom = 't',scrollX = TRUE,ordering=F,
  #                                      rowCallback = rheight20))  

  #tancar app quan es premi el boto de tancar finestra
  
  observeEvent(input$close, {
    showModal(modalDialog(
      title="Are you sure you wanna leave?", size='s',
      footer = tagList(actionButton("confirmclose", "Close"),
                       modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$confirmclose, {
    js$closeWindow()
    stopApp()
  })
  
})
