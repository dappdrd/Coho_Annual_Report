####################################################################################################
# CoTC Annual Report code.  Server.R must be paired with Ui.R
# Code developed by Derek Dapp (WDFW), Angelika Hagen-breaux (WDFW), and the CoTC
# App hosted by Shiny Servers
####################################################################################################

#used for sending emails
library(mailR)
#used for shiny
library(shiny)
library(shinyFiles)
#for drop box interfacing
library(rdrop2)
#for data frame manipulation
library(plyr)
#For data frame manipulation
library(reshape2)
library(readxl)


token <<- readRDS("droptoken.rds")
drop_acc(dtoken = token)

Password <<- "rjdio"

BlankDF <<- data.frame(Stock = as.character())

FinalRunList <<- c("BASE.Cmd", "CB86.Cmd", "CB87.Cmd", "CB88.Cmd", "CB89.Cmd", "CB90.Cmd", "CB91.Cmd", "CB92.Cmd", "CB93.Cmd", "CB94.Cmd", "CB95.Cmd",
                  "CB96.Cmd", "CB97.Cmd", "BK98 w UF H&W", "BK99 w UF H&W", "BK00 w UF H&W", "BK01 w UF H&W Reload Catches", "BK02 w UF H&W Reload catches",
                  "BK03 w catches and BC cohorts", "BK04 w catches BCcohorts", "BK05 w catches and BC cohorts", "BK06 w catches and BC cohorts", 
                  "BK07 catches and BC cohorts", "BK08.cmd", "BK09 New CNR", "bk10PSCFeb14", "Coho2011Post_PSC 2013", "Coho2012Post_PSC SSNPx2 Q Aug 22 BC MSF corrected",
                  "bk 2013 Feb 11 2015 adjust GB recruits", "bc-BK2014 w TAMM inputs final#2", "bc-bkCoho2015 Final", "bc-BK2016 BPMar2013 Feb14")

#This sets up a stock DF that has all the necessary stock information to be used later
#LAC = Low Abundance Cohort; UAC = Upper Abundance Cohort; LAMO = Low Abundance Management Objective (cap); MAMO = Moderate Abundance Management Objective,
#AAMO = Abundant Abundance Management Objective
SkagitRows <- data.frame(PSCStock = 1, FRAMWildStocks = c(17,18,23,24), StockName = "Skagit",LAC = 22857, UAC = 62500, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .35, AAMO = .6, LEG = NA, UEG = NA, MU = "US Inside")
StillyRows <- data.frame(PSCStock = 2, FRAMWildStocks = c(29,30), StockName = "Stillaguamish",LAC = 9385, UAC = 20000, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .35, AAMO = .5, LEG = NA, UEG = NA, MU = "US Inside")
SnohomishRows <- data.frame(PSCStock = 3, FRAMWildStocks = c(35,36), StockName = "Snohomish",LAC = 51667, UAC = 125000, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .4, AAMO = .6, LEG = NA, UEG = NA, MU = "US Inside")
HoodCanalRows <- data.frame(PSCStock = 4, FRAMWildStocks = c(45,46,55,56,59,60), StockName = "Hood Canal",LAC = 19545, UAC = 41000, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .45, AAMO = .65, LEG = NA, UEG = NA, MU = "US Inside")
JDFRows <- data.frame(PSCStock = 5, FRAMWildStocks = c(115,116,117,118), StockName = "US Strait JDF",LAC = 11679, UAC = 27445, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .4, AAMO = .6, LEG = NA, UEG = NA, MU = "US Inside")

QuilRows <- data.frame(PSCStock = 6, FRAMWildStocks = c(131, 132), StockName = "Quillayute",LAC = 7875, UAC = 10500, Cap.Meth = as.character("omu"), LAMO = .2, MAMO = NA, AAMO = NA, LEG = 6300, UEG = 15800, MU = "US Outside")
HohRows <- data.frame(PSCStock = 7, FRAMWildStocks = c(135, 136), StockName = "Hoh",LAC = 2500, UAC = 3333, Cap.Meth = as.character("omu"), LAMO = .2, MAMO = NA, AAMO = NA, LEG = 2000, UEG = 5000, MU = "US Outside")
QueetsRows <- data.frame(PSCStock = 8, FRAMWildStocks = c(139,140), StockName = "Queets",LAC = 7250, UAC = 9667, Cap.Meth = as.character("omu"), LAMO = .2, MAMO = NA, AAMO = NA, LEG = 5800, UEG = 14500, MU = "US Outside")
GraysHarbRows <- data.frame(PSCStock = 9, FRAMWildStocks = c(149, 150, 153, 154, 157, 158), StockName = "Grays Harbor",LAC = 44250, UAC = 59000, Cap.Meth = as.character("omu"), LAMO = NA, MAMO = NA, AAMO = .6, LEG = 35400, UEG = 35400, MU = "US Outside")

LowFRRows <- data.frame(PSCStock = 10, FRAMWildStocks = c(227, 228), StockName = "Lower Fraser",LAC = NA, UAC = NA, Cap.Meth = as.character("None"), LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")
IntFRRows <- data.frame(PSCStock = 11, FRAMWildStocks = c(231, 232), StockName = "Interior Fraser",LAC = NA, UAC = NA, Cap.Meth = as.character("fixed"), LAMO = .2, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada Fixed")
GeoStMLRows <- data.frame(PSCStock = 12, FRAMWildStocks = c(207,208), StockName = "Georgia Strait ML",LAC = NA, UAC = NA, Cap.Meth = as.character("None"), LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")
GeoStVIRows <- data.frame(PSCStock = 13, FRAMWildStocks = c(211, 212), StockName = "Georgia Strait VI",LAC = NA, UAC = NA, Cap.Meth = as.character("None"), LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")

StockDF <<- rbind(SkagitRows, StillyRows, SnohomishRows, HoodCanalRows, JDFRows, QuilRows, HohRows, QueetsRows, GraysHarbRows, LowFRRows, IntFRRows, GeoStMLRows, GeoStVIRows)

TAMMList <<- read.csv("https://dl.dropboxusercontent.com/s/jbclcmqk0xqfnob/TAMMList.csv?dl=1")

#List of Stocks
StockList <<- as.character(unique(StockDF$StockName))

#This lists the terminal fisheries in a data frame for use later
#The TAMM position corresponds to the column at which the stock is found in TAMM table 2
QueetsTermRow <- data.frame(Stock = "Queets", TerminalFish = c(68,65,69, 66, 67), TAMMPosition = 31)
QuillayuteTermRow <- data.frame(Stock = "Quillayute", TerminalFish = c(70,71, 72), TAMMPosition = 26)
HohTermRow <- data.frame(Stock = "Hoh", TerminalFish = c(73,74,75), TAMMPosition = 27)
GHTermRow <- data.frame(Stock = "Grays Harbor", TerminalFish = c(48, 49, 50, 51, 53, 52, 54, 55, 56, 57, 58, 59, 60, 61), TAMMPosition = 37)
CoastalTermFishDF <<- rbind(QueetsTermRow, QuillayuteTermRow, HohTermRow, GHTermRow)

shinyServer(
function(input, output, session){
  
  #Creates a function for plotting pictures
  plot_jpeg = function(path, add=FALSE)
  {
    require('jpeg')
    jpg = readJPEG(path, native=T) # read the file
    res = dim(jpg)[1:2] # get the resolution
    if (!add) # initialize an empty plot area if add==FALSE
      plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
  }
  
   observe({
     # Take a dependency on input$EmailButton
     if (input$EmailButton == 0 | input$PasswordAdd != Password)
       return(NULL)
     # Use isolate() to avoid dependency on input$EmailButton
     isolate({
       
       withProgress(message = 'Sending Email', value = 0, {
         incProgress(1/1, detail = "Attaching Files")
          #sends a mail from my dummy email address.
          send.mail(from = "derek.dapp.dfw@gmail.com",
                 to = input$EmailAdd,
                 subject = "Coho PSC Annual Report Data",
                 body = "Please see the attached for the PSC Annual report files.",
                 smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "derek.dapp.dfw@gmail.com", passwd = "FakePass2", ssl = TRUE),
                 authenticate = TRUE,
                 send = TRUE,
                 attach.files = c("https://dl.dropboxusercontent.com/s/ywkvwz1bkanpnch/Table_1.csv",
                                  "https://dl.dropboxusercontent.com/s/zf5wkw9endtbsgi/Table_2.csv",
                                  "https://dl.dropboxusercontent.com/s/m1fmwvedp6b11l8/Table_3.csv"),
                 
                 file.descriptions = c("Table 1", "Table 2", "Table 3"), # optional parameter
                 debug = TRUE)
       })
     })
   })
   
   observe({
     #If the button hasn't been pressed or the pass is wrong do nothing
     if (input$DataProcessButton == 0 | input$PasswordAdd != Password)
       return(NULL)
     isolate({

         
         #Checks to see if TAMMs are available... if not throw an error message
         TAMMCheck <- subset(TAMMList, year == input$YearAdd)
         
         #Throw an error message if the TAMMs are pre-2004
         if(input$YearAdd < 2004){
           showModal(modalDialog(
             title = "Error message",
             "The application can only be run for years 2004 onward because there are no pre-season run database files prior to 2004."
           ))
         }
         else if (nrow(TAMMCheck) == 0 & input$TAMMAdd == "Yes"){
           showModal(modalDialog(
             title = "Error message",
             "You have selected to run with a TAMM, but a TAMM is not on the server for the specified year."
           ))
         }
         
         else{
           withProgress(message = 'Loading Data', value = 0, {
           #Starts up the progress bar
           incProgress(1/2, detail = "Loading Post-Season Data - this may take a few minutes")
           #Grabs FRAM RunID Table  
           RunIDTab = read.csv("https://dl.dropboxusercontent.com/s/f42478elh38laka/RunList.csv?dl=1")
           
           #removes useless columns in the data
           Drops <- c("PrimaryKey", "SpeciesName", "RunTitle",
                      "RunComments", "CreationDate", "ModifyInputDate", "RunTimeDate")
           RunIDTab<- RunIDTab[ , !(names(RunIDTab) %in% Drops)]
           
           #Grabs FRAM Escapement Table
           
           EscTab = read.csv("https://dl.dropboxusercontent.com/s/d2jh7tqd3yg0h0r/Escapement.csv?dl=1")
           
           #Adds run info to EscTab
           EscTab <- merge(EscTab, RunIDTab, by= "RunID")
           
           EscTab <- subset(EscTab, RunYear == input$YearAdd)
           
           #Convert integers to characters
           EscTab$RunID <- as.character(EscTab$RunID)
           EscTab$RunYear <- as.character(EscTab$RunYear)
           EscTab$StockID <- as.character(EscTab$StockID)
           EscTab$Age <- as.character(EscTab$Age)
           EscTab$TimeStep <- as.character(EscTab$TimeStep)
           EscTab$PrimaryKey <- as.character(EscTab$PrimaryKey)
           EscTab$BasePeriodID <- as.character(EscTab$BasePeriodID)
           
           #Grab FRAM Mortality Table
           MortTab = read.csv("https://dl.dropboxusercontent.com/s/bwpvgc3rc1gop5d/Mortality.csv?dl=1")
           
           #Adds run info to Mort Tab
           MortTab <- merge(MortTab, RunIDTab, by= "RunID")
           
           MortTab <- subset(MortTab, RunYear == input$YearAdd)
           
           #Get summarized Mortalities
           MortTab$TotMort <- MortTab$LandedCatch + MortTab$NonRetention + MortTab$Shaker + MortTab$DropOff + MortTab$MSFLandedCatch + MortTab$MSFNonRetention + MortTab$MSFShaker + MortTab$MSFDropOff
           
           #Remove columns no longer of interest
           Keeps <- c("RunID", "StockID", "Age", "FisheryID", "TimeStep", "TotMort", "RunYear", "BasePeriodID")
           MortTab <- MortTab[, (names(MortTab) %in% Keeps)]
           
           #Convert integers to characters
           MortTab$RunID <- as.character(MortTab$RunID)
           MortTab$RunYear <- as.character(MortTab$RunYear)
           MortTab$StockID <- as.character(MortTab$StockID)
           MortTab$Age <- as.character(MortTab$Age)
           MortTab$TimeStep <- as.character(MortTab$TimeStep)
           MortTab$BasePeriodID <- as.character(MortTab$BasePeriodID)
           MortTab$FisheryID <- as.character(MortTab$FisheryID)
            
            
            
            
            incProgress(2/2, detail = "Loading Pre-Season Data - this may take a few minutes")    
            
            ################Pre-season data loading
            #Load Pre-season RunID, Escapement and Mortality Tables
            
            PreRunIDTab = read.csv("https://dl.dropboxusercontent.com/s/1z1huq2ayg0sspp/PreSeasonRunList.csv?dl=1")
            PreEscTab = read.csv("https://dl.dropboxusercontent.com/s/fzqlvw705bkwh1s/PreSeasonEscapement.csv?dl=1")
            PreMortTab = read.csv("https://dl.dropboxusercontent.com/s/6ja70l7z45fj9k6/PreSeasonMortality.csv?dl=1")
            
            #removes useless columns in the data
            Drops <- c("PrimaryKey", "SpeciesName", "RunTitle",
                       "RunComments", "CreationDate", "ModifyInputDate", "RunTimeDate")
            PreRunIDTab<- PreRunIDTab[ , !(names(PreRunIDTab) %in% Drops)]
            
            #Adds run info to EscTab
            PreEscTab <- merge(PreEscTab, PreRunIDTab, by= "RunID")
            
            PreEscTab <- subset(PreEscTab, RunYear == input$YearAdd)
            
            #Convert integers to characters
            PreEscTab$RunID <- as.character(PreEscTab$RunID)
            PreEscTab$RunYear <- as.character(PreEscTab$RunYear)
            PreEscTab$StockID <- as.character(PreEscTab$StockID)
            PreEscTab$Age <- as.character(PreEscTab$Age)
            PreEscTab$TimeStep <- as.character(PreEscTab$TimeStep)
            PreEscTab$PrimaryKey <- as.character(PreEscTab$PrimaryKey)
            PreEscTab$BasePeriodID <- as.character(PreEscTab$BasePeriodID)
            
            #Get summarized Mortalities
            PreMortTab$TotMort <- PreMortTab$LandedCatch + PreMortTab$NonRetention + PreMortTab$Shaker + 
              PreMortTab$DropOff + PreMortTab$MSFLandedCatch + PreMortTab$MSFNonRetention + PreMortTab$MSFShaker + PreMortTab$MSFDropOff
            
            #Remove columns no longer of interest
            Keeps <- c("RunID", "StockID", "Age", "FisheryID", "TimeStep", "TotMort")
            PreMortTab <- PreMortTab[, (names(PreMortTab) %in% Keeps)]
            
            #Adds run info to Mort Tab
            PreMortTab <- merge(PreMortTab, PreRunIDTab, by= "RunID")
            
            PreMortTab <- subset(PreMortTab, RunYear == input$YearAdd)
            
            #Convert integers to characters
            PreMortTab$RunID <- as.character(PreMortTab$RunID)
            PreMortTab$RunYear <- as.character(PreMortTab$RunYear)
            PreMortTab$StockID <- as.character(PreMortTab$StockID)
            PreMortTab$Age <- as.character(PreMortTab$Age)
            PreMortTab$TimeStep <- as.character(PreMortTab$TimeStep)
            PreMortTab$BasePeriodID <- as.character(PreMortTab$BasePeriodID)
            PreMortTab$FisheryID <- as.character(PreMortTab$FisheryID)
            
            #This is the Esc DF to which all ESC data is saved
            PreMainEscDF <- data.frame(RunYear = as.character(), Escapement= as.double(), Stock = as.character())
            #This is the Mort DF to which all mort data is saved
            PreMainMortDF <- data.frame(RunYear = as.character(), TotMort= as.double(), Stock = as.character())
            
            
            
            #This is the Esc DF to which all ESC data is saved
            MainEscDF <- data.frame(RunYear = as.character(), Escapement= as.double(), Stock = as.character())
            #This is the Mort DF to which all mort data is saved
            MainMortDF <- data.frame(RunYear = as.character(), TotMort= as.double(), Stock = as.character())
            
            #This is a list of stocks to perform coastal iterations on
            CoastalStockList <- c("Queets", "Quillayute", "Hoh", "Grays Harbor")
            
            ######## This section corrects Coastal Stocks, but only if prompted by the user
            ######## Post-season and pre-season are done separately.
            ######## This is because there may be a pre-season TAMM but no post-season TAMM.
            if(input$TAMMAdd == "Yes"){
              
              TAMMRow <- subset(TAMMList, year == input$YearAdd)
              
              #Only include pre-season iterations if a TAMM is available in the TAMMList file, if blank do nothing.
              if (is.na(TAMMRow$PreTAMM[1]) == FALSE){
                filePath <- file.path(tempdir(), "Pre_TAMM.xlsm")
                drop_download(paste("Input Files/TAMMs/", TAMMRow$PreTAMM[1], sep = ""), local_path = filePath, overwrite = TRUE, dtoken = token)
                PreTAMMDF <<- read_xlsx(filePath, sheet = '2')
            
                for (i in 1:length(CoastalStockList)){
                  #This subsets data frames to get the list of terminal fisheries and FRAMIDs for the stock
                  TerminalFisheriesList <- unique(subset(CoastalTermFishDF, Stock == CoastalStockList[i])$TerminalFish)
                  CoastalFRAMStks <- unique(subset(StockDF, StockName == CoastalStockList[i])$FRAMWildStocks)
                  
                  #This gets the column number for the stock in table 2 of TAMM
                  TAMMLocation <- subset(CoastalTermFishDF, Stock == CoastalStockList[i])$TAMMPosition[1]
                  
                  #This changes the total mortality of any terminal fisheries for a given stock to 0
                  PreMortTab$TotMort[PreMortTab$StockID %in% CoastalFRAMStks & PreMortTab$FisheryID %in% TerminalFisheriesList] <- 0
                  
                  #This changes the escapement a given stock to 0
                  PreEscTab$Escapement[PreEscTab$StockID %in% CoastalFRAMStks] <- 0
                  
                  #Finds the location in PreMortTab of the terminal fisheries rows for a given stock
                  TermFishLocations <- which(PreMortTab$StockID %in% CoastalFRAMStks & PreMortTab$FisheryID %in% TerminalFisheriesList)
                  
                  #Lumps together FW Sport and Net catches into the first row that has a terminal fishery
                  #FW net and sport are combined in table 3 so no need to differentiate between the two
                  PreMortTab$TotMort[TermFishLocations[1]] <- as.numeric(PreTAMMDF[37,TAMMLocation]) + as.numeric(PreTAMMDF[38,TAMMLocation])
                  
                  #For Gray's Harbor, there is also FW Net
                  if(CoastalStockList[i] == "Grays Harbor"){
                    PreMortTab$TotMort[TermFishLocations[1]] <- PreMortTab$TotMort[TermFishLocations[1]] + as.numeric(PreTAMMDF[36,TAMMLocation])
                  }
                  
                  #Finds the location in PreEscTab of the given stock
                  EscLocations <- which(PreEscTab$StockID %in% CoastalFRAMStks)
                  
                  #Adds in Escapement, like the above, it just sticks it into the first slot if there are multiple stocks
                  PreEscTab$Escapement[EscLocations[1]] <- as.numeric(PreTAMMDF[47,TAMMLocation])
                }
              }
                
              #Only include post-season iterations if a TAMM is available in the TAMMList file, if blank do nothing.
              if (is.na(TAMMRow$PostTAMM[1]) == FALSE){
                #Queets - Postseason
                
                filePath <- file.path(tempdir(), "Post_TAMM.xlsm")
                drop_download(paste("Input Files/TAMMs/", TAMMRow$PostTAMM[1], sep = ""), local_path = filePath, overwrite = TRUE, dtoken = token)
                PostTAMMDF <<- read_xlsx(filePath, sheet = '2')
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                ##############################
                #Added the code below for a run on 2/14/2018
                #It makes it so that post-season only does coastal iterations for Queets...
                #To do coastal iterations for all stocks, please delete lines 320-348 and
                #remove comments from lines 370 to 397.
                CoastalStockList2 <- c("Queets")

                for (i in 1:length(CoastalStockList2)){
                  #This subsets data frames to get the list of terminal fisheries and FRAMIDs for the stock
                  TerminalFisheriesList <- unique(subset(CoastalTermFishDF, Stock == CoastalStockList2[i])$TerminalFish)
                  CoastalFRAMStks <- unique(subset(StockDF, StockName == CoastalStockList2[i])$FRAMWildStocks)

                  #This gets the column number for the stock in table 2 of TAMM
                  TAMMLocation <- subset(CoastalTermFishDF, Stock == CoastalStockList2[i])$TAMMPosition[1]

                  #This changes the total mortality of any terminal fisheries for a given stock to 0
                  MortTab$TotMort[MortTab$StockID %in% CoastalFRAMStks & MortTab$FisheryID %in% TerminalFisheriesList] <- 0

                  #This changes the escapement a given stock to 0
                  EscTab$Escapement[EscTab$StockID %in% CoastalFRAMStks] <- 0

                  #Finds the location in PostMortTab of the terminal fisheries rows for a given stock
                  TermFishLocations <- which(MortTab$StockID %in% CoastalFRAMStks & MortTab$FisheryID %in% TerminalFisheriesList)

                  #Lumps together FW Sport and Net catches into the first row that has a terminal fishery
                  #FW net and sport are combined in table 3 so no need to differentiate between the two
                  MortTab$TotMort[TermFishLocations[1]] <- as.numeric(PostTAMMDF[37,TAMMLocation]) + as.numeric(PostTAMMDF[38,TAMMLocation])

                  #Finds the location in PostEscTab of the given stock
                  EscLocations <- which(EscTab$StockID %in% CoastalFRAMStks)

                  #Adds in Escapement, like the above, it just sticks it into the first slot if there are multiple stocks
                  EscTab$Escapement[EscLocations[1]] <- as.numeric(PostTAMMDF[47,TAMMLocation])
                }
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                # 
                # for (i in 1:length(CoastalStockList)){
                #   #This subsets data frames to get the list of terminal fisheries and FRAMIDs for the stock
                #   TerminalFisheriesList <- unique(subset(CoastalTermFishDF, Stock == CoastalStockList[i])$TerminalFish)
                #   CoastalFRAMStks <- unique(subset(StockDF, StockName == CoastalStockList[i])$FRAMWildStocks)
                #   
                #   #This gets the column number for the stock in table 2 of TAMM
                #   TAMMLocation <- subset(CoastalTermFishDF, Stock == CoastalStockList[i])$TAMMPosition[1]
                #   
                #   #This changes the total mortality of any terminal fisheries for a given stock to 0
                #   MortTab$TotMort[MortTab$StockID %in% CoastalFRAMStks & MortTab$FisheryID %in% TerminalFisheriesList] <- 0
                #   
                #   #This changes the escapement a given stock to 0
                #   EscTab$Escapement[EscTab$StockID %in% CoastalFRAMStks] <- 0
                #   
                #   #Finds the location in PostMortTab of the terminal fisheries rows for a given stock
                #   TermFishLocations <- which(MortTab$StockID %in% CoastalFRAMStks & MortTab$FisheryID %in% TerminalFisheriesList)
                #   
                #   #Lumps together FW Sport and Net catches into the first row that has a terminal fishery
                #   #FW net and sport are combined in table 3 so no need to differentiate between the two
                #   MortTab$TotMort[TermFishLocations[1]] <- as.numeric(PostTAMMDF[37,TAMMLocation]) + as.numeric(PostTAMMDF[38,TAMMLocation])
                
                #if(CoastalStockList[i] == "Grays Harbor"){
                #  MortTab$TotMort[TermFishLocations[1]] <- MortTab$TotMort[TermFishLocations[1]] + as.numeric(PostTAMMDF[36,TAMMLocation])
                #}
                #   
                #   #Finds the location in PostEscTab of the given stock
                #   EscLocations <- which(EscTab$StockID %in% CoastalFRAMStks)
                #   
                #   #Adds in Escapement, like the above, it just sticks it into the first slot if there are multiple stocks
                #   EscTab$Escapement[EscLocations[1]] <- as.numeric(PostTAMMDF[47,TAMMLocation])
                # }
                
                
                
                
                
                
              }
            }
            
            
            
            
            
            
         })
         
         withProgress(message = 'Processing Data/Preparing figures', value = 0, {
           incProgress(1/2, detail = "Processing Data")
           for(i in 1:length(StockList)){
             
             
             # Subsets the stock DF to get the stock of interest
             SubStockDF <- subset(StockDF, StockName == StockList[i])
             
             #Fram stock list
             FRAMStks <- unique(SubStockDF$FRAMWildStocks)
             
             # Subsets escapement DF to get the stock of interest
             SubEscDF <- subset(EscTab, StockID %in% FRAMStks)
             
             StockEscRows <- ddply(SubEscDF, "RunYear",  numcolwise(sum))
             
             StockEscRows$Stock <- StockList[i]
             
             MainEscDF <- rbind(MainEscDF, StockEscRows)
             
             
             # Subsets Mortality DF to get the stock of interest
             SubMortDF <- subset(MortTab, StockID %in% FRAMStks)
             
             StockMortRows <- ddply(SubMortDF, "RunYear",  numcolwise(sum))
             StockMortRows$Stock <- StockList[i]
             
             #Subsets Mortality DF to get stock/only SUS fisheries
             SubMortDFSUS <- subset(MortTab, StockID %in% FRAMStks & as.numeric(FisheryID) < 167)
             
             #Gets the column number with mortalities in it, renames it to SUS Mort
             ColIndex <- which( colnames(SubMortDFSUS)=="TotMort" )
             colnames(SubMortDFSUS)[ColIndex] <- "SUSMort"
             
             #Sums everything by run year
             StockMortSUSRows <- ddply(SubMortDFSUS, "RunYear",  numcolwise(sum))
             
             #Subsets Mortality DF to get stock/only CA fisheries
             SubMortDFCA <- subset(MortTab, StockID %in% FRAMStks & as.numeric(FisheryID) > 166 & as.numeric(FisheryID) < 194)
             
             #Gets the column number with mortalities in it, renames it to CA Mort
             ColIndex <- which( colnames(SubMortDFCA)=="TotMort" )
             colnames(SubMortDFCA)[ColIndex] <- "CAMort"
             
             StockMortCARows <- ddply(SubMortDFCA, "RunYear",  numcolwise(sum))
             
             #Subsets Mortality DF to get stock/only AK fisheries
             SubMortDFAK <- subset(MortTab, StockID %in% FRAMStks & as.numeric(FisheryID) > 193)
             
             #Gets the column number with mortalities in it, renames it to CA Mort
             ColIndex <- which( colnames(SubMortDFAK)=="TotMort" )
             colnames(SubMortDFAK)[ColIndex] <- "AKMort"
             
             StockMortAKRows <- ddply(SubMortDFAK, "RunYear",  numcolwise(sum))
             
             #Merge in SUS,CA, AK
             StockMortRows <- merge(StockMortRows, StockMortSUSRows, by= "RunYear")
             StockMortRows <- merge(StockMortRows, StockMortCARows, by= "RunYear")
             if(nrow(StockMortAKRows) > 0){
              StockMortRows <- merge(StockMortRows, StockMortAKRows, by= "RunYear")
             }
             else{
               StockMortRows$AKMort = 0
             }
             
             
             MainMortDF <- rbind(MainMortDF, StockMortRows)
             
             
             
             
             ####Pre-season#####
             
             
             
             
             # Subsets escapement DF to get the stock of interest
             PreSubEscDF <- subset(PreEscTab, StockID %in% FRAMStks)
             
             PreStockEscRows <- ddply(PreSubEscDF, "RunYear",  numcolwise(sum))
             
             PreStockEscRows$Stock <- StockList[i]
             
             PreMainEscDF <- rbind(PreMainEscDF, PreStockEscRows)
             
             
             # Subsets Mortality DF to get the stock of interest
             PreSubMortDF <- subset(PreMortTab, StockID %in% FRAMStks)
             
             PreStockMortRows <- ddply(PreSubMortDF, "RunYear",  numcolwise(sum))
             PreStockMortRows$Stock <- StockList[i]
             
             #Subsets Mortality DF to get stock/only SUS fisheries
             PreSubMortDFSUS <- subset(PreMortTab, StockID %in% FRAMStks & as.numeric(FisheryID) < 167)
             
             #Gets the column number with mortalities in it, renames it to SUS Mort
             ColIndex <- which(colnames(PreSubMortDFSUS)=="TotMort" )
             colnames(PreSubMortDFSUS)[ColIndex] <- "SUSMort"
             
             #Sums everything by run year
             PreStockMortSUSRows <- ddply(PreSubMortDFSUS, "RunYear",  numcolwise(sum))
             
             #Subsets Mortality DF to get stock/only CA fisheries
             PreSubMortDFCA <- subset(PreMortTab, StockID %in% FRAMStks & as.numeric(FisheryID) > 166 & as.numeric(FisheryID) < 194)
             
             #Gets the column number with mortalities in it, renames it to CA Mort
             ColIndex <- which(colnames(PreSubMortDFCA)=="TotMort" )
             colnames(PreSubMortDFCA)[ColIndex] <- "CAMort"
             
             PreStockMortCARows <- ddply(PreSubMortDFCA, "RunYear",  numcolwise(sum))
             
             #Subsets Mortality DF to get stock/only AK fisheries
             PreSubMortDFAK <- subset(PreMortTab, StockID %in% FRAMStks & as.numeric(FisheryID) > 193)
             
             #Gets the column number with mortalities in it, renames it to CA Mort
             ColIndex <- which(colnames(PreSubMortDFAK)=="TotMort" )
             colnames(PreSubMortDFAK)[ColIndex] <- "AKMort"
             
             PreStockMortAKRows <- ddply(PreSubMortDFAK, "RunYear",  numcolwise(sum))
             
             #Merge in SUS,CA, AK
             PreStockMortRows <- merge(PreStockMortRows, PreStockMortSUSRows, by= "RunYear")
             PreStockMortRows <- merge(PreStockMortRows, PreStockMortCARows, by= "RunYear")
             
             #Only does this if the length of AK morts is greater than 0
             if(nrow(PreStockMortAKRows) > 0){
                PreStockMortRows <- merge(PreStockMortRows, PreStockMortAKRows, by= "RunYear")
             }
             else{
               PreStockMortRows$AKMort = 0
             }
             
             PreMainMortDF <- rbind(PreMainMortDF, PreStockMortRows)
           }

           #Merging data frames
           MainDataDF <- merge(MainEscDF, MainMortDF, by= c("Stock","RunYear"))
           
           #Ocean Cohort = Escapement + Mortality
           MainDataDF$OceanCohort <- MainDataDF$Escapement + MainDataDF$TotMort
           
           #Merging data frames
           PreMainDataDF <- merge(PreMainEscDF, PreMainMortDF, by= c("Stock","RunYear"))
           
           #Ocean Cohort = Escapement + Mortality
           PreMainDataDF$OceanCohort <- PreMainDataDF$Escapement + PreMainDataDF$TotMort
           
           
           #To get abundance objectives, gets only unique rows for columns 3 to 11
           StockDFOBJ <- unique(StockDF[,3:11])
           
           StockDFOBJ$Stock <- StockDFOBJ$StockName
           
           PreMainDataDF <- merge(PreMainDataDF, StockDFOBJ, by = "Stock")
           PreMainDataDF$PreCap <- NA
           
           #Gets the abundance category
           PreMainDataDF$PreAbund <- NA
           
           for (i in 1:nrow(PreMainDataDF)){
             if(PreMainDataDF$Cap.Meth[i] == "None"){
               #do nothing - just makes sure that the loop isn't failing as it checks NAs
             }
             #Inside Puget Sound management unit
             else if(PreMainDataDF$Cap.Meth[i] == "imu"){
               if (PreMainDataDF$LAC[i] > PreMainDataDF$OceanCohort[i]){
                 PreMainDataDF$PreAbund[i] <- "(L)"
                 PreMainDataDF$PreCap[i] <- paste(PreMainDataDF$LAMO[i] * 100, "%", sep ="")
               }
               else if(PreMainDataDF$LAC[i] < PreMainDataDF$OceanCohort[i] & PreMainDataDF$UAC[i] > PreMainDataDF$OceanCohort[i]){
                 PreMainDataDF$PreAbund[i] <- "(M)"
                 PreMainDataDF$PreCap[i] <- paste(PreMainDataDF$MAMO[i] *100, "%", sep = "")
               }
               else{
                 PreMainDataDF$PreAbund[i] <- "(A)"
                 PreMainDataDF$PreCap[i] <- paste(PreMainDataDF$AAMO[i] * 100, "%", sep="")
               }
             }
             #The rules of OMU follow those outlined on page 117 of the 2014 PST annex booklet.
             #E.g., it uses the max exploitation rate of either 20% or (cohort - lower escapement goal)/cohort
             else if (PreMainDataDF$Cap.Meth[i] == "omu"){
               if (PreMainDataDF$LAC[i] > PreMainDataDF$OceanCohort[i]){
                 PreMainDataDF$PreAbund[i] <- "(L)"
                 CapTest <- c(0.2, round((PreMainDataDF$OceanCohort[i]-PreMainDataDF$LEG[i])/PreMainDataDF$OceanCohort[i],3))
                 CapIndex <- which.max(CapTest)
                 PreMainDataDF$PreCap[i] <- paste(CapTest[CapIndex] * 100, "%", sep ="")
               }
               else if(PreMainDataDF$LAC[i] < PreMainDataDF$OceanCohort[i] & PreMainDataDF$UAC[i] > PreMainDataDF$OceanCohort[i]){
                 PreMainDataDF$PreAbund[i] <- "(M)"
                 CapTest <- c(0.2, round((PreMainDataDF$OceanCohort[i]-PreMainDataDF$LEG[i])/PreMainDataDF$OceanCohort[i],3))
                 CapIndex <- which.max(CapTest)
                 PreMainDataDF$PreCap[i] <- paste(CapTest[CapIndex] * 100, "%", sep ="")
               }
               else{
                 PreMainDataDF$PreAbund[i] <- "(A)"
                 CapTest <- c(0.2, round((PreMainDataDF$OceanCohort[i]-PreMainDataDF$LEG[i])/PreMainDataDF$OceanCohort[i],3))
                 CapIndex <- which.max(CapTest)
                 PreMainDataDF$PreCap[i] <- paste(CapTest[CapIndex] * 100, "%", sep ="")
               }
             }
             #Interior Fraser = always low?
             else if(PreMainDataDF$Cap.Meth[i] == "fixed"){
               PreMainDataDF$PreAbund[i] <- "(L)"
               PreMainDataDF$PreCap[i] <- paste(.2 * 100, "%", sep="")
             }
           }
           
           #Gets rid of all the useless columns
           DFDrops <- c("StockName", "LAC", "UAC", "Cap.Meth", "LAMO", "MAMO", "AAMO", "LEG", "UEG", "MU")
           
           for(i in 1:nrow(PreMainDataDF)){
             if (is.na(PreMainDataDF$PreCap[i]) == FALSE & PreMainDataDF$PreCap[i] == "NA%"){
               PreMainDataDF$PreCap[i] = NA
             }
           }
           
           PreMainDataDF<- PreMainDataDF[ , !(names(PreMainDataDF) %in% DFDrops)]
           
           
           
           
           
           
           MainDataDF <- merge(MainDataDF, StockDFOBJ, by = "Stock")
           MainDataDF$PostCap <- NA
           
           #Gets the abundance category
           MainDataDF$PostAbund <- NA
           
           for (i in 1:nrow(MainDataDF)){
             if(MainDataDF$Cap.Meth[i] == "None"){
               #do nothing - just makes sure that the loop isn't failing as it checks NAs
             }
             else if(MainDataDF$Cap.Meth[i] == "imu"){
               if (MainDataDF$LAC[i] > MainDataDF$OceanCohort[i]){
                 MainDataDF$PostAbund[i] <- "(L)"
                 MainDataDF$PostCap[i] <- paste(MainDataDF$LAMO[i] * 100, "%", sep ="")
               }
               else if(MainDataDF$LAC[i] < MainDataDF$OceanCohort[i] & MainDataDF$UAC[i] > MainDataDF$OceanCohort[i]){
                 MainDataDF$PostAbund[i] <- "(M)"
                 MainDataDF$PostCap[i] <- paste(MainDataDF$MAMO[i] *100, "%", sep = "")
               }
               else{
                 MainDataDF$PostAbund[i] <- "(A)"
                 MainDataDF$PostCap[i] <- paste(MainDataDF$AAMO[i] * 100, "%", sep="")
               }
             }
             #The rules of OMU follow those outlined on page 117 of the 2014 PST annex booklet.
             #E.g., it uses the max exploitation rate of either 20% or (cohort - lower escapement goal)/cohort
             else if (MainDataDF$Cap.Meth[i] == "omu"){
               if (MainDataDF$LAC[i] > MainDataDF$OceanCohort[i]){
                 MainDataDF$PostAbund[i] <- "(L)"
                 CapTest <- c(0.2, round((MainDataDF$OceanCohort[i]-MainDataDF$LEG[i])/MainDataDF$OceanCohort[i],3))
                 CapIndex <- which.max(CapTest)
                 MainDataDF$PostCap[i] <- paste(CapTest[CapIndex] * 100, "%", sep ="")
               }
               else if(MainDataDF$LAC[i] < MainDataDF$OceanCohort[i] & MainDataDF$UAC[i] > MainDataDF$OceanCohort[i]){
                 MainDataDF$PostAbund[i] <- "(M)"
                 CapTest <- c(0.2, round((MainDataDF$OceanCohort[i]-MainDataDF$LEG[i])/MainDataDF$OceanCohort[i],3))
                 CapIndex <- which.max(CapTest)
                 MainDataDF$PostCap[i] <- paste(CapTest[CapIndex] * 100, "%", sep ="")
               }
               else{
                 MainDataDF$PostAbund[i] <- "(A)"
                 CapTest <- c(0.2, round((MainDataDF$OceanCohort[i]-MainDataDF$LEG[i])/MainDataDF$OceanCohort[i],3))
                 CapIndex <- which.max(CapTest)
                 MainDataDF$PostCap[i] <- paste(CapTest[CapIndex] * 100, "%", sep ="")
               }
             }
             #Interior Fraser = always low?
             else if(MainDataDF$Cap.Meth[i] == "fixed"){
               MainDataDF$PostAbund[i] <- "(L)"
               MainDataDF$PostCap[i] <- paste(.2 * 100, "%", sep="")
             }
           }
           
           #Gets rid of all the useless columns
           DFDrops <- c("StockName", "LAC", "UAC", "Cap.Meth", "LAMO", "MAMO", "AAMO", "LEG", "UEG", "MU")
           
           for(i in 1:nrow(MainDataDF)){
             if (is.na(MainDataDF$PostCap[i]) == FALSE & MainDataDF$PostCap[i] == "NA%"){
               MainDataDF$PostCap[i] = NA
             }
           }
           MainDataDF<- MainDataDF[ , !(names(MainDataDF) %in% DFDrops)]
           
           #Total ER
           MainDataDF$EstdER <- MainDataDF$TotMort/MainDataDF$OceanCohort
           PreMainDataDF$ModelER <- PreMainDataDF$TotMort/PreMainDataDF$OceanCohort
           
           
           
           
           
           
           MainDataDFGlob <<- MainDataDF
           PreMainDataDFGlob <<- PreMainDataDF
           
           StockOrder <- c("Lower Fraser", "Interior Fraser", "Georgia Strait ML", "Georgia Strait VI",
                                 "Skagit", "Stillaguamish", "Snohomish", "Hood Canal", "US Strait JDF",
                                 "Quillayute","Hoh","Queets","Grays Harbor")
           
           Table1DF <- data.frame(MU = as.character(), PreStatus = as.character(), PreCap = as.character(),
                                  Model = as.character(), PostStatus = as.character(), PostCap = as.character(),
                                  Estd = as.character(), PreEsc = as.character(), PostEsc = as.character(),
                                  PreCohort = as.character(), PostCohort = as.character())
           
           Table2DF <- data.frame(MU = as.character(), SUSPreCap = as.character(), SUSModel = as.character(), SUSPreUnused = as.character(),
                                  SUSPostCap = as.character(), SUSEstd = as.character(), SUSPostUnused = as.character(),
                                  CAPreCap = as.character(), CAModel = as.character(), CAPreUnused = as.character(),
                                  CAPostCap = as.character(), CAEstd = as.character(), CAPostUnused = as.character())
           
           
           
           
           
           
           
           #determines the number of IMU and OMU stocks in low and normal for use later.
           IMUStocks <- as.character(unique(subset(StockDF, Cap.Meth == "imu")$StockName))
           IMUPre <- subset(PreMainDataDF, Stock %in% IMUStocks)
           IMUPost <- subset(MainDataDF, Stock %in% IMUStocks)
           
           IMUNumLowPre <- length(which(IMUPre$PreAbund == "(L)"))
           IMUNumModPre <- length(which(IMUPre$PreAbund == "(M)"))
           
           IMUNumLowPost <- length(which(IMUPost$PostAbund == "(L)"))
           IMUNumModPost <- length(which(IMUPost$PostAbund == "(M)"))
           
           OMUStocks <- as.character(unique(subset(StockDF, Cap.Meth == "omu")$StockName))
           OMUPre <- subset(PreMainDataDF, Stock %in% OMUStocks)
           OMUPost <- subset(MainDataDF, Stock %in% OMUStocks)
           
           OMUNumLowPre <- length(which(OMUPre$PreAbund == "(L)"))
           OMUNumModPre <- length(which(OMUPre$PreAbund == "(M)"))
           
           OMUNumLowPost <- length(which(OMUPost$PostAbund == "(L)"))
           OMUNumModPost <- length(which(OMUPost$PostAbund == "(M)"))
           
           FixedStocks <- as.character(unique(subset(StockDF, Cap.Meth == "fixed")$StockName))
           
           
           
           
           
           
           for(i in 1:length(StockOrder)){
             Tab1Post <- subset(MainDataDF, Stock == StockOrder[i])
             Tab1Pre <- subset(PreMainDataDF, Stock == StockOrder[i])
             
             
             NewRow <- data.frame(MU = StockOrder[i], PreStatus = Tab1Pre$PreAbund[1], PreCap = Tab1Pre$PreCap[1],
                                  Model = paste(round(Tab1Pre$ModelER[1]*100,1), "%", sep =""), PostStatus = Tab1Post$PostAbund[1], 
                                  PostCap = Tab1Post$PostCap[1],Estd = paste(round(Tab1Post$EstdER[1]*100,1),"%",sep=""), 
                                  PreEsc = format(round(Tab1Pre$Escapement[1],0), big.mark=",", scientific = FALSE), 
                                  PostEsc = format(round(Tab1Post$Escapement[1],0), big.mark = ",", scientific = FALSE),
                                  PreCohort = format(round(Tab1Pre$OceanCohort[1],0), big.mark = ",", scientific = FALSE), 
                                  PostCohort = format(round(Tab1Post$OceanCohort[1],0), big.mark = ",", scientific = FALSE))
             Table1DF <- rbind(Table1DF, NewRow)
             
             
             
             #If Fraser...
             if (StockOrder[i] %in% FixedStocks){
               #If the pre-season ER cap is less than 20%...
               if (as.numeric(sub("%", "", Tab1Pre$PreCap[1])) <= 20){
                 SUSPreCap <- .1
               }
               #If the pre-season ER cap is over 20 but less than 40 - this doesn't exist.. Fraser is always 20%
               else if (as.numeric(sub("%", "", Tab1Pre$PreCap[1])) <= 40){
                 SUSPreCap <- .12
               }
               #If the pre-season ER cap is over 40 - this doesn't exist.. Fraser is always 20%
               else{
                 SUSPreCap <- .15
               }
               
               SUSPreUnused <- SUSPreCap - Tab1Pre$SUSMort[1]/Tab1Pre$OceanCohort[1]
               if(SUSPreUnused > 0){
                 CAPreCap <- (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100 - SUSPreCap + SUSPreUnused
               }
               if(SUSPreUnused <= 0){
                 CAPreCap <- (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100 - SUSPreCap
               }
               
               CAPreUnused <- CAPreCap - Tab1Pre$CAMort[1]/Tab1Pre$OceanCohort[1]
             }
             
             #If Puget Sound...
             else if (StockOrder[i] %in% IMUStocks){
               #If in low status
               if (Tab1Pre$PreAbund[1] == "(L)"){
                 #Normal low
                 if(IMUNumLowPre > 1){
                   CAPreCap <- .11
                 }
                 #Composite low
                 else{
                   CAPreCap <- .13
                 }
                 
               }
               else if (Tab1Pre$PreAbund[1] == "(M)"){
                 #Normal moderate
                 if(IMUNumModPre > 1){
                   CAPreCap <- .124 + .13 * (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100
                 }
                 #Composite moderate
                 else{
                   CAPreCap <- .134 + .13 * (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100
                 }
               }
               else{
                 if (as.numeric(sub("%", "", Tab1Pre$PreCap[1])) > 60){
                   CAPreCap <- .024 + .38 * (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100
                 }
                 else{
                   CAPreCap <- .084 + .28 * (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100
                 }
               }
               
               CAPreUnused <- CAPreCap - Tab1Pre$CAMort[1]/Tab1Pre$OceanCohort[1]
               if(CAPreUnused > 0){
                 SUSPreCap <- (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100 - CAPreCap + CAPreUnused
               }
               if(CAPreUnused <= 0){
                 SUSPreCap <- (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100 - CAPreCap
               }
               SUSPreUnused <- SUSPreCap - Tab1Pre$SUSMort[1]/Tab1Pre$OceanCohort[1]
             }
             
             #If Washington Coastal...
             else if (StockOrder[i] %in% OMUStocks){
               #If in low status
               if (Tab1Pre$PreAbund[1] == "(L)"){
                 #Normal low
                 if(IMUNumLowPre > 1){
                   CAPreCap <- .10
                 }
                 #Composite low
                 else{
                   CAPreCap <- .12
                 }
                 
               }
               else if (Tab1Pre$PreAbund[1] == "(M)"){
                 #Normal moderate
                 if(IMUNumModPre > 1){
                   CAPreCap <- .024 + .38 * (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100
                 }
                 #Composite moderate
                 else{
                   CAPreCap <- .054 + .33 * (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100
                 }
               }
               else{
                 CAPreCap <- .024 + .38 * (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100
               }
               
               CAPreUnused <- CAPreCap - Tab1Pre$CAMort[1]/Tab1Pre$OceanCohort[1]
               if(CAPreUnused > 0){
                 SUSPreCap <- (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100 - CAPreCap + CAPreUnused
               }
               if(CAPreUnused <= 0){
                 SUSPreCap <- (as.numeric(sub("%", "", Tab1Pre$PreCap[1])))/100 - CAPreCap
               }
               SUSPreUnused <- SUSPreCap - Tab1Pre$SUSMort[1]/Tab1Pre$OceanCohort[1]
               
             }
             
             #If not assigned a management method...
             else {
               CAPreCap <- NA
               CAPreUnused <- NA
               SUSPreCap <- NA
               SUSPreUnused <- NA
             }
             
             
             
             
             #Postseason
             #If Fraser...
             if (StockOrder[i] %in% FixedStocks){
               #If the pre-season ER cap is less than 20%...
               if (as.numeric(sub("%", "", Tab1Post$PostCap[1])) <= 20){
                 SUSPostCap <- .1
               }
               #If the pre-season ER cap is over 20 but less than 40 - this doesn't exist.. Fraser is always 20%
               else if (as.numeric(sub("%", "", Tab1Post$PostCap[1])) <= 40){
                 SUSPostCap <- .12
               }
               #If the pre-season ER cap is over 40 - this doesn't exist.. Fraser is always 20%
               else{
                 SUSPostCap <- .15
               }
               
               SUSPostUnused <- SUSPostCap - Tab1Post$SUSMort[1]/Tab1Post$OceanCohort[1]
               if(SUSPostUnused > 0){
                 CAPostCap <- (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100 - SUSPostCap + SUSPostUnused
               }
               if(SUSPostUnused <= 0){
                 CAPostCap <- (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100 - SUSPostCap
               }
               
               CAPostUnused <- CAPostCap - Tab1Post$CAMort[1]/Tab1Post$OceanCohort[1]
             }
             
             #If Puget Sound...
             else if (StockOrder[i] %in% IMUStocks){
               #If in low status
               if (Tab1Post$PostAbund[1] == "(L)"){
                 #Normal low
                 if(IMUNumLowPost > 1){
                   CAPostCap <- .11
                 }
                 #Composite low
                 else{
                   CAPostCap <- .13
                 }
                 
               }
               else if (Tab1Post$PostAbund[1] == "(M)"){
                 #Normal moderate
                 if(IMUNumModPost > 1){
                   CAPostCap <- .124 + .13 * (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100
                 }
                 #Composite moderate
                 else{
                   CAPostCap <- .134 + .13 * (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100
                 }
               }
               else{
                 if (as.numeric(sub("%", "", Tab1Post$PostCap[1])) > 60){
                   CAPostCap <- .024 + .38 * (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100
                 }
                 else{
                   CAPostCap <- .084 + .28 * (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100
                 }
               }
               
               CAPostUnused <- CAPostCap - Tab1Post$CAMort[1]/Tab1Post$OceanCohort[1]
               if(CAPostUnused > 0){
                 SUSPostCap <- (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100 - CAPostCap + CAPostUnused
               }
               if(CAPostUnused <= 0){
                 SUSPostCap <- (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100 - CAPostCap
               }
               SUSPostUnused <- SUSPostCap - Tab1Post$SUSMort[1]/Tab1Post$OceanCohort[1]
             }
             
             #If Washington Coastal...
             else if (StockOrder[i] %in% OMUStocks){
               #If in low status
               if (Tab1Post$PostAbund[1] == "(L)"){
                 #Normal low
                 if(IMUNumLowPost > 1){
                   CAPostCap <- .10
                 }
                 #Composite low
                 else{
                   CAPostCap <- .12
                 }
                 
               }
               else if (Tab1Post$PostAbund[1] == "(M)"){
                 #Normal moderate
                 if(IMUNumModPost > 1){
                   CAPostCap <- .024 + .38 * (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100
                 }
                 #Composite moderate
                 else{
                   CAPostCap <- .054 + .33 * (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100
                 }
               }
               else{
                 CAPostCap <- .024 + .38 * (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100
               }
               
               CAPostUnused <- CAPostCap - Tab1Post$CAMort[1]/Tab1Post$OceanCohort[1]
               if(CAPostUnused > 0){
                 SUSPostCap <- (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100 - CAPostCap + CAPostUnused
               }
               if(CAPostUnused <= 0){
                 SUSPostCap <- (as.numeric(sub("%", "", Tab1Post$PostCap[1])))/100 - CAPostCap
               }
               SUSPostUnused <- SUSPostCap - Tab1Post$SUSMort[1]/Tab1Post$OceanCohort[1]
               
             }
             
             #If not assigned a management method...
             else {
               CAPostCap <- NA
               CAPostUnused <- NA
               SUSPostCap <- NA
               SUSPostUnused <- NA
             }
             
             
             
             
             
             
             
             NewRowTab2 <- data.frame(MU = StockOrder[i], SUSPreCap = paste(round(SUSPreCap*100,1), "%",sep=""), 
                                      SUSModel = paste(round((Tab1Pre$SUSMort[1]/Tab1Pre$OceanCohort[1])*100,1), "%", sep = ""), 
                                      SUSPreUnused =  paste(round(SUSPreUnused*100,1), "%",sep=""),
                                      SUSPostCap = paste(round(SUSPostCap*100,1), "%",sep=""), 
                                      SUSEstd = paste(round((Tab1Post$SUSMort[1]/Tab1Post$OceanCohort[1])*100,1), "%", sep = ""), 
                                      SUSPostUnused = paste(round(SUSPostUnused*100,1), "%",sep=""),
                                      CAPreCap = paste(round(CAPreCap*100,1), "%",sep=""), 
                                      CAModel = paste(round((Tab1Pre$CAMort[1]/Tab1Pre$OceanCohort[1])*100,1), "%", sep = ""), 
                                      CAPreUnused = paste(round(CAPreUnused*100,1), "%",sep=""),
                                      CAPostCap = paste(round(CAPostCap*100,1), "%",sep=""), 
                                      CAEstd = paste(round((Tab1Post$CAMort[1]/Tab1Post$OceanCohort[1])*100,1), "%", sep = ""), 
                                      CAPostUnused = paste(round(CAPostUnused*100,1), "%",sep=""))
             
             Table2DF <- rbind(Table2DF, NewRowTab2)
           }
           
           
           colnames(Table1DF)[1] <- "Management Unit"
           colnames(Table1DF)[2] <- "Status"
           colnames(Table1DF)[3] <- "Cap"
           colnames(Table1DF)[4] <- "Model"
           colnames(Table1DF)[5] <- "Status"
           colnames(Table1DF)[6] <- "Cap"
           colnames(Table1DF)[7] <- "Estd"
           colnames(Table1DF)[8] <- "Pre"
           colnames(Table1DF)[9] <- "Post"
           colnames(Table1DF)[10] <- "Pre"
           colnames(Table1DF)[11] <- "Post"
           
           #This changes the NAs in table 2 to blanks instead for prettiness
           Table2DF[Table2DF == "NA%"] <- " "
           
           Table1DFGlob <<- Table1DF
           
           Table2DFGlob <<- Table2DF
           
           #Table 3
           #Aggregate fisheries into groupings
           BCNCTRRow <- data.frame(Fishery = "BC No/Cent Troll", FRAMFish = c(171,172,173))
           BCNCNetRow <- data.frame(Fishery = "BC No/Cent Net", FRAMFish = c(178, 179))
           BCNCSptRow <- data.frame(Fishery = "BC No/Cent Sport", FRAMFish = c(187, 188))
           BCWCVITrollRow <- data.frame(Fishery = "BC WCVI Troll", FRAMFish = c(174, 175))
           BCWCVINetRow <- data.frame(Fishery = "BC WCVI Net", FRAMFish = c(180, 181))
           BCWCVISptRow <- data.frame(Fishery = "BC WCVI Sport", FRAMFish = c(190, 193))
           BCJSNetRow <- data.frame(Fishery = "BC JnstStr Net & Trl", FRAMFish = c(182, 170))
           BCJSSptRow <- data.frame(Fishery = "BC JnstStr Sport", FRAMFish = c(186))
           BCGSSptRow <- data.frame(Fishery = "BC GeoStr Spt & Trl", FRAMFish = c(176,191,192))
           BCGSNetRow <- data.frame(Fishery = "BC GeoStr Net", FRAMFish = c(183))
           BCJDFSptRow <- data.frame(Fishery = "BC JDF Sport", FRAMFish = c(189))
           BCJDFNetRow <- data.frame(Fishery = "BC JDF Net & Troll", FRAMFish = c(178, 179))
           BCFraserRow <- data.frame(Fishery = "BC Fraser Net & Spt", FRAMFish = c(169, 184, 167, 168))
           
           SEAKRow <- data.frame(Fishery = "SEAK All", FRAMFish = c(194, 195, 196, 197, 198))
           WAOceanTrlRow <- data.frame(Fishery = "WA Ocean Troll", FRAMFish = c(34, 35, 36, 38, 39, 42, 43, 79))
           WAOceanSportRow <- data.frame(Fishery = "WA Ocean Sport", FRAMFish = c(33, 37, 40, 41, 45))
           SofFRow <- data.frame(Fishery = "S of Falcon All", FRAMFish = c(1,2,3,4,5,6,7,8,9,19,11,12,13,14,15,16,17,18,19,20,21,22))
           USJDFRow <- data.frame(Fishery = "U.S. JDF All", FRAMFish = c(44,80,81,91,92))
           SJINetRow <- data.frame(Fishery = "San Juan Isl Net", FRAMFish = c(87,88,96,97))
           SJISptRow <- data.frame(Fishery = "San Juan Isl Sport", FRAMFish = c(93))
           PSSptRow <- data.frame(Fishery = "PS Sport (8-13)", FRAMFish = c(106,107,115,118,129,136,152))
           PSNetRow <- data.frame(Fishery = "PS Net (8-13)", FRAMFish = c(101,102,109,110,111,112,119,120,121,122,123,124,125,130,
                                                                          131,132,133,137,138,139,140,141,142,143,144,145,146,153,
                                                                          154,155,156,157,158,159,160))
           FWRow <- data.frame(Fishery = "FW Net & Sport", FRAMFish = c(23,24,25,26,27,28,29,30,31,32,46,47,48,49,50,51,52,53,54,55,56,57,58,
                                                                        59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,82,83,
                                                                        84,85,86,89,90,94,95,98,99,100,103,104,105,108,113,114,116,117,126,127,128,
                                                                        134,135,147,148,149,150,151,161,162,163,164,165,166))
           
           TabFFishDF <- rbind(BCNCTRRow, BCNCNetRow, BCNCSptRow, BCWCVITrollRow, BCWCVINetRow,
                               BCWCVISptRow, BCJSNetRow, BCJSSptRow, BCGSSptRow, BCGSNetRow,
                               BCJDFSptRow, BCJDFNetRow, BCFraserRow, SEAKRow, WAOceanTrlRow,
                               WAOceanSportRow,SofFRow, USJDFRow, SJINetRow, SJISptRow,
                               PSSptRow, PSNetRow, FWRow)
           
           #Column 1 labels
           ColLabs <- c("Fishery Name","BC No/Cent Troll","BC No/Cent Net","BC No/Cent Sport","BC WCVI Troll","BC WCVI Net",
                        "BC WCVI Sport","BC JnstStr Net & Trl","BC JnstStr Sport","BC GeoStr Spt & Trl","BC GeoStr Net",
                        "BC JDF Sport", "BC JDF Net & Troll", "BC Fraser Net & Spt", "B.C.", "WA Ocean Troll",
                        "WA Ocean Sport", "S of Falcon All","U.S. JDF All","San Juan Isl Net","San Juan Isl Sport","PS Sport (8-13)",
                        "PS Net (8-13)", "FW Net & Sport", "Southern U.S.", "SEAK All","Total", "Escapement", "Cohort (Ocean age-3)")
           #Set up blank table
           Table3 <- data.frame(matrix(, nrow = 29, ncol = length(StockOrder)+1))
           
           #Add in column 1
           for (i in 1:length(ColLabs)){
             Table3[i,1] <- ColLabs[i]
           }
           
           #by stock
           for(j in 1:length(StockOrder)){
             #Escapement
             Esc <- sum(subset(MainDataDF, Stock == StockOrder[j])$Escapement)
             
             #Ocean Age 3 Cohort
             Ocean3Cohort <- sum(subset(MainDataDF, Stock == StockOrder[j])$OceanCohort)
             
             #Add in year
             Table3[1,j+1] <- StockOrder[j]
             
             FRAMStks <- unique(subset(StockDF, StockName == StockOrder[j])$FRAMWildStocks)
             
             #Subsets mortalities to only be from fisheries and year of interest
             Table3[2,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC No/Cent Troll")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[3,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC No/Cent Net")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[4,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC No/Cent Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[5,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC WCVI Troll")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[6,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC WCVI Net")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[7,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC WCVI Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[8,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC JnstStr Net & Trl")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[9,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC JnstStr Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[10,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC GeoStr Spt & Trl")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[11,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC GeoStr Net")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[12,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC JDF Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[13,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC JDF Net & Troll")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[14,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC Fraser Net & Spt")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[26,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "SEAK All")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[16,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "WA Ocean Troll")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[17,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "WA Ocean Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[18,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "S of Falcon All")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[19,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "U.S. JDF All")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[20,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "San Juan Isl Net")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[21,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "San Juan Isl Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[22,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "PS Sport (8-13)")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[23,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "PS Net (8-13)")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[24,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery == "FW Net & Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             
             
             #Get total rows
             Table3[15,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery %in% c("BC No/Cent Troll", "BC No/Cent Net", "BC No/Cent Sport", "BC WCVI Troll", "BC WCVI Net", "BC WCVI Sport", "BC JnstStr Net & Trl", "BC JnstStr Sport", "BC GeoStr Spt & Trl", "BC GeoStr Net", "BC JDF Sport", "BC JDF Net & Troll", "BC Fraser Net & Spt"))$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[25,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery %in% c("WA Ocean Troll", "WA Ocean Sport", "S of Falcon All", "U.S. JDF All", "San Juan Isl Net", "San Juan Isl Sport", "PS Sport (8-13)", "PS Net (8-13)", "FW Net & Sport"))$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             Table3[27,j+1] <- paste(round(sum(subset(MortTab, StockID %in%FRAMStks & FisheryID %in% unique(subset(TabFFishDF, Fishery %in% c("BC No/Cent Troll", "BC No/Cent Net", "BC No/Cent Sport", "BC WCVI Troll", "BC WCVI Net", "BC WCVI Sport", "BC JnstStr Net & Trl", "BC JnstStr Sport", "BC GeoStr Spt & Trl", "BC GeoStr Net", "BC JDF Sport", "BC JDF Net & Troll", "BC Fraser Net & Spt","SEAK All", "WA Ocean Troll", "WA Ocean Sport", "S of Falcon All", "U.S. JDF All", "San Juan Isl Net", "San Juan Isl Sport", "PS Sport (8-13)", "PS Net (8-13)", "FW Net & Sport"))$FRAMFish))$TotMort)/Ocean3Cohort, digits = 3) * 100, "%",sep="")
             
             #Gets additional rows -escapement, cohort
             Table3[28,j+1] <- round(Esc, digits = 0)
             Table3[29,j+1] <- round(Ocean3Cohort, digits = 0)
           }
           
           Table3Glob <<- Table3
           
           #This adds in a year identifier, so that as it saves, the user knows what year it comes from
           Table1DFGlob$Year <- input$YearAdd
           Table2DFGlob$Year <- input$YearAdd
           Table3Glob$Year <- input$YearAdd
           
           
           #Upload files to dropbox
           filePath <- file.path(tempdir(), "Table_1.csv")
           write.csv(Table1DFGlob,filePath)
           
           drop_upload(filePath, path = "Annual Report Outputs")
           
           filePath <- file.path(tempdir(), "Table_2.csv")
           write.csv(Table2DFGlob,filePath)
           drop_upload(filePath, path = "Annual Report Outputs")
           
           filePath <- file.path(tempdir(), "Table_3.csv")
           write.csv(Table3Glob,filePath)
           drop_upload(filePath, path = "Annual Report Outputs")
           
           
           
         })
       }
     })
   })
   

   
   #Plot1 refers to the plot in the ui.R, main panel section
   output$Plot1 <- renderPlot({
     #If the password is incorrect, display PSCLogo
     if(input$PasswordAdd != Password){
       plot_jpeg('PSCLogo.jpg')
     }
     
     
     #password is correct
     else {
         
         WelcomeText <- "Welcome to the PSC CoTC annual report automation tool
         
         Please indicate the year of interest and if a TAMM is required
         then click the button to the left  to process data
         Data processing will take several minutes
         
         
         Rules for running this program:
         - Files on dropbox must be up-to-date
         - There must only be one run in the data base per run year
         
         Thanks for using the tool! 
         
         Please email Derek Dapp at derek.dapp@dfw.wa.gov or
         Angelika Hagen-Breaux at angelika.hagen-breaux@dfw.wa.gov
         If you have any questions related to the tool"
         par(mar = c(0,0,0,0))
         plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
         text(x = 0.5, y = 0.5, WelcomeText, 
              cex = 1.6, col = "black")
       }
     })
  

  output$Table <- renderDataTable({
    #If the password is incorrect, display PSCLogo
    if(input$PasswordAdd != Password | input$table == "None"){
      BlankDF
    }
    else if (input$table == "Table 1"){
      Table1DFGlob
    }
    else if (input$table == "Table 2"){
      Table2DFGlob
    }
    else if (input$table == "Table 3"){
      Table3Glob
    }
    else if (input$table == "Post-Season Summary"){
      MainDataDFGlob
    }
    else if (input$table == "Pre-Season Summary"){
      #PreMainDataDFGlob
      PreTAMMDF
    }
  }) 
}
  
)