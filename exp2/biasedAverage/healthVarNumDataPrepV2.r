library(chMorals)
library(chutils)
library(stringr)

cleanOverlapFile <- FALSE

#set up the new RT variables
fitCol <- "fit.RT"
resCol <- "res.RT"
useTwoParameterModel <- TRUE
overlapDataIsComplete <- TRUE
respChoiceVal <- c("Top Item", "Bottom Item")
item1cols <- c("IA.1", "IA.2")
item2cols <- c("IB.1", "IB.2")
overlapItem1cols <- c("IA1", "IA2")
overlapItem2cols <- c("IB1", "IB2")

# read in parameters
params<-ch.readMoralsDBfile("moralsDBfile.txt")

#set up the group and item directories
mainDir <- getwd()
ch.newDir (mainDir, params$gpSubDir)
gpDir <- getwd()
setwd(mainDir)

ch.newDir (mainDir, params$itemSubDir)
itemDir <- getwd()
setwd(mainDir)

statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))
sink(statsOutputFile, append = F)
  cat("\n***** New Run ****\n\n")
sink(NULL)

### read in data
data.raw <-read.table(params$moralsTaskDataFile, header=T, sep="\t")
data.raw[is.na(data.raw)] <- "NA"
# data.raw[data.raw == "NA"] <- NA
# data.raw[data.raw == "N/A"] <- NA
# data.raw[data.raw == "na"] <- NA
data.ovrlp <-read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

Icolumns <- c("IA.1", "IA.2", "IB.1", "IB.2")
for (i in Icolumns) {
data.raw[[i]] <- ifelse (data.raw[[i]] =='mild abnormal mole', "mildly abnormal mole",data.raw[[i]])
data.raw[[i]] <- ifelse (data.raw[[i]] =='severe abnormal mole', "severely abnormal mole",data.raw[[i]])
}

### If the following has not been run before, run it
if(cleanOverlapFile) {
    #### remove "not having" in the overlap file so they match the raw data file
    Icolumns <- c("IA1", "IA2", "IB1", "IB2")
    for (i in Icolumns) {
      print(i)
      #strip leading "not having" to match with datafile
      data.ovrlp[[i]] <- str_remove(data.ovrlp[[i]], "not having ")
      #strip leading "a" to match with datafile
      data.ovrlp[[i]] <- str_remove(data.ovrlp[[i]], "a ")
    }

    #convert mild and severe into quantities
    #this is for the quantity analysis
    # make "mild"==1 and "severe"==2
    for (i in Icolumns) {
      out <- paste(i,"q", sep="")
      print(out)
      data.ovrlp[[out]] <- ifelse (word(data.ovrlp[[i]], 1)=='mild', paste("1", word(data.ovrlp[[i]], 2,-1)),data.ovrlp[[i]])
      data.ovrlp[[out]] <- ifelse (word(data.ovrlp[[out]], 1)=='severe', paste("2", word(data.ovrlp[[out]], 2,-1)),data.ovrlp[[out]])
      data.ovrlp[[out]] <- ifelse (word(data.ovrlp[[out]], 1,2)=='mildly', paste("1", word(data.ovrlp[[out]], 3,-1)),data.ovrlp[[out]])
      data.ovrlp[[out]] <- ifelse (word(data.ovrlp[[out]], 1,2)=='severely', paste("2", word(data.ovrlp[[out]], 3,-1)),data.ovrlp[[out]])
    }

    write.table(data.ovrlp, file=params$valueOverlapDataFile, append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
}

######_____REMOVE PRACTICE TRIALS _____######
data.raw <- data.raw[data.raw$trial_type >=1, ]

### do Prep analysis
processedData <- ch.moralsDataPrep(data.raw, data.ovrlp, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = respChoiceVal, item1cols = item1cols, item2cols = item2cols, overlapItem1cols = overlapItem1cols, overlapItem2cols = overlapItem2cols, statsOutputFile = statsOutputFile, params = params, overlapDataIsComplete = overlapDataIsComplete)

### get HVO quantity
processedData$HVOq <- ifelse((processedData$QuantOption1 == 2 & processedData$direct.xVy == 1) | (processedData$QuantOption2 == 2 & processedData$direct.xVy == -1) , 2, 1)
### get LVO quantity
processedData$LVOq <- ifelse((processedData$QuantOption1 == 2 & processedData$direct.xVy == -1) | (processedData$QuantOption2 == 2 & processedData$direct.xVy == 1) , 2, 1)

### Filter data
analysisReadyData <- ch.moralsFilterDataQ(processedData, "sn", "keybRT", "overlapRound", "correct",c(1,0), statsOutputFile = statsOutputFile, params = params)


analysisReadyData$Set1 <- ifelse( ((analysisReadyData$Qual1 == "severe" | analysisReadyData$Qual2 == "severe") & (analysisReadyData$Qual3 != "severe" & analysisReadyData$Qual4 != "severe")), "severe_mild", ifelse( ((analysisReadyData$Qual1 != "severe" & analysisReadyData$Qual2 != "severe") & (analysisReadyData$Qual3 == "severe" | analysisReadyData$Qual4 == "severe")), "mild_severe", "same"))  

analysisReadyData$Set2 <- ifelse( (analysisReadyData$Set1 == "severe_mild"  & analysisReadyData$direct.xVy > 0) | (analysisReadyData$Set1 == "mild_severe" & analysisReadyData$direct.xVy < 0), "severe_HVO", ifelse((analysisReadyData$Set1 == "severe_mild"  & analysisReadyData$direct.xVy < 0) | (analysisReadyData$Set1 == "mild_severe" & analysisReadyData$direct.xVy > 0), "mild_HVO", "same" )) 

analysisReadyData$symptomQuantity<-paste("HVO_",analysisReadyData$HVOq,"-LVO_", analysisReadyData$LVOq, sep="")

### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <- ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef",respChoiceVal, "correct",c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.gp, file="analysisReadyData.gp.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0),  useTwoParameterModel = useTwoParameterModel, params = params, minUniqueOverlaps = 2)
write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")
write.table(df.dPrime, file="df.dPrime.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)


#### For experiments with catagory variable manipulations (e.g., different groups), do an analysis
#### by group
#### this one is by side
    grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("Set2"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVal, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = 0)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels, c("Set2"), "overlapRound", analysisReadyData.gp, filenameID = params$dt.set)
    setwd(mainDir)

    grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("symptomQuantity"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVal, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = 0)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels, c("symptomQuantity"), "overlapRound", analysisReadyData.gp, filenameID = params$dt.set)
    setwd(mainDir)

    grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("Set2", "symptomQuantity"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVal, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = 0)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels, c("Set2", "symptomQuantity"), "overlapRound", analysisReadyData.gp, filenameID = params$dt.set)
    setwd(mainDir)
