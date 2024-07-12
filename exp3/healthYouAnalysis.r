library(chMorals)
library(chutils)
library(stringr)

cleanOverlapFile <- TRUE

#set up the new RT variables
fitCol <- "fit.RT"
resCol <- "res.RT"
respChoiceVals <- c("Yes", "No")
item1cols <- c("IA.1", "IA.2")
item2cols <- c("IB.1")
overlapItem1cols <- c("IA1", "IA2")
overlapItem2cols <- c("IB1")

useTwoParameterModel <- TRUE
overlapDataIsComplete <- TRUE

# read in parameters
params<-ch.readMoralsDBfile("healthYouDBfile.txt")

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
data.raw <- read.table(params$moralsTaskDataFile, header = T, sep="\t")
data.ovrlp <- read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

######_____REMOVE PRACTICE TRIALS _____######
data.raw<-data.raw[data.raw$trial_type>=1, ]

### If the following has not been run before, run it
if(cleanOverlapFile) {
    #### remove "not having" in the overlap file so they match the raw data file
    Icolumns <- c("IA1", "IA2", "IB1")
    for (i in Icolumns) {
      print(i)
      #strip leading "not having" to match with datafile
      data.ovrlp[[i]] <- str_remove(data.ovrlp[[i]], "not having ")
      #strip leading "a" to match with datafile
      data.ovrlp[[i]] <- str_remove(data.ovrlp[[i]], "a ")
    }

    write.table(data.ovrlp, file=params$valueOverlapDataFile, append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
}


### do Prep analysis
processedData <- ch.moralsDataPrep(data.raw, data.ovrlp, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = respChoiceVals, item1cols = item1cols, item2cols = item2cols, overlapItem1cols = overlapItem1cols, overlapItem2cols = overlapItem2cols, statsOutputFile = statsOutputFile, params = params, overlapDataIsComplete = overlapDataIsComplete)

### Filter data
analysisReadyData <-ch.moralsFilterDataQ(processedData, "sn", "keybRT", "overlapRound", "correct", c(1,0), statsOutputFile = statsOutputFile, params = params)

### create variables

analysisReadyData$quantity_HVO<-ifelse(analysisReadyData$dirOverlap > 0 & analysisReadyData$nIA == 2, "HVO_2", ifelse(analysisReadyData$dirOverlap > 0 & analysisReadyData$nIA == 1, "HVO_1", ifelse(analysisReadyData$dirOverlap < 0 & analysisReadyData$nIA == 2, "LVO_2", "LVO_1" ) ) ) 

analysisReadyData$modifiers <- recode(analysisReadyData$modifiers, "mild-severe" = "severe")

analysisReadyData$severity_HVO<-ifelse(analysisReadyData$dirOverlap > 0 & analysisReadyData$modifiers == "severe", "HVO_severe",  ifelse(analysisReadyData$dirOverlap > 0 & analysisReadyData$modifiers == "mild", "HVO_mild", ifelse(analysisReadyData$dirOverlap < 0 & analysisReadyData$modifiers == "severe", "LVO_severe", "LVO_mild") ) )

analysisReadyData$severityBias <- ifelse( analysisReadyData$severity_HVO == "HVO_severe" | analysisReadyData$severity_HVO == "LVO_mild", "HVOs_LVOm", "HVOm_LVOs")

analysisReadyData$reference_HVO<-ifelse(analysisReadyData$dirOverlap < 0, "refHVO", "refLVO")

### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <-ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef", respChoiceVals, "correct", c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.gp, file="analysisReadyData.gp.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData.gp, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0),  useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")
write.table(df.dPrime, file="df.dPrime.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
#itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.gp, "Item", "IB.2", "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Donate", "Keep"), params, saveFigures = T, comparisonItemName = "you")

#### For experiments with catagory variable manipulations (e.g., different groups), do an analysis
#### by group
    grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("quantity_HVO"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVals, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = params$minOverlapN)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels, c("quantity_HVO"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)
    setwd(mainDir)

    grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("severityBias"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVals, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = params$minOverlapN)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels, c("severityBias"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)
    setwd(mainDir)

    grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("reference_HVO"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVals, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = params$minOverlapN)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels, c("reference_HVO"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)
    setwd(mainDir)
