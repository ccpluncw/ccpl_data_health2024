library(chMorals)
library(chutils)
library(stringr)

cleanOverlapFile <- FALSE
overlapDataIsComplete <- FALSE

#set up the new RT variables
fitCol <- "fit.RT"
resCol <- "res.RT"
respChoiceVals <- c("Top Item", "Bottom Item")
item1cols <- c("Item1")
item2cols <- c("Item2")
overlapItem1cols = c("IA1")
overlapItem2cols = c("IB1")
useTwoParameterModel <- TRUE

# read in parameters
params<-ch.readMoralsDBfile("healthDBfile.txt")

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
data.raw <-read.table(params$moralsTaskDataFile, header=T, sep="\t", quote="\"")
data.ovrlp <-read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

######_____REMOVE PRACTICE TRIALS _____######
data.raw <- data.raw[data.raw$trial_type >=1, ]
data.raw$Item1 <- recode(data.raw$Item1, "a severe rash" = "severe rash", "a mild rash" = "mild rash", "a mild headache" = "mild headache", "a mild fever" = "mild fever", "a severe headache" = "severe headache", "a severe change in appetite" = "severe change in appetite", "a mild change in appetite" = "mild change in appetite", "a severe fever"  = "severe fever", "a mild change in weight" = "mild change in weight", "a severe change in weight" = "severe change in weight")

data.raw$Item2 <- recode(data.raw$Item2, "a severe rash" = "severe rash", "a mild rash" = "mild rash", "a mild headache" = "mild headache", "a mild fever" = "mild fever", "a severe headache" = "severe headache", "a severe change in appetite" = "severe change in appetite", "a mild change in appetite" = "mild change in appetite", "a severe fever"  = "severe fever", "a mild change in weight" = "mild change in weight", "a severe change in weight" = "severe change in weight")

#first make "mild"==1 and "severe"==2
data.raw$I1severity <- ifelse ( word(data.raw$Item1, 1)=='mild' | word(data.raw$Item1, 1,2)=='a mildly', "mild", "severe")
data.raw$I2severity <- ifelse ( word(data.raw$Item2, 1)=='mild' | word(data.raw$Item2, 1,2)=='a mildly', "mild", "severe")


if(cleanOverlapFile) {
	data.ovrlp$IA1 <- str_remove(data.ovrlp$IA1, "not having ")
	data.ovrlp$IB1 <- str_remove(data.ovrlp$IB1, "not having ")

	#convert mild and severe into quantities
	#first make "mild"==1 and "severe"==2
	data.ovrlp$I1qw <- ifelse (word(data.ovrlp$IA1, 1)=='mild', paste("1", word(data.ovrlp$IA1, 2,-1)),data.ovrlp$IA1)
	data.ovrlp$I1qw <- ifelse (word(data.ovrlp$I1qw, 1)=='severe', paste("2", word(data.ovrlp$I1qw, 2,-1)),data.ovrlp$I1qw)
	data.ovrlp$I1qw <- ifelse (word(data.ovrlp$I1qw, 1,2)=='a mildly', paste("1", word(data.ovrlp$I1qw, 3,-1)),data.ovrlp$I1qw)
	data.ovrlp$I1qw <- ifelse (word(data.ovrlp$I1qw, 1,2)=='a severely', paste("2", word(data.ovrlp$I1qw, 3,-1)),data.ovrlp$I1qw)

	#do the same for item 2
	data.ovrlp$I2qw <- ifelse (word(data.ovrlp$IB1, 1)=='mild', paste("1", word(data.ovrlp$IB1, 2,-1)),data.ovrlp$IB1)
	data.ovrlp$I2qw <- ifelse (word(data.ovrlp$I2qw, 1)=='severe', paste("2", word(data.ovrlp$I2qw, 2,-1)),data.ovrlp$I2qw)
	data.ovrlp$I2qw <- ifelse (word(data.ovrlp$I2qw, 1,2)=='a mildly', paste("1", word(data.ovrlp$I2qw, 3,-1)),data.ovrlp$I2qw)
	data.ovrlp$I2qw <- ifelse (word(data.ovrlp$I2qw, 1,2)=='a severely', paste("2", word(data.ovrlp$I2qw, 3,-1)),data.ovrlp$I2qw)

	write.table(data.ovrlp, file=params$valueOverlapDataFile, append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
}

### do Prep analysis
processedData <- ch.moralsDataPrep(data.raw, data.ovrlp, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = respChoiceVals, item1cols = item1cols, item2cols = item2cols, overlapItem1cols = overlapItem1cols, overlapItem2cols = overlapItem2cols, statsOutputFile = statsOutputFile, params = params, overlapDataIsComplete = overlapDataIsComplete)

### Filter data
analysisReadyData <- ch.moralsFilterDataQ(processedData, "sn", "keybRT", "overlapRound", "correct",c(1,0), statsOutputFile = statsOutputFile, params = params)

analysisReadyData$Set2 <- ifelse((analysisReadyData$I1severity == "severe" & analysisReadyData$I2severity == "mild" &analysisReadyData$direct.xVy > 0) | (analysisReadyData$I1severity == "mild" & analysisReadyData$I2severity == "severe" & analysisReadyData$direct.xVy < 0), "severe_HVO", ifelse((analysisReadyData$I1severity == "mild" & analysisReadyData$I2severity == "severe" & analysisReadyData$direct.xVy > 0) | (analysisReadyData$I1severity == "severe" & analysisReadyData$I2severity == "mild" & analysisReadyData$direct.xVy < 0), "mild_HVO", "same" ) )


### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <- ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef",respChoiceVals, "correct",c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.gp, file="analysisReadyData.gp.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0),  useTwoParameterModel = useTwoParameterModel, params = params, minUniqueOverlaps = 2)
write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
 itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.sn, item1Col = item1cols, item2Col = item2cols, "overlapRound", "dirOverlap","keyDef", respChoiceVal = respChoiceVals, params = params, saveFigures = T)
 
 #### For experiments with quantity variable manipulations, do an analysis a quantity analysis
   ### first plot the directional overlap by quantity grouping
 do.filename <- file.path(itemDir,paste0(params$dt.set,"Severity by Item",".pdf"))
 parOp <- par(mfrow=c(1,1), mai=c(2,1,1,1), omi=c(1,.75,.25,1), las=2, cex=1.25, lwd=2, bty='n', xpd = T)
 overOut <- ch.moralsGetAndPlotQuantDirOverlap(data.ovrlp, "I1qw", "I2qw", "overlap", "direction", c(1,10,40), filename = do.filename, parOp = parOp, cexLegend = 1, lgndPlacement = c(0,1), cex1 = 1.25)

 overOut$meanDO <- round(overOut$meanDO, 3)
 write.table(overOut, file="healthDirectionalOverlapByGroup.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

 par(parOp)
 
 #### For experiments with catagory variable manipulations (e.g., different groups), do an analysis
 #### by group
     grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("Set2"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVals, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = params$minOverlapN)
     ### and plot the data
     setwd(gpDir)
     ch.moralsPlotFitsByGrps(grpFitModels, c("Set2"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)

setwd(mainDir)
