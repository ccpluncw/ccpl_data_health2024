###############################
#### MODEL SPECIFICATIONS #####
###############################



#in this model: positive coefficients for the "s" parameter indicate a bias toward the "keep" boundary and negative coefficients indicate a bias toward the "donate" boundary
grpVars <- c("Set2")

#freeNSD
#Add NSD as a fixed parameter (NSD = 1)
#Here I group by the three variables but add not effects for them.  This will split the data properly
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- grpVars
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(7, 1, 0.01)

  freeNSD  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeDB
  #Add DB as a free parameter
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "db"
  ParameterName <- "db"
  parameterBounds <- c(0.5, 0, 0.001)

  freeDB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeB
  #Add B as a free parameter
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "b"
  ParameterName <- "b"
  parameterBounds <- c(200, 5, 0.01)

  freeB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


  #severitySE
  #now assess whether there is a bias induced by the left (or right) item with "leftItem" by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
  #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
    x1 <- "Set2 == 'severe_HVO'"
    #when the leftItem is LVO then bias towards the upperBound (choose HVO: bias = right Item)
    v1 <- 1
    x2 <- "Set2 == 'mild_HVO'"
    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
    v2 <- -1
    x3 <- "default"
    v3 <- 0

    #this is the columnName of the dummy/effect variable
    columnName <- "severitySEColumn"
    #this dataframe contains the coding inforamtion of the dummy/effect variable
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
    GroupByVariables <- grpVars
    #this is the name given to the parameter that will measure the effect of this dummy/effect variable
    ParameterName <- "s_Severity"
    #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
    parameter <- "s"
    #These are the bounds of the parameter values: c(high, low, interval)
    parameterBounds <- c(0.8, -0.8, 0.001)

    #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
    severitySE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


	  #severityVC
	  #now assess whether there is a bias induced by the left (or right) item with "leftItem" by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
	  #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
	    x1 <- "Set2 == 'severe_HVO'"
	    #when the leftItem is LVO then bias towards the upperBound (choose HVO: bias = right Item)
	    v1 <- 1
	    x2 <- "Set2 == 'mild_HVO'"
	    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
	    v2 <- -1
	    x3 <- "default"
	    v3 <- 0

	    #this is the columnName of the dummy/effect variable
	    columnName <- "severityVCColumn"
	    #this dataframe contains the coding inforamtion of the dummy/effect variable
	    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
	    #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
	    GroupByVariables <- grpVars
	    #this is the name given to the parameter that will measure the effect of this dummy/effect variable
	    ParameterName <- "vc_Severity"
	    #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
	    parameter <- "vc"
	    #These are the bounds of the parameter values: c(high, low, interval)
	    parameterBounds <- c(5, -5, 0.01)

	    #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
	    severityVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


#########################
### build models
#########################


simpleModelList <- NULL
simpleModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(freeNSD, freeDB, freeB))

#severity start bias
severitySEModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(severitySE))

#severity start bias
severityVCModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(severityVC))

#severity start bias
severityVCseveritySEModelList <- rrwAddParameterEffectListToRRWModel(severitySEModelList, c(severityVC))


allModels <- list(simpleModelList= simpleModelList,
    							severitySEModelList = severitySEModelList,
									severityVCModelList = severityVCModelList,
									severityVCseveritySEModelList = severityVCseveritySEModelList)

allFixedModels <- NULL
