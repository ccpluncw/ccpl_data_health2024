###############################
#### MODEL SPECIFICATIONS #####
###############################



#in this model: positive coefficients for the "s" parameter indicate a bias toward the "keep" boundary and negative coefficients indicate a bias toward the "donate" boundary
grpVars <- c("symptomQuantity", "Set2")

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
  #add a column for the fixed parameter.  This is needed because we will be adding another b effect later
    x1 <- "default"
    #Every condition gets this boundary
    v1 <- 1

  columnName <- "bConstant"
  df.code <- data.frame(logic = c(x1), value = c(v1))
  GroupByVariables <- grpVars
  parameter <- "b"
  ParameterName <- "bConstant"
  parameterBounds <- c(200, 5, 1)

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

    #symptomQuantityOverallSE
    #add SE effect to overallSE effect model
      x1 <- "symptomQuantity == 'HVO_2-LVO_1'"
      v1 <- 1
      x2 <- "symptomQuantity == 'HVO_1-LVO_2'"
      v2 <- -1
      x3 <- "default"
      v3 <- 0

      columnName <- "symptomQuantitySEColumn"
      df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
      GroupByVariables <- grpVars
      ParameterName <- "sSymptomQuantity"
      parameter <- "s"
      parameterBounds <- c(0.8, -0.8, 0.001)

    symptomQuantitySE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

    #symptomQuantityVC
    #add value change model
      x1 <- "symptomQuantity == 'HVO_2-LVO_1'"
      v1 <- 1
      x2 <- "symptomQuantity == 'HVO_1-LVO_2'"
      v2 <- -1
      x3 <- "default"
      v3 <- 0

    columnName <- "symptomQuantityVCColumn"
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    GroupByVariables <- grpVars
    ParameterName <- "vSymptomQuantity"
    parameter <- "vc"
    parameterBounds <- c(5, -5, 0.01)

    symptomQuantityVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)
		
    #symptomQuantityB
    #add Boundary model
      x1 <- "symptomQuantity == 'HVO_2-LVO_2'"
      v1 <- 1
      x2 <- "symptomQuantity == 'HVO_1-LVO_1'"
      v2 <- -1
      x3 <- "default"
      v3 <- 0

    columnName <- "symptomQuantityBColumn"
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    GroupByVariables <- grpVars
    ParameterName <- "bSymptomQuantity"
    parameter <- "b"
    parameterBounds <- c(100, -100, 1)

    symptomQuantityB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)
		

#########################
### build models
#########################

#Start with symptom severity SE model (from Previous experiment)
ssSEModelList <- NULL
ssSEModelList <- rrwAddParameterEffectListToRRWModel(ssSEModelList, c(freeNSD, freeDB, freeB, severitySE))

# symptom quantity SE
sqBssSEModelList <- rrwAddParameterEffectListToRRWModel(ssSEModelList, c(symptomQuantityB))

# symptom quantity VC
sqVCssSEModelList <- rrwAddParameterEffectListToRRWModel(ssSEModelList, c(symptomQuantityVC))

# symptom quantity SE + symptom quantity VC
sqVCsqBssSEModelList <- rrwAddParameterEffectListToRRWModel(sqBssSEModelList, c(symptomQuantityVC))



allModels <- list(ssSEModelList = ssSEModelList,
  sqBssSEModelList = sqBssSEModelList,
  sqVCssSEModelList = sqVCssSEModelList,
  sqVCsqBssSEModelList = sqVCsqBssSEModelList)

allFixedModels <- NULL
