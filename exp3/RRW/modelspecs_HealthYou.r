###############################
#### MODEL SPECIFICATIONS #####
###############################



#in this model: positive coefficients for the "s" parameter indicate a bias toward the "keep" boundary and negative coefficients indicate a bias toward the "donate" boundary
grpVars <- c("reference_HVO", "quantity_HVO", "severityBias")

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
    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
    v1 <- 1

  columnName <- "bConstant"
  df.code <- data.frame(logic = c(x1), value = c(v1))
  GroupByVariables <- grpVars
  parameter <- "b"
  ParameterName <- "bConstant"
  parameterBounds <- c(200, 5, 1)

  freeB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

    #symptomQuantityB
    #add Boundary model
      x1 <- "quantity_HVO == 'HVO_2' | quantity_HVO == 'LVO_2'"
      v1 <- 1
      x2 <- "default"
      v2 <- 0

    columnName <- "symptomQuantityBColumn"
    df.code <- data.frame(logic = c(x1,x2), value = c(v1,v2))
    GroupByVariables <- grpVars
    ParameterName <- "bSymptomQuantity"
    parameter <- "b"
    parameterBounds <- c(100, -100, 1)

    symptomQuantityB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

    #symptom quantity VC
    #add VC effect 
      x1 <- "quantity_HVO == 'HVO_2'"
      v1 <- 1
      x2 <- "quantity_HVO == 'LVO_2'"
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


    #symptom Severity SE
     x1 <- "severityBias == 'HVOs_LVOm'"
      v1 <- 1
      x2 <- "severityBias == 'HVOm_LVOs'"
      v2 <- -1
      x3 <- "default"
      v3 <- 0

      columnName <- "symptomSeveritySEColumn"
      df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
      GroupByVariables <- grpVars
      ParameterName <- "sSymptomSeverity"
      parameter <- "s"
      parameterBounds <- c(0.95, -0.95, 0.001)

    symptomSeveritySE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

    #refValue VC
    #add SE effect to overallSE effect model
      x1 <- "reference_HVO == 'refHVO'"
      v1 <- 1
      x2 <- "reference_HVO == 'refLVO'"
      v2 <- -1
      x3 <- "default"
      v3 <- 0

      columnName <- "refValueVCColumn"
      df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
      GroupByVariables <- grpVars
      ParameterName <- "vRefValue"
      parameter <- "vc"
      parameterBounds <- c(5, -5, 0.01)

    refValueVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#########################
### build models
#########################

simpleModelList <- NULL
simpleModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(freeNSD, freeDB, freeB))

# this is the base model
ssSEModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(symptomSeveritySE))

# 1-way
sqVCssSEModelList <- rrwAddParameterEffectListToRRWModel(ssSEModelList, c(symptomQuantityVC))
rVCssSEModelList <- rrwAddParameterEffectListToRRWModel(ssSEModelList, c(refValueVC))
sqBssSEModelList <- rrwAddParameterEffectListToRRWModel(ssSEModelList, c(symptomQuantityB))

# 2-way
sqBsqVCssSEModelList <- rrwAddParameterEffectListToRRWModel(sqVCssSEModelList, c(symptomQuantityB))
rVCsqVCssSEModelList <- rrwAddParameterEffectListToRRWModel(sqVCssSEModelList, c(refValueVC))
rVCsqBssSEModelList <- rrwAddParameterEffectListToRRWModel(sqBssSEModelList, c(refValueVC))

# 3-way
rVCsqBsqVCssSEModelList <- rrwAddParameterEffectListToRRWModel(sqBsqVCssSEModelList, c(refValueVC))

allModels <- list(ssSEModelList= ssSEModelList,
  sqVCssSEModelList = sqVCssSEModelList,
  rVCssSEModelList=rVCssSEModelList,
  sqBssSEModelList=sqBssSEModelList,
  sqBsqVCssSEModelList = sqBsqVCssSEModelList,
  rVCsqVCssSEModelList = rVCsqVCssSEModelList,
  rVCsqBssSEModelList = rVCsqBssSEModelList,
  rVCsqBsqVCssSEModelList = rVCsqBsqVCssSEModelList)

allFixedModels <- NULL
