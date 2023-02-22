################################################################################
#                                                                              #
# This function creates dummy variables from existing variables                #
# Arguments:                                                                   #
# var: Name of the variable or variables in character vector                   #
# data: Name of the data frame                                                 #
# bind: TRUE if you want to bind the dummy variables with the original dataset #
# Originally written by ahsanulislam10@gmail.com                               #
#                                                                              #
################################################################################



DataDummy <- function(var, data, bind = FALSE){
  
  if(length(var)==1){
    # if single variable is passed in `var`
    
    # changing the data type of passed variable(s) into factor
    data[,var] <- factor(data[,var])
    
    # using model.matrix() to get individual variable's dummy
    dummy_vars <- data.frame(model.matrix(~0+get(var), data = data))
    
    # changing column names
    colnames(dummy_vars) <- paste0(var, sub(".*var.", "", colnames(dummy_vars)))
    
    dummy_vars_ret <- if(bind == TRUE){
      cbind(data, dummy_vars)
    }else{
      dummy_vars
    }
    
  }else if (length(var)>1){
    # if multiple variables are passed in `var`
    
    # changing the data type of passed variable(s) into factor
    data[,var] <- lapply(data[,var], factor)
    
    counter = 0
    # loop through the variable names starts
    for(i in var){
      counter <- counter + 1
      
      dummy_vars <- data.frame(model.matrix(~0+get(i), data = data))
      colnames(dummy_vars) <- paste0(i, sub("get.i.", "", colnames(dummy_vars)))
      
      if(bind == TRUE){
        # if bind=TRUE
        if(counter > 1){
          dummy_vars_ret <- cbind(dummy_vars_ret, dummy_vars)
        }else{
          dummy_vars_ret <- cbind(data, dummy_vars)
        }
      }else{
        # if bind=FALSE
        if(counter > 1){
          dummy_vars_ret = cbind(dummy_vars_ret, dummy_vars)
        }else{
          dummy_vars_ret = dummy_vars
        }
      }
    }
  }
  
  # return the final data frame
  return(dummy_vars_ret)
}

# Example: 
# data(Arthritis, package = "vcd")
# > DataDummy(var = "Treatment", data = Arthritis) |> head()
# TreatmentPlacebo TreatmentTreated
# 1                0                1
# 2                0                1
# 3                0                1
# 4                0                1
# 5                0                1
# 6                0                1
# > DataDummy(var = c("Treatment","Improved"), data = Arthritis) |> head()
# TreatmentPlacebo TreatmentTreated ImprovedNone ImprovedSome ImprovedMarked
# 1                0                1            0            1              0
# 2                0                1            1            0              0
# 3                0                1            1            0              0
# 4                0                1            0            0              1
# 5                0                1            0            0              1
# 6                0                1            0            0              1
# > DataDummy(var = c("Treatment","Improved"), data = Arthritis, bind = TRUE) |> head()
# ID Treatment  Sex Age Improved TreatmentPlacebo TreatmentTreated ImprovedNone ImprovedSome ImprovedMarked
# 1 57   Treated Male  27     Some                0                1            0            1              0
# 2 46   Treated Male  29     None                0                1            1            0              0
# 3 77   Treated Male  30     None                0                1            1            0              0
# 4 17   Treated Male  32   Marked                0                1            0            0              1
# 5 36   Treated Male  46   Marked                0                1            0            0              1
# 6 23   Treated Male  58   Marked                0                1            0            0              1






# caret's dummyVars() function cannot deal with ordered factors, whereas DataDummy can.
# > require(caret)
# > noNames <- dummyVars(~ Treatment + Improved, data = Arthritis, sep = "")
# > head(predict(noNames, Arthritis))
# TreatmentPlacebo TreatmentTreated    Improved.L Improved.Q
# 1                0                1 -7.850462e-17 -0.8164966
# 2                0                1 -7.071068e-01  0.4082483
# 3                0                1 -7.071068e-01  0.4082483
# 4                0                1  7.071068e-01  0.4082483
# 5                0                1  7.071068e-01  0.4082483
# 6                0                1  7.071068e-01  0.4082483
# > Arthritis$Improved <- factor(Arthritis$Improved, ordered = F)
# > noNames <- dummyVars(~ Treatment + Improved, data = Arthritis, sep = "")
# > head(predict(noNames, Arthritis))
# TreatmentPlacebo TreatmentTreated ImprovedNone ImprovedSome ImprovedMarked
# 1                0                1            0            1              0
# 2                0                1            1            0              0
# 3                0                1            1            0              0
# 4                0                1            0            0              1
# 5                0                1            0            0              1
# 6                0                1            0            0              1