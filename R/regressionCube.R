# Libaries necessary: fmsb
# load_dataset("/Users/paul/Desktop/patients-100k.csv")
# matrix <- r_squared_matrix(dependent = "gender")
pkg.env <- new.env()
pkg.env$data <- NA

library(rJava)
library(RWeka)

'load_dataset' <- function (csv_file, isURL=TRUE, load_dictionary=FALSE){ #, type_filepath) {
  #csv_filepath <- "/Users/paul/Tresors/Regresson Cubes/js-html/prototype/data/breast_fat.csv"
  #type_filepath <- "/Users/paul/Tresors/SHIP Breast Fat Dataset/Breast Fat Dataset/data/dictionary.json"
  type_filepath <- "/Users/paul/Tresors/SHIP Breast Fat Dataset/Breast Fat Dataset/data/dictionary_new_names.json"
  if (isURL) {
    pkg.env$data <- read.csv(url(csv_file), header = TRUE)
    data <- read.csv(url(csv_file), header = TRUE)
  }
  else {
    pkg.env$data <- read.csv(csv_file, header = TRUE)
    data <- read.csv(csv_file, header = TRUE)
  }
  
  if (load_dictionary) {
    library(rjson)
    dictionary <- fromJSON(file = type_filepath)
    # Extract the variable types
    variable_types = c()
    variable_names <- names(data)
    for (i in 1:length(data)) {
      current_variable_name <- variable_names[[i]]
      current_variable_dict <- dictionary[[current_variable_name]]
      current_variable_type <- "factor"
      if (!is.null(current_variable_dict)) {
        if (current_variable_dict$type == "numerical")
          data[[current_variable_name]] <- as.numeric(data[[current_variable_name]])
        else if (current_variable_dict$type == "ordinal" | current_variable_dict$type == "nominal")
          data[[current_variable_name]] <- as.factor(data[[current_variable_name]])
        else if (current_variable_dict$type == "dichotomous") {
          data[[current_variable_name]] <- as.factor(data[[current_variable_name]])
        }
      }
      else
        print(paste0(current_variable_name, " has no dictionary entry"))
      variable_types <- c(variable_types, current_variable_type)
    }
  }
  return(data)
}

# This function should be deleted as the calculation is now formula based
'constuct_formula' <- function(variables, operators, x, y, z) {
  result_formula <- ''
  # Iterate over all variables
  for (i in 1:length(variables)) {
    current_variable = variables[[i]];
    # Replace placeholders with current x, y and z values
    if (current_variable == 'x') current_variable = x;
    if (current_variable == 'y') current_variable = y;
    if (current_variable == 'z') current_variable = z;
    # If it is not the last variable, append it together with next operator
    if (i != length(variables))
      result_formula <- paste0(result_formula, current_variable, operators[[i]])
    else
      result_formula <- paste0(result_formula, current_variable)
    
    if (i == 1)
      dependent_variable = current_variable
  }
  return(c(formula(result_formula), dependent_variable, result_formula))
}

# Calculates important varibles based on correlation based feature selection (CFS)
# using the RWeka binding.
# @param: data: input data frame
# @param: dependent: name of the dependent variables
# @return: array of variable names
'correlation_based_feature_selection' <- function(data, dependent) {
  # Create the Weka filter
  attribute_selection <- make_Weka_filter("weka/filters/supervised/attribute/AttributeSelection") 
  
  target_formula <- as.formula(paste0(dependent, '~.'))
  attribute_selection_result <- try(attribute_selection(formula=target_formula, data=data, na.action = na.pass, control =Weka_control(
    E="weka.attributeSelection.CfsSubsetEval -P 1 -E 1",
    S="weka.attributeSelection.BestFirst -D 1 -N 5"
  )), silent = FALSE)
  
  # Throw error when reduction fails!
  if(class(attribute_selection_result) == "try-error") {
    message(paste0("Correlation-based feature selection fails for ", dependent))
    return()
  }
  
  return(colnames(attribute_selection_result))
}

# data <- load_dataset('/Users/paul/Desktop/patients-100k.csv', FALSE)
# data <- load_dataset('/Users/paul/Desktop/breast_fat_small.csv', FALSE)
# data <- load_dataset('/Users/paul/Tresors/SHIP Breast Fat Dataset/Breast Fat Dataset/breast_fat.csv', FALSE)
# operators = c('~', '+', '-');
# variables = c('z', 'x', 'y');
'performance_test' <- function() {
  data <- load_dataset('/Users/paul/Desktop/patients-100k.csv', FALSE)
  print('GLM')
  system.time(glm(formula = as.formula('gender~age+bmi'), data = data, family='binomial'))
  print('SpeedGLM')
  system.time(speedglm::speedglm(formula = as.formula('gender~age+bmi'), data = data, family=binomial(link = "logit")))
  print('lrm')
  system.time(rms::lrm(formula = as.formula('gender~age+bmi'), data = data))
  system.time(rms::lrm(formula = as.formula('gender~age+chd'), data = data))
  print('lrm fit')
  system.time(rms::lrm.fit(cbind(data$age, data$bmi), data$gender))
  
  # Model matrices do not include missing data here
  model_matrix <- model.matrix(as.formula('gender~age+chd'), data)
  model_matrix <- model_matrix[,1-2] # Drop 'intercept' row
  system.time(rms::lrm.fit(model_matrix, data$gender))
  system.time(rms::lrm.fit(model.matrix(as.formula('gender~age+chd'), data)[,1-2], data$gender))
  
  #model_matrix <- model.matrix(as.formula('age~gender+chd'), data)
  #system.time(lm.fit(model_matrix, y = data$age))
  system.time(lm.fit(model.matrix(as.formula('age~gender+chd'), data), y = data$age))
  system.time(lm('age~gender+chd', data))
}

# The function takes the formulas as input and iterates over them
'r_squared_matrix_formula_serial' <- function(data, formulas) {
  variable_classes <- lapply(data, class)
  # Iterate over all formulas given in the array
  for (i in 1:nrow(formulas)) {
    current_formula = formulas[i,]
    current_formula_string <- current_formula$formula
    dependent_class <- variable_classes[current_formula$dependentVariable]
    
    # If current class is numeric, apply Linear Regression
    if (dependent_class == 'numeric')
      model <- try(lm(formula = as.formula(current_formula_string), data = data), silent = TRUE)
    else
      model <- try( rms::lrm(formula = as.formula(current_formula_string), data = data), silent = TRUE)
    
    # If binning fails, return null
    if(class(model) == "try-error") {
      message(paste0("'", current_formula_string, "' failed!"))
    } else {
      if (dependent_class == 'numeric') {
        model_summary <- summary(model)
        formulas[i,'rSquared'] <- model_summary$r.squared
      }
      else
        formulas[i,'rSquared'] <- model$stats[['R2']]
    }
  }
  return(formulas)
}

# The function takes the formulas as input and iterates over them
'r_squared_matrix_formula' <- function(data, formulas, parallel=TRUE) {
  #save(list = c("formulas"), file = '/Users/paul/Desktop/formulas.rtmp')
  library(parallel)
  variable_classes <- lapply(data, class)
  workerFunction <- function(current_formula) {
    current_formula_string <- current_formula[,'formula']
    dependent_class <- variable_classes[current_formula[,'dependentVariable']]
    
    # If current class is numeric, apply Linear Regression
    if (dependent_class == 'numeric')
      model <- try(lm(formula = as.formula(current_formula_string), data = data), silent = TRUE)
    else
      model <- try( rms::lrm(formula = as.formula(current_formula_string), data = data), silent = TRUE)
    
    # If calculation fails, return null
    if(class(model) == "try-error") {
      message(paste0("'", current_formula_string, "' failed!"))
      # Set empty rSquared, otherwise the object will be assembled false
      current_formula['rSquared'] <- ''
    } else {
      if (dependent_class == 'numeric') {
        model_summary <- summary(model)
        current_formula['rSquared'] <- model_summary$r.squared
      }
      else
        current_formula['rSquared'] <- model$stats[['R2']]
    }
    return(current_formula)
  }
  # Convert the formula data frame into a list of entries
  formulas_matrix <- as.matrix(formulas)
  formulas_list <- lapply(1:NROW(formulas_matrix), function(i) formulas_matrix[i,,drop=FALSE])
  # detect the number of cores and start the calculation process!
  numWorkers <- detectCores();
  if (parallel)
    res <- mclapply(formulas_list, workerFunction, mc.cores = numWorkers)
  else
    res <- lapply(formulas_list, workerFunction)
  
  # Reconstruct a data frame from the result list
  formulas_names <- names(formulas)
  # when other metrics are added, they also need to get a new name here!
  formulas_names_with_rSquared <- c(formulas_names, 'rSquared')
  # concat results to a data frame
  result <- data.frame(matrix(unlist(res), nrow=length(formulas_list), byrow=T))
  # If no R-squared values are found at all during the calulcation, simply assigning
  # the new names array would result in an error. So we check if there are more dimensions
  # than before!
  if (length(formulas) == length(result))
    names(result) <- formulas_names
  else
    names(result) <- formulas_names_with_rSquared
  
  return(result);
}