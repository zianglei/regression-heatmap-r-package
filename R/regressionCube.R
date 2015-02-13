# Libaries necessary: fmsb
# load_dataset("/Users/paul/Desktop/patients-100k.csv")
# matrix <- r_squared_matrix(dependent = "gender")
pkg.env <- new.env()
pkg.env$data <- NA

'load_dataset' <- function (csv_file, isURL=TRUE){ #, type_filepath) {
  #csv_filepath <- "/Users/paul/Tresors/Regresson Cubes/js-html/prototype/data/breast_fat.csv"
  #type_filepath <- "/Users/paul/Tresors/Regresson Cubes/js-html/prototype/data/dictionary.json"
  if (isURL) {
    pkg.env$data <- read.csv(url(csv_file), header = TRUE)
    data <- read.csv(url(csv_file), header = TRUE)
  }
  else {
    pkg.env$data <- read.csv(csv_file, header = TRUE)
    data <- read.csv(csv_file, header = TRUE)
  }
  #   library(rjson)
  #   dictionary <- fromJSON(file = type_filepath)
  #   # Extract the variable types
  #   variable_types = c()
  #   variable_names <- names(data)
  #   for (i in 1:length(data)) {
  #     current_variable_name <- variable_names[[i]]
  #     current_variable_dict <- dictionary[[current_variable_name]]
  #     current_variable_type <- "factor"
  #     if (!is.null(current_variable_dict))
  #       if (current_variable_dict$type == "numerical")
  #         current_variable_type <- "numeric"
  #       else if (current_variable_dict$type == "ordinal" | current_variable_dict$type == "nominal")
  #         current_variable_type <- "factor"
  #       else if (current_variable_dict$type == "dichotomous")
  #         current_variable_type <- "logical"
  #     else
  #       print(paste0(current_variable_name, " has no dictionary entry"))
  #     variable_types <- c(variable_types, current_variable_type)
  #   }
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

# data <- load_dataset('/Users/paul/Desktop/patients-100k.csv', FALSE)
# data <- load_dataset('/Users/paul/Desktop/breast_fat_small.csv', FALSE)
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

# This function should be deleted as the calculation is now formula based
'r_squared_matrix' <- function(data, z, operators, variables, force_calculation = FALSE, use_fastLm = FALSE) {
  # filename <- paste0("vardumps/goodness_of_fit_matrix_", z, ".Rdmped")
  # if (file.exists(filename) && !force_calculation) {
  #   load(file = filename)
  #   return(goodness_of_fit_matrix)
  # }
  # data <- pkg.env$data
  #print(operators)
  #print(variables)
  # Get Class for each group
  variable_classes <- lapply(data, class)
  variable_names <- colnames(data)
  # Create result matrix
  goodness_of_fit_matrix <- matrix(0, length(variable_names), length(variable_names))
  row.names(goodness_of_fit_matrix) <- variable_names
  colnames(goodness_of_fit_matrix) <- variable_names
  # Class of z variable
  dependent_class <- variable_classes[z]
  # Iterate over all variables
  #for (i in 2:2) {
  for (i in 1:length(variable_names)) {
    current_independent_variable1_name <- variable_names[[i]]
    # Iterate over all other variables
    for (j in 1:length(variable_names)) {
      # No correlation of variables with each other
      #if (i != j && i < j) {
      if (i != j) {
        current_independent_variable2_name <- variable_names[[j]]
        # First element contains the formula, the second one the dependent variable
        formula_result <- constuct_formula(variables, operators, current_independent_variable1_name, current_independent_variable2_name, z)
        current_formula <- formula_result[[1]]
        #current_formula <- formula('age~bmi+smoking')
        dependent_class <- variable_classes[formula_result[[2]]]
        #dependent_class <- 'numeric'
        # If current class is numeric, apply Linear Regression
        if (dependent_class == 'numeric')
            model <- try(lm(formula = current_formula, data = data), silent = TRUE)
        else
          model <- try( rms::lrm(formula = current_formula, data = data), silent = TRUE)

        # If binning fails, return null
        if(class(model) == "try-error") {
          message(paste0("'", formula_result[[3]], "' failed!"))
        } else {
          #coefficient <- model$coefficients[[2]]
          if (dependent_class == 'numeric') {
            model_summary <- summary(model)
            goodness_of_fit_matrix[i,j] <- model_summary$r.squared
          }
          else
            goodness_of_fit_matrix[i,j] <- model$stats[['R2']]
        }
      }
    }
  }
  #save(list = c("goodness_of_fit_matrix"), file = filename)
  return(goodness_of_fit_matrix)
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
'r_squared_matrix_formula' <- function(data, formulas, parallel='true') {
  #save(list = c("formulas"), file = '/Users/paul/Desktop/formulas.rtmp')
  library(parallel)
  variable_classes <- lapply(data, class)
  workerFunction <- function(current_formula) {
    #global_current_formula <<- current_formula
    current_formula_string <- current_formula[,'formula']
    dependent_class <- variable_classes[current_formula[,'dependentVariable']]
    
    # If current class is numeric, apply Linear Regression
    if (dependent_class == 'numeric')
      model <- try(lm(formula = as.formula(current_formula_string), data = data), silent = TRUE)
    else
      model <- try( rms::lrm(formula = as.formula(current_formula_string), data = data), silent = TRUE)
    
    # If binning fails, return null
    if(class(model) == "try-error") {
      message(paste0("'", current_formula_string, "' failed!"))
      #current_formula['rSquared'] = 0;
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
  formulas_matrix <- as.matrix(formulas)
  formulas_list <- lapply(1:NROW(formulas_matrix), function(i) formulas_matrix[i,,drop=FALSE])
  numWorkers <- detectCores();
  if (parallel)
    res <- mclapply(formulas_list, workerFunction, mc.cores = numWorkers)
  else
    res <- lapply(formulas_list, workerFunction)
  
  formulas_names <- names(formulas)
  # when other metrics are added, they also need to get a new name here!
  formulas_names <- c(formulas_names, 'rSquared')
  # concat results to a data frame
  result <- data.frame(matrix(unlist(res), nrow=length(formulas_list), byrow=T))
  names(result) <- formulas_names
  return(result);
}