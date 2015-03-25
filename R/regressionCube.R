# Libaries necessary: fmsb
# load_dataset("/Users/paul/Desktop/patients-100k.csv")
# matrix <- r_squared_matrix(dependent = "gender")
pkg.env <- new.env()
#pkg.env$data <- NA
#pkg.env$data_name <- NA

'load_dataset' <- function (csv_file, isURL=TRUE, load_dictionary=FALSE){ #, type_filepath) {
  #csv_filepath <- "/Users/paul/Tresors/Regresson Cubes/js-html/prototype/data/breast_fat.csv"
  #type_filepath <- "/Users/paul/Tresors/SHIP Breast Fat Dataset/Breast Fat Dataset/data/dictionary.json"
  type_filepath <- "/Users/paul/Tresors/SHIP Breast Fat Dataset/Breast Fat Dataset/data/dictionary_new_names.json"
  if (isURL) {
    # Treat empty elements as NA
    data <- read.csv(url(csv_file), header = TRUE, na.strings="")
  }
  else {
    # Treat empty elements as NA
    data <- read.csv(csv_file, header = TRUE, , na.strings="")
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
# using the RWeka binding. It saves the results to be available faster later on!
# @param: data: input data frame
# @param: dependent: name of the dependent variables
# @param: maximum_features: maximum number of features returned
# @return: array of variable names
'correlation_based_feature_selection_cached' <- function(data, dependent, data_id, maximum_features = 60) {
  #save(list = c('data', 'dependent', 'data_id'), file = '~/correlation_input')
  # Check if there is a file containing this information
  filename <- paste0("~/regressionCubeVardumps/", data_id, "/", data_id, "-cfs.Rdmped")
  if (file.exists(filename)) {
    load(file = filename)
    if (!is.null(cfs[[dependent]])) {
      return(cfs[[dependent]])
    }
  }
  # If there is no cfs object (because there is no file to be loaded) create one
  if (!exists('cfs'))
    cfs <- list()
  # Calculate the dependent variable
  cfs[[dependent]] <- correlation_based_feature_selection(data, dependent, maximum_features)
  # Create directories for the dump and save it to disk
  dir.create('~/regressionCubeVardumps/')
  dir.create(paste0("~/regressionCubeVardumps/", data_id))
  save(list = c("cfs"), file = filename)
  return (cfs[[dependent]])
}

# Calculates important varibles based on correlation based feature selection (CFS)
# using the RWeka binding.
# @param: data: input data frame
# @param: dependent: name of the dependent variables
# @param: maximum: maximum number of features returned
# @return: array of variable names
'correlation_based_feature_selection' <- function(data, dependent, maximum_features = 60) {
  #library(rJava)
  # Create the Weka filter
  #cfs.data <- data
  #cfs.dependent <- dependent
  #save(list = c("cfs.data", 'cfs.dependent'), file = '/Users/paul/Desktop/cfsinput.rtmp')
  attribute_selection <- RWeka::make_Weka_filter("weka/filters/supervised/attribute/AttributeSelection") 
  target_formula <- as.formula(paste0(dependent, '~.'))
  # TODO: This kills the ubuntu server when called a second time
  attribute_selection_result <- try(attribute_selection(formula=target_formula, data=data, na.action = na.pass, control=RWeka::Weka_control(
    E="weka.attributeSelection.CfsSubsetEval -P 1 -E 1",
    S="weka.attributeSelection.BestFirst -D 1 -N 5"
  )), silent = FALSE)
  # Throw error when reduction fails!
  if(class(attribute_selection_result) == "try-error") {
    message(paste0("Correlation-based feature selection fails for ", dependent))
    return()
  }
  # Limit the maximum number of columns to 40
  columns <- colnames(attribute_selection_result)
  if (length(columns) > maximum_features)
    return(columns[1:maximum_features])
  else
    return(columns)
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

'remove_invalid_path_characters' <- function(path) {
  #\ / : * ? " < > |
  #?, *, +, and %.
  path <- gsub("/", "slash", path)
  path <- gsub(":", "dcolon", path)
  path <- gsub("\\*", "asterisk", path)
  path <- gsub("\\?", "questionmark", path)
  path <- gsub("<", "smaller", path)
  path <- gsub(">", "larger", path)
  path <- gsub("\\|", "pipe", path)
  path <- gsub("\\+", "plus", path)
  path <- gsub("%", "percent", path)
  path <- gsub("([\\])", "bslash", path)
  return(path)
}

# This method caches the provided javascript object
# to the disk if it is specified, if not, it attempts to load it
# @param: r_squared: JS Object of R Squared matrix
# @param: formula: Formula as string
# @param: data_id: Data ID
'cache_r_squared_matrix' <- function(r_squared, formula, data_id) {
  # TODO: Remove illegal characters from formula (:, >, ...)
  filename <- paste0("~/regressionCubeVardumps/", data_id, "/", data_id, "-", remove_invalid_path_characters(formula), ".Rdmpd")
  # If r_squared is provided, save it to the disk
  if (!missing(r_squared)) {
    save(list = c("r_squared"), file = filename)
    return()
  } else { # If not, attempt to load it
    if (file.exists(filename)) {
      # It exists, so load it up and return the value
      load(file = filename)
      return(r_squared)
    } else {
      # It could not be found, so return false
      return(FALSE)
    }
  }
}

'get_dependent_feature_from_formula' <- function(formula_string) {
  tilde_index <- regexpr("~", formula_string)
  dependent <- substr(formula_string, 0, tilde_index - 1)
  # Remove whitespaces
  dependent <- gsub("^\\s+|\\s+$", "", dependent)
  return(dependent)
}

'as_html_table' <- function(to_html) {
  result_table <- as.data.frame(to_html)
  result_table <- print(xtable::xtable(result_table), type = "html")
  return(result_table)
}

# The function takes the formulas as input and iterates over them
'r_squared_matrix_formula' <- function(data, formulas, data_id, parallel=TRUE, use_median_regession = FALSE) {
  #save(list = c('data', 'formulas', 'data_id'), file = '~/r_squared_input')
  #save(list = c("formulas"), file = '/Users/paul/Desktop/formulas.rtmp')
  #save(list = c("formulas", 'data', 'data_id'), file = '/Users/paul/Desktop/input.rtmp')
  
  # Open the local formula array of the data set
  filename <- paste0("~/regressionCubeVardumps/", data_id, "/", data_id, "-formulas.Rdmpd")
  if (file.exists(filename)) {
    load(file = filename)
  }
  # If there is no formula_storage object (because there is no file to be loaded) create one
  if (!exists('formula_storage'))
    formula_storage <- list()
  
  if (use_median_regession) {
    library(parallel)
    library(quantreg)
  } else {
    library(parallel)
  }
  
  # Create NA count data structure
  not_na_count <- list()
  dimension_names <- names(data)
  number_rows <- nrow(data)
  for (i in 1:length(data)) {
    current_dimension <- dimension_names[i]
    not_na_count[current_dimension] <- number_rows - length(which(is.na(data[current_dimension])))
  }
  
  variable_classes <- lapply(data, class)
  workerFunction <- function(current_formula) {
    current_formula_string <- current_formula[,'formula']
    # Is there already an entry of this formula in the storage?
    if (!is.null(formula_storage[[current_formula_string]])) {
      # Attach all information derived from the storage 
      # when other metrics are added, they also need to get a new name here!
      current_formula['rSquared'] <- formula_storage[[current_formula_string]][['R2']]
      current_formula['confidenceIntervals'] <- formula_storage[[current_formula_string]][['confidenceIntervals']]
      current_formula['regressionType'] <- formula_storage[[current_formula_string]][['regressionType']]
      current_formula['coefficients'] <- formula_storage[[current_formula_string]][['coefficients']]
      current_formula['featureCount'] <- formula_storage[[current_formula_string]][['featureCount']]
      # And return the object
      return(current_formula)
    }
    dependent_class <- variable_classes[current_formula[,'dependentVariable']]
    
    current_formula_converted <- as.formula(current_formula_string)
    # If current class is numeric, apply Linear Regression
    if (dependent_class == 'numeric') {
      if (use_median_regession)
        model <- try(rq(formula = current_formula_converted, data = data, tau = 0.5), silent = TRUE)
      else
        model <- try(lm(formula = current_formula_converted, data = data), silent = TRUE)
    } else {
      model <- try( rms::lrm(formula = current_formula_converted, data = data), silent = TRUE)
    }
    
    # If calculation fails, return null
    if(class(model) == "try-error") {
      message(paste0("'", current_formula_string, "' failed!"))
      # Set empty rSquared, otherwise the object will be assembled false
      current_formula['rSquared'] <- ''
      current_formula['confidenceIntervals'] <- ''
      current_formula['regressionType'] <- ''
      current_formula['coefficients'] <- ''
    } else {
      if (dependent_class == 'numeric') {
        if (use_median_regession) {
          # Calculate R^2 from median regression
          # http://stackoverflow.com/a/27510106/2274058
          current_dependent_feature <- get_dependent_feature_from_formula(current_formula_string)
          model_only_intercepts <- rq(as.formula(paste0(current_dependent_feature, '~1')), tau=0.5, data=data)
          
          current_formula['rSquared'] <-  1 - model$rho/model_only_intercepts$rho
          current_formula['confidenceIntervals'] <- "Not available"
          current_formula['regressionType'] <- 'median'
          current_formula['coefficients'] <- as_html_table(model$coefficients)
        } else {
          model_summary <- summary(model)
          # Only add if model is valid
          coefficient_length <- length(model_summary$coefficients[,3])
          coefficient_na_length <- length(which(is.na(model_summary$coefficients[,3])))
          if (coefficient_length != coefficient_na_length) {
            # Add the respective values
            current_formula['rSquared'] <- model_summary$r.squared
            confinterval <- confint(model)
            confintTable <- print(xtable::xtable(confinterval), type = "html")
            current_formula['confidenceIntervals'] <- confintTable
            current_formula['regressionType'] <- 'linear'
            current_formula['coefficients'] <- print(xtable::xtable(model_summary$coefficients), type = "html")
          } else {
            current_formula['rSquared'] <- ''
            current_formula['confidenceIntervals'] <- ''
            current_formula['regressionType'] <- ''
            current_formula['coefficients'] <- ''
          }
        }
      }
      else {
        current_formula['rSquared'] <- model$stats[['R2']]
        #confint <- summary(model)[ , c("Lower 0.95", "Upper 0.95")]
        #confintTable <- print(xtable::xtable(confint), type = "html")
        current_formula['confidenceIntervals'] <- ''
        current_formula['regressionType'] <- 'logistic'
        current_formula['coefficients'] <- print(xtable::xtable(data.frame(model$coefficients)), type = "html")
      }
    }
    # Append count of all features
    formula_features <- all.vars(current_formula_converted)
    feature_number_count <- list()
    for (i in 1:length(formula_features)) {
      current_formula_feature <- formula_features[i]
      feature_number_count[current_formula_feature] <- not_na_count[current_formula_feature]
    }
    current_formula['featureCount'] <- as_html_table(feature_number_count)
    # Return the formula
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
  # when other metrics are added, they also need to get a new name here! The order counts!
  formulas_names_with_rSquared <- c(formulas_names, 'rSquared', 'confidenceIntervals', 'regressionType', 'coefficients', 'featureCount')
  # concat results to a data frame
  result <- data.frame(matrix(unlist(res), nrow=length(formulas_list), byrow=T))
  # If no R-squared values are found at all during the calulcation, simply assigning
  # the new names array would result in an error. So we check if there are more dimensions
  # than before!
  if (length(formulas) == length(result))
    names(result) <- formulas_names
  else
    names(result) <- formulas_names_with_rSquared
  
  # Fill the formula storage with the calculated information
  # when other metrics are added, they also need to get a new name here!
  for (i in 1:nrow(result)) {
    current_formula_string <- as.character(result$formula[i])
    formula_storage[[current_formula_string]] <- list()
    formula_storage[[current_formula_string]][['R2']] <- as.character(result$rSquared[i])
    formula_storage[[current_formula_string]][['confidenceIntervals']] <- as.character(result$confidenceIntervals[i])
    formula_storage[[current_formula_string]][['regressionType']] <- as.character(result$regressionType[i])
    formula_storage[[current_formula_string]][['coefficients']] <- as.character(result$coefficients[i])
    formula_storage[[current_formula_string]][['featureCount']] <- as.character(result$featureCount[i])
    #as.numeric(as.matrix(result$rSquared[i]))
  }
  
  # Create directories for the dump and save it to disk
  dir.create('~/regressionCubeVardumps/')
  dir.create(paste0("~/regressionCubeVardumps/", data_id))
  save(list = c("formula_storage"), file = filename)
  #save(list = c("result"), file = '/Users/paul/Desktop/result.rtmp')
  return(result);
}