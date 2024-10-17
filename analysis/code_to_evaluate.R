
# remotes::install_github("mi-erasmusmc/patientlevelprediction@explore")
library(PatientLevelPrediction)

library(data.table)
library(dplyr)

reticulate::use_condaenv("reticulate3.8") # IPCI server
# reticulate::use_condaenv("Explore") # Local laptop

# Paths
root <- getwd()

evaluate_ALL <- TRUE
evaluate_RR <- TRUE
evaluate_RRprint <- TRUE
shiny <- FALSE

name <- "2024-08-final"
covariates <- "Phenotypes" # "Default", "AgeSex", "Full", "Phenotypes", "Reduced"

# OLD:
# name <- "final_linux"
# covariates <- c("Reduced", "Full")

outputFolder <- file.path(root, "shiny", "output", name)
saveFolder <- file.path(outputFolder, paste0("export_", name))

if (!dir.exists(saveFolder)) {
  dir.create(saveFolder, recursive = TRUE)
  file.copy(file.path(outputFolder, "explore_options.csv"), file.path(saveFolder, "explore_options.csv"))
  file.copy(file.path(outputFolder, "cohortsToCreate_study.csv"), file.path(saveFolder, "cohortsToCreate_study.csv"))
  file.copy(file.path(outputFolder, "cohortsToCreate_phenotypes.csv"), file.path(saveFolder, "cohortsToCreate_phenotypes.csv"))
}

### Evaluate - Aggregate results ###
if (evaluate_ALL) {
  source(file.path(root, "code/transform-data.R"))
  source(file.path(root, "code/helper-functions.R"))

  outputList <- list.dirs(path = paste0(outputFolder, "/"), full.names = F, recursive = F)
  outputList <- outputList[!grepl("Data_|export_|old", outputList)] # Remove data / variable selection results

  datasets <- unique(sapply(outputList, function(l) unlist(strsplit(l, split = "_"))[[2]]))

  methods_output <- data.frame()
  for (cov in covariates) { # cov = covariates[1]

    summary_tasks <- data.frame()
    for (plp in datasets) { # plp = datasets[2]

      # Select results for current dataset and covariates
      outputList_plp <- outputList[grepl(plp, outputList)]
      outputList_plp <- outputList_plp[grepl(cov, outputList_plp)]
      outputList_plp <- outputList_plp[!outputList_plp %like% "print-99_Phenotypes"]
      outputList_plp <- outputList_plp[!outputList_plp %like% "print-100_Phenotypes"]
      # outputList_plp <- outputList_plp[!outputList_plp %in% c("")]

      # Load data
      # OLD:
      # path <- file.path(outputFolder, paste0("Data_", plp, "_Reduced"), "TrainTestData")
      path <- file.path(outputFolder, paste0("Data_", plp, "_", cov), "TrainTestData")
      data <- loadTrainTestData(path)

      # Summarize data
      summary_tasks <- summarizePLP(plp, summary_tasks, data)

      # Covariate data
      if (paste0("LASSO_", plp, "_1_", cov) %in% outputList_plp) {
        result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, paste0("LASSO_", plp, "_1_", cov), "plpResult"))

        features <- result$model$model$coefficients$covariateIds
        features <- features[features!='(Intercept)']

      } else if (paste0("EXPLORE_", plp, "_1_RT_", cov) %in% outputList_plp) {
        result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, paste0("EXPLORE_", plp, "_1_RT_", cov), "plpResult"))

        # features <- result$model$covariateImportance$covariateId
        features <- result$covariateSummary$covariateId
      } else if (paste0("EXPLORE_", plp, "_MF4_", cov) %in% outputList_plp) {
        result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, paste0("EXPLORE_", plp, "_MF4_", cov), "plpResult"))

        # features <- result$model$covariateImportance$covariateId
        features <- result$covariateSummary$covariateId
      } else if ("EXPLORE_CHADS2_Phenotypes" %in% outputList_plp) {
        result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, "EXPLORE_CHADS2_Phenotypes", "plpResult"))

        # features <- result$model$covariateImportance$covariateId
        features <- result$covariateSummary$covariateId

      } else if (paste0("EXPLORE_", plp, "_5_", cov) %in% outputList_plp) {
        result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, paste0("EXPLORE_", plp, "_5_", cov), "plpResult"))

        # features <- result$model$covariateImportance$covariateId
        features <- result$covariateSummary$covariateId
      }

      features_names <- result$covariateSummary[c("covariateId", "covariateName")]

      cov_data <- result$covariateSummary
      write.csv(cov_data, file.path(saveFolder, paste0("covariates_", plp, "_", cov, ".csv")), row.names = FALSE)

      # Save objects
      models <- setNames(data.table(matrix(0, nrow = 0, ncol = length(features)+5)), c("method", "iteration", "option", features, "model size vars", "model size comps")) # NO INTERCEPT

      for (o in outputList_plp) { # o = outputList_plp[1]
        print(o)

        method <- unlist(strsplit(o, split = "_"))[1]
        i <- unlist(strsplit(o, split = "_"))[3]

        result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, o, "plpResult"))

        if (is.null(result$prediction)) {
          warning('No predictions!')

        } else {
          # Save computation time method
          time <- result$model$trainDetails$trainingTime

          # Get variables included and model size per model algorithm
          if (method == "LASSO" || method == "IHT") {
            varImp <- result$model$model$coefficients
            vars <- varImp$betas[varImp$covariateIds!='(Intercept)']
            names(vars) <- as.double(varImp$covariateIds[varImp$covariateIds!='(Intercept)'])
            size_vars <- sum(sapply(vars, function(v) ifelse(v != 0, 1, 0)))
            size_comps <- size_vars

          } else if (method == "RandomForest" || method == "DecisionTree") {
            varImp <- result$model$covariateImportance
            vars <- varImp$covariateValue
            names(vars) <- varImp$covariateId
            size_vars <- sum(sapply(vars, function(v) ifelse(v != 0, 1, 0)))

            if (method == "DecisionTree") {
              modelLocation <- reticulate::r_to_py(file.path(result$model$model, "model.pkl"))
              joblib <- reticulate::import('joblib')
              os <- reticulate::import('os')
              model <- joblib$load(os$path$join(modelLocation))

              size_comps <- model$tree_$node_count

            } else if (method == "RandomForest") {
              modelLocation <- reticulate::r_to_py(file.path(result$model$model, "model.json"))
              model <- sklearnFromJson(path = modelLocation)

              size_comps <- 0
              max_nodes <- 0
              n_trees <- reticulate::py_to_r(model$n_estimators)

              for (m in 0:(n_trees-1)) {
                nodes <- reticulate::py_to_r(model$estimators_[m]$tree_$node_count)
                size_comps <- size_comps + nodes

                if (nodes > max_nodes) {
                  max_nodes <-nodes
                }
              }
            }
          } else if (method == "EXPLORE" || method == "RIPPER") {
            varImp <- result$model$covariateImportance
            vars <- varImp$covariateValue
            names(vars) <- varImp$covariateId
            size_vars <- sum(varImp$covariateValue)

            if (method == "EXPLORE") {
              size_comps <- size_vars

            } else if (method == "RIPPER") {
              num_rules <- result$model$model$numRules
              size_comps <- stringr::str_count(result$model$model$modelString, "and") + (num_rules - 1) # sum of rule lengths: count the total number of concatenations plus 1 extra for each rule (minus the default predicted class)

            }
          } else if (method == "XGBoost") {
            varImp <- result$model$covariateImportance
            varImp <- varImp %>% group_by(covariateId) %>% summarise(included=max(included)) # Variables can occur twice in computed var importance
            vars <- varImp$included
            names(vars) <- varImp$covariateId
            size_vars <- sum(varImp$included)

            model <- result$model$model # model <- xgboost::xgb.load(file.path(outputFolder, o, "plpResult", "model", "model.json"))
            trees <- xgboost::xgb.model.dt.tree(model = model)
            size_comps <- nrow(trees)

            max_nodes <- max(trees$Node)
            n_trees <- max(trees$Tree)

          } else if(method == "GOSDT") {
            modelLocation <- reticulate::r_to_py(file.path(result$model$model, "model.pkl"))
            joblib <- reticulate::import('joblib')
            os <- reticulate::import('os')
            gosdt <- reticulate::import('gosdt')
            model <- joblib$load(os$path$join(modelLocation))

            size_comps <- gosdt$GOSDT$leaves(model)  # model$size

            # TODO: needs updating
            # varImp <- result$model$covariateImportance
            # vars <- varImp$covariateValue
            # names(vars) <- varImp$covariateId
            # size_vars <- sum(varImp$covariateValue)

            size_vars <- length(model$tree$features()) # ?

          } else if (method == "Existing") {
            vars <- NA # TODO: check
            size_vars <- 5
            size_comps <- 5

          } else {
            stop('Model not included')
          }

          vars <- vars[vars != 0]
          vars <- sapply(features, function(f) ifelse(f %in% names(vars), 1, 0))
          names(vars) <- features

          model <- c(method=method, iteration=1, option=i, as.list(vars), `model size vars`=size_vars, `model size comps`=size_comps)
          models <- rbind(models, model)

          # Evaluate PLP
          # if (is.null(result$performanceEvaluation)) {
          #   eval_plp_list <- list("brier score_Test"=NA,
          #                         "calibrationInLarge mean prediction_Test"=NA,
          #                         "calibrationInLarge observed risk_Test"=NA,
          #                         "brier score_Train"=NA,
          #                         "calibrationInLarge mean prediction_Train"=NA,
          #                         "calibrationInLarge observed risk_Train"=NA)
          # } else {
          #   eval_plp <- result$performanceEvaluation$evaluationStatistics
          #   eval_plp <- eval_plp[eval_plp$metric %in% c("brier score", "calibrationInLarge mean prediction", "calibrationInLarge observed risk"),]
          #   eval_plp <- eval_plp[eval_plp$evaluation %in% c("Train", "Test"),]
          #   eval_plp$name <- paste0(eval_plp$metric, "_", eval_plp$evaluation)
          #   eval_plp_list <- eval_plp$value
          #   names(eval_plp_list) <- eval_plp$name
          # }

          # Evaluate predictions
          real_train <- result$prediction$outcomeCount[result$prediction$evaluationType == "Train"]
          predictions_train <- result$prediction$value[result$prediction$evaluationType == "Train"]

          real_test <- result$prediction$outcomeCount[result$prediction$evaluationType == "Test"]
          predictions_test <- result$prediction$value[result$prediction$evaluationType == "Test"]

          if (method == "EXPLORE") {
            model_description <- result$model$model$fit

            # OLD:
            # eval_train_prob <- evaluateExplore(modelsCurve = result$model$model$models_AUCcurve, plpModel=result$model, data=data$Train)
            eval_train_prob <- evaluateExplore(modelsCurve = result$model$model$modelsCurve, plpModel=result$model, data=data, train=TRUE)
            eval_train_class <- evaluateModel(predictions_train, real_train, model=model_description)

            # OLD:
            # eval_test_prob <-  evaluateExplore(modelsCurve = result$model$model$models_AUCcurve, plpModel=result$model, data=data$Test)
            eval_test_prob <-  evaluateExplore(modelsCurve = result$model$model$modelsCurve, plpModel=result$model, data=data, train=FALSE)
            eval_test_class <- evaluateModel(predictions_test, real_test, model=model_description)

            # Add feature names
            covs <- unlist(stringr::str_extract_all(model_description, pattern = "[:digit:]{3,}+"))
            names(covs) <- sapply(covs, function(c) features_names$covariateName[features_names$covariateId == c])
            model_description <- stringr::str_replace_all(model_description, setNames(names(covs), covs))

          } else {
            model_description <- paste0(size_vars, " covariates")
            eval_train_prob <- evaluateModel(predictions_train, real_train, model=model_description, class=F)
            eval_train_class <- evaluateModel(prob_to_class(predictions_train, real_train), model=model_description, real_train)

            eval_test_prob <- evaluateModel(predictions_test, real_test, model=model_description, class=F)
            eval_test_class <- evaluateModel(prob_to_class(predictions_test, real_test), model=model_description, real_test)
          }

          # eval <- append(append(append(append(eval_plp_list, eval_test_class), eval_test_prob), eval_train_class), eval_train_prob)
          # names(eval) <- c(names(eval_plp_list), paste0(names(eval_test_class), "_Test_Class"), paste0(names(eval_test_prob), "_Test_Prob"), paste0(names(eval_train_class), "_Train_Class"), paste0(names(eval_train_prob), "_Train_Prob"))

          eval <- append(append(append(eval_test_class, eval_test_prob), eval_train_class), eval_train_prob)
          names(eval) <- c(paste0(names(eval_test_class), "_Test_Class"), paste0(names(eval_test_prob), "_Test_Prob"), paste0(names(eval_train_class), "_Train_Class"), paste0(names(eval_train_prob), "_Train_Prob"))

          output_o <- c(append(list(Run = o, Time = time, Data = plp, Covariates=as.character(cov), Method = method, Iteration = 1, Option = i, Model = model_description), eval))
          output_o <- sapply(output_o, function(i) ifelse(is.null(i), NA, i)) # add NA instead of NULL
          methods_output <- rbind(methods_output, output_o)
        }
      }

      write.csv(models, file.path(saveFolder, paste0("models_", plp, "_", cov, ".csv")), row.names = FALSE)
    }
    write.csv(summary_tasks, file.path(saveFolder, paste0("summary_tasks_", cov, ".csv")), row.names = FALSE)

    colnames(methods_output) <- names(output_o)
    write.csv(methods_output, file.path(saveFolder, paste0("output_methods.csv")), row.names = FALSE)
  }
}

### Evaluate - Additional analysis ###

# Rashomon set and ratio at different cutoffs
if (evaluate_RR) {

  outputList <- list.dirs(path = paste0(outputFolder, "/"), full.names = F, recursive = F)
  outputList <- outputList[!grepl("Data_|export_", outputList)] # Remove data / variable selection results

  datasets <- unique(sapply(outputList, function(l) unlist(strsplit(l, split = "_"))[[2]]))
  datasets <- datasets[!datasets %in% c("CHADS2")]

  rashomon_ratio <- data.table(Data = character(), Option =  numeric(), Percentage = numeric(), RR = numeric(), TotalCandidates = integer(), RestrictedCandidates = integer())

  percentages <- c(90, 92, 94, 96, 98, 99)
  option <- 4:5 # length

  for (cov in covariates) { # cov = covariates[1]
    for (plp in datasets) { # plp = datasets[1]
      for (perc in percentages) { # perc = percentages[1]
        for (o in option) { # o = option[1]
          ParallelLogger::logInfo(print(paste0("Computing Rashomon ratio for ", plp)))

          # Read in results file
          total_candidates <- Explore::candidatesExplore(file.path(outputFolder, paste0("EXPLORE_", plp, "_", o, "_", cov), "Explore", "train_data.result"))
          restricted_candidates <- Explore::candidatesExplore(file.path(outputFolder, paste0("EXPLORE_", plp, "_RR", o , "-", perc, "_", cov), "Explore", "train_data.result"))

          rr <- as.numeric(restricted_candidates) / as.numeric(total_candidates)

          rashomon_ratio <- rbind(rashomon_ratio, c(list(Data = plp,
                                                         Option = o,
                                                         Percentage = perc,
                                                         RR = rr,
                                                         TotalCandidates = total_candidates,
                                                         RestrictedCandidates = restricted_candidates)))
        }
      }
    }
  }

  write.csv(rashomon_ratio, file.path(saveFolder, "rashomon_ratio.csv"), row.names = FALSE)
}

# Rashomon set and ratio examined in terms of generalizability and model diversity
if (evaluate_RRprint) {
  source(file.path(root, "code/helper-functions.R"))

  outputList <- list.dirs(path = paste0(outputFolder, "/"), full.names = F, recursive = F)
  outputList <- outputList[!grepl("Data_|export_", outputList)] # Remove data / variable selection results

  datasets <- unique(sapply(outputList, function(l) unlist(strsplit(l, split = "_"))[[2]]))
  datasets <- datasets[!datasets %in% c("CHADS2")]

  percentages <- c(99)
  option <- 5 # length

  rr_output <- data.frame()

  for (cov in covariates) { # cov = covariates[1]
    for (plp in datasets) { # plp = datasets[1]

      path <- file.path(outputFolder, paste0("Data_", plp, "_", cov), "TrainTestData")
      data <- PatientLevelPrediction:::loadTrainTestData(path)

      features <- as.data.frame(data$Train$covariateData$covariateRef)$covariateId

      # Save objects
      models <- setNames(data.table(matrix(0, nrow = 0, ncol = length(features)+4)), c("candidate", "percentage", "option", features, "model size"))

      for (perc in percentages) { # perc = percentages[1]
        for (o in option) { # o = option[1]
          ParallelLogger::logInfo(print(paste0("Computing Rashomon ratio for ", plp)))

          # Read in candidates
          name <- paste0("EXPLORE_", plp, "_RR", o , "print-", perc, "_", cov)
          all_candidates <- Explore::candidateModelsExplore(file.path(outputFolder, name, "Explore", "train_data.result"))

          # Draw random sample of 1000
          # if (length(all_candidates) > 1000) {
          #   set.seed(123)
          #   select <- sample(1:length(all_candidates), 1000, replace = FALSE)
          #   all_candidates <- all_candidates[select]
          # }

          for (candidate in all_candidates) { # candidate <- all_candidates[1]

            candidate <- stringr::str_remove(candidate, "Candidate model: ")

            vars <- unlist(stringr::str_match_all(candidate, "'\\d*'"))
            vars <- stringr::str_remove_all(vars, "'")

            plpModel <- list(model = list(fit = candidate,
                                          coefficients = vars))
            attr(plpModel, "modelType") <- 'binary'

            predictions_train <- PatientLevelPrediction:::predictExplore(plpModel = plpModel, data = data$Train, cohort = data$Train$labels)
            predictions_test <- PatientLevelPrediction:::predictExplore(plpModel = plpModel, data = data$Test, cohort = data$Test$labels)

            # Get performance of candidates
            eval_test_class <- evaluateModel(predictions_test$value, data$Test$labels$outcomeCount, model=candidate)
            eval_train_class <- evaluateModel(predictions_train$value, data$Train$labels$outcomeCount, model=candidate)

            eval <- append(eval_test_class, eval_train_class)
            names(eval) <- c(paste0(names(eval_test_class), "_Test_Class"), paste0(names(eval_train_class), "_Train_Class"))

            output_c <- c(append(list(Run = name, Data = plp, Percentage = perc, Covariates=as.character(cov), Option = o, Method = "EXPLORE",Model = candidate), eval))
            output_c <- sapply(output_c, function(i) ifelse(is.null(i), NA, i)) # add NA instead of NULL
            rr_output <- rbind(rr_output, output_c)

            # Create overview of all features included
            size_vars <- length(vars)

            values <- stringr::str_remove_all(candidate, '"|\'')
            values <- stringr::str_split_fixed(values, "OR|AND", n=Inf)
            values <- sapply(values, function(v) trimws(stringr::str_remove(v, "\\d+")))
            names(values) <- vars

            model_vars <- sapply(features, function(f) ifelse(f %in% vars, values[paste0(f)], NA)) # TODO: filter out operator + value
            names(model_vars) <- features

            model <- c(candidate=candidate, percentage=perc, option=o, as.list(model_vars), `model size vars`=size_vars)
            models <- rbind(models, model)
          }
          write.csv(models, file.path(saveFolder, paste0("rr_candidatemodels_", plp, "_", o,".csv")), row.names = FALSE)

        }
      }

      colnames(rr_output) <- names(output_c)
      write.csv(rr_output, file.path(saveFolder, paste0("rr_candidatepredictions.csv")), row.names = FALSE)
    }
  }
}

### Launch shiny ###
if (shiny) {
  shiny::runApp('shiny')
}
