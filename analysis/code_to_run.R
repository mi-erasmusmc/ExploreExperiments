# remotes::install_github("mi-erasmusmc/patientlevelprediction@explore")
library(PatientLevelPrediction)

# remotes::install_github("mi-erasmusmc/explore@main")
library(Explore)

library(Eunomia)
library(DatabaseConnector)
library(RWeka)
library(CohortGenerator)
library(data.table)
library(dplyr)
library(caret)
library(pracma)
library(PhenotypeLibrary)

reticulate::use_condaenv("Explore")

# sys <- reticulate::import('sys')
# sys$setrecursionlimit(as.integer(1000000)) # 1000
#
# resource <- reticulate::import('resource')
# resource$setrlimit(resource$RLIMIT_STACK, reticulate::r_to_py(c(536870912, -1))) # 8388608

plp <- "Test"
plptasks <- c("COPDMortality", "HeartfailureStroke", "AsthmaExacerbation", "HospitalReadmission", "EoLConversation") # "COPDMortality", "HeartfailureStroke", "AsthmaExacerbation", "HospitalReadmission", "EoLConversation", "CHADS2"
covariates <- "Phenotypes" # "Default", "AgeSex", "Full", "Phenotypes"

cohorts <- FALSE
selection <- TRUE
train <- TRUE
first <- FALSE

# Paths
root <- getwd()
name <- "2024-08-final" # Sys.Date()

outputFolder <- file.path(root, "shiny", "output", name)
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)

  first <- TRUE
}

for (plp in plptasks) {

  ### Specification prediction tasks ###
  if (plp == "Test") {
    # Get connection details
    connectionDetails <- Eunomia::getEunomiaConnectionDetails()

    # Create cohorts
    Eunomia::createCohorts(connectionDetails)

    # Select cohorts
    cohortId <- 4
    cohortNames <- 'target'
    outcomeId <- 3
    outcomeNames <- 'outcome'

    # Select population
    populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
      requireTimeAtRisk = F,
      riskWindowStart = 1,
      riskWindowEnd = 365)

    databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      cohortDatabaseSchema = "main",
      cohortTable = "cohort",
      targetId = cohortId,
      outcomeIds = outcomeId,
      outcomeDatabaseSchema = "main",
      outcomeTable =  "cohort",
      cdmDatabaseName = 'eunomia'
    )

  } else {
    # Details for connecting to the server:
    dbms <- 'todo'
    user <- 'todo'
    pw <- 'todo'
    server <- 'todo'
    port <- 'todo'

    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                    server = server,
                                                                    user = user,
                                                                    password = pw,
                                                                    port = port)
    cohortTable <- 'cohorts_explore_final'
    baseUrl <- 'todo'

    cdmDatabaseSchema = 'todo'
    cohortDatabaseSchema = 'todo'

    if (plp == "HospitalReadmission") { # 1 dec 2018 – 1 dec 2023 (IPCI-Q end: 1 jan 2024)

      # Select cohorts
      cohortId <- 1033
      cohortNames <- 'inpatient visit'
      outcomeId <- 1034
      outcomeNames <- 'hospital readmission'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 28,
                                                                                  riskWindowStart = 2,
                                                                                  startAnchor = "cohort end",
                                                                                  riskWindowEnd = 30,
                                                                                  endAnchor = "cohort end",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)
    } else if (plp == "EoLConversation") { # 1 jul 2018 – 1 jul 2023 (IPCI-Q end: 1 jan 2024)

      # Select cohorts
      cohortId <- 1036
      cohortNames <- 'old patient with GP visit'
      outcomeId <- 1038
      outcomeNames <- 'conversation'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 179,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 180,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)
    } else if (plp == "HeartfailureStroke") { # 1 jan 2014 – 1 jan 2019 (IPCI-Q end: 1 jan 2024)
      # Select cohorts
      cohortId <- 1060
      cohortNames <- 'T2DM patients'
      outcomeId <- 1058
      outcomeNames <- 'heart failure or stroke'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 1824,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 1825,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)

    } else if (plp == "AsthmaExacerbation") { # 1 jan 2017 – 1 jan 2022 (IPCI-Q end: 1 jan 2024)

      # Select cohorts
      cohortId <- 1078
      cohortNames <- 'asthma patients'
      outcomeId <- 1054
      outcomeNames <- 'asthma exacerbation'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 729,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 730,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)

    } else if (plp == "COPDMortality") { # 1 jan 2017 – 1 jan 2022 (IPCI-Q end: 1 jan 2024)

      # Select cohorts
      cohortId <- 1035
      cohortNames <- 'COPD patients'
      outcomeId <- 1037
      outcomeNames <- 'all-cause mortality'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 729,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 730,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPrior = FALSE)

    } else if (plp == "CHADS2") { # 1 jan 2018 – 1 jan 2023 (IPCI-Q end: 1 jan 2024)

      # Select cohorts
      # From https://github.com/mi-erasmusmc/PlpBenchmarks/tree/main/inst/cohorts
      cohortId <- 1650
      cohortNames <- 'patients with atrial fibrillation'
      outcomeId <- 1651
      outcomeNames <- 'ischemic stroke'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 364,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 365,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)
    }

    databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails,
                                                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                                                     cdmDatabaseId = "todo",
                                                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                                                     cohortTable = cohortTable,
                                                                     targetId = cohortId,
                                                                     outcomeIds = outcomeId,
                                                                     outcomeDatabaseSchema = cohortDatabaseSchema,
                                                                     outcomeTable = cohortTable,
                                                                     cdmDatabaseName = "todo")
  }

  ### Generate cohorts ###
  if (cohorts) {
    cohorts_atlas = FALSE
    cohorts_library = FALSE
    new = FALSE

    if (new) {
      # Create cohort tables
      cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)

      CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                          cohortTableNames = cohortTableNames,
                                          incremental = TRUE)

      # This is to add 'cohort_censor_stats'
      cohortTableNames <- CohortGenerator::getCohortTableNames()
      CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                          cohortTableNames = cohortTableNames,
                                          incremental = TRUE)
    }

    # List of cohorts to create in ATLAS
    cohortsToCreate <- data.frame()

    cohortsToCreate_study <- data.frame(rbind(c("Hospital Readmission target; inpatient visit discharge as end date",1033),
                                              c("Hospital Readmission outcome; inpatient visit",1034),
                                              c("EoL conversations target; latest outpatient visit 2016-2021",1036),
                                              c("EoL conversations outcome; first eol conversation",1038),
                                              c("Mortality in COPD target; first COPD diagnosis",1035),
                                              c("Mortality in COPD outcome; death",1037),
                                              c("Asthma exacerbation target;", 1078),
                                              c("Asthma exacerbation outcome;", 1054),
                                              c("Heart failure Stroke target; type 2 diabetes mellitus with no prior stroke or heart failure",1060),
                                              c("Heart failure Stroke outcome; first heart failure or stroke",1058),
                                              c("CHADS2 target; atrial fibrillation", 1650),
                                              c("CHADS2 outcome; ischemic stroke", 1651)))
    colnames(cohortsToCreate_study)<-c("cohortName","cohortId")
    cohortsToCreate_study$cohortId<-as.numeric(cohortsToCreate_study$cohortId)

    write.csv(cohortsToCreate_study, file.path(outputFolder, "cohortsToCreate_study.csv"), row.names = FALSE)
    cohortsToCreate <- rbind(cohortsToCreate, cohortsToCreate_study)

    cohortsToCreate_phenotypes_atlas <- data.frame(rbind(c("Antibiotics", 1652),
                                                         c("Urinary tract infections", 1653),
                                                         c("Stroke or TIA", 1657),
                                                         c("Anemia", 1658)))
    colnames(cohortsToCreate_phenotypes_atlas)<-c("cohortName","cohortId")
    cohortsToCreate_phenotypes_atlas$cohortId<-as.numeric(cohortsToCreate_phenotypes_atlas$cohortId)

    write.csv(cohortsToCreate_phenotypes_atlas, file.path(outputFolder, "cohortsToCreate_phenotypes_atlas.csv"), row.names = FALSE)
    cohortsToCreate <- rbind(cohortsToCreate, cohortsToCreate_phenotypes_atlas)

    # Set of predictors identified by Reps, 2024.
    cohortsToCreate_phenotypes_library <- PhenotypeLibrary::getPlCohortDefinitionSet(1152:1215)

    # Remove some predictors not in IPCI
    cohortsToCreate_phenotypes_library <- cohortsToCreate_phenotypes_library[!cohortsToCreate_phenotypes_library$cohortId %in% c(1155, 1156, 1163, 1176, 1177, 1186, 1188, 1201:1214),]

    write.csv(cohortsToCreate_phenotypes_library, file.path(outputFolder, "cohortsToCreate_phenotypes_library.csv"), row.names = FALSE)

    # Merge lists of phenotypes
    cohortsToCreate_phenotypes <- rbind(cohortsToCreate_phenotypes_library[,c("cohortName","cohortId")], cohortsToCreate_phenotypes_atlas[,c("cohortName","cohortId")])
    write.csv(cohortsToCreate_phenotypes, file.path(outputFolder, "cohortsToCreate_phenotypes.csv"), row.names = FALSE)

    connection <- connect(connectionDetails)

    if (cohorts_atlas) {
      # Generate target / outcome / predictor cohorts first time running study
      for (i in 1:nrow(cohortsToCreate)) {
        writeLines(paste("Copying cohort:", cohortsToCreate$cohortName[i]))
        ROhdsiWebApi::insertCohortDefinitionInPackage(cohortId = cohortsToCreate$cohortId[i],
                                                      name = cohortsToCreate$cohortName[i],
                                                      jsonFolder = file.path(root, "cohorts", "JSON"),
                                                      sqlFolder = file.path(root, "cohorts", "SQL"),
                                                      baseUrl = baseUrl,
                                                      generateStats = F)

        writeLines(paste("Creating cohort:", cohortsToCreate$cohortName[i]))
        sql <- SqlRender::readSql(paste0("cohorts/SQL/", cohortsToCreate$cohortName[i], ".sql"))
        sql <- SqlRender::render(sql,
                                 vocabulary_database_schema = cdmDatabaseSchema,
                                 cdm_database_schema = cdmDatabaseSchema,
                                 target_database_schema = cohortDatabaseSchema,
                                 target_cohort_table = cohortTable,
                                 target_cohort_id = cohortsToCreate$cohortId[i])
        sql <- SqlRender::translate(sql,
                                    targetDialect = attr(connection, "dbms"))
        DatabaseConnector::executeSql(connection, sql)
      }
    }

    if (cohorts_library) {
      # Generate phenotype library cohorts first time running study
      for (i in 1:nrow(cohortsToCreate_phenotypes_library)) {
        writeLines(paste("Creating cohort:", cohortsToCreate_phenotypes_library$cohortName[i]))
        sql <- cohortsToCreate_phenotypes_library$sql[i]
        sql <- SqlRender::render(sql,
                                 vocabulary_database_schema = cdmDatabaseSchema,
                                 cdm_database_schema = cdmDatabaseSchema,
                                 results_database_schema = cohortDatabaseSchema,
                                 target_database_schema = cohortDatabaseSchema,
                                 target_cohort_table = cohortTable,
                                 target_cohort_id = cohortsToCreate_phenotypes_library$cohortId[i])
        sql <- SqlRender::translate(sql,
                                    targetDialect = attr(connection, "dbms"))
        DatabaseConnector::executeSql(connection, sql)
      }
    }

    # All cohorts generated
    cohorts = FALSE
    new = FALSE
  }

  ### Pre-process data ###
  if (selection) {

    # Possible covariates
    if (covariates == "Default") { # Not for EXPLORE without pre-variable selection
      covariateSettingList <- FeatureExtraction::createDefaultCovariateSettings()
    } else if (covariates == "AgeSex") {
      covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAge = T)
    } else if (covariates == "Full") { # Not for EXPLORE without pre-variable selection
      covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAge = T,
                                                                         useConditionGroupEraAnyTimePrior = T, useConditionGroupEraLongTerm = T,
                                                                         useConditionGroupEraShortTerm = T, useDrugGroupEraLongTerm = T,
                                                                         useDrugGroupEraShortTerm = T, useDrugGroupEraOverlapping = T)
    } else if (covariates == "Phenotypes") {
      cohortsToCreate_phenotypes <- read.csv(file.path(outputFolder, "cohortsToCreate_phenotypes.csv"))

      if (plp  == "CHADS2") {
        cohortsToCreate_phenotypes <- cohortsToCreate_phenotypes[!(cohortsToCreate_phenotypes$cohortId %in% c(1193,1194)),]
        cohortsToCreate_phenotypes <- rbind(cohortsToCreate_phenotypes, list("Diabetes", 888))
      }

      covariateSettingList <- # Phenotypes from Phenotype Library OHDSI.
        list(
          FeatureExtraction::createCovariateSettings(
            useDemographicsGender = T,
            useDemographicsAge = T) # useDemographicsAgeGroup = T
        )
      for (i in 1:nrow(cohortsToCreate_phenotypes)){
        if (plp != "CHADS2") {
          covariateSettingList <- append(covariateSettingList,
                                         list(PatientLevelPrediction::createCohortCovariateSettings(
                                           cohortName = cohortsToCreate_phenotypes$cohortName[i],
                                           settingId = 1,
                                           cohortDatabaseSchema = cohortDatabaseSchema,
                                           cohortTable = cohortTable,
                                           cohortId = cohortsToCreate_phenotypes$cohortId[i],
                                           startDay = -365,
                                           endDay = 0,
                                           analysisId = 999)))
        } else { # CHADS2 existing model
          covariateSettingList <- append(covariateSettingList,
                                         list(PatientLevelPrediction::createCohortCovariateSettings(
                                           cohortName = cohortsToCreate_phenotypes$cohortName[i],
                                           settingId = 1,
                                           cohortDatabaseSchema = cohortDatabaseSchema,
                                           cohortTable = cohortTable,
                                           cohortId = cohortsToCreate_phenotypes$cohortId[i],
                                           startDay = -36500,
                                           endDay = 0,
                                           analysisId = 999)))
        }
      }
    }

    # Selected covariates
    plpData <- PatientLevelPrediction::getPlpData(
      databaseDetails = databaseDetails,
      restrictPlpDataSettings = createRestrictPlpDataSettings(),
      covariateSettings = covariateSettingList
    )

    # Add custom CHADS2 feature
    if (plp == "CHADS2") {
      source(file.path(root, "code/CHADS2-model.R"))

      plpData <- addCHADS2(plpData)
    }

    if (first) {

      # Save covariates
      covData <- FeatureExtraction::aggregateCovariates(plpData$covariateData)

      # table1 <- createTable1(
      #   covariateData1 = covData,
      #   cohortId1 = NULL, # 1
      #   specifications = FeatureExtraction::getDefaultTable1Specifications(),
      #   output = "one column",
      #   showCounts = FALSE,
      #   showPercent = TRUE,
      #   percentDigits = 1,
      #   valueDigits = 1,
      #   stdDiffDigits = 2
      # )

      write.csv(as.data.frame(covData$covariates), file.path(outputFolder, paste0("covData_", plp,".csv")), row.names = FALSE)

      # Get dataset (no selection)
      plpResults <- PatientLevelPrediction::runPlp(plpData = plpData,
                                                   outcomeId = outcomeId,
                                                   modelSettings = PatientLevelPrediction::setLassoLogisticRegression(variance=0.01),
                                                   analysisId = paste0("Data_", plp, "_", covariates),
                                                   analysisName = paste0("No selection"),
                                                   populationSettings = populationSettings,
                                                   splitSettings = createDefaultSplitSetting(testFraction=0.25,
                                                                                             trainFraction = 0.75,
                                                                                             splitSeed=123,
                                                                                             nfold=3,
                                                                                             type = 'stratified'),
                                                   sampleSettings = createSampleSettings(),
                                                   featureEngineeringSettings = createFeatureEngineeringSettings(),
                                                   preprocessSettings = createPreprocessSettings(minFraction=0.001,
                                                                                                 normalize = F,
                                                                                                 removeRedundancy = F),
                                                   logSettings = createLogSettings(),
                                                   executeSettings = createExecuteSettings(runSplitData = T,
                                                                                           runSampleData = F,
                                                                                           runfeatureEngineering = F,
                                                                                           runPreprocessData = T,
                                                                                           runModelDevelopment = F,
                                                                                           runCovariateSummary = F),
                                                   saveDirectory = outputFolder,
                                                   saveData = T)
    }
  }

  ### Train models ###
  analysis_RT <- FALSE
  analysis_M <- TRUE
  analysis_RR <- FALSE
  analysis_RRprint <- FALSE
  analysis_CHADS2 <- FALSE
  analysis_MF <- FALSE

  if (train) {

    # Possible models
    list_models <- list()
    explore_options <- c()

    # Record timings
    if (analysis_RT && plp != "CHADS2") {

      # Parallel method and sorting:
      explore_options_A <- expand.grid(StartRulelength = c(3),
                                       EndRulelength = c(3),
                                       Parallel = c(TRUE),
                                       Sorted = c("none", "phi"),
                                       ParallelMethod = c("ONE", "TWO"),
                                       SampleSize = c(1),
                                       stringsAsFactors = FALSE)
      explore_options_A$Option <- 100+1:nrow(explore_options_A)

      explore_options_B <- expand.grid(StartRulelength = c(4),
                                       EndRulelength = c(4),
                                       Parallel = c(TRUE),
                                       Sorted = c("none", "phi"),
                                       ParallelMethod = c("ONE", "TWO"),
                                       SampleSize = c(1),
                                       stringsAsFactors = FALSE)
      explore_options_B$Option <- 200+1:nrow(explore_options_B)

      # Non-parallel:
      explore_options_C <- expand.grid(StartRulelength = c(3),
                                       EndRulelength = c(3),
                                       Parallel = c(FALSE),
                                       Sorted = c("none"),
                                       ParallelMethod = c("TWO"), # not used
                                       SampleSize = c(1),
                                       stringsAsFactors = FALSE)
      explore_options_C$Option <- 300+1:nrow(explore_options_C)

      explore_options_D <- expand.grid(StartRulelength = c(4),
                                       EndRulelength = c(4),
                                       Parallel = c(FALSE),
                                       Sorted = c("none"),
                                       ParallelMethod = c("TWO"),  # not used
                                       SampleSize = c(1),
                                       stringsAsFactors = FALSE)
      explore_options_D$Option <- 400+1:nrow(explore_options_D)

      # Sample size and rule-length
      explore_options_E <- expand.grid(StartRulelength = c(3),
                                       EndRulelength = c(3),
                                       Parallel = c(TRUE),
                                       Sorted = c("none"),
                                       ParallelMethod = c("TWO"),
                                       SampleSize = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                                       stringsAsFactors = FALSE)
      explore_options_E$Option <- 500+1:nrow(explore_options_E)

      explore_options_F <- expand.grid(StartRulelength = c(4),
                                       EndRulelength = c(4),
                                       Parallel = c(TRUE),
                                       Sorted = c("none"),
                                       ParallelMethod = c("TWO"),
                                       SampleSize = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                                       stringsAsFactors = FALSE)
      explore_options_F$Option <- 600+1:nrow(explore_options_F)

      explore_options_RT <- rbind(explore_options_A, explore_options_B, explore_options_C, explore_options_D, explore_options_E, explore_options_F)

      explore_options_RT$BranchBound <- TRUE
      explore_options_RT$BinaryReduction <- TRUE

      explore_options_RT <- explore_options_RT[c(9,10),] # c(7, 10)
      explore_options_RT$BinaryReduction <- FALSE
      explore_options_RT$Option <- paste0(explore_options_RT$Option, "F")

      explore_options <- rbind(explore_options, explore_options_RT)

      for (option in explore_options_RT$Option) {
        analysisId <- paste0("EXPLORE_", plp, "_", option, "_RT")

        modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                            startRulelength = explore_options_RT$StartRulelength[explore_options_RT$Option == option],
                                                            endRulelength = explore_options_RT$EndRulelength[explore_options_RT$Option == option],
                                                            operatorMethod = "EXHAUSTIVE",
                                                            cutoffMethod = "ALL",
                                                            maximize = "BALANCEDACCURACY",
                                                            sorted = explore_options_RT$Sorted[explore_options_RT$Option == option],
                                                            parallel = explore_options_RT$Parallel[explore_options_RT$Option == option],
                                                            parallelMethod = explore_options_RT$ParallelMethod[explore_options_RT$Option == option],
                                                            sampleSize = explore_options_RT$SampleSize[explore_options_RT$Option == option],
                                                            branchBound = TRUE,
                                                            subsumption = FALSE,
                                                            binaryReduction = TRUE,
                                                            modelsCurve = FALSE,
                                                            saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

        list_models[[paste0("EXPLORE", option)]] <- list(analysisId=analysisId,
                                                         analysisName=paste0("EXPLORE decision rule"),
                                                         modelSettings=modelSettings)
      }
    }

    if (analysis_M && plp != "CHADS2") {

      # LASSO
      list_models[["LASSO"]] <- list(analysisId=paste0("LASSO_", plp, "_10"),
                                     analysisName=paste0("LASSO logistic regression"),
                                     modelSettings=PatientLevelPrediction::setLassoLogisticRegression(variance = 0.01))

      # Gradient boosting machine
      list_models[["XGBoost"]] <- list(analysisId=paste0("XGBoost_", plp, "_10"),
                                       analysisName=paste0("Gradient boosting machine"),
                                       modelSettings=PatientLevelPrediction::setGradientBoostingMachine(ntrees=c(10, 50, 100, 300), maxDepth=c(2, 4, 8)))

      # Random forest
      list_models[["RandomForest"]] <- list(analysisId=paste0("RandomForest_", plp, "_10"),
                                            analysisName=paste0("Random forest"),
                                            modelSettings=PatientLevelPrediction::setRandomForest(ntrees=list(10, 50, 100), maxDepth=list(2, 4, 8))) # 300

      # RIPPER
      list_models[["RIPPER"]] <- list(analysisId=paste0("RIPPER_", plp, "_10"),
                                      analysisName=paste0("RIPPER"),
                                      modelSettings=PatientLevelPrediction::setRIPPER(variableSelection = NULL,
                                                                                      saveDirectory = file.path(outputFolder, paste0("RIPPER_", plp, "_1"))))
      # Decision tree
      list_models[["DecisionTree"]] <- list(analysisId=paste0("DecisionTree_", plp, "_10"),
                                            analysisName=paste0("DecisionTree_"),
                                            modelSettings=PatientLevelPrediction::setDecisionTree(maxDepth = list(2, 4), maxFeatures = list(25)))

      # Iterative hard thresholding
      list_models[["IHT"]] <- list(analysisId=paste0("IHT_", plp, "_10"),
                                   analysisName=paste0("IHT"),
                                   modelSettings=PatientLevelPrediction::setIterativeHardThresholding(K=10, fitBestSubset = TRUE))

      # GOSDT
      list_models[["GOSDT"]] <- list(analysisId=paste0("GOSDT_", plp, "_10"),
                                     analysisName=paste0("GOSDT"),
                                     modelSettings=PatientLevelPrediction::setGOSDT(objective=list("bacc"), maxDepth=list(2, 4, 8), regularization = list(0.025, 0.05, 0.1)))

      # EXPLORE
      explore_options_M <- expand.grid(StartRulelength = c(1),
                                       EndRulelength = c(3, 4, 5),
                                       stringsAsFactors = FALSE)

      explore_options_M$Option <- 3:5
      explore_options_M$Parallel <-TRUE
      explore_options_M$Sorted <- "none"
      explore_options_M$ParallelMethod <- "TWO"
      explore_options_M$BranchBound <- TRUE
      explore_options_M$BinaryReduction <- TRUE
      explore_options_M$SampleSize <- 1

      explore_options <- rbind(explore_options, explore_options_M)

      for (option in explore_options_M$Option) {
        analysisId <- paste0("EXPLORE_", plp, "_", option)

        modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                            startRulelength = explore_options_M$StartRulelength[explore_options_M$Option == option],
                                                            endRulelength = explore_options_M$EndRulelength[explore_options_M$Option == option],
                                                            operatorMethod = "EXHAUSTIVE",
                                                            cutoffMethod = "ALL",
                                                            maximize = "BALANCEDACCURACY",
                                                            sorted = explore_options_M$Sorted[explore_options_M$Option == option],
                                                            parallel = explore_options_M$Parallel[explore_options_M$Option == option],
                                                            parallelMethod = explore_options_M$ParallelMethod[explore_options_M$Option == option],
                                                            branchBound = explore_options_M$BranchBound[explore_options_M$Option == option], # TODO: here FALSE = idem, but check if TRUE for modelsCurve
                                                            subsumption = FALSE,
                                                            binaryReduction = explore_options_M$BinaryReduction[explore_options_M$Option == option],
                                                            modelsCurve = TRUE,
                                                            saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

        list_models[[paste0("EXPLORE", option)]] <- list(analysisId=analysisId,
                                                         analysisName=paste0("EXPLORE decision rule"),
                                                         modelSettings=modelSettings)
      }
    }

    # Rashomon Ratio
    if (analysis_RR && plp != "CHADS2") {

      # Balanced accuracy performance per prediction task (train set)
      performanceBound4 <- list("COPDMortality" = 0.7003,
                                "HeartfailureStroke" = 0.6994,
                                "AsthmaExacerbation" = 0.6218,
                                "HospitalReadmission" = 0.5949,
                                "EoLConversation" = 0.6671)

      performanceBound5 <- list("COPDMortality" = 0.7051,
                                "HeartfailureStroke" = 0.7012,
                                "AsthmaExacerbation" = 0.625,
                                "HospitalReadmission" = 0.5966,
                                "EoLConversation" = 0.6677)

      percentages <- c(90, 92, 94, 96, 98, 99)

      for (perc in percentages) { # explore_options_RR$Option
        analysisId <- paste0("EXPLORE_", plp, "_RR4-", perc)

        modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                            startRulelength = 1,
                                                            endRulelength = 4,
                                                            operatorMethod = "EXHAUSTIVE",
                                                            cutoffMethod = "ALL",
                                                            maximize = "BALANCEDACCURACY",
                                                            balancedAccuracy = (as.numeric(performanceBound4[plp])*perc/100), # TODO: decide on percentage
                                                            outputMethod = "BEST", # "EVERY" -> to print CANDIDATE rules
                                                            sorted = "none",
                                                            parallel = TRUE,
                                                            parallelMethod = "TWO",
                                                            branchBound = TRUE,
                                                            subsumption = FALSE,
                                                            binaryReduction = TRUE,
                                                            modelsCurve = FALSE,
                                                            saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

        list_models[[analysisId]] <- list(analysisId=analysisId,
                                          analysisName=paste0("EXPLORE decision rule"),
                                          modelSettings=modelSettings)

        analysisId <- paste0("EXPLORE_", plp, "_RR5-", perc)

        modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                            startRulelength = 1,
                                                            endRulelength = 5,
                                                            operatorMethod = "EXHAUSTIVE",
                                                            cutoffMethod = "ALL",
                                                            maximize = "BALANCEDACCURACY",
                                                            balancedAccuracy = (as.numeric(performanceBound5[plp])*perc/100), # TODO: decide on percentage
                                                            outputMethod = "BEST", # "EVERY" -> to print CANDIDATE rules
                                                            sorted = "none",
                                                            parallel = TRUE,
                                                            parallelMethod = "TWO",
                                                            branchBound = TRUE,
                                                            subsumption = FALSE,
                                                            binaryReduction = TRUE,
                                                            modelsCurve = FALSE,
                                                            saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

        list_models[[analysisId]] <- list(analysisId=analysisId,
                                          analysisName=paste0("EXPLORE decision rule"),
                                          modelSettings=modelSettings)
      }
    }


    if (analysis_RRprint && plp != "CHADS2") {

      # Balanced accuracy performance per prediction task (train set)
      performanceBound4 <- list("COPDMortality" = 0.7003,
                                "HeartfailureStroke" = 0.6994,
                                "AsthmaExacerbation" = 0.6218,
                                "HospitalReadmission" = 0.5949,
                                "EoLConversation" = 0.6671)

      performanceBound5 <- list("COPDMortality" = 0.7051,
                                "HeartfailureStroke" = 0.7012,
                                "AsthmaExacerbation" = 0.625,
                                "HospitalReadmission" = 0.5966,
                                "EoLConversation" = 0.6677)

      percentages <- c(99)

      for (perc in percentages) { # explore_options_RR$Option
        analysisId <- paste0("EXPLORE_", plp, "_RR4print-", perc)

        modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                            startRulelength = 1,
                                                            endRulelength = 4,
                                                            operatorMethod = "EXHAUSTIVE",
                                                            cutoffMethod = "ALL",
                                                            maximize = "BALANCEDACCURACY",
                                                            balancedAccuracy = (as.numeric(performanceBound4[plp])*perc/100), # TODO: decide on percentage
                                                            outputMethod = "EVERY",
                                                            sorted = "none",
                                                            parallel = TRUE,
                                                            parallelMethod = "TWO",
                                                            branchBound = TRUE,
                                                            subsumption = FALSE,
                                                            binaryReduction = TRUE,
                                                            modelsCurve = FALSE,
                                                            saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

        list_models[[analysisId]] <- list(analysisId=analysisId,
                                          analysisName=paste0("EXPLORE decision rule"),
                                          modelSettings=modelSettings)

        analysisId <- paste0("EXPLORE_", plp, "_RR5print-", perc)

        modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                            startRulelength = 1,
                                                            endRulelength = 5,
                                                            operatorMethod = "EXHAUSTIVE",
                                                            cutoffMethod = "ALL",
                                                            maximize = "BALANCEDACCURACY",
                                                            balancedAccuracy = (as.numeric(performanceBound5[plp])*perc/100), # TODO: decide on percentage
                                                            outputMethod = "EVERY",
                                                            sorted = "none",
                                                            parallel = TRUE,
                                                            parallelMethod = "TWO",
                                                            branchBound = TRUE,
                                                            subsumption = FALSE,
                                                            binaryReduction = TRUE,
                                                            modelsCurve = FALSE,
                                                            saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

        list_models[[analysisId]] <- list(analysisId=analysisId,
                                          analysisName=paste0("EXPLORE decision rule"),
                                          modelSettings=modelSettings)
      }
    }

    # Existing model
    if (analysis_CHADS2 && plp == "CHADS2") {
      source(file.path(root, "code/CHADS2-model.R"))

      analysisId <- paste0("Existing_CHADS2")

      list_models[[analysisId]] <- list(analysisId=analysisId,
                                        analysisName=paste0("Apply CHADS2"),
                                        modelSettings=setCHADS2())

      analysisId <- paste0("EXPLORE_CHADS2")

      modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                          startRulelength = 5,
                                                          endRulelength = 5,
                                                          operatorMethod = "EXHAUSTIVE",
                                                          cutoffMethod = "ALL",
                                                          maximize = "BALANCEDACCURACY",
                                                          featureInclude = "'1002';'115401999';'119801999';'88801999';'165701999'",
                                                          sorted = "none",
                                                          parallel = TRUE,
                                                          parallelMethod = "TWO",
                                                          branchBound = TRUE,
                                                          subsumption = FALSE,
                                                          binaryReduction = TRUE,
                                                          modelsCurve = TRUE,
                                                          saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

      list_models[[analysisId]] <- list(analysisId=analysisId,
                                        analysisName=paste0("EXPLORE decision rule"),
                                        modelSettings=modelSettings)


      analysisId <- paste0("EXPLORE_CHADS2_asfeature")

      modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                          startRulelength = 1,
                                                          endRulelength = 5,
                                                          operatorMethod = "EXHAUSTIVE",
                                                          cutoffMethod = "ALL",
                                                          maximize = "BALANCEDACCURACY",
                                                          featureInclude = "'77701999'", # CHADS2 value
                                                          sorted = "none",
                                                          parallel = TRUE,
                                                          parallelMethod = "TWO",
                                                          branchBound = TRUE,
                                                          subsumption = FALSE,
                                                          binaryReduction = TRUE,
                                                          modelsCurve = TRUE,
                                                          saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

      list_models[[analysisId]] <- list(analysisId=analysisId,
                                        analysisName=paste0("EXPLORE decision rule"),
                                        modelSettings=modelSettings)

    }

    # Mandatory features
    if (analysis_MF && plp != "CHADS2") {

      ### Case including age/sex
      analysisId <- paste0("EXPLORE_", plp, "_MF4")

      modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                          startRulelength = 2,
                                                          endRulelength = 4,
                                                          operatorMethod = "EXHAUSTIVE",
                                                          cutoffMethod = "ALL",
                                                          maximize = "BALANCEDACCURACY",
                                                          featureInclude = "'8532001';'1002'", # TODO: check LABELS -> 8507001 "gender = MALE", 8532001 "gender = FEMALE", 1002 "age in years"
                                                          sorted = "none",
                                                          parallel = TRUE,
                                                          parallelMethod = "TWO",
                                                          branchBound = TRUE,
                                                          subsumption = FALSE,
                                                          binaryReduction = TRUE,
                                                          modelsCurve = TRUE,
                                                          saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

      list_models[[analysisId]] <- list(analysisId=analysisId,
                                        analysisName=paste0("EXPLORE decision rule"),
                                        modelSettings=modelSettings)

      analysisId <- paste0("EXPLORE_", plp, "_MF5")

      modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                          startRulelength = 2,
                                                          endRulelength = 5,
                                                          operatorMethod = "EXHAUSTIVE",
                                                          cutoffMethod = "ALL",
                                                          maximize = "BALANCEDACCURACY",
                                                          featureInclude = "'8532001';'1002'", # TODO: check LABELS -> 8507001 "gender = MALE", 8532001 "gender = FEMALE", 1002 "age in years"
                                                          sorted = "none",
                                                          parallel = TRUE,
                                                          parallelMethod = "TWO",
                                                          branchBound = TRUE,
                                                          subsumption = FALSE,
                                                          binaryReduction = TRUE,
                                                          modelsCurve = TRUE,
                                                          saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

      list_models[[analysisId]] <- list(analysisId=analysisId,
                                        analysisName=paste0("EXPLORE decision rule"),
                                        modelSettings=modelSettings)


      ### Case two task-specific features selected based on clinical knowledge
      # Features to include per prediction task (train set)
      mandatoryFeatures <- list("COPDMortality" = "'1166';'1181'", # smoking, steroids
                                "HeartfailureStroke" = "'1160';'1198'", # atrial fibrillation, hypertension
                                "AsthmaExacerbation" = "'1166';'1181'", # smoking, steroids
                                "HospitalReadmission" ="'1198';'1002'", # hypertension, age
                                "EoLConversation" = "'1215';'1002'") # cancer, age

      analysisId <- paste0("EXPLORE_", plp, "_MF4custom")

      modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                          startRulelength = 2,
                                                          endRulelength = 4,
                                                          operatorMethod = "EXHAUSTIVE",
                                                          cutoffMethod = "ALL",
                                                          maximize = "BALANCEDACCURACY",
                                                          featureInclude = mandatoryFeatures[[plp]],
                                                          sorted = "none",
                                                          parallel = TRUE,
                                                          parallelMethod = "TWO",
                                                          branchBound = TRUE,
                                                          subsumption = FALSE,
                                                          binaryReduction = TRUE,
                                                          modelsCurve = TRUE,
                                                          saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

      list_models[[analysisId]] <- list(analysisId=analysisId,
                                        analysisName=paste0("EXPLORE decision rule"),
                                        modelSettings=modelSettings)

      analysisId <- paste0("EXPLORE_", plp, "_MF5custom")

      modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                          startRulelength = 2,
                                                          endRulelength = 5,
                                                          operatorMethod = "EXHAUSTIVE",
                                                          cutoffMethod = "ALL",
                                                          maximize = "BALANCEDACCURACY",
                                                          featureInclude = mandatoryFeatures[[plp]],
                                                          sorted = "none",
                                                          parallel = TRUE,
                                                          parallelMethod = "TWO",
                                                          branchBound = TRUE,
                                                          subsumption = FALSE,
                                                          binaryReduction = TRUE,
                                                          modelsCurve = TRUE,
                                                          saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

      list_models[[analysisId]] <- list(analysisId=analysisId,
                                        analysisName=paste0("EXPLORE decision rule"),
                                        modelSettings=modelSettings)

    }

    # Run for all models
    write.csv(explore_options, file.path(outputFolder, "explore_options.csv"), row.names = FALSE)

    for (m in names(list_models)) {

      model <- list_models[[m]]

      plpResults <- PatientLevelPrediction::runPlp(plpData = plpData,
                                                   outcomeId = outcomeId,
                                                   modelSettings = model$modelSettings,
                                                   analysisId = paste0(model$analysisId, "_", covariates), # Add name of selected covariates
                                                   analysisName = model$analysisName,
                                                   populationSettings = populationSettings,
                                                   splitSettings = createDefaultSplitSetting(),
                                                   sampleSettings = createSampleSettings(),
                                                   featureEngineeringSettings = createFeatureEngineeringSettings(),
                                                   preprocessSettings = createPreprocessSettings(minFraction=0.001,
                                                                                                 normalize = F,
                                                                                                 removeRedundancy = F),
                                                   logSettings = createLogSettings(),
                                                   executeSettings = createExecuteSettings(runSplitData = F,
                                                                                           runSampleData = F,
                                                                                           runfeatureEngineering = F,
                                                                                           runPreprocessData = F,
                                                                                           runModelDevelopment = T,
                                                                                           runCovariateSummary = T),
                                                   saveDirectory = outputFolder,
                                                   saveData = F,
                                                   loadData = file.path(outputFolder, paste0("Data_", plp, "_", covariates), "TrainTestData"))
    }
  }
}



