
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

data_to_curve <- function(plot_data, performance, evaluate) {

  if (paste0(performance, "_", evaluate) == "Test_Class") {
    plot_data <- plot_data[, !grepl("Perf_", colnames(plot_data)) & !grepl("_Train", colnames(plot_data)) & !grepl("_Test_Prob", colnames(plot_data))]

  } else if (paste0(performance, "_", evaluate)  == "Test_Prob") {
    plot_data <- plot_data[, !grepl("_Train", colnames(plot_data)) & !grepl("_Test_Class", colnames(plot_data))]

  } else if (paste0(performance, "_", evaluate)  == "Train_Class") {
    plot_data <- plot_data[, !grepl("Perf_", colnames(plot_data)) & !grepl("_Test", colnames(plot_data)) & !grepl("_Train_Prob", colnames(plot_data))]

  } else if (paste0(performance, "_", evaluate)  == "Train_Prob") {
    plot_data <- plot_data[, !grepl("Perf_", colnames(plot_data)) & !grepl("_Test", colnames(plot_data)) & !grepl("_Train_Class", colnames(plot_data))]
  }

  plot_data$Group <- paste0(plot_data$Method, "_", plot_data$Option, "_", plot_data$Covariates)

  colnames(plot_data) <- stringr::str_replace(colnames(plot_data), paste0("_", performance, "_", evaluate), "")

  # suppressWarnings: Warning: NAs introduced by coercion (happens because NAs converting to numerical)
  all_output <- suppressWarnings(lapply(1:nrow(plot_data), function(row) {
    values_TPR <- as.numeric(strsplit(plot_data$Curve_TPR[row], split = "_")[[1]])
    values_FPR <- as.numeric(strsplit(plot_data$Curve_FPR[row], split = "_")[[1]])

    if (!is.na(plot_data$Curve_Models[row])) {
      values_models <- strsplit(plot_data$Curve_Models[row], split = "_")[[1]]
    } else {
      values_models <- NA
    }

    if (!is.na(plot_data$Curve_Thresholds[row])) {
      values_threshold <- as.numeric(strsplit(as.character(plot_data$Curve_Thresholds[row]), split = "_")[[1]])
    } else {
      values_threshold <- NA
    }

    if ("Curve_Sensitivity" %in% colnames(plot_data)) {
      values_sensitivity <- as.numeric(strsplit(plot_data$Curve_Sensitivity[row], split = "_")[[1]])
    } else {
      values_sensitivity <- NA
    }

    if ("Curve_Specificity" %in% colnames(plot_data)) {
      values_specificity <- as.numeric(strsplit(plot_data$Curve_Specificity[row], split = "_")[[1]])
    } else {
      values_specificity <- NA
    }

    if ("Curve_PPV" %in% colnames(plot_data)) {
      values_PPV <- as.numeric(strsplit(plot_data$Curve_PPV[row], split = "_")[[1]])
    } else {
      values_PPV <- NA
    }

    if ("Curve_NPV" %in% colnames(plot_data)) {
      values_NPV <- as.numeric(strsplit(plot_data$Curve_NPV[row], split = "_")[[1]])
    } else {
      values_NPV <- NA
    }

    if ("Curve_BalancedAccuracy" %in% colnames(plot_data)) {
      values_BA <- as.numeric(strsplit(plot_data$Curve_BalancedAccuracy[row], split = "_")[[1]])
    } else {
      values_BA <- NA
    }

    if ("Curve_Calibration" %in% colnames(plot_data)) {
      values_Calibration <- as.numeric(strsplit(plot_data$Curve_Calibration[row], split = "_")[[1]])
    } else {
      values_Calibration <- NA
    }

    output <- data.frame(data = plot_data$Data[row], group = plot_data$Group[row], iteration = plot_data$Iteration[row], values_TPR, values_FPR, values_models, values_threshold,
                         values_sensitivity, values_specificity, values_PPV, values_NPV, values_BA, values_Calibration, auc=plot_data$Perf_AUC[row],
                         N_outcomes = plot_data$N_outcomes[row], N_controls = plot_data$N_controls[row],
                         N_total = plot_data$N_total[row], row.names = NULL)

    return(output)
  }))
  all_output <- rbindlist(all_output)

  # Correct bounds thresholds if needed
  all_output$values_threshold[all_output$values_threshold == -Inf] <- 0
  all_output$values_threshold[all_output$values_threshold == Inf] <- 1

  return(all_output)
}

stability_Nogueira <- function(covariates, top = NULL) {
  Z <- selected_covariates(covariates, top)

  if (nrow(Z) >= 5) {
    d <- ncol(Z)
    kbar <- mean(rowSums(Z)) # if !is.null(top) -> k_bar = top
    colVars <- sapply(1:ncol(Z), function(col) var(Z[,col]))

    stability <- 1 - (mean(colVars) / ((kbar/d)*(1-kbar/d)))
  } else {
    stability <- NA
  }

  return(stability)
}


selected_covariates <- function(covariates, top = NULL) {
  # specify matrix Z with selected covariates
  if (!is.null(top)) {
    max_row <- apply(covariates, 1, function(row) {sort(abs(row), decreasing = TRUE)[top]})

    Z <- covariates
    Z <- sapply(1:nrow(Z), function(r) {
      row <- Z[r,]
      row[abs(Z[r,]) < max_row[r]] <- 0
      return(unlist(row))
    })

    Z <- as.matrix(t(Z)) # TODO: check this!!
    Z[Z != 0] <- 1

  } else {
    Z <- covariates
    Z[Z != 0] <- 1  # binary matrix indicating covariates selected
  }

  rownames(Z) <- rownames(covariates)

  return(Z)
}

