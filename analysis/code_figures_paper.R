# Libraries
library(data.table)
library(ggstatsplot)
library(dplyr)
library(ggplot2)
library(survminer)
library(ggpubr)
library(corrplot)
library(ggbreak)
library(cluster)

# Use cases
plptasks <- c("HospitalReadmission", "EoLConversation", "AsthmaExacerbation", "COPDMortality", "HeartfailureStroke")

# Set fixed colors
list_colors <- list("DecisionTree" = "#F8961E", # orange
                    "EXPLORE" = "#90BE6D", # green
                    "GOSDT" = "#265886", # dark blue
                    "IHT" = "#FABDBA" , # pink
                    "LASSO" = "#F94144", # red
                    "RandomForest" = "#76448A", # purple
                    "RIPPER" = "#FBD780", # yellow
                    "XGBoost" = "#9AE0E2")# blue

vec_colors <- as.character(list_colors)
names(vec_colors) <- names(list_colors)

list_colors_tasks <- c("HospitalReadmission" = "#F8961E", # orange
                       "EoLConversation" = "#265886", # dark blue
                       "AsthmaExacerbation" = "#F94144", # red
                       "COPDMortality" = "#FBD780", # yellow
                       "HeartfailureStroke" = "#90BE6D") # green

vec_colors_tasks <- as.character(list_colors_tasks)
names(vec_colors_tasks) <- names(list_colors_tasks)

list_colors_groups <- c("< 10%" = "#FABDBA", # orange
                        "10%-25%" = "#76448A", # dark blue
                        "25%-75%" = "#FBD780", # red
                        "75%-90%" = "#9AE0E2", # yellow
                        "> 90%" = "#90BE6D") # green
vec_colors_groups <- as.character(list_colors_groups)
names(vec_colors_groups) <- names(list_colors_groups)


# Order of columns
col_order <- plptasks

# Get paths
root <- getwd()
resultFolder <- file.path(root, "shiny/output/export_2024-08-final")
outputFolder <- file.path(resultFolder, "output") # Figures and tables

if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}

# Load functions
source(file.path(root, "shiny/helper-plots.R"))
source(file.path(root, "code/helper-functions.R"))


### TABLE: characteristics of prediction tasks
table <- read.csv(file.path(resultFolder, paste0("summary_tasks_Phenotypes.csv")), row.names=1)
table <- round_df(table, 2)

table_plptasks <- t(table)

table_plptasks <- table_plptasks[c("Observations_train", "Outcomes_train", "Outcome_rate_train"),]
rownames(table_plptasks) <- c("Population size", "Number of outcomes", "Outcome rate")

write.csv(table_plptasks, file.path(outputFolder, "table_plptasks.csv"))

### TABLE: phenotype counts per plp task
for (p in 1:length(plptasks)) {
  plp <- plptasks[p]

  covariates <- read.csv(file.path(resultFolder, paste0("covariates_", plp, "_Phenotypes.csv")))
  table <- round_df(covariates, 3)

  filtered_table <- data.frame(table[,c("CovariateMean")])
  colnames(filtered_table) <- c(plp)

  filtered_table[-3,] <- filtered_table[-3,]*100 # multiply all except age = index 3

  if (p == 1) {
    table_phenotypes <- filtered_table
    names <- stringr::str_remove_all(table$covariateName, "Cohort_covariate during day -365 through 0 days relative to index: ")
    names <- trimws(names) # Remove leading and trailing spaces
    rownames(table_phenotypes) <- names
  } else {
    table_phenotypes <- cbind(table_phenotypes, plp=filtered_table)
  }
}

table_phenotypes[is.na(table_phenotypes)] <- 0 # Fill NA with 0 count
write.csv(table_phenotypes, file.path(outputFolder, "table_phenotypes.csv"))

# COMBINED TABLE 1
table1 <- rbind(as.data.frame(table_plptasks), table_phenotypes)
write.csv(table1, file.path(outputFolder, "table1.csv"))

### FIGURE: AUC plots
table <- read.csv(file.path(resultFolder, paste0("output_methods.csv")))

data <- table %>% filter(Option %in% c(5,10))

# --- Save tables for appendix ---
export_train <- round_df(reshape2::dcast(data, Method ~ Data, value.var="Perf_AUC_Train_Prob"),2)
export_train <- export_train[,c("Method", col_order)]
export_test <- round_df(reshape2::dcast(data, Method ~ Data, value.var="Perf_AUC_Test_Prob"),2)
export_test <- export_test[,c("Method", col_order)]

export <- cbind(export_train,export_test)
write.csv(export, file.path(outputFolder, "table_auc.csv"))
# --- End ---

plot_data <- data_to_curve(data, performance = "Test", evaluate = "Prob")

plot_data <- plot_data[order(plot_data$values_TPR, decreasing = FALSE),] # Result can be non-monotonically increasing for test set
plot_data <- plot_data[plot_data$values_models != "model not available",]

plot_data$method <- sapply(plot_data$group, function(g) unlist(strsplit(g, split = "_"))[[1]])

# AUC
ggplot(plot_data, aes(values_FPR, values_TPR, group = group, color = method)) +
  geom_line() + facet_wrap(vars(data), scales="free", ncol=2) +
  theme_minimal(base_size = 16) + xlab("1 - Specificity") + ylab("Sensitivity")  + scale_color_manual(values=vec_colors, name="Model Algorithm")
ggsave(filename = file.path(outputFolder, "figs3_auc_curves.png"), width = 10)

### FIGURE: critical difference diagram AUC
table <- read.csv(file.path(resultFolder, paste0("output_methods.csv")))

data <- table %>% filter(Option %in% c(10,5))

plot_data <- data %>% group_by(Data) %>% mutate(AUC_value=Perf_AUC_Test_Prob-median(Perf_AUC_Test_Prob))

# Perform the test
diff<-compare_means(AUC_value ~ Method,  data = plot_data)
diff<-diff[diff$p < 0.01,]
my_comparisons <- lapply(1:nrow(diff), function(d) c(diff$group1[d],diff$group2[d]))
# my_comparisons <- list(c("EXPLORE", "LASSO"), c("LASSO", "RandomForest"), c("EXPLORE", "RIPPER") )

# Visualize the expression profile
ggboxplot(plot_data, x = "Method", y = "AUC_value", color = "Method", add = "jitter", legend = "none") +
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + # Add pairwise comparisons p-valu
  # stat_compare_means(label = "p.signif", method = "t.test") +    # Pairwise comparison against all
  theme_minimal(base_size = 16) + rotate_x_text(angle = 45) + xlab("Model Algorithm") + ylab("Median-centered AUROC") + scale_color_manual(values=vec_colors,name= "") # + geom_hline(yintercept = mean(table$AUC_value), linetype = 2) # Add horizontal line at base mean
ggsave(filename = file.path(outputFolder, "fig2a_boxplot_critdif_auc.png"), width = 10)


### FIGURE: trade-off
table <- read.csv(file.path(resultFolder, paste0("output_methods.csv")))

data <- table %>% filter(Option %in% c(5, 10))

size_plptasks <- data.frame()
for (name in plptasks) { # name <- plptasks[1]
  table <- read.csv(file.path(resultFolder, paste0("models_", name, "_Phenotypes.csv")))
  table <- table %>% filter(option %in% c(5, 10))

  size_task <- table[table$option %in% c(1, 5, 10), c("method", "model.size.vars", "model.size.comps")]
  size_task$Data <- name

  size_plptasks <- rbind(size_plptasks, size_task)
}

combined_data <- merge(size_plptasks, data, by.x= c("method", "Data"), by.y=c("Method", "Data"))

# --- Save tables for appendix ---
export_size <- reshape2::dcast(combined_data, method ~ Data, value.var="model.size.vars")
export_size <- export_size[,c("method", col_order)]
write.csv(export_size, file.path(outputFolder, "table_size.vars.csv"))

export_size <- reshape2::dcast(combined_data, method ~ Data, value.var="model.size.comps")
export_size <- export_size[,c("method", col_order)]
write.csv(export_size, file.path(outputFolder, "table_size.comps.csv"))
# --- End ---

combined_data$Size <- as.numeric(combined_data$model.size.vars)
combined_data$Inverse_Size <- sapply(combined_data$model.size.comps, function(i) { 1 / i})

# Values AUC
plot_data <- combined_data %>% group_by(Data) %>% mutate(AUC_value_test=Perf_AUC_Test_Prob-median(Perf_AUC_Test_Prob), AUC_value_train=Perf_AUC_Train_Prob-median(Perf_AUC_Train_Prob), BA_value_test=Perf_BalancedAccuracy_Test_Class-median(Perf_BalancedAccuracy_Test_Class), BA_value_train=Perf_BalancedAccuracy_Train_Class-median(Perf_BalancedAccuracy_Train_Class))
# plot_data <- combined_data %>% group_by(Data) %>% mutate(AUC_value_test=Perf_AUC_Test_Prob, AUC_value_train=Perf_AUC_Train_Prob)

# FINAL FIGURES
ggplot(plot_data, aes(x=AUC_value_test, y=Inverse_Size, group=Data, shape=method, color=Data)) +
  stat_smooth(method = "lm", se = FALSE, geom='line', alpha=1) +
  geom_point(size=2.5) +
  theme_minimal(base_size = 16)  + xlab(paste0("Median-centered AUROC")) + ylab("Model interpretability (1/Size)") +scale_shape(name="")+ scale_color_manual(values=vec_colors_tasks, name = "") + xlim(-0.075, 0.1) + ylim(-0.01,0.51) # + xlim(0.6, 0.9)
ggsave(filename = file.path(outputFolder, "fig3a_TradeOffCombined_test_comps.png"), width = 10)

ggplot(plot_data, aes(x=AUC_value_test, y=Size, group=Data, shape=method, color=Data)) +
  stat_smooth(method = "lm", se = FALSE, geom='line', alpha=1) +
  geom_point(size=2.5) +
  theme_minimal(base_size = 16)  + xlab(paste0("Median-centered AUROC")) + ylab("Number of variables") +scale_shape(name="") + scale_color_manual(values=vec_colors_tasks, name = "")  + xlim(-0.075, 0.1) # + xlim(0.6, 0.9)
ggsave(filename = file.path(outputFolder, "fig3b_TradeOffCombined_test_vars.png"), width = 10)


### FIGURE: Rashomon value - generalizability
table <- read.csv(file.path(resultFolder, paste0("rr_candidatepredictions.csv")))

plot_data <- reshape2::melt(table, id = c("Run", "Data" , "Percentage" , "Covariates",  "Option" ,"Method", "Model"))

plot_data$Evaluation <- sapply(as.character(plot_data$variable), function(i) unlist(strsplit(i, split = "_"))[[3]])
plot_data$Evaluation[plot_data$Evaluation=="Train"] <- "Training set"
plot_data$Evaluation[plot_data$Evaluation=="Test"] <- "Test set"
plot_data$Metric <- sapply(as.character(plot_data$variable), function(i) unlist(strsplit(i, split = "_"))[[2]])
plot_data$value <- as.numeric(plot_data$value)

temp <- plot_data %>% filter(Metric=="BalancedAccuracy", Option == 5)

performance_BA5_train <- list("COPDMortality" = 0.7051,
                          "HeartfailureStroke" = 0.7012,
                          "AsthmaExacerbation" = 0.6249,
                          "HospitalReadmission" = 0.5966,
                          "EoLConversation" = 0.6677)

performance_BA5_test <- list("COPDMortality" = 0.6882,
                              "HeartfailureStroke" = 0.6685,
                              "AsthmaExacerbation" = 0.6109,
                              "HospitalReadmission" = 0.5902,
                              "EoLConversation" = 0.6552)

vline <- data.frame(Data=names(performance_BA5_train), Best_Train=unlist(performance_BA5_train), Best_Test=unlist(performance_BA5_test))

ggplot(temp, aes(x=value, fill=Evaluation)) +
  geom_density(alpha=0.5) + geom_vline(data=vline, mapping=aes(xintercept=Best_Train), linetype="dotted")+ ggtitle("a) Generalizability - \n does performance transfer to the test set?") +
  geom_vline(data=vline, mapping=aes(xintercept=Best_Test), linetype="dashed")+
  facet_wrap(vars(Data), scales="free", ncol=2) +
  theme_minimal(base_size = 16) + xlab("Balanced Accuracy") + ylab("Number of models")  + guides(fill = guide_legend(reverse=TRUE)) + scale_fill_discrete(name = "")# + scale_fill_manual(values=c("#9AE0E2", "#FABDBA"))
ggsave(filename = file.path(outputFolder, "fig4a_RR_BA_5.png"), width = 10)


### FIGURE: Rashomon value - interpretability
plot_data <- data.frame()
first=TRUE

for (name in plptasks) { # name <- plptasks[1]
  for (o in c("4","5")) { # o = "4"

    table <- read.csv(file.path(resultFolder, paste0("rr_candidatemodels_", name, "_", o,".csv")))

    data <- table[4:(ncol(table)-1)]
    data[!is.na(data)] <- 1
    data[is.na(data)] <- 0
    col_names <- colnames(data)

    if (first) {
      features <- col_names
      first=FALSE
    }

    data <- apply(data, 1, function(col) as.numeric(col))
    data <- as.data.frame(t(data))

    # Remove duplicate rows
    save <- nrow(data)
    data <- data[!duplicated(data),]

    print(paste0(name, " ", o, " ", stability_Nogueira(data), " before ", save, " after ", nrow(data))) # High stability = smaller search space?

    percVars <- colSums(data)*100/nrow(data)
    names(percVars) <- col_names

    percVars2 <- sapply(features, function(f) ifelse(f %in% names(percVars), percVars[f], 0))
    names(percVars2) <- col_names

    plot_data <- rbind(plot_data, c(name, o, stability_Nogueira(data), percVars2))
  }
}
colnames(plot_data) <- c("Data", "Option", "Stability", features)

transformed_data <- reshape2::melt(plot_data, id.vars=c("Data", "Option"))
transformed_data$value <- as.numeric(transformed_data$value)

transformed_data$groups <- cut(transformed_data$value,
                        breaks=c(-Inf, 10, 25, 75, 95, Inf),
                        labels=c("< 10%","10%-25%", "25%-75%", "75%-90%", "> 90%"))

transformed_data <- transformed_data %>% filter(variable != "Stability")

# Data transformation for rule length 5
df <- transformed_data %>% filter(Option == "5") %>%
  group_by(Data,groups) %>% # Variable to be transformed
  count() %>%
  ungroup() %>%
  group_by(Data) %>%
  mutate(perc = `n` / sum(`n`)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

df$labels[df$perc < 0.15] <- ""

ggplot(data=df, aes(x=Data, y=perc, fill=groups)) + ggtitle("b) Model diversity – \n how frequent are features included?") +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(name="", labels = scales::percent)+
  theme_minimal(base_size = 16) + rotate_x_text(angle = 45)  + xlab("") + scale_fill_manual(values=vec_colors_groups, name="")
ggsave(filename = file.path(outputFolder, "fig4b_RR_interpretability_bars_5.png"), width = 4.2, height = 6)

# --- Save tables for appendix ---
export <- round_df(reshape2::dcast(df, Data ~ groups, value.var="perc"),3)
write.csv(t(export), file.path(outputFolder, "table_perc_included_5.csv"))
# --- End ---

# Data transformation for rule length 2
df <- transformed_data %>% filter(Option == "4") %>%
  group_by(Data,groups) %>% # Variable to be transformed
  count() %>%
  ungroup() %>%
  group_by(Data) %>%
  mutate(perc = `n` / sum(`n`)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

# --- Save tables for appendix ---
export <- round_df(reshape2::dcast(df, Data ~ groups, value.var="perc"),3)
write.csv(t(export), file.path(outputFolder, "table_perc_included_4.csv"))
# --- End ---

### FIGURE: mandatory features
table1 <- read.csv(file.path(resultFolder, paste0("output_methods.csv")))

data <- table1 %>% filter(Method == "EXPLORE" & Option %in% c("4", "5", "MF4", "MF5", "MF4custom", "MF5custom")) # Only results MF

data$EndRulelength <- as.factor(stringr::str_extract(data$Option, "\\d"))
data$MF<- ifelse(data$Option %like% "MF", 1, 0)
data$MF[data$Option %like% "custom"] <- 2
data$MF <- as.factor(data$MF)

plot_data <- melt(data, id.vars = c("Run", "Data","Option" ,"Method", "MF"), measure.vars=c("Perf_BalancedAccuracy_Train_Class", "Perf_BalancedAccuracy_Test_Class", "Perf_AUC_Train_Prob", "Perf_AUC_Test_Prob"))

plot_data$Evaluation <- sapply(as.character(plot_data$variable), function(i) unlist(strsplit(i, split = "_"))[[3]])
plot_data$Metric <- sapply(as.character(plot_data$variable), function(i) unlist(strsplit(i, split = "_"))[[2]])
plot_data$value <- as.numeric(plot_data$value)

plot_data$EndRulelength <- as.factor(stringr::str_extract(plot_data$Option, "\\d"))

res <- plot_data %>% filter(MF == 0) %>% group_by(Metric, Evaluation, Data, EndRulelength) %>% reframe(Baseline=value)
plot_data <- merge(plot_data, res, by=c("Metric", "Evaluation", "Data", "EndRulelength"), all.x=TRUE)

plot_data = plot_data %>% group_by(Metric, Evaluation, Data, EndRulelength) %>% mutate(value_improvement=value-Baseline)
plot_data$Metric <- ifelse(plot_data$Metric=="AUC", "AUROC", "Balanced Accuracy")

plot_data$Evaluation[plot_data$Evaluation == "Train"] <- "Training set"
plot_data$Evaluation[plot_data$Evaluation == "Test"] <- "Test set"

plot_data$MF  <- as.character(plot_data$MF)
plot_data$MF[plot_data$MF == 0] <- "Baseline"
plot_data$MF[plot_data$MF == 1] <- "Age/sex"
plot_data$MF[plot_data$MF == 2] <- "Task-specific features"

plot_data$MF <- factor(plot_data$MF, levels=c("Baseline", "Age/sex", "Task-specific features"))

# Per MF option 1/2/3
temp <- plot_data %>% filter(Metric == "AUROC", EndRulelength==5)
ggplot(temp, aes(MF, value_improvement, fill = Evaluation)) + ggtitle("") +
  geom_bar(stat="identity", position=position_dodge()) + geom_hline(yintercept =0, linetype = "dashed") +
  theme_minimal(base_size=16) + rotate_x_text(angle = 45) + ylim(-0.05,0.05) + xlab("") + ylab("AUROC improvement") + guides(fill = guide_legend(reverse=TRUE)) + scale_fill_discrete(name = "") # + scale_fill_manual(values=c("#9AE0E2", "#FABDBA"))
ggsave(filename = file.path(outputFolder, "fig5a1_AUC.png"), width = 5, height = 6)

temp <- plot_data %>% filter(Metric == "Balanced Accuracy", EndRulelength ==5)
ggplot(temp, aes(MF, value_improvement, fill = Evaluation)) + ggtitle("a) Incorporating domain knowledge") +
  geom_bar(stat="identity", position=position_dodge()) + geom_hline(yintercept =0, linetype = "dashed") +
  theme_minimal(base_size=16) + rotate_x_text(angle = 45) + ylim(-0.05,0.05) + xlab("") + ylab("Balanced accuracy improvement") + guides(fill = guide_legend(reverse=TRUE)) + scale_fill_discrete(name = "") # + scale_fill_manual(values=c("#9AE0E2", "#FABDBA"))
ggsave(filename = file.path(outputFolder, "fig5a2_BA.png"), width = 5, height = 6)

### FIGURE: CHADS2
table <- read.csv(file.path(resultFolder, paste0("output_methods.csv")))

data <- table %>% filter(Data == "CHADS2")
data <- data[,c("Run", "Perf_AUC_Train_Prob", "Perf_AUC_Test_Prob", "Perf_BalancedAccuracy_Train_Class", "Perf_BalancedAccuracy_Test_Class")]
data$Run <- c("CHADS2 score", "EXPLORE - CHADS2 as feature", "EXPLORE - retrain CHADS2")

data <- data[c(1,3,2),] # Reorder rows
data$Run <- factor(data$Run, labels=data$Run, levels=data$Run)

plot_data <- reshape2::melt(data, id = c("Run"))

plot_data$evaluation <- sapply(as.character(plot_data$variable), function(i) unlist(strsplit(i, split = "_"))[[3]])
plot_data$evaluation[plot_data$evaluation == "Train"] <- "Training set"
plot_data$evaluation[plot_data$evaluation == "Test"] <- "Test set"
plot_data$metric <- sapply(as.character(plot_data$variable), function(i) unlist(strsplit(i, split = "_"))[[2]])

plot_data = plot_data %>% group_by(metric, evaluation) %>% mutate(value_improvement=value-min(value)) #
plot_data$metric <- ifelse(plot_data$metric=="AUC", "AUROC", "Balanced Accuracy")

temp <- plot_data %>% filter(metric == "AUROC")
ggplot(temp, aes(Run, value_improvement, fill = evaluation)) +
  geom_bar(stat="identity", position=position_dodge()) + geom_hline(yintercept =0, linetype = "dashed") + ggtitle("") +
  theme_minimal(base_size = 16) + rotate_x_text(angle = 45) + ylim(-0.1,0.1) + xlab("") + ylab("") + guides(fill = guide_legend(reverse=TRUE)) + scale_fill_discrete(name = "")
ggsave(filename = file.path(outputFolder, "fig5b2_CHADS2_AUC.png"), width = 5, height = 6)

temp <- plot_data %>% filter(metric == "Balanced Accuracy")
ggplot(temp, aes(Run, value_improvement, fill = evaluation)) + ggtitle("b) Improving existing models") +
  geom_bar(stat="identity", position=position_dodge()) +   geom_hline(yintercept =0, linetype = "dashed") +
  theme_minimal(base_size = 16) + rotate_x_text(angle = 45) + ylim(-0.1,0.1) + xlab("") + ylab("") + guides(fill = guide_legend(reverse=TRUE)) + scale_fill_discrete(name = "")
ggsave(filename = file.path(outputFolder, "fig5b1_CHADS2_BA.png"), width = 5, height = 6)
