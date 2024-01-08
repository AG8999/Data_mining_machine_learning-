############# INSTALLING and IMPORTING PACKAGES ################################

#install.packages(c('caret', 'ggplot2', 'caTools','skimr','gridExtra','tidyverse','GGally','pROC','corrplot','randomForest','usdm','car','broom','pROC'))

library(caret)
library(ggplot2)
library(caTools)
library(skimr)
library(gridExtra)
library(tidyverse)
library(GGally)
library(pROC)
library(corrplot)
library(randomForest)
library(usdm)
library(car)
library(broom)
library(pROC)
options(warn = -1)


################ IMPORTNG CSV FILE #######################

data <- read.csv("Datasets/heart_attack_predict.csv")


################ EXPLORATORY DATA ANALYSIS (EDA) PHASE #######################

summary(data)

skimmed_data <- skim_to_wide(data)
(skimmed_data)

str(data)

# Checking missing values 
result <- colSums(is.na(data))

# Create a data frame
result_df <- data.frame(Column = names(result), NA_Count = result)

# Print the data frame
print(result_df)

#Checking for unique values in each column
data%>%summarise_all(funs(n_distinct))

################ DATA PRE-PROCESSING PHASE #######################

#converting target variable into class
data$HeartDiseaseorAttack <- factor(data$HeartDiseaseorAttack , labels = c('Heart_Disease','No_Heart_Disease'))

data$imbalanceHealth <-  (data$MentHlth + data$PhysHlth)/2
data$MentHlth <- NULL
data$PhysHlth <- NULL

# Histograms for numeric variables
data %>%
  select(BMI,imbalanceHealth , Age) %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "darkred", color = "black") +
  facet_wrap(~key, scales = "free") +
  labs(title = "Histograms of Numeric Variables")


##################Box Plots and Outliers ###############################

data %>%
  gather(key = "variable", value = "value", -HeartDiseaseorAttack) %>%
  ggplot(aes(x = factor(HeartDiseaseorAttack), y = value)) +
  geom_boxplot(fill = "blue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Box Plots of Variables by Heart Disease or Attack")

#############  HANDLING OUTLIERS #######################################

# Calculate mean, median, and standard deviation
mean_BMI <- mean(data$BMI)
median_BMI <- median(data$BMI)
sd_BMI <- sd(data$BMI)

# Identify outliers beyond a threshold (e.g., mean ± 1.5 * sd) 

lower_threshold <- mean_BMI - 1.5 * sd_BMI
upper_threshold <- mean_BMI + 1.5 * sd_BMI
outliers <- data$BMI[data$BMI < lower_threshold | data$BMI > upper_threshold]

# Impute outliers with the median

data$BMI[data$BMI < lower_threshold | data$BMI > upper_threshold] <- median_BMI

# checking handled outliers
data %>%
  gather(key = "variable", value = "value", -HeartDiseaseorAttack) %>%
  ggplot(aes(x = factor(HeartDiseaseorAttack), y = value)) +
  geom_boxplot(fill = "blue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Box Plots of Variables by Heart Disease or Attack")


############## Correlation Analysis#######################################

cor_matrix <- cor(data[, c("BMI", "imbalanceHealth", "Age")])
print(cor_matrix)

corrplot(cor_matrix, method = 'square', order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('BrBG'))

############## CHECK MULTICOLLINEARITY ##################################
data[, c(5, 18, 21)] <- lapply(data[, c(5, 18, 21)], as.numeric)

vif <- usdm::vif(data[,c(5,18,21)])
(vif)

all_column_names <- colnames(data)
exclude_column_name <- "HeartDiseaseorAttack"

# Remove the specified column name from the vector
categorical_value <- all_column_names[all_column_names != exclude_column_name]


#categorical_value <- c(colnames(~data$HeartDiseaseorAttack)) 
categorical_value
# Create a list to store plots
plot_list <- list()

for (variable in categorical_value) {
  # Create a grouped bar plot
  plot <- ggplot(data, aes_string(x = variable , fill = "HeartDiseaseorAttack")) +
    geom_bar(position = "dodge", color = "black") +
    labs(title = paste("Plot of", variable),
         x = variable, y = "Frequency", fill = "Target") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  variable
  # Add the plot to the list
  plot_list[[length(plot_list) + 1]] <- plot
}

# Arrange and display the plots in a 2x2 grid
grid.arrange(grobs = plot_list, ncol = 4)


####################### PRE-PROCESSSING ################################

preProcessSteps_data <- c("nzv","center", "scale")
preprocessed_data <- preProcess(data, method = preProcessSteps_data)
scaled_data <- predict(preprocessed_data, newdata = data)

vif2 <- usdm::vif(scaled_data[,c(1,3:19)])
(vif2)

cor_matrix2 <- cor(data[, c(1,3:19)])

corrplot(cor_matrix2,  method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.6, order = 'AOE', diag=FALSE)
####################  SPLIT DATA INTO TRAIN AND TEST DATA ########################

set.seed(23106786)
rows <- sample(nrow(scaled_data))
shuffled_data <- scaled_data[rows, ]
split <- round(nrow(scaled_data) * 0.80)

train_data <- shuffled_data[1: split, ]
test_data <- shuffled_data[(split+1): nrow(scaled_data), ]


#################### KNN MODEL ##################################################


k1 <- round(sqrt(dim(train_data)[1]))

knnGrid <- expand.grid(k = c(k1, 5))

model_knn <- train(HeartDiseaseorAttack ~ .,
                 data = train_data,
                 method = "knn",
                 trControl = trainControl(
                   method = "cv",
                   number = 2,
                   summaryFunction = twoClassSummary,
                   classProbs = TRUE,
                   verboseIter = TRUE,
                 ),
                 tuneGrid = knnGrid
)
(model_knn)
p_knn <- predict(model_knn,test_data)

colAUC(as.numeric(p_knn), test_data$HeartDiseaseorAttack, plotROC = TRUE)

confusionMatrix(reference= test_data$HeartDiseaseorAttack, data= p_knn, mode="everything", positive="Heart_Disease")




###################### RANDOM FOREST MODEL ##########################################


model_RF <- train(HeartDiseaseorAttack ~ .,
                 data = train_data,
                 method = "rf",
                 trControl = trainControl(
                   method = "cv",
                   number = 2,
                   summaryFunction = twoClassSummary,
                   classProbs = TRUE,
                   verboseIter = TRUE
                 ),
)
(model_RF)
p_RF <- predict(model_RF,test_data)

colAUC(as.numeric(p_RF), test_data$HeartDiseaseorAttack, plotROC = TRUE)

confusionMatrix(reference= test_data$HeartDiseaseorAttack, data= p_RF, mode="everything", positive="Heart_Disease")



############################# LOGISTIC REGRESSION MODEL ###############################


model_LR <- train(HeartDiseaseorAttack ~ .,
                 data = train_data,
                 method="glm", 
                 family="binomial",
                 trControl = trainControl(
                   method = "cv",
                   number = 2,
                   summaryFunction = twoClassSummary,
                   classProbs = TRUE,
                   verboseIter = TRUE,
                 ),
)
(model_LR)
p_LR <- predict(model_LR,test_data)

colAUC(as.numeric(p_LR), test_data$HeartDiseaseorAttack, plotROC = TRUE)

confusionMatrix(reference= test_data$HeartDiseaseorAttack, data= p_LR, mode="everything", positive="Heart_Disease")


################## ROC PLOT FOR COMPARISON ##################


pred_LR <- predict(model_LR, newdata = test_data, type = "prob")[, "Heart_Disease"]
pred_RF <- predict(model_RF, newdata = test_data, type = "prob")[, "Heart_Disease"]
pred_knn <- predict(model_knn, newdata = test_data, type = "prob")[, "Heart_Disease"]

# Assign column names
colnames_pred_LR <- "pred_LR"
colnames_pred_RF <- "pred_RF"
colnames_pred_knn <- "pred_knn"


# Create predictor_matrix with column names
predictor_matrix <- data.frame(pred_LR = pred_LR, pred_RF = pred_RF,pred_knn = pred_knn)

# Extract response variable from test_data
response_variable <- as.numeric(test_data$HeartDiseaseorAttack) - 1  # Convert factor to numeric (0 or 1)

# Create ROC curves for both models
roc_RF <- roc(response_variable, pred_RF)
roc_LR <- roc(response_variable, pred_LR)
roc_knn <- roc(response_variable, pred_knn)


# Display column names of predictor_matrix
colnames(predictor_matrix)

# Plot ROC curves for both models
plot(roc_LR, col = "blue", main = "ROC Curve Comparison", lty = 2, lwd = 2.5)
plot(roc_RF, col = "red", add = TRUE, lty = 2, lwd = 2.5)
plot(roc_knn, col = "green", add = TRUE, lty = 2, lwd = 2.5)
legend("bottomright", legend = c("Logistic Regression", "Random Forest" ,"KNN"), col = c("blue", "red","yellow"), lty = 1, lwd = 5)

# Getting Meaningful analysis by achieving Top variables affecting heart disease

coefficients <- coef(model_LR$finalModel)
print(coefficients)

# Create a data frame for visualization
coeff_df <- data.frame(
  Variable = names(coefficients),
  Coefficient = coefficients
)

# Filter and arrange coefficients
coeff_filtered <- coeff_df %>%
  arrange(desc(abs(Coefficient))) %>%
  filter(abs(Coefficient) > 0.05)

ggplot(coeff_filtered, aes(x = Variable, y = Coefficient, fill = Variable)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top variables that affects Heart Disease or Heart attack ")
