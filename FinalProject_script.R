# Importing the library
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

# Loading the data
df <-
  read_csv("C:/Users/kunle/Desktop/Spring 2022/Stat-515/final_project/diabetes.csv")

# Structure of the data
str(df)
dim(df)
names(df)

# change the datatype of the response variable
df$Outcome = as.factor(df$Outcome)
df$Outcome <- factor(df$Outcome, levels=c("0", "1"),
                     labels = c("Neg", "Pos")) 

# Diabetes Distribution
ggplot(df, aes(x= Outcome, fill = Outcome)) + 
  geom_bar(color = 'black') +
  labs(title = "Diabetes Distribution", x = "Diabetes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,550),
                     expand = c(0,0))

# Descriptive Statistics
summary(df)

# Check sum of missing values
sum(is.na(df))

list( Column = colSums(df == 0), 
      Row = sum(rowSums(df == 0)))

# EXploratory Analysis

# Blood Pressure
mean_bp <- mean(df$BloodPressure[df$BloodPressure > 0])
df$BloodPressure <- 
  ifelse(df$BloodPressure == 0,
         round(mean_bp,0), df$BloodPressure)

# Glucose
mean_Glu <- mean(df$Glucose[df$Glucose > 0])
df$Glucose <- 
  ifelse(df$Glucose == 0,
         round(mean_Glu,0), df$Glucose)

# SkinThickness
mean_SkT <- mean(df$SkinThickness[df$Glucose > 0])
df$SkinThickness <- 
  ifelse(df$SkinThickness == 0,
         round(mean_SkT,0), df$Glucose)

# Insulin
mean_Insulin <- mean(df$Insulin[df$Insulin > 0])
df$Insulin <- 
  ifelse(df$Insulin == 0,
         round(mean_Insulin,0), df$Insulin)

# BMI
mean_BMI <- mean(df$BMI [df$BMI  > 0])
df$BMI  <- 
  ifelse(df$BMI  == 0,
         round(mean_BMI ,0), df$BMI )


# Splitting the dataset
set.seed(123)
test_index  <- createDataPartition(y = df$Outcome,
                                   times = 1,
                                   p = 0.75,
                                   list = FALSE)

# create training dataset
training_set <- df[test_index, ] %>%
  as.data.frame()

# create test dataset
test_set <- df[-test_index, ]


# Univariate Analysis

univar_graph <- 
  function(univar_name, univar, data, outcome) {
    
    g_1 <- ggplot(data, aes(x=univar)) + 
      geom_density() + 
      xlab(univar_name) + 
      theme_bw()
    
    g_2 <- ggplot(data, aes(x=univar, fill= outcome)) + 
      geom_density(alpha=0.4) + 
      xlab(univar_name) + 
      theme_bw()
    
    gridExtra::grid.arrange(g_1,
                            g_2,
                            ncol=2,
                            top = paste(univar_name,
                                        "variable","/ [ Skew:",
                                        timeDate::skewness(univar),"]"))
  }

for (x in 1:(ncol(training_set)-1)) {
  univar_graph(univar_name = names(training_set)[x],
               univar = training_set[,x],
               data = training_set,
               outcome = training_set[,'Outcome'])
}


# Bivariate Analysis

# relationship between BMI and Skin thickness
ggplot(df,
        aes(x = BMI,
            y = SkinThickness,
            color = Outcome)) +
 geom_point () + 
 facet_wrap(~Outcome) +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "BMI vs Skin Thickness",
       x = "BMI(kg/m squared)",
       y = "Skin Thickness(mm)") +
  theme(plot.title = element_text(hjust = 0.5))


# relationship between Insulin and Glucose
ggplot(df,
       aes(x = Insulin,
           y = Glucose,
           color = Outcome)) +
  geom_point () + 
  facet_wrap(~Outcome) +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Insulin vs Glucose",
       x = "Insulin(mu U/ml)",
       y = "Glucose") +
  theme(plot.title = element_text(hjust = 0.5))


# Relationship between BMI and diabetes 
box1 <-
  ggplot(df,
         aes(x = Outcome,
             y = BMI,
             color = Outcome)) +
  geom_boxplot(size=1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size  = 3) + 
  geom_jitter(alpha = 0.5, 
              width=.2) +
  labs(title = " BMI vs Diabetes",
       x = "Diabetes",
       y = "BMI(kg/m squared)") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

# Relationship between pregnancies and diabetes 
box2 <-
  ggplot(df,
       aes(x = Outcome,
           y = Pregnancies,
           color = Outcome)) +
  geom_boxplot(size=1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size  = 3) + 
  geom_jitter(alpha = 0.5, 
              width=.2) +
  labs(title = "Pregnancies vs Diabetes",
       x = "Diabetes",
       y = "Pregnancies") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(box1, box2)

# plot the distribution using violin and boxplots
ggplot(df, 
       aes(x = Outcome, 
           y = DiabetesPedigreeFunction)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "black",
               outlier.size = 2) + 
  labs(title = "Diabetes Pedigree Function vs Diabetes",
       x = "Diabetes",
       y = "Diabetes Pedigree Function") +
  theme(plot.title = element_text(hjust = 0.5))

# determine if there is any correlation that exist between all 
# the variables plotted against each other

df_vars <-
  unlist(lapply(df, is.numeric))  

corrplot::corrplot(cor(df[ , df_vars]),
                   method="number")

# Outlier Detection
box_plot <- 
  function(bivar_name,
           bivar,
           data,
           outcome) {
  
  g_1 <-
    ggplot(data = data,
           aes(y = bivar,
               fill = outcome)) +
    geom_boxplot() +
    theme_bw() +
    labs( title = paste(bivar_name,"Outlier Detection", sep =" "),
          y = bivar_name) +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot(g_1)
}

for (x in 1:(ncol(training_set)-1)) {
  box_plot(bivar_name = names(training_set)[x],
           bivar = training_set[,x],
           data = training_set,
           outcome = training_set[,'Outcome'])
}

# Feature Scaling
training_set[, 1:8] = scale(training_set[, 1:8])
test_set[, 1:8] = scale(test_set[, 1:8])

# Logistic Regression

# Fit the model
logit_model <-
  glm(formula = Outcome ~ .,
      family = binomial,
      data = training_set)

# produce the model summary
summary(logit_model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(logit_model)

# Predicting the Test set results
probability_pred <-
  predict(logit_model,
          type = 'response',
          newdata = test_set)

LR_pred = ifelse(probability_pred > 0.5, "Pos", "Neg")
LR_pred = as.factor(LR_pred)

# Making the Confusion Matrix
cm_LR <- 
  confusionMatrix(table(LR_pred, test_set$Outcome))

# Assessing model accuracy
mean(LR_pred == test_set$Outcome)


# Random Forest

# Fit a random forest to predict 
Rf_model <- 
  randomForest(Outcome ~ .,
               data = training_set,
               importance=TRUE)
Rf_model

# predict the diabetes status on the test set
Rf_model_pred <-
  predict(Rf_model,
          newdata = test_set)

Rf_model_pred

# Making the Confusion Matrix
cm_Rf <- 
  confusionMatrix(Rf_model_pred, test_set$Outcome)

# Assessing model accuracy
mean(Rf_model_pred == test_set$Outcome)

# Evaluate variable-importance measures
importance(Rf_model)
varImpPlot(Rf_model, main = "Random Forest Variable importance")


# Decision Tree

# fit the model
m.rpart <- rpart(Outcome ~ .,
                 method = "class",
                 data = training_set,
                 minsplit=1, minbucket=1,
                 cp = 0)  

m.rpart

printcp(m.rpart)

# predict the diabetes status on the test set
DT_rpart_pred <- predict(m.rpart, 
                   newdata = test_set, 
                   type = "class")

DT_rpart_pred 

# Making the Confusion Matrix
cm_DT <- 
  confusionMatrix(DT_rpart_pred, test_set$Outcome)

# Assessing model accuracy
mean(DT_rpart_pred == test_set$Outcome)


# Decision Tree Visualization
rpart.plot(m.rpart,
           type = 2,
           fallen.leaves = T,
           extra = 2,
           cex = 0.70,
           main="Diabetes Classification Tree")


# Visualization to compare accuracy of ML models
# Logistic regression accuracy
graphics::fourfoldplot(cm_LR$table,
                       color= c("#ed3b3b", "#0099ff"),
                       conf.level = 0.95,
                       margin = 1, 
                       main = paste("Logistic Regression Accuracy(",
                                    round(cm_LR$overall[1]*100),"%)", sep = ""))

# Random Forest accuracy
graphics::fourfoldplot(cm_Rf$table,
                       color= c("#ed3b3b", "#0099ff"),
                       conf.level = 0.95,
                       margin = 1, 
                       main = paste("Random Forest Accuracy(",
                                    round(cm_Rf$overall[1]*100),"%)", sep = ""))


# Decision tree accuracy
graphics::fourfoldplot(cm_DT$table,
                        color= c("#ed3b3b", "#0099ff"),
                       conf.level = 0.95,
                       margin = 1, 
                       main = paste("Decision Tree Accuracy(",
                                    round(cm_DT$overall[1]*100),"%)", sep = ""))


