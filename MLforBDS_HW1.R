# William Lee
# Code for ML HW #1

bdir = "~/Desktop/machine_learning_code"
setwd(bdir)

diabetes_csv = "diabetes.csv"
diabetes_df = read.table(header=T, quote = "", sep=",", fill=T, file = diabetes_csv, stringsAsFactors=FALSE)

# 3a
diabetes_df_3a = diabetes_df

diabetes_df_3a$Outcome = NULL
p = boxplot(diabetes_df_3a, col = "#00BFC4", border = "#F8766D", xlab = "Attributes",
            main = "Boxplot of Pima Indians Diabetes Dataset Attributes")

# 3b
diabetes_df_3b = diabetes_df

install.packages("scatterplot3d") 
library("scatterplot3d")

color_binary = c("#00BFC4","#F8766D")
class_binary = color_binary[factor(diabetes_df_3b$Outcome, levels = c(0,1))]
p = scatterplot3d(x=diabetes_df_3b$Glucose, y=diabetes_df_3b$Insulin, z=diabetes_df_3b$BMI,
                  xlab="Plasma Glucose", ylab="Plasma Insulin", zlab= "BMI", 
                  main="Diabetes Scatterplot,\nColored by Outcome", color=class_binary, pch=16)
diabetes_df$Outcome = factor(diabetes_df$Outcome, levels = c(0,1))
legend("right", legend=levels(diabetes_df$Outcome), col = c("#00BFC4","#F8766D"), pch = 16)


# 4a
college_csv = "College.csv"
college_df = read.table(header=T, quote = "", sep=",", fill=T, file = college_csv, stringsAsFactors=FALSE)

college_c2to19 = colnames(college_df)[2:19]
college_predictors = college_c2to19[college_c2to19!="Accept"]
college_predictors = paste(college_predictors, collapse = "+")

formula_lm = as.formula(paste("Accept~", college_predictors))

solution_4a = lm(formula_lm, data=college_df)
summary(solution_4a)

# 4b
college_df_4b = college_df

college_df_4b$Private[college_df_4b$Private=="Yes"] = 1
college_df_4b$Private[college_df_4b$Private=="No" ] = 0
college_df_4b$Private = as.numeric(college_df_4b$Private)

private_predictors = colnames(college_df_4b)[3:19]
private_predictors = paste(private_predictors, collapse = "+")

formula_glm = as.formula(paste("Private~", private_predictors))

solution_4b = glm(formula_glm, data=college_df_4b, family=binomial())
summary(solution_4b)


# 4c
solution_4c = prcomp(college_df[,3:19])

pca_df = as.data.frame(solution_4c[["rotation"]])

p = scatterplot3d(x=pca_df$PC1, y=pca_df$PC2, z=pca_df$PC3,
                  xlab="PC1", ylab="PC2", zlab= "PC3", pch=16,
                  main="Scatterplot of First 3 PCs")

# Question 5 (Dr. Scott)
install.packages("brms") 
library("brms")
library(dplyr)

infection_tsv = "quarantine_data_ml_course.tsv"
infection_df = read.table(header=T, quote = "", sep="\t", fill=T, file = infection_tsv, stringsAsFactors=FALSE)

training_set = infection_df[1:50,]
testing_set = infection_df[51:100,]

mod2 = brm(infected ~ sex + age + fever * location,
           data=training_set,
           family=bernoulli(link="logit"),
           iter=4000,
           chains=4,
           control = list(adapt_delta = 0.97))

mod = brm(infected ~ sex + age + fever + location,
          data=training_set,
          family=bernoulli(link="logit"),
          iter=4000,
          chains=4,
          control = list(adapt_delta = 0.97))

testing_set$brms_prob = predict(mod2, newdata = testing_set)
testing_set$infected = ifelse(testing_set$brms_prob[,1] > 0.6, (testing_set$infected = 1), (testing_set$infected = 0))
pred=ifelse(testing_set$brms_prob[,1] > 0.6, 1)
pull(testing_set,infected)
table(pred,pull(testing_set,infected))

testing_set$brms_prob = predict(mod, newdata = testing_set)
testing_set$infected = ifelse(testing_set$brms_prob[,1] > 0.6, (testing_set$infected = 1), (testing_set$infected = 0))
pred=ifelse(testing_set$brms_prob[,1] > 0.6, 1)
pull(testing_set,infected)
table(pred,pull(testing_set,infected))

test_df$infected = ifelse(test_df$brms_prob[,1] > 0.6, (test_df$infected=1), (test_df$infected=0))

library(dpylr)
predict=ifelse(test_df$brms_prob[,1] > 0.6,1,0)
pull(test_df,infected)
table(predict,pull(test_df,infected))

# 6a
problem6_csv = "assignment1_problem6_scores.csv"
problem6_df = read.table(header=T, quote = "", sep=",", fill=T, file = problem6_csv, stringsAsFactors=FALSE)

# defines function for 6a and 6b
roc_curve_plotter <- function(predict_scores_c1, predict_scores_c2, true_labs) {
  
  # orders the data points by classifier scores
  true_labs_c1 = true_labs[order(predict_scores_c1, decreasing=TRUE)]
  true_labs_c2 = true_labs[order(predict_scores_c2, decreasing=TRUE)]
  
  # calculates cumulative sums (vectors) to be used in TPR and FPR calculations 
  lab1_csum_c1 = cumsum(true_labs_c1==1)
  lab0_csum_c1 = cumsum(true_labs_c1==0)
  #
  lab1_csum_c2 = cumsum(true_labs_c2==1)
  lab0_csum_c2 = cumsum(true_labs_c2==0)
  
  # calculates sums to be used in TPR and FPR calculations
  lab1_sum_c1 = sum(true_labs_c1==1)
  lab0_sum_c1 = sum(true_labs_c1==0)
  #
  lab1_sum_c2 = sum(true_labs_c2==1)
  lab0_sum_c2 = sum(true_labs_c2==0)
  
  # calculates TPR and FPR for each data entry
  TPR_c1 = lab1_csum_c1/lab1_sum_c1
  FPR_c1 = lab0_csum_c1/lab0_sum_c1
  #
  TPR_c2 = lab1_csum_c2/lab1_sum_c2
  FPR_c2 = lab0_csum_c2/lab0_sum_c2
  
  # plotting
  p = plot(TPR_c1, x=FPR_c1,type = "o",col = "red", 
       xlab = "FPR", ylab = "TPR",
       main = "Classifier 1 vs. Classifier 2:\nROC Curves")
  lines(TPR_c2,x=FPR_c2, type = "o", col = "blue")
  
  legend("bottomright", legend=c("Classifier 1", "Classifier 2"),
         col=c("red", "blue"), pch = 16)
  
  ########## 6b ##########
  
  # calculates TPR and FPR differenced vectors to be used in AUC calculations
  TPR_c1_diffVec = c(diff(TPR_c1))
  FPR_c1_diffVec = c(diff(FPR_c1))
  #
  TPR_c2_diffVec = c(diff(TPR_c2))
  FPR_c2_diffVec = c(diff(FPR_c2))
  
  # calculates AUC for classifiers 1 and 2
  AUC_c1 = sum(TPR_c1 * FPR_c1_diffVec) + (sum(TPR_c1_diffVec * FPR_c1_diffVec))/2
  AUC_c2 = sum(TPR_c2 * FPR_c2_diffVec) + (sum(TPR_c2_diffVec * FPR_c2_diffVec))/2
  
  print("Classifier 1 AUC Score:"); print(AUC_c1)
  print("Classifier 2 AUC Score:"); print(AUC_c2)

}

# uses function to answer 6a and 6b
roc_curve_plotter(problem6_df$classifier1, problem6_df$classifier2, problem6_df$y_true)

#######################################################################################

# defines function for 6c and 6d
prc_curve_plotter <- function(predict_scores_c1, predict_scores_c2, true_labs) {
  
  # orders the data points by classifier scores
  true_labs_c1 = true_labs[order(predict_scores_c1, decreasing=TRUE)]
  true_labs_c2 = true_labs[order(predict_scores_c2, decreasing=TRUE)]
  
  # calculates cumulative sums (vectors) to be used in TPR and Precision calculations 
  lab1_csum_c1 = cumsum(true_labs_c1==1) ##
  lab0_csum_c1 = cumsum(true_labs_c1==0) ##
  #
  lab1_csum_c2 = cumsum(true_labs_c2==1) ##
  lab0_csum_c2 = cumsum(true_labs_c2==0) ##
  
  # calculates sums to be used in TPR and Precision calculations
  lab1_sum_c1 = sum(true_labs_c1==1) ##
  lab1_sum_c2 = sum(true_labs_c2==1) ##
  
  # calculates TPR and Precision for each data entry
  TPR_c1 = lab1_csum_c1/lab1_sum_c1
  Prc_c1 = lab1_csum_c1/(lab1_csum_c1 + lab0_csum_c1)
  #
  TPR_c2 = lab1_csum_c2/lab1_sum_c2
  Prc_c2 = lab1_csum_c2/(lab1_csum_c2 + lab0_csum_c2)
  
  # generates df for classifier 1, true label = 0
  c1_df_L0 = cbind.data.frame(true_labs_c1, TPR_c1, Prc_c1)
  c1_df_L0 = c1_df_L0[c1_df_L0$true_labs_c1==0,]
  
  # generates df for classifier 1, true label = 1
  c1_df_L1 = cbind.data.frame(true_labs_c1, TPR_c1, Prc_c1)
  c1_df_L1 = c1_df_L1[c1_df_L1$true_labs_c1==1,]
  
  # generates df for classifier 2, true label = 0
  c2_df_L0 = cbind.data.frame(true_labs_c2, TPR_c2, Prc_c2)
  c2_df_L0 = c2_df_L0[c2_df_L0$true_labs_c2==0,]
  
  # generates df for classifier 2, true label = 1
  c2_df_L1 = cbind.data.frame(true_labs_c2, TPR_c2, Prc_c2)
  c2_df_L1 = c2_df_L1[c2_df_L1$true_labs_c2==1,]
  
  # prc plot, true label = 0
  plot(c1_df_L0$Prc_c1, x=c1_df_L0$TPR_c1,type = "o",col = "red", 
       xlab = "TPR/Recall", ylab = "Precision",
       main = "Classifier 1 vs. Classifier 2:\nPRC Curves, True Label = 0")
  lines(c2_df_L0$Prc_c2, x=c2_df_L0$TPR_c2, type = "o", col = "blue")
  
  legend("bottomleft", legend=c("Classifier 1", "Classifier 2"),
         col=c("red", "blue"), pch = 16)
  
  #prc plot, true label = 1
  plot(c1_df_L1$Prc_c1, x=c1_df_L1$TPR_c1,type = "o",col = "red", 
       xlab = "TPR/Recall", ylab = "Precision",
       main = "Classifier 1 vs. Classifier 2:\nPRC Curves, True Label = 1")
  lines(c2_df_L1$Prc_c2, x=c2_df_L1$TPR_c2, type = "o", col = "blue")
  
  legend("bottomleft", legend=c("Classifier 1", "Classifier 2"),
         col=c("red", "blue"), pch = 16)
  
  ########## 6d ##########
  
  # calculates TPR and FPR differenced vectors to be used in AUPRC calculations
  TPR_c1_diffVec = c(diff(TPR_c1))
  Prc_c1_diffVec = c(diff(Prc_c1))
  #
  TPR_c2_diffVec = c(diff(TPR_c2))
  Prc_c2_diffVec = c(diff(Prc_c2))
  
  # calculates AUPRCs for classifiers 1 and 2
  AUPRC_c1 = sum(TPR_c1_diffVec * Prc_c1_diffVec) + (sum(TPR_c1_diffVec * Prc_c1_diffVec))/2
  AUPRC_c2 = sum(TPR_c2_diffVec * Prc_c2_diffVec) + (sum(TPR_c2_diffVec * Prc_c2_diffVec))/2
  
  print("Classifier 1 AUPRC Score:"); print(AUPRC_c1)
  print("Classifier 2 AUPRC Score:"); print(AUPRC_c2)
  
}

# uses function to answer 6c and 6d
prc_curve_plotter(problem6_df$classifier1, problem6_df$classifier2, problem6_df$y_true)


###################################################################################################################

# 8a

b_Fmeasure = c(0.780, 0.826, 0.779, 0.746, 0.746, 0.728, 0.722, 0.610, 0.479, 0.364)

g_Fmeasure = c(0.901, 0.915, 0.904, 0.892, 0.892, 0.887, 0.885, 0.858, 0.837, 0.821)

ROC_area = c(0.825, 0.856, 0.885, 0.911, 0.927, 0.927, 0.917, 0.895, 0.868, 0.859)

k_values = c(1,2,3,5,10,15,20,30,40,50)

plot(b_Fmeasure, x=k_values,type = "o",col = "red", ylim=c(0.35,1),
     xlab = "value of k", 
     main = "b F-measure, g F-measure, and\nAUC Score as Value of k Increases")
lines(g_Fmeasure,x=k_values, type = "o", col = "blue")
lines(ROC_area, x=k_values, type = "o", col = "green")

legend("bottomleft", legend=c("b F-measure", "g F-measure", "AUC Score"),
       col=c("red", "blue","green"), pch = 1)


# 9a
ROC_area = c(0.631, 0.620, 0.618, 0.618, 0.618, 0.618, 0.618, 0.618, 0.618, 0.618)

num_iter = c(10,20,30,40,50,60,70,80,90,100)

plot(ROC_area, x=num_iter,type = "o",col = "green", 
     xlab = "# of Iterations", ylab = "AUC Score",
     main = "AUC Score as #\nof Iterations Increases")

# 9b
ROC_area = c(0.679, 0.674, 0.672, 0.662, 0.652, 0.666, 0.659, 0.662, 0.653, 0.654)

num_iter = c(10,20,30,40,50,60,70,80,90,100)

plot(ROC_area, x=num_iter,type = "o",col = "green", 
     xlab = "# of Iterations", ylab = "AUC Score",
     main = "AUC Score as #\nof Iterations Increases")

# 9c
ROC_area = c(0.631, 0.627, 0.633, 0.639, 0.641, 0.641, 0.642, 0.637, 0.636, 0.634)

num_iter = c(10,20,30,40,50,60,70,80,90,100)

plot(ROC_area, x=num_iter,type = "o",col = "green", 
     xlab = "# of Trees", ylab = "AUC Score",
     main = "AUC Score as #\nof Trees Increases")
