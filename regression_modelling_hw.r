library(readxl)

df <- read.csv("AmesHousing.csv")
n <- nrow(df) # total number of variables -> 2930

#Filtering out variables with missing values
number_of_missing_values <- colSums(is.na(df))
number_of_missing_values

df_non_null <- df[, number_of_missing_values == 0]

# Selecting the categorical variables
df_cat <- df_non_null[, sapply(df_non_null, is.character)]
# Calculating the number of unique values
unique_values <- sapply(df_cat, function(x) length(unique(x)))
unique_values

#Selecting columns with at least 4 unique values
df_cat_filter <- df_cat[, unique_values >= 4]


# Filtering out those columns where the number of observations in the groups are 
# not exceeding 1

cat_filter_final <- data.frame(row.names = seq_len(nrow(df_cat_filter))) # initializing
# data frame with the same number of rows
for (cat_var in names(df_cat_filter)) {
  freq_calc <- table(df_cat_filter[[cat_var]])
  print(freq_calc)
  if (all(freq_calc > 1)) {
    cat_filter_final[[cat_var]] <- df_cat_filter[[cat_var]]
  }
}
cat_filter_final
cat_filter_final$SalePrice <- df$SalePrice

# Performing ANOVA to determine which categorical variable has the highest
#variability comparing to the sale Price

# F test for anova - determining the P value

p_value_df <- data.frame(
  variable = character(),
  p_value = numeric()
)

for (cat_var in names(cat_filter_final)) {
  print(cat_var)
  if (cat_var != "SalePrice") {
    formula <- as.formula(
      paste("SalePrice ~", cat_var)
    )
  p_value_anova <- oneway.test(formula, data 
                               = cat_filter_final, var.equal = FALSE)
  p_value_df <- rbind(p_value_df,data.frame(
    cat_var, p_value_anova$p.value))
  }
}
p_value_df

# Conclusion --> All the p values are really close to zero because of the
# large observation number therefore performing the F test was not meaningful


# Calculate the r^2 value of the categorical variable and made it easier to rank
# the variables

# Variance ratio calculation
df_cat_filter$SalePrice <- df$SalePrice
var_ratio_df <- data.frame(
  variable = character(),
  var_ratio = numeric()
)

for (cat_var in names(df_cat_filter)) {
  if (cat_var != 'SalePrice') {
    formula <- as.formula(paste("SalePrice~", cat_var))
    aov_test <- aov(formula, data = df_cat_filter)
    anova_table <- anova(aov_test)
    SSR <- anova_table['Residuals', 'Sum Sq']
    SSB <- anova_table[1,'Sum Sq']
    SST <- SSR + SSB
    var_ratio_df <- rbind(var_ratio_df,data.frame(
      cat_var, SSB/SST))
  }
}

colnames(var_ratio_df) <- c('Variable', 'Variance Ratio')  

# Sorting the dataframe based on the variance ratio
var_ratio_df <- var_ratio_df[order(var_ratio_df$`Variance Ratio`, decreasing = TRUE),]

# Selecting the 3 highest value

top_4_cat_variable <- c(var_ratio_df[1:4,1])
top_4_cat_variable

# Let's analyze now the numerical variables

df_numerical <- df_non_null[, sapply(df_non_null, is.numeric)]

num_df <- data.frame()
for (numeric_var in names(df_numerical)) {
  if (numeric_var != 'SalePrice') {
    formula <- as.formula(paste('SalePrice ~', numeric_var))
    model <- lm(formula, data = df_numerical)
    num_df <- rbind(num_df, data.frame(numeric_var, 
                                       summary(model)$r.squared))
  }
}


colnames(num_df) <- c('Variable', 'r^2')  
num_df <- num_df[order(num_df$`r^2`, decreasing = TRUE),]


selected_cols_num <- c(num_df[1:6,1])
selected_cols_cat <- c(var_ratio_df[1:3,1])

final_df <- df_non_null[,c(selected_cols_num,selected_cols_cat)]
final_df$SalePrice <- df$SalePrice


#------------------------------------------------------------------------------

# Analysing the heteroscedascity of the selected variables
# 1. Fit baseline model
final_df <- subset(final_df, Neighborhood != "Landmrk" & Kitchen.Qual != "Po")
heteroskedascity_model <- lm(SalePrice ~ Overall.Qual +Gr.Liv.Area + X1st.Flr.SF
                              +Year.Built + Full.Bath +Year.Remod.Add + 
                               Neighborhood +Exter.Qual + Kitchen.Qual,
                              data = final_df)

final_df$sq_Errors <-  heteroskedascity_model$residuals^2                          

#Creating a scatter plot to visualize the residuals 
library(ggplot2)
ggplot(final_df, aes(x = SalePrice, y =sq_Errors)) + geom_point()
labs(title="Residuals vs SalePrice", 
     x="SalePrice", y="Squared Residuals")
# We can observe slight heteroscedasticity based on the plot

# Performing White test to decide whether we should deal with heteroscedasticity

# H0 there is no heteroscedasticity - residuals are homoscedastic
# H1 there is heteroscedasticity - residuals are not homoscedastic

#Creating the helper table - predictors and the squared error
# it is enough only to include the numerical variables - categorical variables are
# not meaningful because squaring them creates no difference

# Formal test for scedascity
#install.packages("skedastic")
library(skedastic)
white_basic <- white(heteroskedascity_model, interactions = FALSE)
print(paste("White Test P-value:", white_basic$p.value))
# We can reject the null hypothesis in all the common significance level
# so therefore we need to deal with heterskedascity

# Breusch Pagan Test
library(lmtest)
bptest(heteroskedascity_model, studentize = TRUE)
# p value is 2.2e^-16 - really small
bptest(heteroskedascity_model, studentize = FALSE)
# p value is 2.2e^-16 - really small
#Kolmogorov-Smirnov test for the residuals distribution
ks.test(heteroskedascity_model$residuals, "pnorm")
# rejecting normality
hist(heteroskedascity_model$residuals)


# Using the GLS method
table_white_test <- final_df[,1:6]
table_white_test$Sq_errors <- final_df$sq_Errors

helper_gls <- lm(log(Sq_errors) ~. -Sq_errors, data = table_white_test)
elements_of_omega <- exp(fitted(helper_gls))

base_model_gls <- lm(SalePrice ~ Overall.Qual +Gr.Liv.Area + X1st.Flr.SF
                     +Year.Built + Full.Bath +Year.Remod.Add + 
                       Neighborhood +Exter.Qual + Kitchen.Qual, 
                     weights = 1/elements_of_omega,data = final_df)
summary(base_model_gls)
#p value is 2.2e^-16

# Using log-transform model
ggplot(final_df, aes(x = SalePrice)) + geom_histogram() # long right tail 
ggplot(final_df, aes(x = log(SalePrice))) + geom_histogram() 

heteroskedascity_model_log <- lm(log(SalePrice) ~ Overall.Qual +Gr.Liv.Area + X1st.Flr.SF
                             +Year.Built + Full.Bath +Year.Remod.Add + 
                               Neighborhood +Exter.Qual + Kitchen.Qual,
                             data = final_df)

white_basic_log <- white(heteroskedascity_model_log, interactions = FALSE)
print(paste("Log Model White Test P-value:", white_basic_log$p.value)) 
#slight improvement in the p value, 6.28e^-208
# Reason of this is the large sample size and therefore the white test
# find uneven variance which casue the low p value

final_df$log_sq_Errors <-  heteroskedascity_model_log$residuals^2
final_df$fitted_log <- fitted(heteroskedascity_model_log)

ggplot(final_df, aes(x = log(SalePrice), y =log_sq_Errors)) + geom_point()
labs(title="Residuals vs Loged price Model", 
     x="Predicted log(SalePrice)", y="Log Squared Residuals")

# 3. Huber White Robust Standard errors (HWC3)
library(car)
robust_log_model <- coeftest(heteroskedascity_model_log, 
         vcov = hccm(heteroskedascity_model_log, type = "hc3"))
print(robust_log_model)

# Heteroscedascity was handled by log model and then making it robust with the 
# Huber White method

#-------------------------------------------------------------------------------

#To analyzing multicollinearity in my dataset I have to perform VIF test
# I use the log_model -> robust model is not working becuase that is a coef object
vif_outputs <- vif(heteroskedascity_model_log)
print(vif_outputs)
# For numerical columns GVIF can be checked - here the df is 1
# For categorical columns the GVIF^(1/(2*Df)) column should be checked because it
# adjusts the huge df value
# Problematical columns are the ones where the value is over 10
# Year built column has multicollinearity over 5 which still not too large to
# considering as a potential drop

#-------------------------------------------------------------------------------
#Building models

#Convert categorical variables to factor so R handles dummy encoding
#automatically reference category is dropped
final_df$Neighborhood <- as.factor(final_df$Neighborhood)
final_df$Exter.Qual   <- as.factor(final_df$Exter.Qual)
final_df$Kitchen.Qual <- as.factor(final_df$Kitchen.Qual)

#Model1 - Additive baseline (no interaction)
first_model <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF +
                    Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                    Exter.Qual + Kitchen.Qual, data = final_df)

summary(first_model)

library(sjPlot)
plot_model(first_model, type = "pred", terms = c("Overall.Qual", "Exter.Qual"))
#All lines are parallel so each Exter.Qual group has the same slope for
#Overall.Qual, only the intercept differs between groups

#Model2 - Interaction model (Neighborhood * Gr.Liv.Area)
second_model <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF +
                     Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                     Exter.Qual + Kitchen.Qual + Neighborhood * Gr.Liv.Area,data = final_df)

summary(second_model)
                        
ggplot(final_df, aes(x = Gr.Liv.Area, y = SalePrice, color = Neighborhood)) +
  geom_point() + geom_smooth(method = "lm")
                        
#Lines are now not paralell, the slope of Neighborhood differs by group
                        

#Model3 - Log-Log model

hist(final_df$SalePrice)
hist(final_df$Gr.Liv.Area)
hist(final_df$X1st.Flr.SF)
#histogram of SalePrice and numerical predictors (Gr.Liv.Area, X1st.Flr.SF) show long right tails



third_model <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                            Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                            Exter.Qual + Kitchen.Qual + Neighborhood * Gr.Liv.Area,data = final_df)
summary(third_model)

ggplot(final_df, aes(x = log(Gr.Liv.Area), y = log(SalePrice))) +
  geom_point() + geom_smooth(method = lm) + geom_smooth(color = 'red')

ggplot(final_df, aes(x = log(X1st.Flr.SF), y = log(SalePrice))) +
  geom_point() + geom_smooth(method = lm) + geom_smooth(color = 'red')
#parabolic effect

third_model <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) +
                               log(X1st.Flr.SF) + Year.Built + Full.Bath +
                               Year.Remod.Add + Neighborhood + Exter.Qual + 
                               Kitchen.Qual, data = final_df)
summary(third_model)

third_model_squared <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                    Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                    Exter.Qual + Kitchen.Qual + Neighborhood * Gr.Liv.Area +
                    I(log(Gr.Liv.Area)^2) + I(log(X1st.Flr.SF)^2),data = final_df)
summary(third_model_squared)

#Model4 - Squared terms

fourth_model <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                     Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                     Exter.Qual + Kitchen.Qual + Neighborhood * Gr.Liv.Area +
                     I(log(Gr.Liv.Area)^2) + I(log(X1st.Flr.SF)^2) +
                     I(Year.Built^2) + I(Year.Remod.Add^2),data = final_df)
#

#-------------------------------------------------------------------------------
#Comparing the models

#AIC/BIC only comparable within the same response scale:
#M1 vs M2 (original SalePrice), M3 vs M4 (log SalePrice)

cat("Adj R2")
cat("M1 Linear:", round(summary(first_model)$adj.r.squared,  4), "\n")
cat("M2 Interaction:", round(summary(second_model)$adj.r.squared, 4), "\n")
cat("M3 Log-Log:", round(summary(third_model)$adj.r.squared,  4), "(log scale)\n")
cat("M4 Log-Lin+sq:", round(summary(fourth_model)$adj.r.squared, 4), "(log scale)\n")

cat("AIC / BIC (M1 vs M2, original scale)")
print(AIC(first_model, second_model))
print(BIC(first_model, second_model))

#All three criteria agree: Model2 wins over Model1
#The interaction between Overall.Qual and Exter.Qual is worth keeping (lower AIC, lower BIC
#and higher Adj R2) despite spending 3 extra parameters. The Wald test already confirmed this
#(F = 21.83, p = 0), so the criteria are consistent.

print(cat("AIC / BIC (M3 vs M4, log scale"))
print(AIC(third_model, fourth_model))
print(BIC(third_model, fourth_model))

#All three criteria agree: Model3 wins over Model4. M3 is ahead by ~188 AIC points and ~187 BIC points
#The log-log specification with logged area predictors is clearly the better functional form

#-------------------------------------------------------------------------------
# Ramsey RESET Test for Model2 and Model3

library(lmtest)
lmtest::resettest(second_model)
lmtest::resettest(third_model)

#Both models reject H0, but there is a difference in F-statistics
#F dropped from 178.83 -> 5.35 by switching from M2 to M3
#This quantifies exactly how much the log-log transformation improved
#the functional form specification
