df <- read.csv("AmesHousing.csv")
n <- nrow(df)
ncol(df)  # total number of variables -> 82
#----------------------------------------------------------------------------------------------------
# VARIABLE SELECTION
# We have too many variables in the dataset, we have to select ~10 
# meaningful to work with


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


# Filtering out those columns where the number of observations in all groups are 
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

# has only 1 observations
final_df <- subset(final_df, Kitchen.Qual != "Po")

#-----------------------------------------------------------------------------------------
# NEIGBOURHOOD_SIMPLE

# Creating the Neighbourhood_simple colum
# Making the Neighbourhood variable simpler -> less categories
# Now its 27, almost unimpretable
final_df$Neighborhood_simple <- as.character(final_df$Neighborhood)

counts <- table(final_df$Neighborhood)
# We put those hoods where the obervations are less than a 100 (~3% of the set)
rare_neighborhoods <- names(counts[counts < 100])
final_df$Neighborhood_simple[final_df$Neighborhood_simple 
                             %in% rare_neighborhoods] <- "Other"

table(final_df$Neighborhood_simple)
# cut down to 14 categories

final_df$Neighborhood <- final_df$Neighborhood_simple
final_df$Neighborhood <- relevel(as.factor(final_df$Neighborhood), ref="Other")

#------------------------------------------------------------------------------
# HETEROSKEDASTICITY
# Analysing the heteroscedascity of the selected variables
# 1. Fit baseline model

heteroskedasticity_model <- lm(SalePrice ~ Overall.Qual +Gr.Liv.Area + X1st.Flr.SF
                              +Year.Built + Full.Bath +Year.Remod.Add + 
                               Neighborhood +Exter.Qual + Kitchen.Qual,
                              data = final_df)

final_df$sq_Errors <-  heteroskedasticity_model$residuals^2                          

#Creating a scatter plot to visualize the residuals 
library(ggplot2)
ggplot(final_df, aes(x = SalePrice, y =sq_Errors)) + geom_point() + stat_smooth() +
labs(title="Residuals vs SalePrice", 
     x="SalePrice", y="Squared Residuals")
# We can observe slight heteroscedasticity based on the plot

# Performing White test to decide whether we should deal with heteroscedasticity

# H0 there is no heteroscedasticity - residuals are homoscedastic
# H1 there is heteroscedasticity - residuals are not homoscedastic

#Creating the helper table - predictors and the squared error
# it is enough only to include the numerical variables - categorical variables are
# not meaningful because squaring them creates no difference

# Formal test for heteroskedasticity
# We used White test with interactions, as n > 600
library(skedastic)
white <- white(heteroskedasticity_model, interactions = TRUE)
print(paste("White Test P-value:", white$p.value)) # p = 2.799e-188
# We can reject the null hypothesis in all the common significance level
# so therefore we need to deal with heterskedasticity


# Using log-transform model
ggplot(final_df, aes(x = SalePrice)) + geom_histogram() # long right tail 
ggplot(final_df, aes(x = log(SalePrice))) + geom_histogram() 

heteroskedasticity_model_log <- lm(log(SalePrice) ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF
                             + Year.Built + Full.Bath + Year.Remod.Add + 
                               Neighborhood + Exter.Qual + Kitchen.Qual,
                             data = final_df)

final_df$sq_Errors <-  heteroskedasticity_model_log$residuals^2                          
ggplot(final_df, aes(x = log(SalePrice), y =sq_Errors)) + geom_point() + stat_smooth() +
labs(title="Residuals vs SalePrice", 
     x="SalePrice", y="Squared Residuals")

white_log <- white(heteroskedasticity_model_log, interactions = TRUE)
print(paste("Log Model White Test P-value:", white_log$p.value)) 
#slight improvement in the p value, 6.28e^-208
# Reason of this is the large sample size and therefore the white test
# find uneven variance which casue the low p value
# LOG TRANSFORMATION DOES NOT SOLVE THE ISSUE
# only improving it

#-------------------------------------------------------------------------------
# MULTICOLLINEARITY

#To analyzing multicollinearity in my dataset I have to perform VIF test
# I use the log_model -> robust model is not working becuase that is a coef object
vif_outputs <- car::vif(heteroskedasticity_model_log)
print(vif_outputs)
# For numerical columns GVIF can be checked - here the df is 1
# For categorical columns the GVIF^(1/(2*Df)) column should be checked because it
# adjusts the huge df value
# Problematical columns are the ones where the value is over 10
# Year built column has multicollinearity over 5 which still not too large to
# considering as a potential drop

#-------------------------------------------------------------------------------------------------
# MODEL BUILDING

#Convert categorical variables to factor so R handles dummy encoding
#automatically reference category is dropped
final_df$Neighborhood <- relevel(as.factor(final_df$Neighborhood), ref="Other")
final_df$Exter.Qual   <- as.factor(final_df$Exter.Qual)
final_df$Kitchen.Qual <- as.factor(final_df$Kitchen.Qual)

#Model1 - Additive baseline (no interaction)
first_model <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF +
                    Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                    Exter.Qual + Kitchen.Qual, data = final_df)
coeftest(first_model, vcov=sandwich::vcovHC(first_model, type = "HC3"))

#Model2 - Interaction model (Neighborhood * Gr.Liv.Area)

ggplot(final_df, aes(x = Gr.Liv.Area, y = SalePrice, color = Neighborhood)) +
  geom_point() + geom_smooth(method = "lm")
# The slopes are very different => might warrant an interaction term

second_model <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF +
                     Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                     Exter.Qual + Kitchen.Qual + Neighborhood * Gr.Liv.Area,data = final_df)
coeftest(second_model, vcov=hccm(second_model))

plot_model(first_model, type = "pred", terms = c("Gr.Liv.Area", "Neighborhood"))
plot_model(second_model, type = "pred", terms = c("Gr.Liv.Area", "Neighborhood"))
#Lines are now not parallel, the slope of Neighborhood differs by group

# HC-robust Wald test to account for heteroskedasticity
lmtest::waldtest(first_model, second_model, vcov=sandwich::vcovHC(second_model, type = "HC3"))
# p<2.2e-16 => the second model is superior

# HC-robust Ramsey-reset test for later
final_df$fitted_sq <- fitted(second_model)^2
final_df$fitted_cb <- fitted(second_model)^3
second_model_reset <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF +
                           Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                           Exter.Qual + Kitchen.Qual + Neighborhood * Gr.Liv.Area +
                           fitted_sq + fitted_cb ,data = final_df)
second_reset <- lmtest::waldtest(second_model, second_model_reset, vcov=sandwich::vcovHC(second_model_reset, type="HC3"))
second_reset
# p<2.2e-16 => the model is missing some non-linear specifications

#Model3 - Log-Log model

hist(final_df$SalePrice)
hist(final_df$Gr.Liv.Area)
hist(final_df$X1st.Flr.SF)
#histogram of SalePrice and numerical predictors (Gr.Liv.Area, X1st.Flr.SF) show long right tails

ggplot(final_df, aes(x=Gr.Liv.Area, y = SalePrice)) + geom_point() + 
  geom_smooth(method = lm) + geom_smooth(color="red")
ggplot(final_df, aes(x=log(Gr.Liv.Area), y = log(SalePrice))) + geom_point() + 
  geom_smooth(method = lm) + geom_smooth(color="red")
ggplot(final_df, aes(x=X1st.Flr.SF, y = SalePrice)) + geom_point() + 
  geom_smooth(method = lm) + geom_smooth(color="red")
ggplot(final_df, aes(x=log(X1st.Flr.SF), y = log(SalePrice))) + geom_point() + 
  geom_smooth(method = lm) + geom_smooth(color="red")
# plots are more linear when using log transformation

third_model <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                            Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                            Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area),data = final_df)
coeftest(third_model, vcov=sandwich::vcovHC(third_model, type = "HC3"))

# HC-robust Ramsey-reset test
final_df$fitted_sq <- fitted(third_model)^2
final_df$fitted_cb <- fitted(third_model)^3
third_model_reset <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                          Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                          Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area) +
                          fitted_sq + fitted_cb ,data = final_df)
third_reset <- lmtest::waldtest(third_model, third_model_reset, vcov=sandwich::vcovHC(third_model_reset, type = "HC3"))

c(second_reset$`Pr(>F)`, third_reset$`Pr(>F)`)
# Third model passes the RESET Test
# Wald test is not applicable because of difference in scale of target
# We go by the reset test results

#Model4 - Squared term

ggplot(final_df, aes(x = Year.Built, y = log(SalePrice))) +
  geom_point() + stat_smooth(method = lm) + 
  stat_smooth(method=lm, formula=y~x+I(x^2), color = 'red')
# Quadratic function seems to fit better on the plot

fourth_model <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                     Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                     Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area) +
                     I(Year.Built^2),data = final_df)
coeftest(fourth_model, vcov=sandwich::vcovHC(fourth_model, type = "HC3"))
lmtest::waldtest(fourth_model, third_model, vcov=sandwich::vcovHC(fourth_model, type = "HC3"))
# p=0.00056 Wald test pefers the extended model


final_df$fitted_sq <- fitted(fourth_model)^2
final_df$fitted_cb <- fitted(fourth_model)^3
fourth_reset_model <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                           Year.Built + Full.Bath + Year.Remod.Add + Neighborhood + 
                           Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area) +
                           I(Year.Built^2) + fitted_sq + fitted_cb,data = final_df)
lmtest::waldtest(fourth_reset_model, fourth_model, vcov=sandwich::vcovHC(fourth_reset_model, type = "HC3"))
# Reset test still passed

# Model 5: Exclude non-significant terms

coeftest(fourth_model, vcov=sandwich::vcovHC(fourth_model, type = "HC3"))
# "Somerst" and "NridgHt" neighborhoods and Full.Bath seem insignificant

# Exclude the two neighborhoods by stuffing them into "Other"
final_df$Neighborhood_new <- relevel(as.factor(
  ifelse(as.character(final_df$Neighborhood) %in% c("Somerst", "NridgHt"), "Other", 
         as.character(final_df$Neighborhood))
), ref="Other")
final_df$Neighborhood <- final_df$Neighborhood_new

fifth_model <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                    Year.Built + Year.Remod.Add + Neighborhood + 
                    Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area) +
                    I(Year.Built^2),data = final_df)
coeftest(fifth_model, vcov=sandwich::vcovHC(fifth_model, type="HC3"))
# Significances are now largely okay
lmtest::waldtest(fifth_model, fourth_model, vcov=sandwich::vcovHC(fourth_model, type="HC3"))
# p=0.1539 Wald test says the exclusions were jointly insignificant

final_df$fitted_sq <- fitted(fifth_model)^2
final_df$fitted_cb <- fitted(fifth_model)^3
fifth_reset_model <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                          Year.Built + Year.Remod.Add + Neighborhood + 
                          Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area) +
                          I(Year.Built^2) + fitted_sq + fitted_cb,data = final_df)
lmtest::waldtest(fifth_reset_model, fifth_model, vcov=sandwich::vcovHC(fifth_reset_model, type = "HC3"))
# Reset test still passes

white_test_table <- as.data.frame(model.matrix(fifth_model))[,-1]
sq_predictors <- white_test_table[,]^2
colnames(sq_predictors) <- paste0(colnames(sq_predictors), "_sq")
white_test_table <- cbind(white_test_table, sq_predictors)
white_test_table$sq_errors <- sixth_model$residuals^2

gls_helper_reg <- lm(log(sq_errors) ~ ., data = white_test_table)
omega <- exp(fitted(gls_helper_reg))

fifth_model_gls <- lm(log(SalePrice) ~ Overall.Qual + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                        Year.Built + Year.Remod.Add + Neighborhood + 
                        Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area) +
                        I(Year.Built^2), weights=1/omega, data = final_df)
summary(fifth_model_gls)

# Model 6: Quality as a factor instead of interval scale numeric

final_df$QualFactor <- as.factor(ifelse(final_df$Overall.Qual<=2, 2, final_df$Overall.Qual))

sixth_model <- lm(log(SalePrice) ~ QualFactor + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                    Year.Built + Year.Remod.Add + Neighborhood + 
                    Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area) +
                    I(Year.Built^2),data = final_df)
coeffs <- coeftest(sixth_model, vcov=sandwich::vcovHC(sixth_model, type="HC3"))
coeffs

final_df$fitted_sq <- fitted(sixth_model)^2
final_df$fitted_cb <- fitted(sixth_model)^3
sixth_reset_model <- lm(log(SalePrice) ~ QualFactor + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                          Year.Built + Year.Remod.Add + Neighborhood + 
                          Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area) +
                          I(Year.Built^2) + fitted_sq + fitted_cb,data = final_df)
lmtest::waldtest(sixth_reset_model, sixth_model, vcov=sandwich::vcovHC(sixth_reset_model, type = "HC3"))

coef_df <- as.data.frame(3:10)
colnames(coef_df) <- c("quality")
coef_df$coeffs <- coeffs[2:9]

ggplot(coef_df, aes(x=quality, y=coeffs)) + geom_point() + stat_smooth(method=lm)

white_test_table <- as.data.frame(model.matrix(sixth_model))[,-1]
sq_predictors <- white_test_table[,]^2
colnames(sq_predictors) <- paste0(colnames(sq_predictors), "_sq")
white_test_table <- cbind(white_test_table, sq_predictors)
white_test_table$sq_errors <- sixth_model$residuals^2

gls_helper_reg <- lm(log(sq_errors) ~ ., data = white_test_table)
omega <- exp(fitted(gls_helper_reg))

sixth_model_gls <- lm(log(SalePrice) ~ QualFactor + log(Gr.Liv.Area) + log(X1st.Flr.SF) +
                        Year.Built + Year.Remod.Add + Neighborhood + 
                        Exter.Qual + Kitchen.Qual + Neighborhood * log(Gr.Liv.Area) +
                        I(Year.Built^2), weights=1/omega, data = final_df)
summary(sixth_model_gls)


