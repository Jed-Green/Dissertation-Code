### Dissertation code
library(haven)
library(ggplot2)
library(stargazer)
library(ltm)
library(psych)
library(dplyr)
library(car)
library(corrplot)
library(summarytools)

original_df<-read_sav("C19PRC_UK_W4_archive_final.sav")

edudf<-read_sav("C19PRC_UKW1W2_archive_final.sav")
edudf1<-read_sav("C19PRC_UK_W3_archive_final.sav")

ldf1<-as.data.frame(cbind(edudf$pid,edudf$W1_Education, edudf$W1_Political_Scale))
colnames(ldf1)<-c("pid","W1_Edu", "W1_Political_Scale")

ldf2<-as.data.frame(cbind(edudf1$pid,edudf1$W3_Education, edudf1$W3_Political_Scale))
colnames(ldf2)<-c("pid","W2_Edu","W3_Political_Scale")

mdf1<-merge.data.frame(original_df,ldf1, by ="pid", all = T)
mdf2<-merge.data.frame(mdf1,ldf2, by ="pid", all = T)


# Replace missing values in W4_Education using W1_Edu or W2_Edu
mdf2 <- mdf2 %>%
  mutate(W4_Education = ifelse(is.na(W4_Education), ifelse(W4_Type == 1, coalesce(W1_Edu, W2_Edu), NA), W4_Education))

# Merge W1_Edu and W2_Edu into W4_Education based on conditions
mdf2 <- mdf2 %>%
  mutate(W4_Education = ifelse(W4_Type == 1 & is.na(W4_Education), coalesce(W1_Edu, W2_Edu), W4_Education)) # Filter out rows where W4_Type is NA

mdf2 <- mdf2 %>%
  select(-W1_Edu, -W2_Edu)
mdf2$W4_Education <- as.numeric(as.character(mdf2$W4_Education))

mdf2 <- mdf2 %>%
  mutate(W4_Political_scale = ifelse(is.na(W4_Political_scale), ifelse(W4_Type == 1, coalesce(W1_Political_Scale, W3_Political_Scale), NA), W4_Political_scale))

# Merge W1_Political_Scale and W3_Political_Scale into W4_Political_scale based on conditions
mdf2 <- mdf2 %>%
  mutate(W4_Political_scale = ifelse(W4_Type == 1 & is.na(W4_Political_scale), coalesce(W1_Political_Scale, W3_Political_Scale), W4_Political_scale)) %>%
  filter(!is.na(W4_Type))

mdf2 <- mdf2 %>%
  select(-W1_Political_Scale, -W3_Political_Scale)
mdf2$W4_Political_scale<- as.numeric(as.character(mdf2$W4_Political_scale))

df<-mdf2


### Remove anyone who isn't born in the UK

df<- subset(df,df$W4_WhereBorn != 5)

### Initial descriptive statistics on dependent variables:

hist(df$W4_MigrantAttitudes1,
     breaks = 10,
     xlim = c(1,10),
     ylim = c(0,700),
     xlab = "Opinion on migrant impact on the economy",
     main = "Opinion on migrant impact on the economy")

hist(df$W4_MigrantAttitudes2,
     breaks = 10,
     xlim = c(1,10),
     ylim = c(0,700),
     xlab = "Opinion on migrant impact on the culture",
     main = "Opinion on migrant impact on the culture")

hist(df$W4_MigrantAttitudes3,
     xlim = c(1,5),
     ylim = c(0,1000),
     xlab = "Opinion on migrant impact on the education",
     main = "Opinion on migrant impact on the education")

hist(df$W4_Immigration_1,
     xlim = c(1,5),
     ylim = c(0,1500),
     xlab = "Diverse mix of people make country better",
     main = "Diverse mix of people make country better")

hist(df$W4_Immigration_2,
     xlim = c(1,5),
     ylim = c(0,1500),
     xlab = "Sharing customs and traditions is better",
     main = "Sharing customs and traditions is better")

### Initial descriptive statistics on independent variables

hist(df$W4_Dep_Total,
     breaks = 30,
     xlim = c(0,30),
     ylim = c(0,1500),
     xlab = "PHQ-9 Total Test Score",
     main = "Histogram of PHQ-9 Total Test Score")

hist(df$W4_GAD_Total,
     breaks = 25,
     xlim = c(0,25),
     ylim = c(0,2000),
     xlab = "GAD-7 Total Test Score",
     main = "Histogram of GAD-7 Total Test Score")

hist(df$W4_EmpathyEC_Total,
     breaks = 20,
     xlim = c(0,20),
     ylim = c(0,400),
     xlab = "EmpathyEc Total Score",
     main = "Histogram of EmpathyEc Total Score")

barplot(table(df$W4_Religion_binary),
        ylim = c(0,2000),
        xlab = "Religious or not",
        main = "barplot representing the number of people who say they are religious or not"
        )

hist(df$W4_Conspiracy_Total,
     breaks = 20,
     xlim = c(0,60),
     ylim = c(0,700),
     xlab = "Conspiracy total score",
     main = "Histogram of Conspiracy total score")

hist(df$W4_Conscientious_Total,
     breaks = 10,
     xlim = c(2,10),
     ylim = c(0,1000),
     xlab = " Conscientiousness Total",
     main = "Histogram of Conscientiousness Total")

### re-coding dependent variables into additive scale:

table(df$W4_MigrantAttitudes1)
# Define the transformation function
transform_likert <- function(x) {
  transformed_value <- ceiling(x / 2)
  return(transformed_value)}

# Apply the transformation function to the data
df$W4_MigrantAttitudes1_rescaled <- transform_likert(df$W4_MigrantAttitudes1)

# Print the transformed data
print(df$W4_MigrantAttitudes1_rescaled)
table(df$W4_MigrantAttitudes1_rescaled)


table(df$W4_MigrantAttitudes2)
# Define the transformation function
transform_likert <- function(x) {
  transformed_value <- ceiling(x / 2)
  return(transformed_value)}

# Apply the transformation function to the data
df$W4_MigrantAttitudes2_rescaled <- transform_likert(df$W4_MigrantAttitudes2)

# Print the transformed data
print(df$W4_MigrantAttitudes2_rescaled)
table(df$W4_MigrantAttitudes2_rescaled)



df$W4_TotalMigrantAttitudes <- rowSums(df[, c("W4_MigrantAttitudes1_rescaled", 
                                 "W4_MigrantAttitudes2_rescaled", 
                                 "W4_Immigration_1", 
                                 "W4_Immigration_1")])

summary(df$W4_TotalMigrantAttitudes)

dependent_data<-data.frame(df[, c("W4_MigrantAttitudes1_rescaled", 
                               "W4_MigrantAttitudes2_rescaled", 
                               "W4_Immigration_1", 
                               "W4_Immigration_1")])
cronbach.alpha(dependent_data)

Extraversion_data<-data.frame(df[, c("W4_Personality1R",
                                     "W4_Personality6")])
cronbach.alpha(Extraversion_data)

Agreeableness_data<-data.frame(df[, c("W4_Personality2",
                                     "W4_Personality7R")])
cronbach.alpha(Agreeableness_data)
omega_result <- omega(Agreeableness_data, nfactors = 1)
print(omega_result)

Conscientiousnes_data<-data.frame(df[, c("W4_Personality3R",
                                      "W4_Personality8")])
cronbach.alpha(Conscientiousnes_data)

Neuroticism_data<-data.frame(df[, c("W4_Personality4R",
                                    "W4_Personality9")])
cronbach.alpha(Neuroticism_data)

Openness_data<-data.frame(df[, c("W4_Personality5R",
                                    "W4_Personality10")])
cronbach.alpha(Openness_data)

Depression_data<-data.frame(df[, c("W4_Dep1",
                                   "W4_Dep2",
                                   "W4_Dep3",
                                   "W4_Dep4",
                                   "W4_Dep5",
                                   "W4_Dep6",
                                   "W4_Dep7",
                                   "W4_Dep8",
                                   "W4_Dep9")])
cronbach.alpha(Depression_data)


Paranoia_data<-data.frame(df[, c("W4_Paranoia1",
                                   "W4_Paranoia2",
                                   "W4_Paranoia3",
                                   "W4_Paranoia4",
                                   "W4_Paranoia5")])
cronbach.alpha(Paranoia_data)

Conspiracy_data<-data.frame(df[, c("W4_Conspiracy_1",
                                 "W4_Conspiracy_2",
                                 "W4_Conspiracy_3",
                                 "W4_Conspiracy_4",
                                 "W4_Conspiracy_5")])
cronbach.alpha(Conspiracy_data)

Empathy_data<-data.frame(df[, c("W4_Empathy_1R",
                                   "W4_Empathy_2R",
                                   "W4_Empathy_3",
                                   "W4_Empathy_4R")])
cronbach.alpha(Empathy_data, na.rm = T)

table(df$W4_Gender)
df$W4_Gender_Binary<- NA
df$W4_Gender_Binary[df$W4_Gender == 1] <- 1
df$W4_Gender_Binary[df$W4_Gender == 2] <- 0
table(df$W4_Gender_Binary)

table(df$W4_Ethnicity)
df$W4_Visible_minority<- NA
df$W4_Visible_minority<-ifelse(df$W4_Ethnicity == 1 | df$W4_Ethnicity == 2, 0,1)
table(df$W4_Visible_minority)

table(df$W4_Education)
df$W4_Education_categories<-NA
df$W4_Education_categories[df$W4_Education == 1] <- "No Qualifications"
df$W4_Education_categories[df$W4_Education == 2] <- "O-Level/GCSE or A-Level or Technical qualification"
df$W4_Education_categories[df$W4_Education == 3] <- "O-Level/GCSE or A-Level or Technical qualification"
df$W4_Education_categories[df$W4_Education == 4] <- "O-Level/GCSE or A-Level or Technical qualification"
df$W4_Education_categories[df$W4_Education == 5] <- "Undergraduate degree or Diploma"
df$W4_Education_categories[df$W4_Education == 6] <- "Undergraduate degree or Diploma"
df$W4_Education_categories[df$W4_Education == 7] <- "Postgraduate degree"
df$W4_Education_categories[df$W4_Education == 8] <- "Other"
df$W4_Education_categories<- factor(df$W4_Education_categories, levels = c("No Qualifications", "O-Level/GCSE or A-Level or Technical qualification", "Undergraduate degree or Diploma","Postgraduate degree", "Other"))
table(df$W4_Education_categories)

table(df$W4_Education)
df$W4_Education_categories_Num<-NA
df$W4_Education_categories_Num[df$W4_Education == 1] <- "1"
df$W4_Education_categories_Num[df$W4_Education == 2] <- "2"
df$W4_Education_categories_Num[df$W4_Education == 3] <- "2"
df$W4_Education_categories_Num[df$W4_Education == 4] <- "2"
df$W4_Education_categories_Num[df$W4_Education == 5] <- "3"
df$W4_Education_categories_Num[df$W4_Education == 6] <- "3"
df$W4_Education_categories_Num[df$W4_Education == 7] <- "4"
df$W4_Education_categories_Num[df$W4_Education == 8] <- "5"
df$W4_Education_categories_Num<- factor(df$W4_Education_categories_Num, levels = c(1, 2, 3, 4, 5))
table(df$W4_Education_categories_Num)

table(df$W4_Education)
df$W4_Education_categories_Num1<-NA
df$W4_Education_categories_Num1[df$W4_Education == 1] <- "1"
df$W4_Education_categories_Num1[df$W4_Education == 2] <- "2"
df$W4_Education_categories_Num1[df$W4_Education == 3] <- "2"
df$W4_Education_categories_Num1[df$W4_Education == 4] <- "2"
df$W4_Education_categories_Num1[df$W4_Education == 5] <- "3"
df$W4_Education_categories_Num1[df$W4_Education == 6] <- "3"
df$W4_Education_categories_Num1[df$W4_Education == 7] <- "4"
df$W4_Education_categories_Num1<- factor(df$W4_Education_categories_Num1, levels = c(1, 2, 3, 4))
table(df$W4_Education_categories_Num1)






table(df$W4_Employment)
df$W4_Employment_categories<-NA
df$W4_Employment_categories[df$W4_Employment == 1] <- "Employed"
df$W4_Employment_categories[df$W4_Employment == 2] <- "Employed"
df$W4_Employment_categories[df$W4_Employment == 3] <- "Employed"
df$W4_Employment_categories[df$W4_Employment == 4] <- "Employed"
df$W4_Employment_categories[df$W4_Employment == 5] <- "Unemployed"
df$W4_Employment_categories[df$W4_Employment == 6] <- "Unemployed"
df$W4_Employment_categories[df$W4_Employment == 7] <- "Unemployed"
df$W4_Employment_categories[df$W4_Employment == 8] <- "Other Situations"
df$W4_Employment_categories[df$W4_Employment == 9] <- "Other Situations"
df$W4_Employment_categories[df$W4_Employment == 10] <- "Student"
df$W4_Employment_categories<- factor(df$W4_Employment_categories, levels = c("Unemployed", "Employed", "Other Situations", "Student"))
table(df$W4_Employment_categories)

table(df$W4_Employment)
df$W4_Employment_categories_Num<-NA
df$W4_Employment_categories_Num[df$W4_Employment == 1] <- "1"
df$W4_Employment_categories_Num[df$W4_Employment == 2] <- "1"
df$W4_Employment_categories_Num[df$W4_Employment == 3] <- "1"
df$W4_Employment_categories_Num[df$W4_Employment == 4] <- "1"
df$W4_Employment_categories_Num[df$W4_Employment == 5] <- "2"
df$W4_Employment_categories_Num[df$W4_Employment == 6] <- "2"
df$W4_Employment_categories_Num[df$W4_Employment == 7] <- "2"
df$W4_Employment_categories_Num[df$W4_Employment == 8] <- "2"
df$W4_Employment_categories_Num[df$W4_Employment == 9] <- "3"
df$W4_Employment_categories_Num[df$W4_Employment == 10] <- "4"
df$W4_Employment_categories_Num<- factor(df$W4_Employment_categories_Num, levels = c(2, 1, 3, 4))
table(df$W4_Employment_categories_Num)

table(df$W4_Income_2019)
df$W4_Income_2019_categories<-NA
df$W4_Income_2019_categories[df$W4_Income_2019 == 1] <- "£0-15,490 per year"
df$W4_Income_2019_categories[df$W4_Income_2019 == 2] <- "£15,491-£25,340 per year"
df$W4_Income_2019_categories[df$W4_Income_2019 == 3] <- "£25,341-£38,740 per year"
df$W4_Income_2019_categories[df$W4_Income_2019 == 4] <- "£38,741-£57,930 per year"
df$W4_Income_2019_categories[df$W4_Income_2019 == 5] <- "£57,931 or more per year"
df$W4_Income_2019_categories<- factor(df$W4_Income_2019_categories)
table(df$W4_Income_2019_categories)

table(df$W4_Income_2019)
df$W4_Income_2019_categories_1<-NA
df$W4_Income_2019_categories_1[df$W4_Income_2019 == 1] <- "£0-15,490 per year"
df$W4_Income_2019_categories_1[df$W4_Income_2019 == 2] <- "£15,491-£25,340 per year"
df$W4_Income_2019_categories_1[df$W4_Income_2019 == 3] <- "£25,341-£38,740 per year"
df$W4_Income_2019_categories_1[df$W4_Income_2019 == 4] <- "£38,741-£57,930 per year"
df$W4_Income_2019_categories_1[df$W4_Income_2019 == 5] <- "£57,931 or more per year"
df$W4_Income_2019_categories_1<- factor(df$W4_Income_2019_categories_1)
table(df$W4_Income_2019_categories_1)

table(df$W4_Political_scale)

### Descriptive statistics:

cor_df<-data.frame(df[, c("W4_Age_year",
                          "W4_Gender_Binary",
                          "W4_Visible_minority",
                          "W4_Education_categories_Num",
                          "W4_Employment_categories_Num",
                          "W4_Income_2019",
                          "W4_Political_scale",
                          "W4_Extraversion_Total",
                          "W4_Agreeable_Total",
                          "W4_Conscientious_Total",
                          "W4_Neuroticism_Total",
                          "W4_Openness_Total",
                          "W4_Dep_Total",
                          "W4_PTSDdx",
                          "W4_Paranoia_Total",
                          "W4_Conspiracy_Total",
                          "W4_TotalMigrantAttitudes")])

new_names <- c("Age (Years)", "Male", "Visible minority", "Education", "Employment", "Income", "Political scale", 
               "Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness",
               "Depression", "PTSD_dx", "Paranoia", "Conspiracy", "Migrant Attitudes")
names(cor_df) <- new_names

### Descriptive Stats:
comprehensive_summary <- describe(cor_df, na.rm = T)
write.csv(comprehensive_summary, "comprehensive_summary.csv")


cor_df1<-data.frame(df[, c("W4_Age_year",
                          "W4_Gender_Binary",
                          "W4_Visible_minority",
                          "W4_Education_categories_Num1",
                          "W4_Income_2019_1",
                          "W4_Political_scale",
                          "W4_Extraversion_Total",
                          "W4_Agreeable_Total",
                          "W4_Conscientious_Total",
                          "W4_Neuroticism_Total",
                          "W4_Openness_Total",
                          "W4_Dep_Total",
                          "W4_PTSDdx",
                          "W4_Paranoia_Total",
                          "W4_Conspiracy_Total",
                          "W4_TotalMigrantAttitudes")])

# Convert 'W4_Education_categories_Num1' from factor to numeric
cor_df1$W4_Education_categories_Num1 <- as.numeric(as.character(cor_df1$W4_Education_categories_Num1))

# Convert 'W4_Income_2019' from labelled variable to numeric
cor_df1$W4_Income_2019_1 <- as.numeric(labels(cor_df1$W4_Income_2019)[cor_df1$W4_Income_2019])

# Convert 'W4_PTSDdx' from labelled variable to numeric
cor_df1$W4_PTSDdx <- as.numeric(as.character(cor_df1$W4_PTSDdx))

new_names1 <- c("Age (Years)", "Male", "Visible minority", "Education", "Income", "Political scale", 
               "Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness",
               "Depression", "PTSD_dx", "Paranoia", "Conspiracy", "Migrant Attitudes")
names(cor_df1) <- new_names1




cor_matrix1 <- cor(cor_df1, use = "pairwise.complete.obs")
# Calculate p-values for correlations
p_values <- cor.mtest(cor_df1, conf.level = 0.95)$p

# Create a correlation plot with colors, highlighting significant correlations
corrplot(cor_matrix1, method = "color", type = "upper", tl.col = "black", tl.srt = 45, p.mat = p_values,sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', order = "AOE",number.cex = 0.7, tl.cex = 0.7, mar = c(0,0,2,0))
corrplot(cor_matrix1, method = "color", type = "upper", tl.col = "black", tl.srt = 45,order = "AOE",number.cex = 0.7, tl.cex = 0.7,mar = c(0,0,2,0))$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2), cex = 0.5)



### Linear models:

m1<-lm(df$W4_TotalMigrantAttitudes ~ df$W4_Age_year + df$W4_Gender_Binary + df$W4_Visible_minority + df$W4_Education_categories + df$W4_Employment_categories + df$W4_Income_2019_categories + df$W4_Political_scale)
summary(m1)

m2<-lm(df$W4_TotalMigrantAttitudes ~ df$W4_Extraversion_Total + df$W4_Agreeable_Total + df$W4_Conscientious_Total + df$W4_Neuroticism_Total + df$W4_Openness_Total)
summary(m2)

m3<-lm(df$W4_TotalMigrantAttitudes ~ df$W4_Dep_Total + df$W4_PTSDdx + df$W4_Paranoia_Total + df$W4_Conspiracy_Total)
summary(m3)

m4<-lm(df$W4_TotalMigrantAttitudes ~ df$W4_Age_year + df$W4_Gender_Binary + df$W4_Visible_minority + df$W4_Education_categories + df$W4_Employment_categories + df$W4_Income_2019_categories + df$W4_Political_scale + df$W4_Extraversion_Total + df$W4_Agreeable_Total + df$W4_Conscientious_Total + df$W4_Neuroticism_Total + df$W4_Openness_Total +df$W4_Dep_Total + df$W4_PTSDdx + df$W4_Paranoia_Total + df$W4_Conspiracy_Total )
summary(m4)

# Calculate VIF
vif_values <- vif(m4)
write.csv(vif_values, "vif_values.csv")

stargazer(m1,m2,m3,m4)


table(df$W4_PTSDdx)
