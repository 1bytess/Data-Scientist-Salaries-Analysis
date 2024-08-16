
#===== Data Preparation =====

#Loading Packages
library(ggplot2)
library(dplyr)
library(plotrix)

#Importing Data
data <- read.csv("C:/Users/ezrah/Documents/[다변량프로젝트]/Data/ds_salaries.csv")
str(data)

#===== Data Pre-processing =====

#--Categorizing Data to Smaller Category
table(data$company_location)
data <- data %>%
  mutate(continent = case_when(
    company_location %in% c("AE", "CN", "IN", "IR", "JP", "VN", "SG", "MY", "PK") ~ "Asia",
    company_location %in% c("AT", "BE", "CH", "CZ", "DE", "DK", "ES", "FR", "GB", "GR", "HR", "HU", "IE", "IT", "LU", "MD", "MT", "NL", "PL", "PT", "RO", "RU", "SI", "UA") ~ "Europe",
    company_location %in% c("US", "CA", "MX", "BR", "CL", "CO") ~ "America",
    company_location %in% c("AU", "NZ") ~ "Australia",
    TRUE ~ "Other"
  ))

table(data$job_title)
data <- data %>%
  mutate(job_category = case_when(
    grepl("Scientist", job_title) ~ "Data Scientist",
    grepl("Science", job_title) ~ "Data Scientist",
    grepl("Machine Learning", job_title) ~ "Data Scientist",
    grepl("Specialist", job_title) ~ "Data Scientist",
    grepl("Engineer", job_title) ~ "Data Engineer",
    grepl("Analyst", job_title) ~ "Data Analyst",
    grepl("Analytics", job_title) ~ "Data Analyst",
    grepl("Architect", job_title) ~ "Data Architect",
    grepl("Manager", job_title) ~ "Data Manager",
    grepl("Consultant", job_title) ~ "Data Consultant",
    grepl("Developer", job_title) ~ "Developer",
    grepl("Research", job_title) ~ "Researcher",
    TRUE ~ "Other"
  ))

Extracted_Data <- data[c("experience_level", "job_category", "continent", "company_size", "salary_in_usd")]
str(Extracted_Data)

#--Handling Missing Value
sum(is.na(Extracted_Data))

#--Handling Outlier
boxplot(Extracted_Data$salary_in_usd)

outlier_rows <- with(Extracted_Data, boxplot.stats(salary_in_usd)$out)

RefinedData <- Extracted_Data %>%
  filter(!salary_in_usd %in% outlier_rows)

str(RefinedData)

boxplot(RefinedData$salary_in_usd)

#--Data Extraction for Entry Level Experience
EN_RefinedData <- Extracted_Data %>%
  filter(experience_level == "EN")

str(EN_RefinedData)

#=========== Exploratory Data Analysis of Data Scientist Salary ==========

#--Average Salary to Experience Level
mean_salary_by_experience_level <- RefinedData %>%
  group_by(experience_level) %>%
  summarize(mean_salary = mean(salary_in_usd, na.rm = TRUE))

mean_salary_by_experience_level$mean_salary <- as.numeric(mean_salary_by_experience_level$mean_salary)

print(mean_salary_by_experience_level)
ggplot(mean_salary_by_experience_level, aes(x = reorder(experience_level, mean_salary), y = mean_salary, fill=experience_level)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0("$", round(mean_salary))), vjust = -0.5, color = "black", size=4) +
  xlab("Experience Level") +
  ylab("Average Salary") +
  guides(fill=FALSE) +
  theme_minimal()

#--Average Salary to Job Industries
mean_salary_by_industry <- RefinedData %>%
  group_by(job_category) %>%
  summarize(mean_salary = mean(salary_in_usd, na.rm = TRUE))

print(mean_salary_by_industry)

ggplot(mean_salary_by_industry, aes(x = reorder(job_category, mean_salary), y = mean_salary, fill=job_category)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0("$", round(mean_salary))), vjust = -0.5, color = "black", size = 4) +
  xlab("Job Industry") +
  ylab("Average Salary") +
  guides(fill=FALSE) +
  theme_minimal()

#--Average Salary based on Continents
mean_salary_by_continent <- RefinedData %>%
  group_by(continent) %>%
  summarize(mean_salary = mean(salary_in_usd, na.rm = TRUE))

print(mean_salary_by_continent)

ggplot(mean_salary_by_continent, aes(x = reorder(continent, mean_salary), y = mean_salary, fill=continent)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0("$", round(mean_salary))), vjust = -0.5, color = "black", size = 4) +
  xlab("Company Location") +
  ylab("Average Salary") +
  guides(fill=FALSE) +
  theme_minimal()

#=========== Further Analysis for Entry Level ==========
str(EN_RefinedData)
summary(EN_RefinedData$salary_in_usd)

#--EN Average Salary based on Continents
mean_en_salaries <- EN_RefinedData %>% group_by(continent) %>% summarize(mean_salary = mean(salary_in_usd, na.rm = TRUE))
mean_en_salaries <- mean_en_salaries %>% arrange(desc(mean_salary))
mean_en_salaries

ggplot(mean_en_salaries, aes(x = reorder(continent, mean_salary), y = mean_salary, fill = continent)) +
  geom_bar(stat = "identity", color="black") +
  geom_text(aes(label = paste0("$", round(mean_salary))), vjust = -0.5, color = "black", size = 4) +
  xlab("Company Region") +
  ylab("Average Salary") +
  guides(fill=FALSE)

#--EN Employees Count on Company Size
en_company_size <- as.data.frame(table(EN_RefinedData$company_size))
en_company_size <- en_company_size %>% arrange(desc(Freq))
en_company_size

#--EN Average Salary to Company Size
en_csize_csalary <- EN_RefinedData %>% group_by(company_size) %>% summarize(mean_salary = mean(salary_in_usd, na.rm = TRUE))
en_csize_csalary <- en_csize_csalary %>% arrange(desc(mean_salary))
en_csize_csalary

ggplot(en_csize_csalary, aes(x =company_size, y = mean_salary, fill = company_size)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label = paste0("$", round(mean_salary))), vjust = -0.5) +
  xlab("Company Size") +
  ylab("Average Salary") +
  guides(fill=FALSE)

#--EN Popular Industries
en_industry <- as.data.frame(table(EN_RefinedData$job_category))
en_industry <- en_industry %>% arrange(desc(Freq))
en_industry

pie3D(en_industry$Freq, labels = "", explode = 0.1, main = "Distribution of Job Industry for EN")
percentages <- en_industry$Freq / sum(en_industry$Freq) * 100
proportions <- en_industry$Freq / sum(en_industry$Freq)
midpoints <- (cumsum(proportions) - proportions / 2) * 360
text(midpoints, labels = paste0(en_industry$Var1, " (", en_industry$Freq, ", ", round(percentages, 1), "%)"), cex = 0.8, pos = 2)
legend("topright", legend = en_industry$Var1, fill = rainbow(length(en_industry$Var1)), cex = 0.8)

#--En Average Salary Summarize
EN_average_salary <- EN_RefinedData %>%
  group_by(continent, job_category, company_size) %>%
  summarize(avg_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(avg_salary))

print(EN_average_salary, n=29)

ggplot(EN_average_salary, aes(x = job_category, y = avg_salary, fill = company_size)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  facet_grid(continent ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Job Category", y = "Average Salary", fill = "Company Size") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

#=========== Correlation Analysis ==========
#Correlation Data Preparation
cor_data <- RefinedData
cor_data$experience_level <- as.factor(cor_data$experience_level)
cor_data$job_category <- as.factor(cor_data$job_category)
cor_data$continent <- as.factor(cor_data$continent)
cor_data$company_size <- as.factor(cor_data$company_size)


# ANOVA with experience_level
anova_exp <- aov(salary_in_usd ~ experience_level, data = cor_data)
summary(anova_exp)

# ANOVA with job_category
anova_job <- aov(salary_in_usd ~ job_category, data = cor_data)
summary(anova_job)

# ANOVA with continent
anova_cont <- aov(salary_in_usd ~ continent, data = cor_data)
summary(anova_cont)

# ANOVA with company_size
anova_size <- aov(salary_in_usd ~ company_size, data = cor_data)
summary(anova_size)

#=========== Regression Analysis ==========

#--Model Building
model_data <- Extracted_Data
model_data <- model_data %>%
  group_by(job_category) %>%
  filter(n()>5)

set.seed(1712)
S <- sample(nrow(model_data), nrow(model_data)*0.7)
train <- model_data[S,]
test <- model_data[-S,]

model <- lm(salary_in_usd ~., data = train)
summary(model)

#--Feature Selection
fs <- step(model, direction = "both")
summary(fs)

Extracted_Model <- model_data[model_data$experience_level %in% c("EX", "MI", "SE") & 
                                model_data$company_size %in% c("M", "S") &
                                model_data$job_category %in% c("Data Architet", "Data Engineer", "Data Scientist"), ]
head(Extracted_Model)
train_data <- Extracted_Model[S,] 
test_data <- Extracted_Model[-S,]

model1 <- lm(salary_in_usd~., data = train_data)
summary(model1)

predicted <- predict(model, newdata = test_data)
linear_prediction <- data.frame(Predicted = predicted, Observed = test_data$salary_in_usd)

ggplot(linear_prediction, aes(x=Predicted, y=Observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", size=1)+
  theme_minimal()



rm(list=ls())
