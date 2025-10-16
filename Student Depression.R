#LIBRARIESSSSS
library(dplyr)
library(tidyverse)
library(readxl)
install.packages("reshape2")
install.packages("corrplot")
library(reshape2)
library(corrplot)

#DEPRESSION DATASET
depression_dataset <- read_excel("C:/Users/sammi/Documents/Student Depression/Student Depression in College Students In Ghana.xlsx")
depression_dataset

view(depression_dataset)
str(depression_dataset)

#data summary
#By gender
gender_grouped <- depression_dataset %>%
  group_by(Gender) %>%
  summarize(
    count = n(),
    avg_CGPA = mean(`CGPA/CWA`, na.rm = TRUE),
    avg_Depression = mean(Depression, na.rm = TRUE),
    avg_AcademicPressure = mean(`Academic pressure`, na.rm = TRUE),
    avg_StudySatisfaction = mean(`Study Satisfaction`, na.rm = TRUE),
    .groups = "drop"
  ) %>% mutate(percentage = round((count / sum(count)) * 100, 2))%>%
  select(Gender,count, percentage)
gender_grouped


#By Age
age_grouped <- depression_dataset %>%
  mutate(
    age_range = case_when(
      Age >= 16 & Age < 20 ~ "16-20",
      Age >= 20 & Age < 30 ~ "20-30",
      Age >= 30 & Age < 40 ~ "30-40",
      Age >= 40 ~ "40+",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(age_range) %>%
  summarize(
    count = n(),
    avg_CGPA = mean(`CGPA/CWA`, na.rm = TRUE),
    avg_Depression = mean(Depression, na.rm = TRUE),
    avg_AcademicPressure = mean(`Academic pressure`, na.rm = TRUE),
    avg_StudySatisfaction = mean(`Study Satisfaction`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
  select(age_range, count, percentage) %>%
  arrange(factor(age_range, levels = c("16-20", "20-30", "30-40", "40+")))  # Sort by age range
age_grouped


#By City
City_grouped <- depression_dataset %>%
  group_by(City) %>%
  summarize(
    count = n(),
    avg_CGPA = mean(`CGPA/CWA`, na.rm = TRUE),
    avg_Depression = mean(Depression, na.rm = TRUE),
    avg_AcademicPressure = mean(`Academic pressure`, na.rm = TRUE),
    avg_StudySatisfaction = mean(`Study Satisfaction`, na.rm = TRUE),
    .groups = "drop"
  ) %>%  mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
  select(City,count, percentage)
view(City_grouped)

#Socio-demographic scatter plot
ggplot(depression_dataset, aes(x = City, y = Age)) +
  geom_jitter(aes(color = Gender), size = 4, width = 0.2, height = 0.2) +  
  labs(
    title = "Scatter Plot of Age by Location with Gender as Color",
    x = "Age",
    y = "Location (City)",
    color = "Gender"
  ) +
  theme_minimal()



#CONVERT CWA TO CGPA BASED ON KNUST STANDARD
depression_dataset_new <- depression_dataset %>%
  mutate(
    `CGPA/CWA` = ifelse(`CGPA/CWA` > 10, `CGPA/CWA` / 20, `CGPA/CWA`)  # Divide by 20 if value < 10
  )


head(depression_dataset_new)
colnames(depression_dataset_new)


# Your existing data preparation code...
depression_dataset_heat <- depression_dataset_new %>%
  mutate(
    Gender = ifelse(Gender == "Male", 0, 1),
    Dietary_Habits = recode(tolower(trimws(`Dietary Habits`)),
                            "healthy" = 2,
                            "moderate" = 1,
                            "unhealthy" = 0,
                            .default = NA_real_),
    Study_Hours_per_day = recode(tolower(trimws(`Study Hours per day`)),
                                 "5 hours or less" = 0,
                                 "5-8 hours" = 1,
                                 "above 8 hours" = 2,
                                 .default = NA_real_)
  ) %>%
  select(-City, -Program, -Timestamp, -`Study Hours per day`, -`Dietary Habits`)

# Convert to numeric
depression_dataset_heat[] <- lapply(depression_dataset_heat, function(x) {
  if (is.factor(x) | is.character(x)) {
    as.numeric(as.character(x))  
  } else {
    x  
  }
})

# Compute correlation matrix
cor_matrix <- cor(depression_dataset_heat, use = "complete.obs", method = "pearson")

# Melt the matrix
cor_matrix_melted <- melt(cor_matrix)

# ADD THIS: Format correlation values for display
cor_matrix_melted$value_label <- sprintf("%.2f", cor_matrix_melted$value)  # Format to 2 decimal places

# MODIFIED HEATMAP CODE WITH geom_text()
ggplot(data = cor_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  # Add this line to display the r-values:
  geom_text(aes(label = value_label), color = "black", size = 2) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limits = c(-1, 1), name = "Correlation (r)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(
       x = "", y = "") +
  coord_fixed() # Makes the tiles square

# Depression Distribution
ggplot(depression_dataset_new, aes(x = as.factor(Depression))) +  
  geom_bar(fill = "skyblue") + 
  labs(
    title = "Depression Distribution (0 = No, 1 = Yes)",
    x = "Depression",
    y = "Count"
  ) +
  theme_minimal() 


head(depression_dataset_new)

# Depression Distribution By Gender
depression_dataset_new %>% 
  mutate(
    Gender = factor(Gender, levels = c("Female", "Male")),  
    Depression_Factor = factor(Depression, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>% 
  ggplot(aes(x = Gender, fill = Depression_Factor)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Blues", name = "Depression") +
  labs(title = "Depression by Gender", x = "Gender", y = "Count") +
  theme_minimal()

# Depression Distribution By cwa

depression_dataset_new  %>% 
  mutate(
    Depression_Factor = factor(Depression, levels = c(0, 1), labels = c("No", "Yes"))
  )  %>% 
  ggplot(aes(x = `CGPA/CWA`, fill = Depression_Factor)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  scale_fill_brewer(palette = "Blues", name = "Depression") +
  labs(title = "CGPA Distribution by Depression Status", x = "CGPA", y = "Count") +
  theme_minimal()


# Depression Distribution By cwa density graph
depression_dataset_new  %>% 
  mutate(
    Depression_Factor = factor(Depression, levels = c(0, 1), labels = c("No", "Yes"))
  )  %>% 
  ggplot(aes(x = `CGPA/CWA`, color = Depression_Factor)) +
  geom_density(size = 1.2) +
  scale_color_brewer(palette = "Blues", name = "Depression") +
  labs(title = "CGPA Density by Depression Status", x = "CGPA", y = "Density") +
  theme_minimal()

# Depression Distribution By cwa density graph
depression_dataset_new  %>% 
  mutate(
    Depression_Factor = factor(Depression, levels = c(0, 1), labels = c("No", "Yes"))
  )  %>% 
  ggplot(aes(x = Depression_Factor, y = `Academic pressure`, fill = Depression_Factor)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues", name = "Depression") +
  labs(title = "Academic Pressure by Depression Status",
       x = "Depression Status",
       y = "Academic Pressure") +
  theme_minimal()

depression_dataset_new  %>% 
  mutate(
    Depression_Factor = factor(Depression, levels = c(0, 1), labels = c("No", "Yes"))
  )  %>% 
  ggplot(aes(x = `Study Hours per day`, fill = Depression_Factor)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Blues", name = "Depression") +
  labs(title = "Study Hours per Day vs Depression",
       x = "Study Hours per Day",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





