# Load readr and tidyverse packages 
library(readr)
library(tidyverse)

# Optional: if you need to set the working directory 
# # setwd("path_where_the_csv_file_is_in_your_pc")


# Create a data frame and load the data set 
data <- read_csv("financial_data.csv")

#Visualize the first rows to verify the loading was done correctly
head(data)


# First look into the data 

#Data Structure
str(data)

#Numeric variable statistics resume
summary(data)

#Data set dimensions
dim(data)


# Descriptive analysis and first visual look
#Field: economy
table(data$economy)

#Field: regionwb
table(data$regionwb)
#Visualize the region distribution
ggplot(data, aes(x = regionwb)) +
  geom_bar() +
  labs(
    title = "Observation Distribution by Region (WB)",
    x = "Region",
    y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle =  45, hjust = 1, size = 6))

#Field: pop_adult (population over 15)
#Visualization
ggplot(data, aes(x = pop_adult)) +
  geom_histogram(
    binwidth = 10000000,
    fill = "steelblue",
    color = "black") +
  labs(
    title = "Adult population Distribution (>15)",
    x = "Adult Population",
    y = "Frequency") +
  theme_minimal()

#Field: Gender 
#Visualization
ggplot(data, aes(x = gender)) +
  geom_bar(fill = c("lightblue", "pink")) +
  labs(title = "Distribution by Gender",
       x = "Gender",
       y = "Frequency") +
  theme_minimal()

#Field: age
summary(data$age)
#Visualization
not_age_na <- data %>%
  filter(!is.na(age))

ggplot(not_age_na, aes(x = age)) +
  geom_histogram(binwidth = 5,
                 fill = "lightgreen",
                 color = "black") +
  labs(title = "Distribution by Age",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

#Field: educ (Education)
table(data$educ)
#Visualization
ggplot(data, aes(x = educ)) +
  geom_bar(fill = "orange") +
  labs(title = "Education Level Distribution",
       x = "Education Level",
       y = "Frequency") +
  theme_minimal()

#Field: urbanicity (housing area)
table(data$urbanicity_fUrbanf)
#Visualization
ggplot(data, aes(x = urbanicity_fUrbanf)) +
  geom_bar(fill = "purple") +
  labs(title = "Housing area Distribution",
       x = "Housing Area",
       y = "Frequency") +
  theme_minimal()
  
# Age vs Gender
ggplot(not_age_na, aes(x = age, fill = gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age by Gender Distribution",
       x = "Age",
       y = "Density",
       fill = "Gender") +
  theme_minimal()

# Difference in Population by Region
ggplot(data, aes(x = regionwb, y = pop_adult, fill = regionwb)) +
  geom_boxplot() +
  labs(title = "Adult Population by Region (WB)",
       x = "Region",
       y = "Adult Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "none")

# Frequency Analysis Visualization
#Function 
proportion_plot <- function(variable, title){
  prop_table <- prop.table(table(variable))
  df_prop <- data.frame(Result = names(prop_table),
                        Proportion = as.numeric(prop_table))
  
  ggplot(df_prop, aes(x = Result, y = Proportion, fill = Result)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(Proportion)), vjust = -0.3) +
    labs(title = title, x = NULL, y = "Proportion") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(legend.position = "none")
}

proportion_plot(data$emp_in, "People in Working Force")
proportion_plot(data$account, "People with Financial Account")
proportion_plot(data$saved, "People with Savings")
proportion_plot(data$borrowed, "People who have taken out a Loan")
proportion_plot(data$anydigpayment, "People who have made a digital payment")
proportion_plot(data$mobileowner, "People who owns a Mobile Device")
proportion_plot(data$internetaccess, "People with Internet Access")


# Bivariate Analysis: Binary vs Demographic variables
# Gender vs Financial Inclusion
ggplot(data, aes(x = gender, fill = account)) +
  geom_bar(position = "dodge") + #"dodge" for side to side bars
  labs(title = "Financial Account by Gender",
       x = "Gender",
       y = "Frequency",
       fill = "Has an Account") +
  theme_minimal()

# Education vs Internet Access
ggplot(data, aes(x = educ, fill = internetaccess)) +
  geom_bar(position = "fill") + #"fill" to get proportion to each category
  labs(title = "Internet Acces by Education Level",
       x = "Educational Level",
       y = "Proportion",
       fill = "Internet Access") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Housing Area vs Digital Payments
ggplot(data, aes(x = urbanicity_fUrbanf, fill = anydigpayment)) +
  geom_bar(position = "dodge") +
  labs(title = "Any Digital Payment by Housing Area",
       x = "Housing Area",
       y = "Frequency",
       fill = "Use Digital Payment") +
  theme_minimal()


#Distribution Analysis by Category
table(data$receive_wages)
prop.table(table(data$receive_wages))

table(data$receive_pension)
prop.table(table(data$receive_pension))

table(data$receive_agriculture)
prop.table(table(data$receive_agriculture))

#Visualization
distribution_plot <- function(variable, title){
  df_freq <- as.data.frame(table(variable))
  colnames(df_freq) <- c("Category", "Frequency")
  
  ggplot(df_freq, aes(x = Category, y = Frequency, fill = Category)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Frequency), vjust = -0.3) +
    labs(title = title, x = NULL, y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1))
}

distribution_plot(data$receive_wages, "Wage Reception Distribution")
distribution_plot(data$receive_pension, "Pension Reception Distribution")
distribution_plot(data$receive_agriculture, "Agricultural Payments Reception Distribution")


# Bivariate Distribution: Wages vs Other variables
#Gender vs Wage Reception
ggplot(data, aes(x = gender, fill = receive_wages)) +
  geom_bar(position = "fill") +
  labs(title = "Salary Reception by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "Reception Method") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() 

#Educational Level vs Receiving Pension
ggplot(data, aes(x = educ, fill = receive_pension)) +
  geom_bar(position = "fill") +
  labs(title = "Educational Level",
       x = "Educational Level",
       y = "Proportion",
       fill = "Reception Method") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Having an account vs Receiving Wages
ggplot(data, aes(x = account, fill = receive_wages)) +
  geom_bar(position = "fill") +
  labs(title = "Having an Account vs Receive Wages",
       x = "Has an Account",
       y = "Proportion",
       fill = "Reception Method") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
