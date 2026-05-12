"Final Public Health Project"

install.packages("tidyverse")
install.packages("haven")

library(tidyverse)
library(haven)

demo <- read_xpt("demographics data.xpt")

body <- read_xpt("examination data.xpt")

activity <- read_xpt("Physical Activity Data.xpt")

smoking <- read_xpt("Smoking Data.xpt")

nhanes <- demo %>%
  left_join(body, by = "SEQN") %>%
  left_join(activity, by = "SEQN") %>%
  left_join(smoking, by = "SEQN")

nhanes2 <- nhanes %>%
  select(
    RIAGENDR,
    RIDAGEYR,
    BMXBMI,
    PAQ650,
    SMQ020
  )

nhanes2 <- nhanes2 %>%
  rename(
    Sex = RIAGENDR,
    Age = RIDAGEYR,
    BMI = BMXBMI,
    PhysicalActivity = PAQ650,
    Smoking = SMQ020
  )

nhanes_clean <- drop_na(nhanes2)

nhanes_clean$Sex <- factor(
  nhanes_clean$Sex,
  levels = c(1,2),
  labels = c("Male","Female")
)

ggplot(nhanes_clean,
       aes(x = BMI)) +
  geom_histogram()

nhanes_clean$PhysicalActivity <- factor(
  nhanes_clean$PhysicalActivity,
  levels = c(1,2),
  labels = c("Yes","No")
)

ggplot(nhanes_clean,
       aes(x = PhysicalActivity,
           y = BMI)) +
  geom_boxplot()

nhanes_clean$Smoking <- factor(
  nhanes_clean$Smoking,
  levels = c(1,2),
  labels = c("Yes","No")
)

anova_result <- aov(
  BMI ~ PhysicalActivity,
  data = nhanes_clean
)

summary(anova_result)

anova_result <- aov(
  BMI ~ PhysicalActivity,
  data = nhanes_clean
)

summary(anova_result)

















    
    
    
    