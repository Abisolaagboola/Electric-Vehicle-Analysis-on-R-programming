setwd("C:/Users/s5612871/OneDrive - Bournemouth University/BU  FOLDER/DDM/DDM ASSESMENT")

#Call relevant libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(skimr)
library(gridExtra)

# Import the data from an excel file
electricvehicle <- read_excel("ev_19.xlsx")
representative <- read_excel("rep_19.xlsx")

#joining both datasets using left join
electricvehicle_rep <-electricvehicle %>%
  left_join(representative, by = "rep_id")

skim(electricvehicle_rep)


#Data restrictions

#removing NA from marketing
electricvehicle_rep <- electricvehicle_rep %>%
  filter(!is.na(marketing))

#removing NA from purchase
electricvehicle_rep <- electricvehicle_rep %>%
  filter(!is.na(purchase))

#removing "stow" from product
electricvehicle_rep <- electricvehicle_rep %>%
  filter(product != "stow")


#removing "pelt" from promotion
electricvehicle_rep <- electricvehicle_rep %>%
  filter(promotions != "pelt")

#removing NA from buyer
electricvehicle_rep <- electricvehicle_rep %>%
  filter(!is.na(buyer),
         buyer != "klip")

#removing "drup" from campaign
electricvehicle_rep <- electricvehicle_rep %>%
  filter(campaign != "drup")

#removing "NA" from jobtype
electricvehicle_rep <- electricvehicle_rep %>%
  filter(!is.na(jobtype))

#removing "NA" from qualifications
electricvehicle_rep <- electricvehicle_rep %>%
  filter(!is.na(qualification))

#Data Transformation
#Rounding the period
electricvehicle_rep <- electricvehicle_rep %>%
  mutate(period = round(period, 0))

#renaming fbook to facebook
electricvehicle_rep <- electricvehicle_rep %>%
  mutate(campaign = ifelse(campaign == "fbook", "facebook", campaign))

# grouping the years 
electricvehicle_rep <- electricvehicle_rep %>%
  mutate(experience_years = case_when(experience <= 8 ~ "junior",
                                      experience <= 16 ~ "intermediate",
                                      experience <= 24 ~ "midlevel",
                                      experience <= 32 ~ "experienced",
                                      experience <= 40 ~ "senior",))

# grouping the period 
electricvehicle_rep <- electricvehicle_rep %>%
  mutate(period_groups = case_when(period <= 3 ~ "Early Stage",
                                   period <= 6 ~ "Mid Stage",
                                   period <= 9 ~ "Intermediate Stage",
                                   period <= 12 ~ "Late Stage",))

# Removing period and experience  
electricvehicle_rep <- electricvehicle_rep %>%
  select(-period)
electricvehicle_rep <- electricvehicle_rep %>%
  select(-experience)

str(electricvehicle_rep)


#Data analysis

#How effective is the promotion
f1 <- electricvehicle_rep %>%
  count(promotions) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = promotions, y = percentage, fill = promotions)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 1.05),
            size = 5) +
  labs(title = "Percentage of Purchases with Promotions",
       x = "Promotion Used",
       y = "Percentage") +
  scale_fill_manual(values = c("yes" = "#7CB9E8", "no" = "#00308F" )) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.border = element_blank()) +  
  coord_flip()


#Products bought with and without promotions
f2 <- electricvehicle_rep %>%
  count(product, promotions) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0(round(percentage, 1), "%")) %>%
  ggplot(aes(x = product, y = n, fill = promotions)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = label), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 5) +
  labs(title = "Products Bought with and Without Promotions",
       x = "Product",
       y = "Number of Purchases",
       fill = "Promotions") +
  scale_fill_manual(values = c("yes" = "#7CB9E8", "no" = "#00308F")) +  
  scale_y_continuous(labels = scales::number_format())+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white"),
        panel.grid = element_blank()) 


#Buyers for Each Product Category without Promotions

# Filter data for purchases made without promotions
f3 <- electricvehicle_rep %>%
  filter(promotions == "no") %>%
  count(product, buyer) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = product, y = n, fill = buyer)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("single" = "#00308F", "couples" = "#72A0C1", "family" = "#7CB9E8")) + 
  labs(title = "Buyers for Each Product Category without Promotions",
       x = "Product",
       y = "Number of Buyers",
       fill = "Buyer Type") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white"),
        panel.grid = element_blank())


#summary statistics of marketing 
summary(electricvehicle_rep$marketing)

# Calculate the total marketing spend on each product
f4 <- electricvehicle_rep %>%
  group_by(product) %>%
  summarise(total_marketing_spent = sum(marketing, na.rm = TRUE)) %>%
  mutate(percentage = total_marketing_spent / sum(total_marketing_spent) * 100) %>%
  ggplot(aes(x = product, y = percentage)) +
  geom_bar(stat = "identity", fill = "#72A0C1") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 1.06), size = 5, color = "black") +
  labs(title = "Percentage of Total Marketing Spend per Product",
       x = "Product",
       y = "Percentage of Total Marketing Spend") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))


# Calculate the total marketing spend on each Campaign channel
f5 <- electricvehicle_rep %>%
  group_by(campaign) %>%
  summarise(total_marketing_spent = sum(marketing, na.rm = TRUE)) %>%
  mutate(percentage = total_marketing_spent / sum(total_marketing_spent) * 100) %>%
  ggplot(aes(x = campaign, y = percentage)) +
  geom_bar(stat = "identity", fill = "#72A0C1") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 1.05), size = 5, color = "black") +
  labs(title = "Percentage of Total Marketing Spend per Product",
       x = "campaign",
       y = "Percentage of Total Marketing Spend") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))




# Calculate total marketing spend per experience level
f6 <- electricvehicle_rep %>%
  group_by(experience_years) %>%
  summarise(total_marketing_spent = sum(marketing, na.rm = TRUE)) %>%
  mutate(percentage = (total_marketing_spent / sum(total_marketing_spent)) * 100) %>%
  ggplot(aes(x = experience_years, y = percentage)) +
  geom_bar(stat = "identity", fill = "#72A0C1", width = 0.5) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            vjust = -0.5, size = 5) +
  labs(title = "Percentage of Total Marketing Spend by Experience Level",
       x = "Experience Level",
       y = "Percentage of Total Marketing Spend (%)") +
  theme_minimal()

# Calculate total marketing spend per qualification
f7 <- electricvehicle_rep %>%
  group_by(qualification) %>%
  summarise(total_marketing_spent = sum(marketing, na.rm = TRUE)) %>%
  mutate(percentage = (total_marketing_spent / sum(total_marketing_spent)) * 100) %>%
  ggplot(aes(x = qualification, y = percentage)) +
  geom_bar(stat = "identity", fill = "#72A0C1", width = 0.5) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            vjust = -0.5, size = 5) +
  labs(title = "Percentage of Total Marketing Spend by Qualification",
       x = "Qualification",
       y = "Percentage of Total Marketing Spend (%)") +
  theme_minimal()


# Calculate total marketing spend per job type
f8 <- electricvehicle_rep %>%
  group_by(jobtype) %>%
  summarise(total_marketing_spent = sum(marketing, na.rm = TRUE)) %>%
  mutate(percentage = (total_marketing_spent / sum(total_marketing_spent)) * 100) %>%
  ggplot(aes(x = jobtype, y = percentage)) +
  geom_bar(stat = "identity", fill = "#72A0C1", width = 0.5) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            vjust = -0.5, size = 5) +
  labs(title = "Percentage of Total Marketing Spend by Job Type",
       x = "Job Type",
       y = "Percentage of Total Marketing Spend (%)") +
  theme_minimal()

#creating the performance dashboard
grid.arrange(f1,f3,f4,f5)


