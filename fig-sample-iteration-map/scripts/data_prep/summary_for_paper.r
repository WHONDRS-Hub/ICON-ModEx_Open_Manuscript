# for manuscript: 
#    - get the median value from 1st and last iteration


unique(combined_df$Date)

unique(combined_df$iteration_num)
glimpse(combined_df)




second_iter_df <- combined_df %>% filter(Date == '2021-12-01')

glimpse(diff_df)


#  FIRST ITERATION AT EACH POINT
first_mean <- mean(diff_df$Predicted_Normalized_Respiration_Rate_firstiter) %>% round(2)
first_sd <- sd(diff_df$Predicted_Normalized_Respiration_Rate_firstiter) %>% round(2)

# SECOND ITERATION
second_mean <- mean(second_iter_df$Predicted_Normalized_Respiration_Rate) %>% round(2)
second_sd <- sd(second_iter_df$Predicted_Normalized_Respiration_Rate) %>% round(2)


#  LAST ITERATION
last_mean <- mean(diff_df$Predicted_Normalized_Respiration_Rate_lastiter) %>% round(2)
last_sd <- sd(diff_df$Predicted_Normalized_Respiration_Rate_lastiter) %>% round(2)


print(paste('first iteration mean:', first_mean))
print(paste('first iteration sd:', first_sd))


print(paste('second iteration mean:', second_mean))
print(paste('second iteration sd:', second_sd))


print(paste('last iteration mean:', last_mean))
print(paste('last iteration sd:', last_sd))



# /----------------------------
#/  ACTUAL ITERATION #1 AND #19



iter2019_df <- 
  combined_df %>% 
  filter(iteration_num == 1)

iter2023_11 <-   
  combined_df %>% 
  filter(iteration_num == 19)




#  FIRST ITERATION AT EACH POINT
nrow(iter2019_df)
mean(iter2019_df$Predicted_Normalized_Respiration_Rate) %>% round(2)
sd(iter2019_df$Predicted_Normalized_Respiration_Rate) %>% round(2)
sum(iter2019_df$Predicted_Normalized_Respiration_Rate < -529) / nrow(iter2019_df) *100



#  LAST ITERATION
nrow(iter2023_11)
mean(iter2023_11$Predicted_Normalized_Respiration_Rate) %>% round(2)
sd(iter2023_11$Predicted_Normalized_Respiration_Rate) %>% round(2)
sum(iter2023_11$Predicted_Normalized_Respiration_Rate < -529) / nrow(iter2023_11) *100






