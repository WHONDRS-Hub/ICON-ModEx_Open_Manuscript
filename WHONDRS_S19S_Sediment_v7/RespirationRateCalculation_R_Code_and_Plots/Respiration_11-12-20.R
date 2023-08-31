#12 November 2020
#WHONDRS S19S Sediment Data Package
#The script completes the following steps:
# 1) Removes samples for which dissolved oxygen reached 5 mg/L O2 in less than 3 minutes
#2) Calculates respiration rates as the slope of the linear regression between dissolved oxygen concentration and incubation time
#3) Removes samples that had an Rsq< 0.25 from their regressions

### User must change working directory (line 22)

###### Load Library ######

library(dplyr); library(ggplot2);library(ggsignif)
library(ggpubr);library(reshape2);library(ggpmisc)
library(segmented);library(broom)
library(ggpmisc);library(segmented);library(lubridate)

##### Load data ######
rm(list=ls());graphics.off()

# Set working directory to data file
#Example:
setwd("//pnl/projects/SBR_SFA/Campaign C/Hydropeaking_Network/WHONDRS_and_Non-WHONDRS_Data/Data_Packaging/Uploaded_to_ESS-DIVE/WHONDRS_S19S_Sediment/RespirationRateCalculation_R_Code_and_Plots/Inputs")


data = read.csv("DO_data_formatted.csv") #reads all the data from all treatments

data$AVG_DO_mg_per_L = as.numeric((data$AVG_DO_mg_per_L))
data$StartMeasureTime = as.POSIXct(data$StartMeasureTime, format = "%H:%M")

ymax = max(data$AVG_DO_mg_per_L)
ymin = min(data$AVG_DO_mg_per_L)

##### Calculate Respiration Rates ######

# Calculate rates per sediment location 

location=c("-U","-M","-D") # Represents upstream, midstream and downstream sediment locations

# Creating a matrix to store the rates
respiration.metrics = as.data.frame(matrix(NA, ncol = 8, nrow=1)) #ncol has to be equal to the number of exported variables

colnames(respiration.metrics) = c("Sample_ID","slope_of_the_regression","rate_mg_per_L_per_min","rate_mg_per_L_per_h","R_squared","R_squared_adj","p_value","total_incubation_time_min") 

# Subseting data by sediment sample location
for (i in 1:length(location)){
  
      data_location_subset = data[grep(location[i],data$Sample_ID),]
      
      # Incubations associated to each sampling location
      unique.incubations = unique(data_location_subset$Sample_ID)
      
      # Creating a matrix to store the rates
       rate =as.data.frame(matrix(NA, ncol = 8, nrow= length(unique(data$Sample_ID))))
      
      colnames(rate) = c("Sample_ID","slope_of_the_regression","rate_mg_per_L_per_min","rate_mg_per_L_per_h","R_squared","R_squared_adj","p_value","total_incubation_time_min") 
      
      # Subseting data for unique incubation and organizing sampling time in decreasing order
      for (j in 1:length(unique.incubations)){
        
      data_site_subset = subset(data_location_subset,data_location_subset$Sample_ID==unique.incubations[j])
      data_site_subset = data_site_subset[order(data_site_subset$StartMeasureTime, decreasing = FALSE),]
      data_site_subset$StartMeasureTime = as.POSIXct(data_site_subset$StartMeasureTime, format = "%H:%M")
      
      # Only calculating rates for incubations with data collected over 3 minutes. See "readme". 
      data_collection_time_min = as.numeric(difftime(data_site_subset$StartMeasureTime[nrow(data_site_subset)],data_site_subset$StartMeasureTime[1],units="mins"))#in minutes
      if (data_collection_time_min > 3){
      if (nrow(data_site_subset)>1){
        for (k in 1:nrow(data_site_subset)){
          data_site_subset$time_elapsed_min[k] = as.numeric(difftime(data_site_subset$StartMeasureTime[k],data_site_subset$StartMeasureTime[1],units="mins"))#in minutes
        }}        
      
       
      # Calculating rate as the slope of the linear regression between dissolved oxygen and time
        fit = lm(data_site_subset$AVG_DO_mg_per_L~data_site_subset$time_elapsed_min)
        u = fit$coefficients
        b = u[[1]] #Intercept
        c = u[[2]] #rate mg/L min
        r = summary(fit)$r.squared
        r.adj = summary(fit)$adj.r.squared
        p = summary(fit)$coefficients[4]  
        
        # Only reporting data with R_squared > 0.25. See "readme"
        if (r > 0.25){
        
          #Plotting dissolved oxygen concentration vs elapsed incubation time  
        my.formula <- y ~ x
        ggplot(data_site_subset, aes(x = time_elapsed_min, y = AVG_DO_mg_per_L)) + coord_cartesian(ylim = c(ymin,ymax))+ geom_point(size = 2) + expand_limits(x = 0, y = 0) + 
          geom_smooth(method = "lm", se=F, formula = my.formula) +
          stat_poly_eq(formula = my.formula,label.y = "top",label.x = "left", aes(label = paste( ..rr.label.., sep = "~~~"),size=1), parse = TRUE)+stat_fit_glance(data=data_site_subset, method = 'lm', method.args = list(formula = my.formula),geom = 'text',aes(label =paste("p = ",signif(..p.value.., digits = 1), sep = ""),size=1),label.y = c(11.75),label.x = "right") + 
          theme_bw()+theme(legend.title = element_blank(),legend.background = element_rect(fill = 'NA'), legend.text = element_text(size = 12,face="bold"))+
          labs(y = expression(Dissolved_Oxygen_mg_per_L), x = expression(Time_Elapsed_min))+ theme(axis.text.x=element_text(size = 12,face="bold"))+
          ggtitle(data_site_subset$Sample_ID[1]) + 
          theme(plot.title = element_text(lineheight=.8, face="bold"))
          theme(axis.text.x=element_text(colour = c("black","black")))+
          theme(aspect.ratio=1)+
          theme(axis.text.y=element_text(size = 12,face="bold"))+
          theme(axis.title.x =element_text(size = 12,face="bold"))+
          theme(axis.title =element_text(size = 12,face="bold"))+
          theme(axis.title.y =element_text(size = 12,face="bold"))
        ggsave(file=paste("DO_vs_Incubation_Time_",data_site_subset$Sample_ID[1],".pdf",sep = ""))
        
        # Extracting relevant regression metrics 
        rate$Sample_ID[j] = as.character(data_site_subset$Sample_ID[1])
        rate$slope_of_the_regression[j] = as.numeric((c)) #in mg O2/L min
        rate$rate_mg_per_L_per_min[j] = abs(as.numeric((c))) #in mg O2/L min
        rate$rate_mg_per_L_per_h[j] = abs(as.numeric((c))*60) #in mg O2/L h 
        rate$R_squared[j] = as.numeric(abs(r))
        rate$R_squared_adj[j] = as.numeric(abs(r.adj))
        rate$p_value[j] = p
        rate$total_incubation_time_min[j] = as.numeric(difftime(data_site_subset$StartMeasureTime[nrow(data_site_subset)],data_site_subset$StartMeasureTime[1],units="mins"))#in minutes
        }}}
    # Removing rows where the Sample_ID was an NA  
    rate = rate[!is.na(rate$Sample_ID),]   
    # Combining the regression metrics across locations and unique incubations
    respiration.metrics = rbind(respiration.metrics,rate)
  
    }

#removes the 1st row because it is an NA
respiration.metrics = respiration.metrics[-1,]

#Exports data
write.csv(respiration.metrics,"WHONDRS_S19S_Sediment_Incubations_Respiration_Rates.csv", row.names = F)






