
# Script for processing sediment incubation dissolved oxygen time series data to 
# produce respiration rates. 
# 
# Vanessa Garayburu-Caruso 
# https://whondrs.pnnl.gov
# whondrs@pnnl.gov
# River Cooridor SFA
# Pacific Northwest National Laboratory

###### Load Library ######

library(dplyr); library(ggplot2);library(ggsignif); library(tidvyverse)
library(ggpubr);library(reshape2);library(ggpmisc)
library(segmented);library(broom)
library(ggpmisc);library(segmented);library(lubridate); library(readxl)

##### Load data ######
rm(list=ls());graphics.off()

# Set working directory to data file
#Example:
pnnl.user = 'gara009'
input.path = paste0("C:/Users/",pnnl.user,"/OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/RC4/ICON_ModEX/02_Raw_DO_INC_data/SSS/")
output.path = paste0("C:/Users/",pnnl.user,"/OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/RC4/ICON_ModEX/03_Processed_Respiration_by_kit/")
setwd(input.path)

data = read_csv("SSS_DO_INC.xlsx") #reads all the data from all treatments
data$Sample_label = gsub('_INC', '', as.character(Sample_Name))

data$AVG_DO_mg_per_L = as.numeric((data$AVG_DO_mg_per_L))
data$StartMeasureTime = as.POSIXct(data$StartMeasureTime, format = "%H:%M:%S")

ymax = max(na.omit(data$AVG_DO_mg_per_L))
ymin = min(na.omit(data$AVG_DO_mg_per_L))
threshold = 0.99
DO.threshold = 9.5
##### Calculate Respiration Rates ######

# Calculate rates per sediment replicate

location=c("-1","-2","-3") # Represents each of the replicates

# Creating a matrix to store the rates
respiration.metrics = as.data.frame(matrix(NA, ncol = 11, nrow=1)) #ncol has to be equal to the number of exported variables

colnames(respiration.metrics) = c("Sample_ID","slope_of_the_regression","rate_mg_per_L_per_min","rate_mg_per_L_per_h","R_squared","R_squared_adj","p_value","total_incubation_time_min","number_of_points","No_points_removed","DO_time_zero") 

# Subsetting data by sediment sample location
for (i in 1:length(location)){
  
      data_location_subset = data[grep(location[i],data$Sample_ID),]
      
      # Incubations associated to each sampling location
      unique.incubations = unique(data_location_subset$Sample_ID)
      
      # Creating a matrix to store the rates
       rate =as.data.frame(matrix(NA, ncol = 8, nrow= length(unique(data$Sample_ID))))
      
      colnames(rate) = c("Sample_ID","slope_of_the_regression","rate_mg_per_L_per_min","rate_mg_per_L_per_h","R_squared","R_squared_adj","p_value","total_incubation_time_min") 
      
      # Subsetting data for unique incubation and organizing sampling time in decreasing order
      for (j in 1:length(unique.incubations)){
        
      data_site_subset = subset(data_location_subset,data_location_subset$Sample_ID==unique.incubations[j])
      data_site_subset = data_site_subset[order(data_site_subset$StartMeasureTime, decreasing = FALSE),]
      data_site_subset$StartMeasureTime = as.POSIXct(data_site_subset$StartMeasureTime, format = "%H:%M:%S") # the I in the format is for 24 hour system
      
      # Only calculating rates for incubations with data collected over 3 minutes. 
      data_collection_time_min = as.numeric(difftime(data_site_subset$StartMeasureTime[nrow(data_site_subset)],data_site_subset$StartMeasureTime[1],units="mins"))#in minutes
      data_site_subset$time_elapsed_min = NA
     # if (data_collection_time_min > 3){ # add again later if needed to subset based on incubation time
      if (nrow(data_site_subset)>1){
        for (k in 1:nrow(data_site_subset)){
          data_site_subset$time_elapsed_min[k] = as.numeric(difftime(data_site_subset$StartMeasureTime[k],data_site_subset$StartMeasureTime[1],units="mins"))#in minutes
        }}        
      
       
     
        dataog = data_site_subset
        
        # Checking that data that we measured makes physical and biological sense. According to the Pressure and temperature conditions of the environmental chamber, the maximum concentration of DO where we would see 100% saturation is 8.56 mg/L. We will check if the first data point makes physical and biological sense by comparing it to a threshold set to 9.5. If the DO in the first point is higher then that then we need to remove it. 
        pts.removed = as.data.frame(matrix(NA,nrow= 1, ncol = ncol(data_site_subset))); colnames(pts.removed) = colnames(data_site_subset)
        
        if (data_site_subset$AVG_DO_mg_per_L[1]>DO.threshold){
          pts.removed = rbind(pts.removed, data_site_subset[1,])
          data_site_subset = data_site_subset[-1,]
          
        }

        # Calculating rate as the slope of the linear regression between dissolved oxygen and time
        fitog = lm(data_site_subset$AVG_DO_mg_per_L~data_site_subset$time_elapsed_min)
        rog = summary(fitog)$r.squared
        pog = summary(fitog)$coefficients[4] 
#Now if new R is below the threshold, remove points from the end of the regression until R.sq stops increasing or we get down to 5 points. Note that the rule is to remove data but only if the R2 goes up after removing. If it goes down, then we don't remove. 
# Removing one point
        temp = data_site_subset[-nrow(data_site_subset),]
        fit.temp = lm(temp$AVG_DO_mg_per_L~temp$time_elapsed_min)
        rtemp = summary(fit.temp)$r.squared
      
        if (rog< threshold&nrow(data_site_subset)>=5&rtemp > rog){
         pts.removed = rbind(pts.removed,data_site_subset[nrow(data_site_subset),])
        data_site_subset = data_site_subset[-nrow(data_site_subset),]
        fit = lm(data_site_subset$AVG_DO_mg_per_L~data_site_subset$time_elapsed_min)
        r2 = summary(fit)$r.squared
     
# Removing the second point
        temp = data_site_subset[-nrow(data_site_subset),]
        fit.temp = lm(temp$AVG_DO_mg_per_L~temp$time_elapsed_min)
        rtemp = summary(fit.temp)$r.squared
        
        if (r2< threshold&nrow(data_site_subset)>=5&rtemp > r2){        
         pts.removed = rbind(pts.removed,data_site_subset[nrow(data_site_subset),])
         data_site_subset = data_site_subset[-nrow(data_site_subset),]
         fit = lm(data_site_subset$AVG_DO_mg_per_L~data_site_subset$time_elapsed_min)
         r3 = summary(fit)$r.squared
     
        
# Removing a 3rd point
        temp = data_site_subset[-nrow(data_site_subset),]
        fit.temp = lm(temp$AVG_DO_mg_per_L~temp$time_elapsed_min)
        rtemp = summary(fit.temp)$r.squared
        
        if (r3< threshold&nrow(data_site_subset)>=5&rtemp > r3){ 
          pts.removed = rbind(pts.removed,data_site_subset[nrow(data_site_subset),])
          data_site_subset = data_site_subset[-nrow(data_site_subset),]
          fit = lm(data_site_subset$AVG_DO_mg_per_L~data_site_subset$time_elapsed_min)
          rend = summary(fit)$r.squared
        }
        }
        
        }
        
# Final regression after all calculations are done
        fit = lm(data_site_subset$AVG_DO_mg_per_L~data_site_subset$time_elapsed_min)
        u = fit$coefficients
        b = u[[1]] #Intercept
        cfinal = u[[2]] #rate mg/L min
        rfinal = summary(fit)$r.squared
        r.adj = summary(fit)$adj.r.squared
        pfinal = summary(fit)$coefficients[4]
          #Plotting dissolved oxygen concentration vs elapsed incubation time  
        
        my.formula <- y ~ x
if (exists("pts.removed")==T){
  pts.removed = pts.removed[-1,]
        ggplot(data_site_subset, aes(x = time_elapsed_min, y = AVG_DO_mg_per_L)) + coord_cartesian(ylim = c(ymin,ymax))+ geom_point(size = 2) + expand_limits(x = 0, y = 0) + 
          geom_smooth(method = "lm", se=F, formula = my.formula, color = "black") +
          stat_poly_eq(formula = my.formula,label.y = "bottom",label.x = "left", aes(label = paste( ..rr.label.., sep = "~~~"),size=1), parse = TRUE)+stat_fit_glance(data=data_site_subset, method = 'lm', method.args = list(formula = my.formula),geom = 'text',aes(label =paste("p = ",signif(..p.value.., digits = 1), sep = ""),size=1),label.y = "top",label.x = "right") +
          geom_point(data = pts.removed,aes(x = time_elapsed_min, y = AVG_DO_mg_per_L), color = "red")+
          geom_text(aes(label = paste0("R2 = ",round(rog,3)), y = 0, x = data_collection_time_min-6),color = "red")+
          geom_text(aes(label = paste0("p = ",round(pog,6)), y = ymax, x = data_collection_time_min-6),color = "red")+
          theme_bw()+theme(legend.title = element_blank(),legend.background = element_rect(fill = 'NA'), legend.text = element_text(size = 12,face="bold"))+
          labs(y = expression(Dissolved_Oxygen_mg_per_L), x = expression(Time_Elapsed_min))+ theme(axis.text.x=element_text(size = 12,face="bold"))+
          ggtitle(data_site_subset$Sample_label[1]) + 
          theme(plot.title = element_text(lineheight=.8, face="bold"))
          theme(axis.text.x=element_text(colour = c("black","black")))+
          theme(aspect.ratio=1)+
          theme(axis.text.y=element_text(size = 12,face="bold"))+
          theme(axis.title.x =element_text(size = 12,face="bold"))+
          theme(axis.title =element_text(size = 12,face="bold"))+
          theme(axis.title.y =element_text(size = 12,face="bold"))
        ggsave(file=paste0(output.path,"Plots/DO_vs_Incubation_Time_",data_site_subset$Sample_label[1],".pdf"))}else{
          ggplot(data_site_subset, aes(x = time_elapsed_min, y = AVG_DO_mg_per_L)) + coord_cartesian(ylim = c(ymin,ymax))+ geom_point(size = 2) + expand_limits(x = 0, y = 0) + 
            geom_smooth(method = "lm", se=F, formula = my.formula, color = "black") + geom_text(aes(label = paste0("R2 = ",round(rfinal,3)), y = 0, x = 10),color = "black")+
            geom_text(aes(label = paste0("p = ",pfinal), y = ymax, x = data_collection_time_min-6),color = "black")  + geom_text(aes(label = paste0("R2 = ",round(rog,3)), y = 0, x = data_collection_time_min-6),color = "red")+
            geom_text(aes(label = paste0("p = ",pog), y = ymax, x = data_collection_time_min-6),color = "red")+
            theme_bw()+theme(legend.title = element_blank(),legend.background = element_rect(fill = 'NA'), legend.text = element_text(size = 12,face="bold"))+
            labs(y = expression(Dissolved_Oxygen_mg_per_L), x = expression(Time_Elapsed_min))+ theme(axis.text.x=element_text(size = 12,face="bold"))+
            ggtitle(data_site_subset$Sample_label[1]) + 
            theme(plot.title = element_text(lineheight=.8, face="bold"))
          theme(axis.text.x=element_text(colour = c("black","black")))+
            theme(aspect.ratio=1)+
            theme(axis.text.y=element_text(size = 12,face="bold"))+
            theme(axis.title.x =element_text(size = 12,face="bold"))+
            theme(axis.title =element_text(size = 12,face="bold"))+
            theme(axis.title.y =element_text(size = 12,face="bold"))
          ggsave(file=paste0(output.path,"Plots/DO_vs_Incubation_Time_",data_site_subset$Sample_label[1],".pdf"))
        }
        
        # Extracting relevant regression metrics 
        rate$Sample_ID[j] = as.character(data_site_subset$Sample_label[1])
        rate$slope_of_the_regression[j] = round(as.numeric((cfinal)),3) #in mg O2/L min
        rate$rate_mg_per_L_per_min[j] = round(abs(as.numeric((cfinal))),3) #in mg O2/L min
        rate$rate_mg_per_L_per_h[j] = round(abs(as.numeric((cfinal))*60),3) #in mg O2/L h 
        rate$R_squared[j] = round(as.numeric(abs(rfinal)),3)
        rate$R_squared_adj[j] = round(as.numeric(abs(r.adj)),3)
        rate$p_value[j] = pfinal
        rate$total_incubation_time_min[j] = as.numeric(difftime(data_site_subset$StartMeasureTime[nrow(data_site_subset)],data_site_subset$StartMeasureTime[1],units="mins"))#in minutes
        rate$number_of_points[j] = nrow(data_site_subset)
        rate$No_points_removed[j] = nrow(dataog)-nrow(data_site_subset)
        if (dataog$AVG_DO_mg_per_L[1]<DO.threshold){
          rate$DO_time_zero[j] = dataog$AVG_DO_mg_per_L[1]
        }else{
          rate$DO_time_zero[j] = "N/A"
        }
      }
    # Removing rows where the Sample_ID was an NA  
    rate = rate[!is.na(rate$Sample_ID),]   
    # Combining the regression metrics across locations and unique incubations
    respiration.metrics = rbind(respiration.metrics,rate)
  
   }

#removes the 1st row because it is an NA
respiration.metrics = respiration.metrics[-1,]

#Exports data
write.csv(respiration.metrics,paste0(output.path,"SSS_ICON-ModEx_Sediment_Incubations_Respiration_Rates_merged_by_",pnnl.user,"_on_",Sys.Date(),".csv"), row.names = F)






