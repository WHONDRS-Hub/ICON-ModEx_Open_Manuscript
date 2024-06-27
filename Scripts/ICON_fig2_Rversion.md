Fig2_Rversion
================
Bre Rivera Waterman
2024-02-22

- import libraries

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.2

    ## Warning: package 'ggplot2' was built under R version 4.3.2

    ## Warning: package 'tibble' was built under R version 4.3.2

    ## Warning: package 'tidyr' was built under R version 4.3.2

    ## Warning: package 'readr' was built under R version 4.3.2

    ## Warning: package 'purrr' was built under R version 4.3.2

    ## Warning: package 'dplyr' was built under R version 4.3.2

    ## Warning: package 'stringr' was built under R version 4.3.2

    ## Warning: package 'forcats' was built under R version 4.3.2

    ## Warning: package 'lubridate' was built under R version 4.3.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(colourpicker)
```

    ## Warning: package 'colourpicker' was built under R version 4.3.2

``` r
library(lubridate)
library(emmeans)
```

    ## Warning: package 'emmeans' was built under R version 4.3.2

``` r
library(sf)
```

    ## Warning: package 'sf' was built under R version 4.3.2

    ## Linking to GEOS 3.11.2, GDAL 3.7.2, PROJ 9.3.0; sf_use_s2() is TRUE

``` r
library(ggspatial)
```

    ## Warning: package 'ggspatial' was built under R version 4.3.2

``` r
library(spData)
```

    ## Warning: package 'spData' was built under R version 4.3.2

    ## To access larger datasets in this package, install the spDataLarge
    ## package with: `install.packages('spDataLarge',
    ## repos='https://nowosad.github.io/drat/', type='source')`

``` r
library(ggpmisc)
```

    ## Warning: package 'ggpmisc' was built under R version 4.3.2

    ## Loading required package: ggpp

    ## Warning: package 'ggpp' was built under R version 4.3.2

    ## Registered S3 methods overwritten by 'ggpp':
    ##   method                  from   
    ##   heightDetails.titleGrob ggplot2
    ##   widthDetails.titleGrob  ggplot2
    ## 
    ## Attaching package: 'ggpp'
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate
    ## 
    ## Registered S3 method overwritten by 'ggpmisc':
    ##   method                  from   
    ##   as.character.polynomial polynom

``` r
#install.packages("ggpubr")
library("rstudioapi")
```

    ## Warning: package 'rstudioapi' was built under R version 4.3.2

``` r
#install.packages("ggExtra")
library(ggExtra)
```

    ## Warning: package 'ggExtra' was built under R version 4.3.3

    ## 
    ## Attaching package: 'ggExtra'
    ## 
    ## The following object is masked from 'package:colourpicker':
    ## 
    ##     runExample

``` r
library(patchwork)
```

    ## Warning: package 'patchwork' was built under R version 4.3.2

``` r
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./../")
getwd()
```

    ## [1] "C:/Users/b923w423/Documents/GitHub/ICON-ModEx_Open_Manuscript"

## Scatter plot

- using new merged dataset to create scatter plot
- add kernel densities to the plots (along the side)

``` r
#some filtering/mutating first 
#C:/Users/b923w423/Documents/GitHub/ICON-ModEx_Open_Manuscript
combods <- read.csv("ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv") %>% 
#remova Na's
  filter(!is.na(Log_Observed_Normalized_Respiration_Rate), #Log_Observed_Normalized_Respiration_Rate
         !is.na(Log_Predicted_Normalized_Respiration_Rate_Jul2022), !is.na(Log_Predicted_Normalized_Respiration_Rate_Nov2023)) %>% filter(Log_Observed_Normalized_Respiration_Rate > -9990,                                            Log_Predicted_Normalized_Respiration_Rate_Nov2023 > -9990 ,
 Log_Predicted_Normalized_Respiration_Rate_Jul2022 > -9990) %>% #Log_Predicted_Normalized_Respiration_Rate
#group by above/below log500 
  mutate(ds_split = ifelse(Log_Observed_Normalized_Respiration_Rate > 2.7, "500+",                         ifelse(Log_Observed_Normalized_Respiration_Rate < 2.7, "500-", "500-") )) %>% 

  #clasifying by quad by year
  mutate(ds_quadrant23 =                                                    ifelse(Log_Observed_Normalized_Respiration_Rate > 2.7 & Log_Predicted_Normalized_Respiration_Rate_Nov2023 > 2.7 , "hs23",
  ifelse(Log_Observed_Normalized_Respiration_Rate > 2.7 & Log_Predicted_Normalized_Respiration_Rate_Nov2023 < 2.7 , "misclass_hs23",                                                                                              
ifelse(Log_Observed_Normalized_Respiration_Rate < 2.7 & Log_Predicted_Normalized_Respiration_Rate_Nov2023 < 2.7 , "cs23",    
ifelse(Log_Observed_Normalized_Respiration_Rate < 2.7 & Log_Predicted_Normalized_Respiration_Rate_Nov2023 > 2.7 , "misclass_cs23", NA)))) ,                   
      
ds_quadrant22 =        
 ifelse(Log_Observed_Normalized_Respiration_Rate > 2.7 & Log_Predicted_Normalized_Respiration_Rate_Jul2022 > 2.7 , "hs22",
  ifelse(Log_Observed_Normalized_Respiration_Rate > 2.7 & Log_Predicted_Normalized_Respiration_Rate_Jul2022 < 2.7 , "misclass_hs22",  
 ifelse(Log_Observed_Normalized_Respiration_Rate < 2.7 & Log_Predicted_Normalized_Respiration_Rate_Jul2022 > 2.7 , "misclass_cs22",
ifelse(Log_Observed_Normalized_Respiration_Rate < 2.7 & Log_Predicted_Normalized_Respiration_Rate_Jul2022 < 2.7 , "cs22", NA ) )))) 




#scatter plot
#needs to have kernel density plots on y and x axis
#needs use log data
#needs to have overall line, line for above 500, below 500, and R^2 values for all three
#facet wrap or color dots by iteration (first versus last)


fullscatter_firstiteration <- 
  combods %>% ggplot(aes(abs(Log_Observed_Normalized_Respiration_Rate ),
                      abs(Log_Predicted_Normalized_Respiration_Rate_Jul2022) #)) +
                      ,  color =  ds_split))+ 
#to see R^2 for above/below thresholds, remove color grouping above and use stat poly eq fx (below)
  geom_point(color = "darkgrey", size = 3, pch = 21) + 
    geom_vline (xintercept = 2.7, linetype = "dashed", color="grey") + 
    geom_hline (yintercept = 2.7, linetype = "dashed", color="dark grey") + #not a clear threshold 
      geom_abline (linetype = "longdash", color="black") + #not a clear threshold 
   stat_poly_line(color = "black", alpha = 0.5) + #overall best fit
 # stat_poly_eq(use_label(c( "R2"))) + #remove hashtag to see R^2 of log500 split
  annotate(geom="text", x=3.5, y=0.75, label="R^2 == 0.54",parse =TRUE, color="black", size =4.5) + #update if data changes - not automatically added
    geom_smooth( aes(abs(Log_Observed_Normalized_Respiration_Rate ), #above/below threshold best fit
                      abs(Log_Predicted_Normalized_Respiration_Rate_Jul2022),
                      color =  ds_split), method = "lm", alpha = 0.2) + 
  xlab("log10(Observed rates+1)") + 
  ylab("log10(Predicted rates+1)") + 
scale_shape_manual(values = c(21, 16)) + #shape or color by iteration
  scale_color_manual(values = c( "lightblue", "maroon")) +
#update if data changes - not automatically added
  annotate(geom="text", x=3.5, y=0.5, label="R^2 == 0.22",parse =TRUE, color="maroon", size =4.5) + #update if data changes - not automatically added
 annotate(geom="text", x=3.5, y=0.25, label="R^2 == 0.15", parse =TRUE,
               color="lightblue", size =4.5) + #update - not automatically added
  lims(y = c(0,4), x = c(0,4)) + 
  theme_classic() +
   theme(text = element_text(size = 16), 
         legend.position = 0)

#adding density plots on axes
 ggExtra::ggMarginal(fullscatter_firstiteration, fill = "lightgrey")
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](ICON_fig2_Rversion_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggsave(filename = "Scripts/fullscatter_firstiteration.tiff", plot = fullscatter_firstiteration, width = 6, height = 6, dpi = 300)   
```

    ## `geom_smooth()` using formula = 'y ~ x'

``` r
fullscatter_lastiteration <- 
  combods %>% ggplot(aes(abs(Log_Observed_Normalized_Respiration_Rate ),
                      abs(Log_Predicted_Normalized_Respiration_Rate_Nov2023) )) + #
                      #,  color =  ds_split))+ 
#to see R^2 for above/below thresholds, remove color grouping above and use stat poly eq fx (below)
  geom_point(color = "darkgrey", size = 3, pch = 21) + 
    geom_vline (xintercept = 2.7, linetype = "dashed", color="grey") + 
    geom_hline (yintercept = 2.7, linetype = "dashed", color="dark grey") + #not a clear threshold 
      geom_abline (linetype = "longdash", color="black") + #not a clear threshold 
   stat_poly_line(color = "black", alpha = 0.5) + #overall best fit
 # stat_poly_eq(use_label(c( "R2"))) + #remove hashtag to see R^2 of log500 split
  annotate(geom="text", x=3.5, y=0.75, label="R^2 == 0.61",parse =TRUE, color="black", size =4.5) + #update if data changes - not automatically added
    geom_smooth( aes(abs(Log_Observed_Normalized_Respiration_Rate ), #above/below threshold best fit
                      abs(Log_Predicted_Normalized_Respiration_Rate_Nov2023),
                      color =  ds_split), method = "lm", alpha = 0.2) + 
  xlab("log10(Observed rates+1)") + 
  ylab("log10(Predicted rates+1)") + 
scale_shape_manual(values = c(21, 16)) + #shape or color by iteration
  scale_color_manual(values = c( "lightblue", "maroon")) +
#update if data changes - not automatically added
  annotate(geom="text", x=3.45, y=0.5, label="R^2 == 0.20",parse =TRUE, color="maroon", size =4.5) + #update if data changes - not automatically added
 annotate(geom="text", x=3.5, y=0.25, label="R^2 == 0.21", parse =TRUE,
               color="lightblue", size =4.5) + #update - not automatically added
  lims(y = c(0,4), x = c(0,4)) + 
  theme_classic() +
   theme(text = element_text(size = 16), 
         legend.position = 0)

#adding density plots on axes
 ggExtra::ggMarginal(fullscatter_lastiteration, fill = "lightgrey")
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](ICON_fig2_Rversion_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggsave(filename = "Scripts/fullscatter_lastiteration.tiff", plot = fullscatter_lastiteration, width = 6, height = 6, dpi = 300)   
```

    ## `geom_smooth()` using formula = 'y ~ x'

- exploring misclass patterns

``` r
combods_date <- combods %>%
   mutate(Date_whole = ymd(Observed_Sample_Date), Year = year(Observed_Sample_Date), Month = month(Observed_Sample_Date), Day = day(Observed_Sample_Date))
 


#based on previous plots, temporal outliers are from 2019 only
#recreate slide 7
combods_date %>% ggplot(aes(abs(Log_Observed_Normalized_Respiration_Rate),
                      abs(Log_Predicted_Normalized_Respiration_Rate_Nov2023), color = Year ))+ 
#to see R^2 for above/below thresholds, remove color grouping above and use stat poly eq fx (below)
  geom_point( pch = 21, size = 3) + 
    geom_vline (xintercept = 2.7, linetype = "dashed", color="grey") + 
    geom_hline (yintercept = 2.7, linetype = "dashed", color="dark grey") + #not a clear threshold 
      geom_abline (linetype = "longdash", color="black") + #not a clear threshold 
  # stat_poly_line(color = "black", alpha = 0.5) + #overall best fit
  #stat_poly_eq(use_label(c( "R2"))) + #remove hashtag to see R^2 of log500 split
   # geom_smooth( aes(abs(Log_Observed_Normalized_Respiration_Rate ), #above/below threshold best fit
    #                  abs(Log_Predicted_Normalized_Respiration_Rate_Nov2023),
     #                 color =  ds_split), method = "lm", alpha = 0.2) +
  
  xlab("log10(Observed rates+1)") + 
  ylab("log10(Predicted rates+1)") + 
  lims(y = c(0,4), x = c(0,4)) + 
  theme_classic() +
   theme(text = element_text(size = 16))
```

![](ICON_fig2_Rversion_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
combods_date %>% ggplot(aes(abs(Log_Observed_Normalized_Respiration_Rate),
                      abs(Log_Predicted_Normalized_Respiration_Rate_Jul2022), color = Year ))+ 
#to see R^2 for above/below thresholds, remove color grouping above and use stat poly eq fx (below)
  geom_point( pch = 21, size = 3) + 
    geom_vline (xintercept = 2.7, linetype = "dashed", color="grey") + 
    geom_hline (yintercept = 2.7, linetype = "dashed", color="dark grey") + #not a clear threshold 
      geom_abline (linetype = "longdash", color="black") + #not a clear threshold 
  # stat_poly_line(color = "black", alpha = 0.5) + #overall best fit
  #stat_poly_eq(use_label(c( "R2"))) + #remove hashtag to see R^2 of log500 split
  # scale_color_gradientn(colours = rev(rainbow(12))) +

   # geom_smooth( aes(abs(Log_Observed_Normalized_Respiration_Rate ), #above/below threshold best fit
    #                  abs(Log_Predicted_Normalized_Respiration_Rate_Nov2023),
     #                 color =  ds_split), method = "lm", alpha = 0.2) +
  
  xlab("log10(Observed rates+1)") + 
  ylab("log10(Predicted rates+1)") + 
  lims(y = c(0,4), x = c(0,4)) + 
  theme_classic() +
   theme(text = element_text(size = 16))
```

![](ICON_fig2_Rversion_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

- checking classifications

``` r
#to check that classifications are correct
combods_date %>% ggplot(aes(abs(Log_Observed_Normalized_Respiration_Rate),
                      abs(Log_Predicted_Normalized_Respiration_Rate_Nov2023),  color = ds_quadrant23 ))+ 
#to see R^2 for above/below thresholds, remove color grouping above and use stat poly eq fx (below)
  geom_point( pch = 21) + 
    geom_vline (xintercept = 2.7, linetype = "dashed", color="grey") + 
    geom_hline (yintercept = 2.7, linetype = "dashed", color="dark grey") + #not a clear threshold 
      geom_abline (linetype = "longdash", color="black") + #not a clear threshold 
  # stat_poly_line(color = "black", alpha = 0.5) + #overall best fit
  #stat_poly_eq(use_label(c( "R2"))) + #remove hashtag to see R^2 of log500 split
   #scale_color_gradientn(colours = rev(rainbow(12))) +

   # geom_smooth( aes(abs(Log_Observed_Normalized_Respiration_Rate ), #above/below threshold best fit
    #                  abs(Log_Predicted_Normalized_Respiration_Rate_Nov2023),
     #                 color =  ds_split), method = "lm", alpha = 0.2) +
  
  xlab("log10(Observed rates+1)") + 
  ylab("log10(Predicted rates+1)") + 
  lims(y = c(0,4), x = c(0,4)) + 
  theme_classic() +
   theme(text = element_text(size = 16))
```

![](ICON_fig2_Rversion_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#making sure classification is correct so we can create map for spatial patterns
  combods_date %>% ggplot(aes(abs(Log_Observed_Normalized_Respiration_Rate),
                      abs(Log_Predicted_Normalized_Respiration_Rate_Nov2023),  color = ds_quadrant23 ))+ 
#to see R^2 for above/below thresholds, remove color grouping above and use stat poly eq fx (below)
  geom_point( pch = 21) + 
    geom_vline (xintercept = 2.7, linetype = "dashed", color="grey") + 
    geom_hline (yintercept = 2.7, linetype = "dashed", color="dark grey") + #not a clear threshold 
      geom_abline (linetype = "longdash", color="black") + #not a clear threshold 
  # stat_poly_line(color = "black", alpha = 0.5) + #overall best fit
  #stat_poly_eq(use_label(c( "R2"))) + #remove hashtag to see R^2 of log500 split
   #scale_color_gradientn(colours = rev(rainbow(12))) +

   # geom_smooth( aes(abs(Log_Observed_Normalized_Respiration_Rate ), #above/below threshold best fit
    #                  abs(Log_Predicted_Normalized_Respiration_Rate_Nov2023),
     #                 color =  ds_split), method = "lm", alpha = 0.2) +
  
  xlab("log10(Observed rates+1)") + 
  ylab("log10(Predicted rates+1)") + 
  lims(y = c(0,4), x = c(0,4)) + 
  theme_classic() +
   theme(text = element_text(size = 16))
```

![](ICON_fig2_Rversion_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
combods_date %>% ggplot(aes(abs(Log_Observed_Normalized_Respiration_Rate),
                      abs(Log_Predicted_Normalized_Respiration_Rate_Jul2022),  color = ds_quadrant22 ))+ 
#to see R^2 for above/below thresholds, remove color grouping above and use stat poly eq fx (below)
  geom_point( pch = 21) + 
    geom_vline (xintercept = 2.7, linetype = "dashed", color="grey") + 
    geom_hline (yintercept = 2.7, linetype = "dashed", color="dark grey") + #not a clear threshold 
      geom_abline (linetype = "longdash", color="black") + #not a clear threshold 
  # stat_poly_line(color = "black", alpha = 0.5) + #overall best fit
  #stat_poly_eq(use_label(c( "R2"))) + #remove hashtag to see R^2 of log500 split
  # scale_color_gradientn(colours = rev(rainbow(12))) +

   # geom_smooth( aes(abs(Log_Observed_Normalized_Respiration_Rate ), #above/below threshold best fit
                     # abs(Log_Predicted_Normalized_Respiration_Rate),
                     # color =  ds_split), method = "lm", alpha = 0.2) +
  
  xlab("log10(Observed rates+1)") + 
  ylab("log10(Predicted rates+1)") + 
  lims(y = c(0,4), x = c(0,4)) + 
  theme_classic() +
   theme(text = element_text(size = 16))
```

![](ICON_fig2_Rversion_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

\#map to see miss-classifications spatially

``` r
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

outliers2023 <- 
ggplot(usa) +
  geom_sf(color = "#2b2b2b", fill = "lightgrey", size=0.125, alpha = 0.15) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
   geom_spatial_point(data=combods, aes(x=Longitude, y=Latitude, color = ds_quadrant23), size = 3) + 
               scale_color_manual(values=c(   "#B9D3EE", "maroon", "darkgrey", "#CDBA96" ) )+
     ylab("") + 
  xlab("Longitude") + 
    theme_minimal() +
   labs(title="2023 iteration")
outliers2023
```

    ## Assuming `crs = 4326` in stat_spatial_identity()

![](ICON_fig2_Rversion_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
outliers2022 <- 
ggplot(usa) +
  geom_sf(color = "#2b2b2b", fill = "lightgrey", size=0.125, alpha = 0.15) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
   geom_spatial_point(data=combods, aes(x=Longitude, y=Latitude, color = ds_quadrant22), size = 3) + 
               scale_color_manual(values=c(   "#B9D3EE", "maroon", "darkgrey", "#CDBA96" ) )+
     ylab("Latitude") + 
  xlab("Longitude") + 
    theme_minimal() + 
  theme(legend.position = 0)+
     labs(title="2022 iteration")



misclassmap <- outliers2022 + outliers2023
misclassmap
```

    ## Assuming `crs = 4326` in stat_spatial_identity()
    ## Assuming `crs = 4326` in stat_spatial_identity()

![](ICON_fig2_Rversion_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
ggsave(filename = "Scripts/misclassmap.tiff", plot = misclassmap, width = 10, height = 5, dpi = 300)   
```

    ## Assuming `crs = 4326` in stat_spatial_identity()
    ## Assuming `crs = 4326` in stat_spatial_identity()
