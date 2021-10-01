
library(tidyverse)
library(stringr)
library (readr)


batch_df <- read_csv('./Project2/bachelorette.csv' )
batch_df
 
 
sdf <- read_csv('./Project2/bad-drivers.csv' )
df
 

choropleth r
data.ny.gov
county.regions access




The nyc.gov website has both the hopitalization and mortality rates by county and zip code over time.  It would be interesting to compare the relationship between these two data set.  How it hospitalization and death rates differ by county in NYC?
df <- read_csv('https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/by-boro.csv')
df

I access a dataset from Data.gov about recipients of TAP (Tuition Assistant Program)


The data set contains information on the following:
    
    Academic year received
College code
Level of study
College Name
Sector Type
Sector Group
Headcount of TAP recipients
From this dataset we can get transform and analyze to see trends on recipients by academic year, by sector type. We can do some calculation and get the percentage of students receiving TAP by sector type or academic year. This will be better represented in graphs.


    
