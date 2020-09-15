---
title: "Health and Economic Damages of Storm Events"
output: 
  html_document:
    keep_md: true
---

By: Armin Najarpour Foroushani

## Synopsis

Severe weather events and storms yield both public health problems and economic damages in the society. They can cause fatalities, injuries, property, and crop damages. Preventing such outcomes is a key concern.

In this project we explore the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database includes characteristics of storms and weather events in the United States from the year 1950 till November 2011. 

Here, we focus on the estimates of fatalities, injuries, property damage, and crop damages corresponding to each type of weather event. Specifically we calculate the total Health effect (i.e. fatalities and injuries) and the economical effects (i.e. property damage and crop damages) to determine which event impacts the most.

## Data Processing

In this section, we first load and preprocess the data and then calculate the amount that each weather type impacts.

### Loading the data

```r
repdata_data_StormData<-read.csv("repdata_data_StormData.csv",header=TRUE)
cat("Dataset dimensions: ",dim(repdata_data_StormData))
```

```
## Dataset dimensions:  902297 37
```

```r
cat("Dataset columns: ",names(repdata_data_StormData))
```

```
## Dataset columns:  STATE__ BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
```

### Preprocessing and preparing the data for plotting

```r
#Fatal events
FATALEVT <- as.data.frame.table(tapply(repdata_data_StormData$FATALITIES,list(repdata_data_StormData$EVTYPE),sum,na.rm=TRUE))
names(FATALEVT)<-c("EVTYPE","FATALITIES")

#Injury events
INJREVT <- as.data.frame.table(tapply(repdata_data_StormData$INJURIES,list(repdata_data_StormData$EVTYPE),sum,na.rm=TRUE))
names(INJREVT)<-c("EVTYPE","INJURIES")

#A new dataframe of economical variables
Economic_data <- repdata_data_StormData[,c("EVTYPE","PROPDMG",
                                         "PROPDMGEXP","CROPDMG","CROPDMGEXP")]

#Check the levels in each variable 
levels(Economic_data$PROPDMGEXP)
```

```
## NULL
```

```r
levels(Economic_data$CROPDMGEXP)
```

```
## NULL
```

```r
#Replace power symbols with numeric values
library(plyr)
Economic_data$PROPDMGEXP<-revalue(Economic_data$PROPDMGEXP, c("-" = "1","?" = "1","+" = "1","B" = "1000000000","h" = "100", "H" = "100","K" = "1000", "m" = "1000000","M" = "1000000"))

Economic_data$PROPDMGEXP[Economic_data$PROPDMGEXP==""]<-"1"

Economic_data$CROPDMGEXP<-revalue(Economic_data$CROPDMGEXP, c("?" = "1","B" = "1000000000","k" = "1000","K" = "1000","m" = "1000000","M" = "1000000"))

Economic_data$CROPDMGEXP[Economic_data$CROPDMGEXP==""]<-"1"

#Convert characters to numeric values
Economic_data$PROPDMGEXP <- as.numeric(Economic_data$PROPDMGEXP)

Economic_data$CROPDMGEXP <- as.numeric(Economic_data$CROPDMGEXP)

#Calculate the damage values and save them in new columns
Economic_data$PROPERTYDAMAGE <- Economic_data$PROPDMGEXP*Economic_data$PROPDMG

Economic_data$CROPDAMAGE <- Economic_data$CROPDMGEXP*Economic_data$CROPDMG

#Create Property damage and crop damage dataset

ECONOEVT_PROP <- as.data.frame.table(tapply(Economic_data$PROPERTYDAMAGE,list(Economic_data$EVTYPE),sum,na.rm=TRUE))
names(ECONOEVT_PROP)<-c("EVTYPE","PROPERTYDAMAGE")
head(ECONOEVT_PROP)
```

```
##                  EVTYPE PROPERTYDAMAGE
## 1    HIGH SURF ADVISORY         200000
## 2         COASTAL FLOOD              0
## 3           FLASH FLOOD          50000
## 4             LIGHTNING              0
## 5             TSTM WIND        8100000
## 6       TSTM WIND (G45)           8000
```

```r
ECONOEVT_CROP <- as.data.frame.table(tapply(Economic_data$CROPDAMAGE,list(Economic_data$EVTYPE),sum,na.rm=TRUE))
names(ECONOEVT_CROP)<-c("EVTYPE","CROPDAMAGE")
head(ECONOEVT_CROP)
```

```
##                  EVTYPE CROPDAMAGE
## 1    HIGH SURF ADVISORY          0
## 2         COASTAL FLOOD          0
## 3           FLASH FLOOD          0
## 4             LIGHTNING          0
## 5             TSTM WIND          0
## 6       TSTM WIND (G45)          0
```


## Results

In this section, we present economical and health effects of different storm types.

### The most harmful storm events for population health in the United States

```r
#Barplot of the 10 most harmful fatal events
l <- length(FATALEVT$FATALITIES)
fatal_ind_sort <- sort(FATALEVT$FATALITIES,index.return=TRUE)$ix
fatal_highest <- FATALEVT[fatal_ind_sort[(l-9):l],]
barplot(fatal_highest$FATALITIES,names.arg = fatal_highest$EVTYPE,main="10 Most Harmful Fatal Events", ylab="Fatality",las=2)
```

![](Explore_NOAA_files/figure-html/High_Fatality-1.png)<!-- -->

```r
cat("Event with the highest fatalities: ",as.character(FATALEVT$EVTYPE[which(FATALEVT$FATALITIES==max(FATALEVT$FATALITIES))]))
```

```
## Event with the highest fatalities:  TORNADO
```

```r
#Barplot of the 10 most harmful injury events
l <- length(INJREVT$INJURIES)
injury_ind_sort <- sort(INJREVT$INJURIES,index.return=TRUE)$ix
injury_highest <- INJREVT[injury_ind_sort[(l-9):l],]
barplot(injury_highest$INJURIES,names.arg = injury_highest$EVTYPE,main="10 Most Harmful Injury Events", ylab="Injury",las=2)
```

![](Explore_NOAA_files/figure-html/High_Fatality-2.png)<!-- -->

```r
cat("Event with the highest injuries: ",as.character(INJREVT$EVTYPE[which(INJREVT$INJURIES==max(INJREVT$INJURIES))]))
```

```
## Event with the highest injuries:  TORNADO
```
### Storm events with the greatest economic consequences across the United States

```r
#Calculate the economical damage

#Property
cat("Event with the highest damage to properties: ",as.character(ECONOEVT_PROP$EVTYPE[which(ECONOEVT_PROP$PROPERTYDAMAGE==max(ECONOEVT_PROP$PROPERTYDAMAGE))]))
```

```
## Event with the highest damage to properties:  FLOOD
```

```r
#Crop
cat("Event with the highest damage to properties: ",as.character(ECONOEVT_CROP$EVTYPE[which(ECONOEVT_CROP$CROPDAMAGE==max(ECONOEVT_CROP$CROPDAMAGE))]))
```

```
## Event with the highest damage to properties:  DROUGHT
```

