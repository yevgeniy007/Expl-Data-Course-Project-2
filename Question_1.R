# Course project 2
# Research quesion 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# the graph is built using the base plotting system 

library(dplyr)
library(base)
library(grDevices)


# Read the data

datafileNEI<-"summarySCC_PM25.rds"
	myNEI<-readRDS(datafileNEI)

datafileSCC<-"Source_Classification_Code.rds"
	mySCC<-readRDS(datafileSCC)

# The key variables to use in this analysis are Year (1999, 2002, 2005 and 2008) and Emissions
# A quich review of the data before constructing charts suggests:

# A. considerable number of observations
# >tapply(myNEI$Emissions, myNEI$year, sum)
#    1999    2002    2005    2008 
# 7332967 5635780 5454703 3464206 

# B. High spread of data, with high outliers, probably reflecting variability 
# of observations and heterogeneity of sources 


# Let's create 4 dataframes:
myNEI1999<-filter(myNEI, year == 1999 & Emissions)
	myNEI1999<-select(myNEI1999, Emissions)
myNEI2002<-filter(myNEI, year == 2002 & Emissions)
	myNEI2002<-select(myNEI2002, Emissions)
myNEI2005<-filter(myNEI, year == 2005 & Emissions)
	myNEI2005<-select(myNEI2005, Emissions)
myNEI2008<-filter(myNEI, year == 2008 & Emissions) 
	myNEI2008<-select(myNEI2008, Emissions)


# summary(myNEI1999)			# summary(myNEI2002)
#      year        Emissions       	#     year        Emissions 
# Min.   :1999   Min.   :    0.00  	# Min.   :2002   Min.   :     0.0
# 1st Qu.:1999   1st Qu.:    0.01  	# 1st Qu.:2002   1st Qu.:     0.0
# Median :1999   Median :    0.04  	# Median :2002   Median :     0.0
# Mean   :1999   Mean   :    6.62  	# Mean   :2002   Mean   :     3.3
# 3rd Qu.:1999   3rd Qu.:    0.26  	# 3rd Qu.:2002   3rd Qu.:     0.1  
# Max.   :1999   Max.   :66696.32  	# Max.   :2002   Max.   :646952.0 

# summary(myNEI2005)			# summary(myNEI2008)
#      year        Emissions       	#     year        Emissions
# Min.   :2005   Min.   :    0.00  	# Min.   :2008   Min.   :    0.000 
# 1st Qu.:2005   1st Qu.:    0.00	# 1st Qu.:2008   1st Qu.:    0.000  
# Median :2005   Median :    0.01	# Median :2008   Median :    0.005
# Mean   :2005   Mean   :    3.18	# Mean   :2008   Mean   :    1.753 
# 3rd Qu.:2005   3rd Qu.:    0.07	# 3rd Qu.:2008   3rd Qu.:    0.062  
# Max.   :2005   Max.   :58896.10	# Max.   :2008   Max.   :20799.70

# The above tables suggests a decline of the mean emissions which we will show in graph (boxplot)
# Bacause of the high variability we will use log10 of the emissions 


png(filename = "Chart1.png", width = 960, height = 960, units = "px")

# Two sets of boxplote will be presentedm with the emissions on log scale
par(mfrow=c(2,2))

# Switch off warning messages, that are caused by log transfirmation of 0 values 
options(warn=-1)

# Both boxplots are presented with the same y-axis limits 

# Boxplot 1 - with dots showm 

boxplot(log10(myNEI1999$Emissions), 
	log10(myNEI2002$Emissions), 	
	log10(myNEI2005$Emissions), 
	log10(myNEI2008$Emissions), 
		xlab="year", 
		ylab="Log10 (Emissions)", 
		boxwex=0.95, 
			ylim=c(-15,5),
			names=c("1999", "2002", "2005", "2008"), 
			main="Total Emissions (Log 10)")

# boxplot 2 - with more clear graph presentation of the means 

boxplot(log10(myNEI1999$Emissions), 
	log10(myNEI2002$Emissions), 
	log10(myNEI2005$Emissions), 
	log10(myNEI2008$Emissions), 
	xlab="year", 
	ylab="Log10 (Emissions)", 
	boxwex=0.95, 
		ylim=c(-15,5),
		names=c("1999", "2002", "2005", "2008"), 
		main="Total Emissions (Log 10)", outline=FALSE, notch=TRUE)


# boxplot 3 - the mean values 
barplot(c(mean(myNEI1999$Emissions), 
		mean(myNEI2002$Emissions), 
		mean(myNEI2005$Emissions), 
		mean(myNEI2008$Emissions)),  
			xlab="year",  
				ylab="Emissions", 
					names=c("1999", "2002", "2005", "2008"),  
					main="Total Emissions (means)", col="red")


# boxplot 4 - the medianvalues 
barplot(c(median(myNEI1999$Emissions), 
		median(myNEI2002$Emissions), 
		median(myNEI2005$Emissions), 
		median(myNEI2008$Emissions)),  
			xlab="year",  
			ylab="Emissions", 
				names=c("1999", "2002", "2005", "2008"),  
					main="Total Emissions (medians)", col="blue" )


# Resore warning messages 

options(warn=0)
dev.off()

# Answer to question 1:

# a. Total emissions from PM2.5 in the US have decreased in the from 1999 to 2008
# b. however the mean annual values may be affected by the wide spread of data due to variability and heterogeneity
# c. The bar plot shows a decreas of mean values 
# d. The bar plot for median values suggest skewed distributions of emission values 
#    which can be further explored by looking at different sources of emissions 
     





