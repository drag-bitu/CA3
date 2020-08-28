
# Reading the Northern ireland Crime data which was downloaded from official website:

ni_crime_data <- read.csv("D:/DS/CA2/police-recorded-crime-monthly-data.csv")

#first 6 records of data
head(ni_crime_data)

#Checking the data type of the file
class(ni_crime_data)

#Checking the structure of the file
str(ni_crime_data)


######################## data Cleaning #################

### shortening the names of variables for easy visualisation.

# checking the diffrent crimes in col crime.type
levels(ni_crime_data$Crime_Type)

## shortening the names of crime.type.
attach(ni_crime_data)
ni_crime_data$Crime_Type <- as.character(ni_crime_data$Crime_Type)
ni_crime_data$Crime_Type[Crime_Type == "All other theft offences"] <- "AOTF"
ni_crime_data$Crime_Type[Crime_Type == "Bicycle theft"] <- "BCTF"
ni_crime_data$Crime_Type[Crime_Type == "Criminal damage"] <- "CD"
ni_crime_data$Crime_Type[Crime_Type == "Miscellaneous crimes against society"] <- "MCAS"
ni_crime_data$Crime_Type[Crime_Type == "Possession of drugs"] <- "DRUG"
ni_crime_data$Crime_Type[Crime_Type == "Possession of weapons offences"] <- "WO"
ni_crime_data$Crime_Type[Crime_Type == "Public order offences"] <- "PUBO"
ni_crime_data$Crime_Type[Crime_Type == "Robbery"] <- "ROBY"
ni_crime_data$Crime_Type[Crime_Type == "Sexual offences"] <- "SEXO"
ni_crime_data$Crime_Type[Crime_Type == "Theft - burglary business & community"] <- "TBBC"
ni_crime_data$Crime_Type[Crime_Type == "Theft - burglary residential"] <- "TBR"
ni_crime_data$Crime_Type[Crime_Type == "Theft - domestic burglary"] <- "TDB"
ni_crime_data$Crime_Type[Crime_Type == "Theft - non-domestic burglary"] <- "TNDB"
ni_crime_data$Crime_Type[Crime_Type == "Theft - shoplifting"] <- "TSHOP"
ni_crime_data$Crime_Type[Crime_Type == "Theft - vehicle offences"] <- "TVO"
ni_crime_data$Crime_Type[Crime_Type == "Theft from the person"] <- "TFTP"
ni_crime_data$Crime_Type[Crime_Type == "Total police recorded crime"] <- "TPRC"
ni_crime_data$Crime_Type[Crime_Type == "Trafficking of drugs"] <- "TDRUG"
ni_crime_data$Crime_Type[Crime_Type == "Violence with injury (including homicide & death/serious injury by unlawful driving)"] <- "VI"
ni_crime_data$Crime_Type[Crime_Type == "Violence without injury (including harassment)"] <- "VWI"
ni_crime_data$Crime_Type <- as.factor(ni_crime_data$Crime_Type)
detach(ni_crime_data)

# to check the changes made in above command.
levels(ni_crime_data$Crime_Type)


#checking the names of datameasure.
levels(ni_crime_data$Data_Measure)

#Changing the names of data measure
ni_crime_data$Data_Measure <- as.character(ni_crime_data$Data_Measure)
ni_crime_data$Data_Measure[ni_crime_data$Data_Measure == "Police Recorded Crime"] <- "Recorded_Crimes"
ni_crime_data$Data_Measure[ni_crime_data$Data_Measure == "Police Recorded Crime Outcomes (number)"] <- "Dealt_Crimes"
ni_crime_data$Data_Measure[ni_crime_data$Data_Measure == "Police Recorded Crime Outcomes (rate %)"] <- "Dealt_Percentage"
ni_crime_data$Data_Measure <- as.factor(ni_crime_data$Data_Measure)


#checking the names of Policing_District.
levels(ni_crime_data$Policing_District)

#Shortening the names of Policing_District.
attach(ni_crime_data)
ni_crime_data$Policing_District <- as.character(ni_crime_data$Policing_District)
ni_crime_data$Policing_District[Policing_District == "Antrim & Newtownabbey"] <- "AN"
ni_crime_data$Policing_District[Policing_District == "Ards & North Down"] <- "AND"
ni_crime_data$Policing_District[Policing_District == "Armagh City Banbridge & Craigavon"] <- "ABC"
ni_crime_data$Policing_District[Policing_District == "Belfast City"] <- "BC"
ni_crime_data$Policing_District[Policing_District == "Causeway Coast & Glens"] <- "CG"
ni_crime_data$Policing_District[Policing_District == "Derry City & Strabane"] <- "DS"
ni_crime_data$Policing_District[Policing_District == "Fermanagh & Omagh"] <- "FO"
ni_crime_data$Policing_District[Policing_District == "Lisburn & Castlereagh City"] <- "LC"
ni_crime_data$Policing_District[Policing_District == "Mid & East Antrim"] <- "ME"
ni_crime_data$Policing_District[Policing_District == "Mid Ulster"] <- "MU"
ni_crime_data$Policing_District[Policing_District == "Newry Mourne & Down"] <- "ND"
ni_crime_data$Policing_District[Policing_District == "Northern Ireland"] <- "NI"
ni_crime_data$Policing_District <- as.factor(ni_crime_data$Policing_District)
detach(ni_crime_data)

##lets check the changes of names in data set
str(ni_crime_data)
levels('ni_crime_data$Policing_District', 'ni_crime_data$Crime_Type', 'ni_crime_data$Data_Measure' )

ni_crime_data$Count[ni_crime_data$Count == "0" ] <- NA
ni_crime_data$Count[ni_crime_data$Count == "/0" ] <-NA
ni_crime_data$Count[ni_crime_data$Count == "0.0" ] <-NA
#Counting the total number of `NA` values to see, how many null values was there in the entire dataset
sum(is.na(ni_crime_data$Count))

#Viewing the records with NA
na_records <- ni_crime_data[!complete.cases(ni_crime_data),]
na_records

#Using mice library to display NA values and its count 
library(mice)
md.pattern(ni_crime_data)

#Seems like, there are n other missing values except in column `Count`
library(VIM)
null_crimes <- aggr(ni_crime_data, prop = FALSE, numbers = TRUE)

#Creating a subset which has the data of `reported crimes` recorded by police 
recorded_crimes <- subset(ni_crime_data, ni_crime_data$Data_Measure == "Recorded_Crimes")
#Creating a subset which has the data of Crimes out of total crimes, dealt by the police 
dealt_crimes <- subset(ni_crime_data, ni_crime_data$Data_Measure == "Dealt_Crimes")
#Creating a subset which has the Percentage of Crimes out of total crimes, dealt by the police 
dealt_percentage <- subset(ni_crime_data, ni_crime_data$Data_Measure == "Dealt_Percentage")

#Storing the data in subset `Reported_Crimes` to `Total_Crimes`
total_crimes <- recorded_crimes

#Checking whether the values in the rows of subset `total_crimes` is identical to the rows in the subset `dealt_crimes`
total_crimes %in% dealt_crimes
#Got result that all rows in all columns matches , except for the last two columns `Data Measure` and `Count`
#Creating a new column `Dealt_Crimes` with counts from the subset `dealt_crimes`
total_crimes$Dealt_Crimes <- dealt_crimes$Count
#Checking whether the values in the rows of subset `total_crimes` is identical to the rows in the subset `dealt_percentage`
total_crimes %in% dealt_percentage
#Got result that all rows in all columns matches , except for the last two columns `Data Measure` and `Count`
#Creating a new column `Dealt_Percentage` with counts from the subset `dealt_percentage`
total_crimes$Dealt_Percentage <- dealt_percentage$Count

#Renaming the column name `Count` to `Recorded_Crimes`
names(total_crimes)[names(total_crimes) == "Count"] <- "Recorded_Crimes"
#Displaying the column names
colnames(total_crimes)

#As the data is now split based on the data measured type, the column `Data_Measure` can now be removed
total_crimes <- total_crimes[, -5]

#Counting no crimes reported
sum(is.na(total_crimes$Recorded_Crimes))

#There are 6585 records in the dataset which has no crimes recorded
#So removing thise entriies with no crimes
total_crimes <- total_crimes[complete.cases(total_crimes$Recorded_Crimes),]

#Counting the number of crimes which were not dealt 
sum(is.na(total_crimes$Dealt_Crimes))
#There are `5739` crimes which has no outcomes 

#Using mice library to display null values in dealt crime and dealt percentage
library(mice)
md.pattern(total_crimes)
#There are a total of `6584` crimes recorded with null values
#There are a total of `11924` crimes, which were not dealt
#There are a total of `12322` null values in the dealt percentage

#Assigning NA records with value `0`
total_crimes[is.na(total_crimes)] <- 0


# --------------------------------------------------------------------------------------
# Describing the data
# --------------------------------------------------------------------------------------
#The proportion of cases that have outcomes are already available in the dataset called `Dealt_Percentage`.

#Converting the column `Dealt_Percentage` from factor to numeric without loosing the decimal values
total_crimes$Dealt_Percentage <- as.numeric(paste(total_crimes$Dealt_Percentage))
#Rouding the decimal values of colum `Dealt_Percentage` and storing the runded values to a new column `Rounded_Rate`
total_crimes$Rounded_Rate <- round(total_crimes$Dealt_Percentage)
#Converting columns with numeric values to type numeric
total_crimes$Recorded_Crimes <- as.numeric(paste(total_crimes$Recorded_Crimes))
total_crimes$Dealt_Crimes <-as.numeric(paste(total_crimes$Dealt_Crimes))

#It is noted that, the `Rounded_Rate` has values above 100. 
#But as it is the percentage value, it should not be greater than hundred
#Checking the values of column `Rounded_Rate` with values above 100
length(total_crimes$Rounded_Rate[total_crimes$Rounded_Rate > 100])


#There are `2081` values in column `Rounded_Rate`, which has value greater than 100
#The percentage values are higher as the values of crimes recorded are less than the values of crimes delat with
#So removing those crimes from the dataset 
total_crimes$Rounded_Rate[total_crimes$Rounded_Rate > 100] <- NA
total_crimes <- total_crimes[complete.cases(total_crimes$Rounded_Rate),]


#Viewing the summary of the data 
summary(total_crimes)



#Creating a new column `Outcome_Level` which describes whether there are high, medium or low outcomes 
#The summary provides the Mean of the Rounded rate to be approximately 32
#So setting the rate percentage below 32 to be low
total_crimes$Outcome_Level[total_crimes$Rounded_Rate < 32] <- "Low" 
#The summary sjows the third quartlie to be 50. So setting the outcome level between 32 to 50 to be medium
total_crimes$Outcome_Level[total_crimes$Rounded_Rate >= 32 & total_crimes$Rounded_Rate <= 50] <- "Medium" 
#Setting the outcome_level above 50 to be high
total_crimes$Outcome_Level[total_crimes$Rounded_Rate > 50 ] <- "High"

#Creating a table for outcome_levels
table(total_crimes$Outcome_Level)
#Creating proportions of the table
prop.table(table(total_crimes$Recorded_Crimes, total_crimes$Calendar_Year))
#Creating a barplot of outcome level
barplot(height = prop.table(table(total_crimes$Outcome_Level)),
        main = "Outcome Level (Low, Medium, High)", 
        ylab = "Crime Frequency(in proportions)", 
        xlab = "Outcomes",
        col = "Brown")
#From the barplot, it is understood that, there are lot of crimes with a low outcome_level



#Looking at values of the crime which was not delat with the most
disp_variables <- c("Crime_Type", "Policing_District", "Calendar_Year", "Month")

#Values show that, the crimes that was most dealt was on the year 2005 and on month May
#The policing District that had the most crimes without outcomes was on `Northern Ireland`
#and the type of crime was `Total Police Recorded Crime`
total_crimes[which.max(total_crimes$Dealt_Crimes), disp_variables]

#Checking whether the total police recorded crime is the sum of all crimes of that month
Individual_calc <- sum(total_crimes$Recorded_Crimes[total_crimes$Calendar_Year == "2005" & total_crimes$Month == "May" & total_crimes$Policing_District == "NI"])
Individual_calc
Total_calc <- total_crimes$Recorded_Crimes[total_crimes$Crime_Type == "TPRC" & total_crimes$Calendar_Year == "2005" & total_crimes$Month == "May" & total_crimes$Policing_District == "NI"]
Total_calc
Individual_calc - Total_calc


#As the Total Police Recorded Crimes is an extra row with aggregate of recorded crimes
#removing that to get a detailed overview
TPRC <- subset(total_crimes, total_crimes$Crime_Type == "TPRC")
#Removing the type `Total Police Recorded Crime` from total_crimes
total_crimes <- total_crimes[!total_crimes$Crime_Type == "TPRC", ]


#As `Northern Ireland` is not a specific policing district, but represents the entire area, creating it as a new subset
NI <- subset(TPRC, TPRC$Policing_District == "NI")
#Removing the Policig District `Northern Ireland` from total_crimes
total_crimes <- total_crimes[!total_crimes$Policing_District == "NI", ]


#Values show that, the crimes which was most dealt was on the year 2002 and on month Aug
#The policing District that had the most crimes with outcomes was on `Belfast City`
#and the type of crime was `Violence without Injury`
total_crimes[which.max(total_crimes$Dealt_Crimes), disp_variables]

# --------------------------------------------------------------------------------------
# Data Analysis
# --------------------------------------------------------------------------------------
install.packages("ggplot2")
library(ggplot2)
library(viridis)
library(highcharter)
library(magrittr)
library(anytime)
library(rjson)
library(forecast)
library(data.table)

total_crimes <- as.data.table(total_crimes)

#Finding the crimes that are mostly recorded
disp1 = total_crimes[,.(sum = sum(Recorded_Crimes)),by = Crime_Type]
hchart(disp1, 'pie', hcaes(x = Crime_Type, y = sum, color = sum)) %>%
  hc_title(text = "Sum of Crime Type") %>%
  hc_add_theme(hc_theme_darkunica())

#Finding the crimes that are mostly dealt with
dealt1 = total_crimes[,.(sum = sum(Dealt_Crimes)),by = Crime_Type]
hchart(dealt1, 'pie', hcaes(x = Crime_Type, y = sum, color = sum)) %>%
  hc_title(text = "Sum of Dealt Crime Type") %>%
  hc_add_theme(hc_theme_darkunica())

#Finding the recorded crime variations based on the policing district
disp2 = total_crimes[, .(sum = sum(Recorded_Crimes)), by = .(Policing_District, Crime_Type)] %>%
  .[order(sum)]
hchart(disp2,'column', hcaes(x = Policing_District, y = sum, group = Crime_Type)) %>%
  hc_add_theme(hc_theme_flatdark()) %>%
  hc_plotOptions(column = list(stacking = 'normal')) %>%
  hc_legend(align = 'right', float = T)

#Finding the delat crime variations based on the policing district
dealt2 = total_crimes[, .(sum = sum(Dealt_Crimes)), by = .(Policing_District, Crime_Type)] %>%
  .[order(sum)]
hchart(dealt2,'column', hcaes(x = Policing_District, y = sum, group = Crime_Type)) %>%
  hc_add_theme(hc_theme_flatdark()) %>%
  hc_plotOptions(column = list(stacking = 'normal')) %>%
  hc_legend(align = 'right', float = T)


NI <- as.data.table(NI)
#Finding variations in the recorded crime count based on year
disp3 = NI[,.(sum = sum(Recorded_Crimes)), by = Calendar_Year][order(Calendar_Year)]
hchart(disp3, 'line', hcaes(Calendar_Year, sum)) %>%
  hc_add_theme(hc_theme_flatdark())

#Finding variations in the dealt crime count based on year
dealt3 = NI[,.(sum = sum(Dealt_Crimes)), by = Calendar_Year][order(Calendar_Year)]
hchart(dealt3, 'line', hcaes(Calendar_Year, sum)) %>%
  hc_add_theme(hc_theme_flatdark())

#Finding the recorded crime variation by year based on crime types
disp4 = total_crimes[,.(sum = sum(Recorded_Crimes)), by = .(Crime_Type, Calendar_Year)] %>%
  .[order(Crime_Type, Calendar_Year)]
hchart(disp4, "line",hcaes(Calendar_Year, sum, color = Crime_Type, group = Crime_Type)) %>%
  hc_yAxis(text = "sum") %>%
  hc_add_theme(hc_theme_flatdark())

#Finding the delat crime variation by year based on crime types
dealt4 = total_crimes[,.(sum = sum(Dealt_Crimes)), by = .(Crime_Type, Calendar_Year)] %>%
  .[order(Crime_Type, Calendar_Year)]
hchart(dealt4, "line",hcaes(Calendar_Year, sum, color = Crime_Type, group = Crime_Type)) %>%
  hc_yAxis(text = "sum") %>%
  hc_add_theme(hc_theme_flatdark())

#Finding the recorded cime variations based on months and year
disp5 = NI[, .(sum = sum(Recorded_Crimes)), by = .(Month, Calendar_Year)] %>%
  .[,date := paste0(Calendar_Year,'/',Month) %>% anydate ] %>%
  .[order(date, sum)]
disp5$Calendar_Year %<>% as.character() %<>% as.factor
disp5$Month %<>% as.factor
hchart(disp5, "line", hcaes(x = Month, y = sum, color = Calendar_Year, group = Calendar_Year)) %>%
  hc_add_theme(hc_theme_flatdark()) %>%
  hc_xAxis(categories = month.abb)

#Finding the dealt cime variations based on months and year
dealt5 = NI[, .(sum = sum(Dealt_Crimes)), by = .(Month, Calendar_Year)] %>%
  .[,date := paste0(Calendar_Year,'/',Month) %>% anydate ] %>%
  .[order(date, sum)]
dealt5$Calendar_Year %<>% as.character() %<>% as.factor
dealt5$Month %<>% as.factor
hchart(dealt5, "line", hcaes(x = Month, y = sum, color = Calendar_Year, group = Calendar_Year)) %>%
  hc_add_theme(hc_theme_flatdark()) %>%
  hc_xAxis(categories = month.abb)

NI <- as.data.frame(NI)

#Creating subset for crimes where the policing district is belfast city
Belfast_Crimes <- subset(total_crimes, total_crimes$Policing_District == "BC")
#Storing only crimes "VI, VWI & CD"
Belfast_Crimes <- Belfast_Crimes[Belfast_Crimes$Crime_Type == "VWI" | Belfast_Crimes$Crime_Type == "VI" | Belfast_Crimes$Crime_Type == "CD"]
Belfast_Crimes <- Belfast_Crimes[, -c(3, 7, 9)]

Belfast_VWI <- subset(Belfast_Crimes, Belfast_Crimes$Crime_Type == "VWI")
Belfast_VI <- subset(Belfast_Crimes, Belfast_Crimes$Crime_Type == "VI")
Belfast_CD <- subset(Belfast_Crimes, Belfast_Crimes$Crime_Type == "CD")

Belfast_Crm <- Belfast_CD
Belfast_Crm$VWI_Rate <- Belfast_VWI$Rounded_Rate
Belfast_Crm$VI_Rate <- Belfast_VI$Rounded_Rate

Belfast_Crm<- Belfast_Crm[, -c(3, 4, 5)]
colnames(Belfast_Crm)[3] <- "CD_Rate"




Belfast_Crm$Month <- match(Belfast_Crm$Month,month.abb)
Belfast_Crm <- as.data.frame(Belfast_Crm)


#As the dataset contains non-numeric values, the attributes containing numeric values are stored to a list using sapply
Belfast_numeric_attributes <- sapply(Belfast_Crm, is.numeric)
Belfast_numeric_attributes

#Creating a named list of summary
lapply(Belfast_Crm, summary)

#Creating a new dataset with summary values as columns and variables as rows 
Belfast_numeric_summary <-  do.call(rbind, lapply(Belfast_Crm, summary))
#Displaying `num_summary``
Belfast_numeric_summary

#Changing the outcome levals of VWI Rate
write.csv(Belfast_Crm,"Belfast_Crm.csv", row.names = FALSE)

#Testing correlation 
pairs(Belfast_Crm[Belfast_numeric_attributes])

#Plotting correlation between the dealt rate of Criminal Damage and Year
scatter.smooth(x = Belfast_Crm$Calendar_Year, 
               y = Belfast_Crm$CD_Rate, 
               main = "Year ~ CD",
               xlab = "Calendar Year",
               ylab = "Criminal Damage Rate")


#Plotting histograms to check for normality
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 4))
hist(Belfast_Crm$VWI_Rate, main = "Histogram for Violence Without Injury Rate", xlab = "")
hist(Belfast_Crm$VI_Rate, main = "Histogram for Violence With Injury Rate", xlab = "")
hist(Belfast_Crm$CD_Rate, main = "Histogram for Violence Criminal Damage", xlab = "")
hist(Belfast_Crm$Unemp_Rate, main = "Unemployment Rate", xlab = "")
# Reset par values
par(opar)


# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(Belfast_Crm$CD_Rate)
normality_test$p.value
#The p-value obtained is 0.02

# Formal test of normality for Calendar_Year
# provided through widely used Shapiro-Wilks test
normality_test1 <- shapiro.test(Belfast_Crm$Calendar_Year)
normality_test1$p.value

#Checking the normality of CD_Rate 
opar <- par(no.readonly = TRUE)
attach(Belfast_Crm)

boxplot(CD_Rate, 
        main = "Criminal Damage Rate", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(CD_Rate)$out)) # box plot for 'CD_Rate'
detach(Belfast_Crm)
#The boxplots suggest that there is one outlier in the data in the CD_Rate

#Removing the outlier from the dataset
Belfast_Crm <- subset(Belfast_Crm, Belfast_Crm$CD_Rate!=20)

#Checking whether outlier value exist again using the box plot
opar <- par(no.readonly = TRUE)
attach(Belfast_Crm)
boxplot(CD_Rate, 
        main = "Criminal Damage Rate", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(CD_Rate)$out)) # box plot for 'speed'
detach(Belfast_Crm)




# Skewness function to examine normality of data
library(e1071)
# density plot for 'CD_Rate'
plot(density(Belfast_Crm$CD_Rate), main = "Density Plot: CD_Rate", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 (e1071::skewness(Belfast_Crm$CD_Rate))))
# Lets fill in the area under the density plot in red
polygon(density(Belfast_Crm$CD_Rate), col = "red")
# The value falls between: -0.5 to 0.5, so it is symmetric

# using the corrplot() function
# Creating a matrix of high correlations
opar <- par(no.readonly = TRUE)
library(corrplot)
corrplot(corr = cor(Belfast_Crm, use = "na.or.complete"),tl.col = "Black", tl.cex = 0.9)

#Checking correlation between Calendar Year and CD
#The value is 0.5, which gives a positive correlation
cor(Belfast_Crm$Calendar_Year, Belfast_Crm$CD_Rate)
#Checking correlation between VI and CD
#The value is 0.2, which gives a medium positive correlation
cor(Belfast_Crm$VI_Rate, Belfast_Crm$CD_Rate)


#________PCA________

# Passing this numeric data (6 variables) into the prcomp() function
# and setting two arguments, center and scale, to be TRUE. 
pca <- prcomp(Belfast_Crm, center = TRUE, scale. = TRUE)
summary(pca)
#Maximum variations are shown in PC1 and in PC2. 
str(pca)
#Finding the (correlation or anticorrelation, etc) between the initial variables and the principal components 
pca$rotation

# eigenvalues measure the amount of variation retained by each principal component. 
# Eigenvalues are large for the first PCs and small for the subsequent PCs. 
# That is, the first PCs corresponds to the directions with the maximum amount of
# variation in the data set.
# We examine the eigenvalues to determine the number of principal components to be considered. 
# The eigenvalues and the proportion of variances (i.e., information) retained by the 
# principal components (PCs) can be extracted using the function get_eigenvalue() 
# from the factoextra package.
library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values

# An alternative method to determine the number of principal components is to look at a Scree Plot, 
# which is the plot of eigenvalues ordered from largest to the smallest. The number of component 
# is determined at the point, beyond which the remaining eigenvalues are all relatively small and of 
# comparable size
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))
# From the plot, we might want to stop at the forth principal component. 
# 93% of the information (variances) contained in the data are retained by the first five principal components.

# A simple method to extract the results, for variables, from a PCA output 
# is to use the function get_pca_var() from the factoextra package. 
# This function provides a list of matrices containing all the results 
# for the active variables (coordinates, correlation between variables and axes, squared cosine and contributions)

# The components of the get_pca_var() can be used in the plot of variables as follow:
pca_for_variables <- get_pca_var(pca)
pca_for_variables


#The corrplot() function is used to highlight 
# the most contributing variables for each dimension:
library("corrplot")
corrplot(pca_for_variables$cos2, is.corr = FALSE)
# Looks like "VWI_Rate", "VI_Rate" "Calendar_Year" and "Unemp_Rate"is contributing to Dim 1.


# A variable correlation plots shows the relationships between all variables. 
# Positively correlated variables are grouped together.
# Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
fviz_pca_var(pca, col.var = "black")

# The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates). 
# We can access to the cos2 as follows:
head(pca_for_variables$cos2, 6)

#Showing a bar plot of variables cos2 using the function fviz_cos2()
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca, choice = "var", axes = 1:2)


# A biplot is a type of plot that will allow you to visualise how 
# the samples relate to one another in our PCA (which samples are 
# similar and which are different) and will simultaneously reveal how 
# each variable contributes to each principal component.
# Colour by cos2 values: quality on the factor map
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE # Avoid text overlapping
)  


# Contribution of variables to each PC
# The larger the value of the contribution, the more the variable contributes to the component. 
head(pca_for_variables$contrib, 20)

# The most important (or, contributing) variables can be highlighted on the correlation plot as follows
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green"),
)


#Drawing a bar plot of variable contributions to PC1
# The red dashed line on the graphs indicate the expected 
# average contribution. For a given component, a variable with a contribution larger than this 
# cutoff could be considered as important in contributing to the component. 
fviz_contrib(pca, choice = "var", axes = 1)
#Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2)


library("ggpubr")
ggscatter(Belfast_Crm, x = "Calendar_Year", y = "CD_Rate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Calendar Year", ylab = "Criminal Damage Rate (%)")


spearman_res <-cor.test(Belfast_Crm$Calendar_Year, Belfast_Crm$CD_Rate,  method = "spearman")
spearman_res

