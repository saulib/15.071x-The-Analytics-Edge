##readmvtWeek1.csv
mvt = read.csv("mvtWeek1.csv")
##view structure and summary of the data 
str(mvt)
summary(mvt)
##what is the maximum value of the variable "ID"
max(mvt$ID)


##How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
str(subset(mvt, Arrest == TRUE))

##How many observations have a LocationDescription value of ALLEY?
str(subset(mvt, LocationDescription == "ALLEY"))

mvt$LocationDescription
mvt$LocationDescription == ALLEY


##In many datasets, like this one, you have a date field. Unfortunately, R does not automatically recognize entries that look like dates. 
##We need to use a function in R to extract the date and time. 
##Take a look at the first entry of Date (remember to use square brackets when looking at a certain entry of a variable).
##In what format are the entries in the variable Date?
mvt$Date[1]

##let's convert these characters into a Date object in R
##This converts the variable "Date" into a Date object in R. Take a look at the variable DateConvert using the summary function.
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

##Now, let's extract the month and the day of the week, and add these variables to our data frame mvt.
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
##replace the old Date variable with DateConvert 
mvt$Date = DateConvert

##In which month did the fewest motor vehicle thefts occur?
min(table(mvt$Month))
##On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)
max(table(mvt$Weekday))

##histogram of the variable date
hist(mvt$Date, breaks=100)

##Create a boxplot of the variable "Date", sorted by the variable "Arrest" 
##Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period? 
boxplot(mvt$Date ~ mvt$Arrest)

##For what proportion of motor vehicle thefts in 2001 was an arrest made? 
table(mvt$Arrest, mvt$Year)
proportionArrest2001 = 2152/(2152+18517) 
proportionArrest2001

##For what proportion of motor vehicle thefts in 2007 was an arrest made? 
table(mvt$Arrest, mvt$Year)
proportionArrest2007 = 1212/(1212+13068)
proportionArrest2007

##For what proportion of motor vehicle thefts in 2012 was an arrest made? 
proportionArrest2012 = 550/(550+13542)
proportionArrest2012


##We want to find the top five locations where motor vehicle thefts occur
##If you create a table of the LocationDescription variable, it is unfortunately very hard to read since there are 78 different locations in the data set. 
##By using the sort function, we can view this same table, but sorted by the number of observations in each category
sort(table(mvt$LocationDescription))

##Create a subset of your data, only taking observations for which the theft happened in one of these five locations, and call this new data set "Top5"
#How many observations are in Top5?
Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" 
              | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")
str(Top5)

##running table(Top5$LocationDescription) will have a lot of unnecessary output. 
##To make our tables a bit nicer to read, we can refresh this factor variable. 
Top5$LocationDescription = factor(Top5$LocationDescription)
##One of the locations has a much higher arrest rate than the other locations. Which is it?
top5Table = table(Top5$LocationDescription,Top5$Arrest)
top5Matrix = as.matrix(top5Table)
totalPerLocation = top5Matrix[ ,1] + top5Matrix[,2]
##rate
rate = top5Matrix[,2]/totalPerLocation
rate
#On which day of the week do the most motor vehicle thefts at gas stations happen?
gasSta <- subset(Top5, Top5$LocationDescription=="GAS STATION")
which.max(table(gasSta$Weekday))
##On which day of the week do the fewest motor vehicle thefts in residential driveways happen
resDri <- subset(Top5, Top5$LocationDescription=="DRIVEWAY - RESIDENTIAL")
which.min(table(resDri$Weekday))




