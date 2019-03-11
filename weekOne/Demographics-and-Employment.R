##Demographics and Employment in the United States
##load the dataset from CPSData.csv
CPS = read.csv("CPSData.csv")
## view the dataset with the summary() and str() commands
summary(CPS)
str(CPS)
##How many interviewees are in the dataset?
nrow(CPS)
#Among interviewees with a value reported for the Industry variable, what is the most common industry of employment?
sort(table(CPS$Industry), decreasing=TRUE)[1:1]
##Which state has the fewest interviewees?
sort(table(CPS$Region), increasing = TRUE)[1:1]
##Which state has the largst number of interviewees
sort(table(CPS$Region), decreasing = TRUE)[1:1]
##What proportion of interviewees are citizens of the United States?
table(CPS$Citizenship) #123712/131302=0.942
##For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(CPS$Race, CPS$Hispanic)
#Which variables have at least one interviewee with a missing (NA) value?
summary(CPS)
## breakdown of whether Married is missing based on the reported value of the Region variable
table(CPS$Region, is.na(CPS$Married))
##looking for more relationships
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
##How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)?
##How many states had all interviewees living in a metropolitan area? (treat the District of Columbia as a state).
table(CPS$State, is.na(CPS$MetroAreaCode))
##Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))
##Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
##Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

## Read the two dictionaries  MetroAreaMap and CountryMap into data frames
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
##How many observations are there in MetroAreaMap
nrow(MetroAreaMap)
##How many observations are there in CountryMap
nrow(CountryMap)

## connect the field MetroAreaCode from the CPS data frame with the field Code in MetroAreaMap
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
##What is the name of the variable that was added to the data frame
summary(CPS)
##Which of the following metropolitan areas has the largest number of interviewees?
table(CPS$MetroArea)
##Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? 
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
##determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
##sorted proportion of interviewees from each metropolitan area who have not received a high school diploma
##determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

##merge the CountryMap df
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

##What is the name of the variable added to the CPS data frame by this merge operation?
##How many interviewees have a missing value for the new country of birth variable?
summary(CPS)

##Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))

##What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States?
tapply(CPS$Country != "United States", CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA", mean, na.rm=TRUE)

##Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India?
sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))
## "..." in Brazil?
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))
## "..." in Somalia?
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))
