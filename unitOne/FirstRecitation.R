USDA = read.csv("USDA.csv")

HighSodium = subset(USDA, Sodium>10000)

##Creating Plots
##plot protein vs fat
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")

##histogram
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0, 100), breaks = 2000)

#boxplot for sugar
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels", ylab = "Sugar(g)")

##...................................................................##
##Adding Variables##

USDA$Sodium[1] > mean(USDA$Sodium, na.rm = TRUE)

USDA$Sodium[50] > mean(USDA$Sodium, na.rm = TRUE)

HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
str(HighSodium)
##add the variable vector HighSodium to the data frame
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
str(USDA)
##add variable vectors high protien, fat and carbs
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Sodium, na.rm = TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))
str(USDA)

########################
##table function
table(USDA$HighSodium)
##see high sodium and fat, 712
table(USDA$HighSodium, USDA$HighFat)
##iron by sodium and protien
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)


tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)
