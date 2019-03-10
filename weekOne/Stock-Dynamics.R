setwd("C:/Users/User/Desktop/Courses/15.071x-The-Analytics-Edge/weekOne/datasets/")

#read required data 
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

#Convert "date" variable from Factor to a "Date" object 
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#How many observations are there in each data set?
nrow(IBM)
#What is the earliest year in our datasets?
min(IBM$Date)
#What is the latest year in our datasets?
max(IBM$Date)
#What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice)
#What is the minimum stock price of General Electric (GE) over this time period?
min(GE$StockPrice)
#What is the maximum stock price of Coca-Cola over this time period?
max(CocaCola$StockPrice)
#What is the median stock price of Boeing over this time period?
median(Boeing$StockPrice)
#What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)

#Visualizing Stock Dynamics
#Plot Coca-Cola with a line to visualize trends in stock prices during the time period.
plot(CocaCola$Date, CocaCola$StockPrice, type="l", xlab = "Date", ylab = "Stock Price", col="red")
#Coca-Cola has its highest stock price in this time period around the year 1973
#Coca-Cola has its lowest stock price in this time perioud around the year 1980

#Add a line for P&G, give color blue to differentiate the lines 
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")

##Using this plot, answer the following:
#In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more? 
abline(v=as.Date(c("2000-03-01")), lwd=2)

##look at how the stock prices changed from 1995-2005 for all five companies
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
##use the lines function to add in the other four companies
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col = "blue")
lines(GE$Date[301:432], Boeing$StockPrice[301:432], col = "green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = "purple")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = "orange")
##create legend to easily differentiate between lines
legend('topleft', legend =c("Coca-Cola", "Boeing", "GE", "IBM", "ProcterGamble"), col =c('red', 'blue', 'green', 'purple', 'orange'), lty=1, bty='n')

## see if stocks tend to be higher or lower during certain months
tapply(IBM$StockPrice, months(IBM$Date), mean)
## IBM, compare the monthly averages to the overall average stock price. In which months has IBM historically had a higher stock price
tapply(IBM$StockPrice, months(IBM$Date), mean ) > mean(IBM$StockPrice)

##Repeat the tapply function from the previous problem for each of the other four companies to answer the remaining questions
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)

##General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean) == max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)) 
tapply(GE$StockPrice, months(GE$Date), mean) == max(tapply(GE$StockPrice, months(GE$Date), mean))

##the months of December and January, every company's average stock is higher in one month. In which month are the stock prices lower?
tapply(IBM$StockPrice, months(IBM$Date), mean)


