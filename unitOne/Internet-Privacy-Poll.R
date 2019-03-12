##read in poll data
poll = read.csv("AnonymityPoll.csv")
##How many people participated in the poll?
nrow(poll)

##breakdown of the number of people with smartphones
table(poll$Smartphone)
summary(poll$Smartphone)

##create a table of the variables "Sex" and "Region"
table(poll$Sex, poll$Region)
##which are states in the Midwest census region
MidwestInterviewees = subset(poll, Region == "Midwest")
table(MidwestInterviewees$State)
##Which was the state in the South census region with the largest number of interviewees?
SouthInterviewees = subset(poll, Region == "South")
table(SouthInterviewees$State) 
##table of internet use by smartphone use
table(poll$Internet.Use, poll$Smartphone) 
##How many interviewees have a missing value for their Internet use?
##How many interviewees have a missing value for their smartphone use?
summary(poll)
##create data frame limited to interviewees who reported Internet use or who reported smartphone use
limited = subset(poll, Internet.Use == 1 | Smartphone == 1)

##Which variables have missing values in the limited data frame? 
summary(limited)
##What is the average number of pieces of personal information on the Internet, according to the Info.On.Internet variable?
mean(limited$Info.On.Internet)
##How many interviewees reported a value of 0 for Info.On.Internet?
##How many interviewees reported the maximum value of 11 for Info.On.Internet?
table(limited$Info.On.Internet)

##proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet
summary(limited$Worry.About.Info)
##What proportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet?
summary(limited$Anonymity.Possible)
##What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the Internet?
summary(limited$Tried.Masking.Identity)
##What proportion of interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective?
summary(limited$Privacy.Laws.Effective)

##Build a histogram of the age of interviewees. What is the best represented age group in the population?
hist(limited$Age)

##What is the largest number of interviewees that have exactly the same value in their Age AND the same value in their Info.On.Internet?
max(table(limited$Age, limited$Info.On.Internet)) 

##Experimenting with the command jitter(c(1, 2, 3)), what appears to be the functionality of the jitter command?
##jitter adds or subtracts a small amount of random noise to the values passed to it, and two runs will yield different results 
jitter(c(1, 2, 3))

##plot Age against Info.On.Internet with jitter
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

##What is the average Info.On.Internet value for smartphone users?
##What is the average Info.On.Internet value for non-smartphone users?
tapply(limited$Info.On.Internet, limited$Smartphone, summary)

##What proportion of smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
##What proportion of non-smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)


