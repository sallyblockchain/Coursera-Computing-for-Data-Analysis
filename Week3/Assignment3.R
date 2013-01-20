# Part 1
outcome <- read.csv(file="/.../data/outcome-of-care-measures.csv", colClasses = "character")
# "..." is the directory in your computer; masked here for privacy
head(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], xlab="30-day Dearth Rate", main="Heart Attack 30-day Death Rate")
library(googleVis)
gvt = gvisTable(outcome, options = list(showRowNumber = T, height = 800,width=1200))
plot(gvt)
# Part 2
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])
par(mfrow=c(3, 1))
hist(outcome[, 11], xlab="30-day Dearth Rate", main="Heart Attack")
hist(outcome[, 17], xlab="30-day Dearth Rate", main="Heart Failure")
hist(outcome[, 23], xlab="30-day Dearth Rate", main="Pneumonia")
m1 = median(outcome[, 11], na.rm = T)
m2 = median(outcome[, 17], na.rm = T)
m3 = median(outcome[, 23], na.rm = T)
par(mfrow=c(1, 3))
hist(outcome[, 11], xlab="30-day Dearth Rate", main=substitute(bar(X) == k, list(k = mean(outcome[, 11], na.rm = T))))
abline(v = m1, col = 2)
hist(outcome[, 17], xlab="30-day Dearth Rate", main="Heart Failure")
abline(v = m2, col = 2)
hist(outcome[, 23], xlab="30-day Dearth Rate", main="Pneumonia")
abline(v = m3, col = 2)
# Part 3
# outcome2 <- outcome[(table(outcome$State)>=20)[outcome$State],]
table(outcome$State) < 20
table(outcome$State)[table(outcome$State) < 20]
exclu = c("AK","DC","DE","GU","HI","RI","VI","VT")
outcome2 <- outcome[!outcome$State %in% exclu, ]
outcome2$State
table(outcome2$State)
table(outcome$State)
death <- outcome2[, 11]
state <- outcome2$State
boxplot(death ~ state, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State")
# Challenge!
by.m<-reorder(state, death, median, na.rm=T)
boxplot(death ~ by.m, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State", las=2, cex.axix=0.7, xaxt="n")
axis(1, by.m, paste0(by.m,"(",table(outcome2$State)[outcome2$State],")"), las=2, cex.axis=0.7)
# Part 4
hospital <- read.csv(file="/.../data/hospital-data.csv", colClasses = "character")
# "..." is the directory in your computer; masked here for privacy
head(hospital)
outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)
library(lattice)
xyplot(death ~ npatient | owner, xlab="Number of Patients Seen", ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by Ownership",panel = function(x, y, ...){
panel.xyplot(x, y, ...)
panel.lmline(x, y, lwd = 2)
#fit <- lm(y ~ x)
#panel.abline(fit, lwd = 2)
})
#######---------------------------------------
source("best.R")
best("TX", "heart attack") 
best("MD", "heart attack")
best("TX", "heart failure")
best("MD", "pneumonia")
source("http://spark-public.s3.amazonaws.com/compdata/scripts/submitscript.R")
submit()
source("rankhospital.R") 
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("TX", "heart attack")
source("rankall.R")
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
rankall("heart failure", 10)