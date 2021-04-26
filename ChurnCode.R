##Data Reading
churn <- read.csv("E:\\Data\\Sara\\FCIS\\Year 4\\Semester 2\\Big Data\\Project\\karan-churn.csv",
                  na.strings="")
summary(churn)

##Data Preprocessing: Replace Missing Values

churn$Day.Charge<- as.character(churn$Day.Charge)
temp.day <- churn$Day.Charge
#churn$Day.Charge[churn$Day.Charge == "?"] <- "0"
#med<-median(as.double(churn$Day.Charge))
#churn$Day.Charge[churn$Day.Charge == "?"] <- "30.1" ##median value = 30.11
temp.day[temp.day == "?"] <- 0
options(digits=4)
med<- median(as.double(temp.day))
churn$Day.Charge[churn$Day.Charge == "?"] <- med
churn$Day.Charge<- as.double(churn$Day.Charge)

churn$Eve.Mins<- as.character(churn$Eve.Mins)
temp.eve.mins <- churn$Eve.Mins
temp.eve.mins[temp.eve.mins == "?"] <- "0"
#churn$Eve.Mins[churn$Eve.Mins == "?"] <- "0"
options(digits=4)
#med<-median(as.double(churn$Eve.Mins))
med<-median(as.double(churn$Eve.Mins))
churn$Eve.Mins[churn$Eve.Mins == "?"] <- med ##median value = 199.9
churn$Eve.Mins<- as.double(churn$Eve.Mins)

churn$Eve.Calls<- as.character(churn$Eve.Calls)
temp.eve.calls <- churn$Eve.Calls
temp.eve.calls[temp.eve.calls == "?"] <- "0"
#churn$Eve.Calls[churn$Eve.Calls == "?"] <- "0"
#med<-median(as.integer(churn$Eve.Calls))
med<-median(as.integer(temp.eve.calls))
churn$Eve.Calls[churn$Eve.Calls == "?"] <- med ##median value = 95
churn$Eve.Calls<- as.integer(churn$Eve.Calls)

churn$Night.Charge<- as.character(churn$Night.Charge)
temp.night.charge<- churn$Night.Charge
temp.night.charge[temp.night.charge == "?"] <- "0"
#churn$Night.Charge[churn$Night.Charge == "?"] <- "0"
options(digits=4)
#med<-median(as.double(churn$Night.Charge))
med<-median(as.double(temp.night.charge))
churn$Night.Charge[churn$Night.Charge == "?"] <- med ##median value = 8.71
churn$Night.Charge<- as.double(churn$Night.Charge)

churn$Intl.Calls<- as.character(churn$Intl.Calls)
temp.intl.calls<- churn$Intl.Calls
temp.intl.calls[temp.intl.calls == "?"] <- "0"
#churn$Intl.Calls[churn$Intl.Calls == "?"] <- "0"
#med<-median(as.integer(churn$Intl.Calls))
med<-median(as.integer(temp.intl.calls))
churn$Intl.Calls[churn$Intl.Calls == "?"] <- med  ##median value = 3
churn$Intl.Calls<- as.integer(churn$Intl.Calls)

churn$Intl.Charge<- as.character(churn$Intl.Charge)
temp.intl.charge <- churn$Intl.Charge
temp.intl.charge[temp.intl.charge == "?"] <- "0"
#churn$Intl.Charge[churn$Intl.Charge == "?"] <- "0"
options(digits=4)
#med<-median(as.double(churn$Intl.Charge))
med<-median(as.double(temp.intl.charge))
churn$Intl.Charge[churn$Intl.Charge == "?"] <- med ##median value = 2.67
churn$Intl.Charge<- as.double(churn$Intl.Charge)

##Exporting the new data frame as a .csv file
write.csv(churn, "E:\\Data\\Rehab\\FCIS\\Year 4\\Semester 2\\Big Data\\Project\\new-karan-churn.csv")

churn <- read.csv("E:\\Data\\Rehab\\FCIS\\Year 4\\Semester 2\\Big Data\\Project\\new-karan-churn.csv", 
               na.strings="")
##Visualization
DayMinHist <- function()
{
  par(mfrow = c(3,1))
  hist(churn$Day.Mins, col = "lightblue", main = "Histogram of Day Minutes", xlab = "Day Mins", breaks = seq(0, 400, by = 10))
  plot(density(churn$Day.Mins), col = "red", lwd = 4, main = "Day Minutes Density")
  hist(churn$Day.Mins, col = "lightblue", probability = TRUE, xlab = "Day Mins", breaks = seq(0, 400, by = 10), main = "Histogram and Density of Day Minutes")
  lines(density(churn$Day.Mins) ,lty = "dotted", lwd = 4, col = "red")
}
DayMinHist()

EveMinHist <- function()
{
  par(mfrow = c(3,1))
  hist(churn$Eve.Mins, col = "lightblue", main = "Histogram of Evening Minutes", xlab = "Eve Mins", breaks = seq(0, 400, by = 10))
  plot(density(churn$Eve.Mins), col = "red", lwd = 4, main = "Evening Minutes Density")
  hist(churn$Eve.Mins, col = "lightblue", probability = TRUE, xlab = "Eve Mins", main = "Histogram and Density of Eve Minutes", breaks = seq(0, 400, by = 10))
  lines(density(churn$Eve.Mins) ,lty = "dotted", lwd = 4, col = "red")
}
EveMinHist()

NightMinHist <- function()
{
  par(mfrow = c(3,1))
  hist(churn$Night.Mins, col = "lightblue", main = "Histogram of Night Minutes", xlab = "Night Mins", breaks = seq(0, 400, by = 10))
  plot(density(churn$Night.Mins), col = "red", lwd = 4, main = "Night Minutes Density")
  hist(churn$Night.Mins, col = "lightblue", probability = TRUE, xlab = "Night Mins", main = "Histogram and Density of Night Minutes", breaks = seq(0, 400, by = 10))
  lines(density(churn$Night.Mins), ylim = c(0, 0.006) ,lty = "dotted", lwd = 4, col = "red")
}
NightMinHist()

#MinBoxPlot <- function()
#{
#  par(mfcol = c(1,3))
#  boxplot(churn$Day.Mins, col = "lightblue", main = "Distribution of Day Minutes", ylim=c(0,400))
#  boxplot(churn$Eve.Mins, col = "green", main = "Distribution of Evening Minutes", ylim=c(0,400))
#  boxplot(churn$Night.Mins, col = "pink", main = "Distribution of Night Minutes", ylim=c(0,400))
#}
#MinBoxPlot()

#CallVis <- function()
#{
#  par(mfcol = c(1, 3))
#  boxplot(churn$Day.Calls, col = "pink", main = "Day Calls distribution", ylim=c(0,200))
#  boxplot(churn$Eve.Calls, col = "lightblue", main = "Eve Calls distribution", ylim=c(0,200))
#  boxplot(churn$Night.Calls, col = "green", main = "Night Calls distribution", ylim=c(0,200))
#}
#CallVis()

ChargeVis <- function()
{
  par(mfcol = c(1, 3))
  boxplot(churn$Day.Charge, col = "pink", main = "Day Charge distribution", ylim=c(0,60))
  boxplot(churn$Eve.Charge, col = "lightblue", main = "Eve Charge distribution", ylim=c(0,60))
  boxplot(churn$Night.Charge, col = "green", main = "Night Charge distribution", ylim=c(0,60))
}
ChargeVis()

#CallBarPlot <- function()
#{
#  par(mfrow = c(3,1))
#  counts <- table(churn$Day.Calls)
#  barplot(counts, main = "Day Calls Distribution", col = "green")
#  counts <- table(churn$Eve.Calls)
#  barplot(counts, main = "Evening Calls Distribution", col = "lightblue")
#  counts <- table(churn$Night.Calls)
#  barplot(counts, main = "Night Calls Distribution", col = "pink")
#}
#CallBarPlot()

#CustCallsVsStates <- function()
#{
#  plot(churn$State, churn$CustServ.Calls, xaxt = "n", xlab = "", ylab = "Customer Service Calls Count", 
#       main = "Distribution of Customer Service Calls Count on States", col = "pink")
#  axis(1, at=unique(churn$State), labels = unique(churn$State), las=2)
#}
#CustCallsVsStates()

CustCallsVsChurn <- function()
{
  par(mfcol = c(1,2))
  boxplot(churn$CustServ.Calls[churn$Churn.=="False."], ylab = "Customer Service Calls Count", 
       main = "Customer Service Calls for Non-Churners", col = "lightblue")
  boxplot(churn$CustServ.Calls[churn$Churn.=="True."], ylab = "Customer Service Calls Count", 
       main = "Customer Service Calls for Churners", col="pink")
}
CustCallsVsChurn()

IntlCallsVsChurn <- function()
{
  par(mfcol = c(1,2))
  boxplot(churn$Intl.Calls[churn$Churn. == "False."], ylab = "", 
       main = "International Calls for Non-Churners", col = "lightblue")
  boxplot(churn$Intl.Calls[churn$Churn.=="True."], ylab = "", 
       main = "International Calls for Churners", col="pink")
}
IntlCallsVsChurn()

VmailPlanVsChurn <- function()
{
  df<-data.frame(length(churn$VMail_plan[(churn$Churn. == "False.") & (churn$VMail_plan == "yes")]), length(churn$VMail_plan[(churn$Churn. == "True.") & (churn$VMail_plan == "yes")]))
  names(df) <- c("Non-Churners", "Churners")
  barplot(as.matrix(df), col = "lightblue", main = "Voicemail Plan vs Churn")
}
VmailPlanVsChurn()

IntlPlanVsChurn <- function()
{
  df<-data.frame(length(churn$Intl_plan[(churn$Churn. == "False.") & (churn$Intl_plan == "yes")]), length(churn$Intl_plan[(churn$Churn. == "True.") & (churn$Intl_plan == "yes")]))
  names(df) <- c("Non-Churners", "Churners")
  barplot(as.matrix(df), col = "pink", main = "International Plan vs Churn")
}
IntlPlanVsChurn()

churnVisualization <- function()
{
  par(mfrow = c(2,1))
  counts <- table(churn$Churn.)
  barplot(counts, main = "Distribution of Churn", col = rainbow(2))
  count <- length(churn$Churn.[churn$Churn. == "True."])
  count1 <- length(churn$Churn.[churn$Churn. == "False."])
  slices <- c(count1, count)
  lbls <- c("False", "True")
  pct <- slices/sum(slices) * 100
  lbls <- paste(lbls, pct)
  lbls <- paste(lbls, "%", sep = "")
  pie(slices, labels = lbls, col = rainbow(length(lbls)), main = "Pie Chart of Churn")
}
churnVisualization()

plansVisualization <- function()
{
  par(mfrow = c(2,1))
  
  count <- length(churn$VMail_plan[churn$VMail_plan == "yes"])
  count1 <- length(churn$VMail_plan[churn$VMail_plan == "no"])
  slices <- c(count1, count)
  lbls <- c("no", "yes")
  pct <- slices/sum(slices) * 100
  lbls <- paste(lbls, pct)
  lbls <- paste(lbls, "%", sep = "")
  pie(slices, labels = lbls, col = rainbow(length(lbls)), main = "Pie Chart of Voice Mail Plan")
  
  count <- length(churn$Intl_plan[churn$Intl_plan == "yes"])
  count1 <- length(churn$Intl_plan[churn$Intl_plan == "no"])
  slices <- c(count1, count)
  lbls <- c("no", "yes")
  pct <- slices/sum(slices) * 100
  lbls <- paste(lbls, pct)
  lbls <- paste(lbls, "%", sep = "")
  pie(slices, labels = lbls, col = rainbow(length(lbls)), main = "Pie Chart of International Plan")
}
plansVisualization()

##Logistic Regression
#ind <- sample(2, nrow(churn), prob=c(0.7, 0.3), replace = TRUE)
#train.data <- churn[ind==1,]
#test.data <- churn[ind==2,]
mylogit<-glm(churn$Churn.~churn$Intl_plan+churn$VMail_plan+churn$VMail.Message+churn$Day.Mins+churn$Day.Calls+
             churn$Day.Charge+churn$Eve.Mins+churn$Eve.Calls+churn$Eve.Charge+churn$Night.Calls+churn$Night.Charge+
             churn$Night.Mins+churn$Intl.Mins+churn$Intl.Calls+churn$Intl.Charge+churn$CustServ.Calls,
             family = binomial(link="logit"), data = churn)
summary(mylogit)

#Estimate 

#(Intercept)          -8.4582221 
#churn$Intl_planyes    2.0461146
#churn$VMail_planyes  -2.0148859
#churn$VMail.Message   0.0359730
#churn$Day.Mins        0.0120447
#churn$Day.Calls       0.0031528
#churn$Day.Charge      0.0063660  
#churn$Eve.Mins       -0.0074020  
#churn$Eve.Calls       0.0006253  
#churn$Eve.Charge      0.1711961  
#churn$Night.Calls     0.0006723
#churn$Night.Charge   -0.1592730  
#churn$Night.Mins      0.0103737  
#churn$Intl.Mins       0.0939705
#churn$Intl.Calls     -0.0943628  
#churn$Intl.Charge    -0.0236201
#churn$CustServ.Calls  0.5165527

mylogit$coefficients

churnFormula<-mylogit$coefficients[[1]]+mylogit$coefficients[[2]]*1+mylogit$coefficients[[3]]*1+mylogit$coefficients[[4]]*23+
              mylogit$coefficients[[5]]*241+mylogit$coefficients[[6]]*6+mylogit$coefficients[[7]]*23.4+
              mylogit$coefficients[[8]]*124.5+mylogit$coefficients[[9]]*55+mylogit$coefficients[[10]]*17.6+
              mylogit$coefficients[[11]]*84+mylogit$coefficients[[12]]*8.5+mylogit$coefficients[[13]]*223.5+
              mylogit$coefficients[[14]]*12.3+mylogit$coefficients[[15]]*3+mylogit$coefficients[[16]]*4.6+
              mylogit$coefficients[[17]]*3
churnFormula  #0.9318647

##Decision Tree
library(party)
ind <- sample(2, nrow(churn), prob=c(0.7, 0.3), replace = TRUE)
train.data <- churn[ind==1,]
test.data <- churn[ind==2,]
churn.tree <- ctree(Churn.~Eve.Charge+Intl.Mins+Intl_plan+CustServ.Calls+VMail.Message, data = train.data)
table(predict(churn.tree), train.data$Churn.)
#       False. True.
#False. 1932   235
#True.    40   103
testPred <- predict(churn.tree, newdata=test.data)
t <- table(testPred, test.data$Churn.)
t
#testPred False. True.
#False.    857   102
#True.      21    43
plot(churn.tree, type="simple")
accuracy<-(sum(diag(t))/sum(t))*100
accuracy   #[1] 87.98
