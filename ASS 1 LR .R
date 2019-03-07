## --------------------------Data Upload and data sanity check------------------#### 
data_crime<-read.csv("crime.csv",stringsAsFactors = T)


dim(data_crime)
head(data_crime)
tail(data_crime)
str(data_crime)
summary(data_crime)
####----------------------------- Data Prepration ----------------####
## missgin values spoted in Ed
## As Ed is integer type imouting these values with mean 
## for perofmimg impute function we require libarary "Hmisc"
library(Hmisc)

data_crime$Ed<-as.numeric(impute(data_crime$Ed, mean)) 
summary(data_crime)

## checking for linearity and uniform distribution -------> do we have to do it for indivisual varibles ?

## Outilier detection and treatment 

boxplot(sentiment_data[c(-3,)]) ## this is code to detect outliers for all variables at once. 


## outlier treatment of all varibles 
stats


##### detection of OT for R
outlier_values_crime <- boxplot.stats(data_crime$R)$out  # outlier values.
boxplot(data_crime$R, main="R", pars=list(boxwex=0.1))

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)  ## what is this comand for?

#### treatment of OT for R

x <-data_crime$R
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)

# replacing the existing column

data_crime$R<-x
summary(data_crime$R)
boxplot(data_crime$R, main="R", pars=list(boxwex=0.1))

##### detection of OT for Age
outlier_values_crime <- boxplot.stats(data_crime$Age)$out  # outlier values.
boxplot(data_crime$Age, main="Age", pars=list(boxwex=0.1))

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)  ## what is this comand for?

#### treatment of OT for R

x <-data_crime$Age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)

# replacing the existing column

data_crime$Age<-x
summary(data_crime$R)
boxplot(data_crime$Age, main="Age", pars=list(boxwex=0.1))


##### detection of OT for Ex1
outlier_values_crime <- boxplot.stats(data_crime$Ex1)$out  # outlier values.
boxplot(data_crime$Ex1, main="Ex1", pars=list(boxwex=0.1))

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)  ## what is this comand for?

#### treatment of OT for R

x <-data_crime$Ex1
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)

# replacing the existing column

data_crime$Ex1<-x
summary(data_crime$R)
boxplot(data_crime$R, main="Ex1", pars=list(boxwex=0.1))

##### detection of OT for M
outlier_values_crime <- boxplot.stats(data_crime$M)$out  # outlier values.
boxplot(data_crime$M, main="R", pars=list(boxwex=0.1))

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)  ## what is this comand for?

#### treatment of OT for M

x <-data_crime$M
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)

# replacing the existing column

data_crime$M<-x
summary(data_crime$M)
boxplot(data_crime$M, main="M", pars=list(boxwex=0.1))


##### detection of OT for N
outlier_values_crime <- boxplot.stats(data_crime$N)$out  # outlier values.
boxplot(data_crime$N, main="N", pars=list(boxwex=0.1))

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)  ## what is this comand for?

#### treatment of OT for N

x <-data_crime$N
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)

# replacing the existing column

data_crime$N<-x
summary(data_crime$N)
boxplot(data_crime$N, main="R", pars=list(boxwex=0.1))


##### detection of OT for NW
outlier_values_crime <- boxplot.stats(data_crime$NW)$out  # outlier values.
boxplot(data_crime$NW, main="NW", pars=list(boxwex=0.1))

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)  ## what is this comand for?

#### treatment of OT for NW

x <-data_crime$R
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)

# replacing the existing column

data_crime$NW<-x
summary(data_crime$NW)
boxplot(data_crime$NW, main="NW", pars=list(boxwex=0.1))

##### detection of OT for U1
outlier_values_crime <- boxplot.stats(data_crime$U1)$out  # outlier values.
boxplot(data_crime$U1, main="U1", pars=list(boxwex=0.1))

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)  ## what is this comand for?

#### treatment of OT for U1

x <-data_crime$U1
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)

# replacing the existing column

data_crime$U1<-x
summary(data_crime$U1)
boxplot(data_crime$U1, main="R", pars=list(boxwex=0.1))

##### detection of OT for U2
outlier_values_crime <- boxplot.stats(data_crime$U2)$out  # outlier values.
boxplot(data_crime$U2, main="U2", pars=list(boxwex=0.1))

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)  ## what is this comand for?

#### treatment of OT for U2

x <-data_crime$U2
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- (qnt[1] - H)
x[x > (qnt[2] + H)] <- (qnt[2] + H)

# replacing the existing column

data_crime$U2<-x
summary(data_crime$U2)
boxplot(data_crime$U2, main="U2", pars=list(boxwex=0.1))

########---- Outliers of all the varivles areremoved ------######

######- checking for uniform distribution---------########

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
##Age-->OK
plot(density(data_crime$Age), main="Density Plot: Age", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$Age), col="red")

##R--> NOT OK 
plot(density(data_crime$R), main="Density Plot: R", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$R), col="red")

##S---> Not OK
plot(density(data_crime$S), main="Density Plot: S", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$S), col="red")

##Ed---> Not Ok
plot(density(data_crime$Ed), main="Density Plot: Ed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$Ed), col="red")
##Ex0 ----> OK
plot(density(data_crime$Ex0), main="Density Plot: Ex0", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$Ex0), col="red")
##Ex1 ----> OK
plot(density(data_crime$Ex1), main="Density Plot: Ex1", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$Ex1), col="red")
##LF
plot(density(data_crime$LF), main="Density Plot: LF", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$LF), col="red")
##M ---->OK
plot(density(data_crime$M), main="Density Plot: M", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$M), col="red")
##N ---->OK
plot(density(data_crime$N), main="Density Plot: N", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$N), col="red")
##NW ---->OK
plot(density(data_crime$NW), main="Density Plot: NW", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$NW), col="red")
##U1 ----> OK
plot(density(data_crime$U1), main="Density Plot: U1", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$U1), col="red")
##U2 ----> OK
plot(density(data_crime$U2), main="Density Plot: U2", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$U2), col="red")
#W -----> OK
plot(density(data_crime$W), main="Density Plot: W", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$W), col="red")
#X -----> Not OK
plot(density(data_crime$X), main="Density Plot: X", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(data_crime$X), col="red")

######------------------ Checking for Co relation --------######
install.packages("yaml")
install.packages("ggplot2")
install.packages("GGally")

# Visualizing correlation
library(yaml)
library(ggplot2)
library(GGally)
ggpairs(data=data_crime, columns=3:14, title="crime")  ## what does columns mean what ?

# creating a matrix of correlation

correlation_matrix<-cor(data_crime[,-3])
class(correlation_matrix)
CRM<-data.frame(correlation_matrix)
class(CRM)
write.csv(x = CRM,file = "CRMLR.csv",)

####-------- Creating linear model ----------#######

crime_model<-lm(R~., data=data_crime)


##########--------- Checking for multi Coliniearity----------#######
install.packages("car")
library(car)

vif(crime_model) 
## as it is the 1st iterration so taking all values of vif less than 5

####--------- Data Partion --------#########
## 75% of the sample size
smp_size <- floor(0.75 * nrow(data_crime))

## set the seed to make your partition reproducible
       
smp_size <- floor(0.75 * nrow(data_crime))
set.seed(123)
train_ind <- sample(seq_len(nrow(data_crime)), size = smp_size)

train <- data_crime[train_ind, ]
test <- data_crime[-train_ind, ]

#######------- BUiling MOdel on test data--------####
linearMod1<- lm(R ~ ., data=train)

linearMod2<- lm(R ~ ., data=train[,-c(2,3,4,5,7,10,11,12,13)])
### Checking the model output

summary(linearMod2)

#### removal of insignificant variables -----###

#### checking for VIF ###

vif(linearMod3)


## step wise regession 
#step<-stepAIC(fit,direction="both")
#fit2<-lm(medv`crim+zn+nox+rm)
#"http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/"





