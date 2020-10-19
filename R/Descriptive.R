###########################################
# Title: R for Kinesiology students
#  
# Description: 
# <A short description of the script>
#
# Author: <Darko Katovic>  <darko.katovic@kif.hr>
# Date: <28.01.2019>
#
# Important source:
# https://swcarpentry.github.io/r-novice-inflammation/13-supp-data-structures/
# https://www.statmethods.net/stats/descriptives.html
###########################################

getwd()
setwd("~/Dropbox/Projects_R/Quantitative_Methods")

rm(list=ls())

Judo <- read.table("~/Dropbox/Projects_Data/Judo.csv", sep = ",", header = TRUE)
Uspjeh <- read.table("~/Dropbox/Projects_Data/Uspjeh.csv", sep = ",", header = TRUE)

# or

my_data <- read.csv(file.choose())

ONT <- Judo$ONT

      #################
####### Data cleaning ####### https://www.statmethods.net/input/importingdata.html
      #################

names(Judo) # list the variables in mydata
str(Judo) # list the structure of mydata
dim(Judo) # red matrice
head(Judo, n=10) # print first 10 rows of mydata
tail(Judo, n=5) # print last 5 rows of mydata
class(object) # class of an object (numeric, matrix, data frame, etc)



# provjera podataka

ifelse(Judo$BML >= 400 & Judo$BML <= 740, Judo$BML, sign(Judo$BML)) # stavi "1" na svako mjesto vrijednosti izvan zadanog intervala

# ili

Judo$BML
BML2 <- ifelse(Judo$BML >= 400 & Judo$BML <= 740, "Out", NA)
BML2

# ili

ifelse(Judo$BML >= 400 & Judo$BML <= 740, Judo$BML, "out of range")

# ili

BML3 <- ifelse(between(Judo$BML,400,740), "OK", NA)
BML3

# ili

install.packages("dplyr")
library(dplyr)
df <- data.frame(Judo)
f <- filter(df, Judo$BML >= 400 & Judo$BML <= 740) # zadrzava samo ispitanike sa vrijednostima varijable unutar zadanog intervala

# ili (malo kompleksnije)
# https://cran.r-project.org/web/packages/formattable/vignettes/formattable-data-frame.html
install.packages("formattable")
library(formattable)
formattable(Judo)

sign_formatter <- formatter("span", 
                    style = x ~ style(color = ifelse(x > 400, "green", 
                    ifelse(x < 300, "green", "red"))))
sign_formatter(c(-1, 0, 1))
formattable(Judo, list(BML = sign_formatter))

# kodna lista

Uspjeh <- read.table("~/Dropbox/Projects_Data/Uspjeh.csv", sep = ",", header = TRUE)
Uspjeh$Spol <- factor(Uspjeh$Spol,
                    levels = c("Musko", "Zensko"),
                    labels = c(1,2)) # varijabla je kodirana "Musko" "Zensko" dekodiranje u 1 i 2

#mydata$v1 <- ordered(mydata$y,
                     #levels = c(1,3, 5),
                     #labels = c("Low", "Medium", "High")) # primjereno koristenje funkcije 'ordered' na rangiranim podacima


# kodiranje kvalitativnih podataka # http://rprogramming.net/recode-data-in-r/

# Replace the data in a field based on equal to some value
SchoolData$Grade[SchoolData$Grade==5] <- "Grade Five"

# Or replace based on greater than or equal to some value
SchoolData$Grade[SchoolData$Grade<=5] <- "Grade Five or Less"

# Or replace based on equal to some text
SchoolData$Grade[SchoolData$Grade=="Five"] <- "Grade Five"

# Or replace only missing data
# Note that ==NA does not work!
SchoolData$Grade[is.na(SchoolData$Grade)] <- "Missing Grade"


# dugo ime varijabli koristenjem paketa 'Hmisc'

#install.packages(Hmisc)
#library(Hmisc)

#label(Judo$ONT) <- "Okretnost na tlu"
#label(Judo$OUZ) <- "Okretnost u zraku"
#label(Judo$NEB) <- "Neritmicko bubnjanje"
#label(Judo$SKL) <- "Sklekovi"
#label(Judo$TRB) <- "Trbusnjaci"
#label(Judo$CUC) <- "Cucnjevi"
#label(Judo$SDM) <- "Skuk u dalj s mjesta"
#label(Judo$BML) <- "Bacanje medicinke iz lezeceg polozaja"
#describe(Judo)

# dugo ime varijabli

names(Judo)[1] <- "Okretnost na tlu"
names(Judo)[2] <- "Okretnost u zraku"
Judo[1] # list the variable
Judo[2]



      ##########################
####### Descriptive Statistics #######
      ##########################

summary(Judo)

#install.packages("psych")
#library(psych)
#describe(Judo)

########
# Mean #
########

all.means.judo <- apply(Judo, 2, mean)
all.means.judo

#################
# Harmonic Mean #
#################

# https://www.rdocumentation.org/packages/lmomco/versions/2.3.1/topics/harmonic.mean
# Harmonic Mean (With Zero-Value Correction)

# https://www.statisticshowto.datasciencecentral.com/harmonic-mean/

Z <- c(5,6,7)
harmonic.mean(Q)

Q <- c(0,0,5,6,7)
Z <- c(5,6,7)
a = c(10, 2, 19, 24, 6, 23, 47, 24, 54, 77)

1/mean(1/a) #compute the harmonic mean

library(psych)
harmonic.mean(Q)
harmonic.mean(a)
harmonic.mean(Z)

harmonic.mean(Q, zero=FALSE)

n/(sum(1/a)) # rucno
length(Z)/(sum(1/Z)) # rucno

# https://towardsdatascience.com/on-average-youre-using-the-wrong-average-geometric-harmonic-means-in-data-analysis-2a703e21ea0


##########
# Median #
##########

all.median.judo <- apply(Judo, 2, median)
all.median.judo

########
# Mode #
########

# Create the function for Mode
getmode <- function(Judo) 
  {
    uniqJ <- unique(Judo)
    uniqJ[which.max(tabulate(match(Judo, uniqJ)))]
  }

  
all.mode.judo <- apply(Judo, 2, getmode)
all.mode.judo

# For multimodal purposes

Mode = function(ONT)
  { 
    freqVar = table(ONT)
    maxFreq = max(freqVar)
    if (all(freqVar == maxFreq))
    mode = NA
  else
    if(is.numeric(ONT))
    mode = as.numeric(names(freqVar)[freqVar == maxFreq])
  else
    mode = names(freq)[freqVar == maxFreq]
    return(mode)
  }
Mode(ONT)

# ili

freqVar = table(ONT) 
subset(freqVar, freqVar==max(freqVar))

# ili

all.mode.judo <- apply(Judo, 2, Mode)
all.mode.judo

#########
# Range #
#########

range(ONT)

all.range.judo <- lapply(Judo, range)
all.range.judo

#############################
# Percentile (or a centile) #
#############################
# 25th percentile is also known as the first quartile (Q1), 
# the 50th percentile as the median or second quartile (Q2), 
# and the 75th percentile as the third quartile (Q3).

quantile(Judo$BML, probs = c(0.05, 0.95))

#############
# Quartiles #
#############

quantile(Judo$BML)

#######################
# Interquartile Range #
#######################

IQR(Judo$BML) 

############
# Variance #
############

#sample
var(ONT)
# var.s <- sum((ONT - mean(ONT))^2) / 59

all.variance_s.judo <- lapply(Judo, var)
all.variance_s.judo

#population
var.p = function(x)
  {
    var(x)*(length(x)-1)/length(x)
  }
# var.p <- sum((ONT - mean(ONT))^2) / 60

all.variance_p.judo <- lapply(Judo, var.p)
all.variance_p.judo

######################
# Standard Deviation #
######################

#sample
sd(ONT)

all.stdev_s.judo <- apply(Judo, 2, sd)
all.stdev_s.judo

# var.s <- sum((ONT - mean(ONT))^2) / 59
# stdev.s <- sqrt(var.s)

#population
stdev.p = function(x)
  {
    sd(x)*(sqrt((length(x)-1)/length(x)))
  }

all.stdev_p.judo <- apply(Judo, 2, stdev.p)
all.stdev_p.judo

# var.p <- sum((ONT - mean(ONT))^2) / 60
# stdev.p <- sqrt(var.p)

stdev.p2 <- function(x) sqrt(var.p)
all.stdev_p2.judo <- apply(Judo, 2, stdev.p2)
all.stdev_p2.judo

#########################################
# Coefficient of variation (as percent) #
#########################################

sd(ONT)/ mean(ONT)*100

cv <- function(x)
  {
    (sd(x)/mean(x))*100
}

cv(ONT)

all.cv.judo <- apply(Judo, 2, cv)
all.cv.judo

#######################################################
#######################################################

# Initial Stastical steps - grouping and ploting data #

# Grouping and ploting of Qualitative data

Uspjeh <- read.table("~/Dropbox/Projects_Data/Uspjeh.csv", sep = ",", header = TRUE)

# Add a new columns (Razred i Ocjena) to a dataframe "Uspjeh" using values I:IV
# Save dataframe to '~/Dropbox/Projects_Data/Uspjeh.csv'

#Uspjeh$Razred <- sample(c("I","II", "III", "IV"), 29, replace = TRUE)
#Uspjeh$Ocjena <- sample(c("nedovoljan", "dovoljan", "dobar", "vrlo dobar", "odlican"), 29, replace = TRUE)
#write.csv(Uspjeh,'~/Dropbox/Projects_Data/Uspjeh.csv')

# Jednodimenzionalno grupiranje

table(Uspjeh$Razred)

# Dvodimenzionalno grupiranje

table(Uspjeh$Razred, Uspjeh$Ocjena)

#Visedimenzionalno grupiranje

ftable(Uspjeh)
ftable(Uspjeh, row.vars = 1:4)

# Frequency table

#install.packages("summarytools")
#library(summarytools)
#summarytools::freq(Somatotype$type, order = "freq")

install.packages("plyr")
library(plyr)
count(Uspjeh, 'Razred')


# Graphical representation of Qualitative data
# https://www.statmethods.net/graphs/bar.html

# Simple Vertical Bar Plot 
counts <- table(Uspjeh$Razred)
barplot(counts, main="Uspjeh", xlab="Razred")

# Simple Horizontal Bar Plot with Added Labels 
counts <- table(Uspjeh$Ocjena)
#ne radi kako treba
barplot(counts, main="Uspjeh", horiz=TRUE, names.arg=c("Dovoljan", "Dobar", "Vrlo dobar", "Odlican"), col=c("darkblue","red"))

# Simple Pie Chart
counts <- table(Uspjeh$Razred)
lbls <- c("I", "II", "III", "IV")
pie(counts, labels = lbls, main="Strukturni krug")


# Grouping and ploting of Quantitative data

# Sorting

ONTsort_asc <- sort(ONT)
ONTsort_desc <- sort(ONT, decreasing=TRUE)

# Frequency table

# range(Judo$ONT)
ONTcut1 <- cut(Judo$ONT, c(10.4, 14.5, 18.5, 22.5, 26.5, 30.5))
summary(ONTcut1)

absolut.freq <- table(ONTcut1)
rel.freq <- absolut.freq / nrow(Judo)
cum.freq <- cumsum(absolut.freq) 
cbind(absolut.freq, rel.freq, cum.freq)

# Histogram

hist(Judo$SDM)
hist(Judo$SDM, breaks='FD', xlim=c(120,220), ylim=c(1,20))

# or

hist(Judo$ONT, 
     main="Histogram (ONT)", 
     xlab="ONT", 
     border="white", 
     col="blue",
     xlim=c(10,30), ylim=c(0,0.15), #velicine x i y osi
     las=1, #orjentacija vrijednosti na x osi
     breaks=c(10.5, 14.5, 18.5, 22.5, 26.5, 30.5), #vektor granica razreda
     prob = TRUE) # iskazuje gustocu (Density) 
                  # ako je ukljuceno, smanjiti ylim na 0.15 (inace 30)
                  # ako je FALSE, korigirati y os
     
     lines(density(Judo$ONT))
     
# or
     
hist(Judo$ONT, xlim=c(10.5,30.5), ylim=c(0,0.15), breaks=c(10.5, seq(10.5,30.5, 4))) # breaks argumenti: prvi: pozicionira start hisograma, drugi i treci: pocetna i krajnja vrijednost, cetvrti: interval razreda

# or

plot(cut(Judo$SDM, breaks = 5), main="Frekvencije (ONT)", ylim=c(0, 30))

# or

# Add a Normal Curve (Thanks to Peter Dalgaard)
# Provjeriti (!!!)
x <- Judo$SDM 
h <- hist(x, breaks=5, col="red", xlab="SDM", main="Skok u dalj s mjesta") 
xfit <- seq(min(x),max(x),length=40) 
yfit <- dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

################################
# Cumulative frequency poligon #
################################

SDM = Judo$SDM 
breaks=c(135, 150, 165, 180, 195, 210)
SDM.cut = cut(SDM, breaks, right=FALSE) 
SDM.freq = table(SDM.cut)

cumfreq0 = c(0, cumsum(SDM.freq)) 
plot(breaks, cumfreq0, 
     main="Cumulative Frequencies",  # main title 
     xlab="SDM",        # x−axis label 
     ylab="Cumulative frequencies")   # y−axis label 
lines(breaks, cumfreq0)

# or

# dobar kumulativni poligon
n <- 30
x <- rnorm(n)
fred <- ecdf(x)
plot(fred)
curve(pnorm(x), add=TRUE)


##############
# Covariance #
##############

cov(Judo$SDM, Judo$BML)
cov(Judo)

round(cov(Judo),3) # zaokruzivanje na trecu decimalu

###########################
# Correlation Coefficient #
###########################

# cor(x, method = c("pearson", "kendall", "spearman"))

cor(Judo$SDM, Judo$BML)
cor(Judo, method = "pearson") # korelacijska matrica

round(cor(Judo),3) # zaokruzivanje na trecu decimalu

cor.test(Judo$SDM, Judo$BML)

cor.test(Judo$SDM, Judo$BML)$p.value

# or

# Computes a matrix of Pearson's r or Spearman's rho rank correlation coefficients for all possible pairs
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

install.packages("Hmisc")
library(Hmisc)

matrixJudo <- rcorr(as.matrix(Judo), type="pearson") # type can be pearson or spearman, output: r-values and P-values matrix

matrixJudo$r # Extract the correlation coefficients

matrixJudo$P # Extract p-values


############
# Z-Scores #
############

# https://www.r-bloggers.com/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/

scale(Judo$SDM, center = TRUE, scale = TRUE)
scale(Judo[,1:8], center = TRUE, scale = TRUE)


# https://stats.seandolinar.com/calculating-z-scores-with-r/

#######################
# Normal distribution # 
#######################

# http://msenux2.redwoods.edu/MathDept/R/StandardNormal.php

# http://michaelminn.net/tutorials/r-normal-rank-order/

# The Probability Density Function

x <- seq(-4,4,length=200)
x

y <- 1/sqrt(2*pi)*exp(-x^2/2)
y

plot(x,y,type="l",lwd=2,col="red")

# ...or

x <- seq(-4,4,length=200)
y <- dnorm(x,mean=,sd=1)
plot(x,y,type="l",lwd=2,col="red")


# The Area Under the Probability Density Function

pnorm(0, mean=0, sd=1)
pnorm(1, mean=0, sd=1)

x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue")
#---
x=seq(-4,1,length=200)
y=dnorm(x)
polygon(c(-4,x,1),c(0,y,0),col="gray")


# 68%-95%-99.7% Rule

pnorm(1,mean=0,sd=1)-pnorm(-1,mean=0,sd=1)
pnorm(2,mean=0,sd=1)-pnorm(-2,mean=0,sd=1)
pnorm(3,mean=0,sd=1)-pnorm(-3,mean=0,sd=1)

x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue")
x=seq(-2,2,length=200) # promijeniti interval
y=dnorm(x)
polygon(c(-2,x,2),c(0,y,0),col="gray") # promijeniti interval

########################
#Skewness and Kurtosis #
########################

library(moments)
skewness(Judo$SDM)

library(psych)
skew(Judo$SDM)

library(e1071)
skewness(Judo$SDM, na.rm = FALSE, type = 1)
skewness(Judo$SDM, na.rm = FALSE, type = 2)
skewness(Judo$SDM, na.rm = FALSE, type = 3)


library(moments)
kurtosis(Judo$SDM)

library(psych)
kurtosi(Judo$SDM)

library(e1071)
kurtosis(Judo$SDM, na.rm = FALSE, type = 1)
kurtosis(Judo$SDM, na.rm = FALSE, type = 2)
kurtosis(Judo$SDM, na.rm = FALSE, type = 3)


#Histogram
library(ggplot2)
datasim <- data.frame(Judo$SDM)
ggplot(datasim, aes(x = Judo$SDM), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Skok u dalj s mjesta'))) + 
  ylab(expression(bold('Density')))









#######################################################################################################
# TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST #
#######################################################################################################

# Crtanje i objasnjenje oblika distribucije
# http://complete-markets.com/wp-content/uploads/2014/01/skewness_and_kurtosis_post.html

f <- function(x) {
  
  x <- Judo$SDM
  
  plot(density(x), main = "Skok u dalj s mjesta", lty = 1, 
       col = 1, lwd = 2, xlab = "SDM", sub = "Sub")
  
  lines(density(rnorm(5e+05, mean = mean(x), sd = sd(x))), lty = 4, col = 2, 
        lwd = 2)
  
  cat("The sample skewness is", skewness(x), "\n")
  cat("For a t-statistic of", skewness(x)/(sqrt(6/length(x))), "\n")
  p1 <- 2 * (1 - pt(abs(skewness(x)/(sqrt(6/length(x)))), length(x) - 1))
  cat("And a p-value of", p1, "\n")
  cat("So we", ifelse(p1 < 0.05, "reject the null, and find the distribution is skewed.", 
                      "do not reject the null, the distribution is symmetric."), "\n")
  cat("\n")
  
  cat("The sample excess kurtosis is", kurtosis(x)[1], "\n")
  cat("For a t-statistic of", kurtosis(x)/(sqrt(24/length(x))), "\n")
  p2 <- 2 * (1 - pt(abs(kurtosis(x)/(sqrt(24/length(x)))), length(x) - 1))
  cat("And a p-value of", p2, "\n")
  cat("So we", ifelse(p2 < 0.05, "reject the null, and find the distribution has fat tails.", 
                      "do not reject the null, the distribution does not have fat tails."), 
      "\n")
  cat("\n")
}

#####################################################################################

# Kernel Density Plot

x <- Judo$BML
d <- density(x) # returns the density data 
plot(d) # plots the results

lines(density(rnorm(5e+05, mean = mean(x), sd = sd(x))), lty = 4, col = 2, lwd = 2)

######################################################################################

# Normality Test in R
# http://www.sthda.com/english/wiki/normality-test-in-r
# svakako pogledati testove normalnosti u R Package ‘nortest’

shapiro.test(Judo$SDM)

ks.test(Judo$SDM,"pnorm",mean(Judo$SDM),sd(Judo$SDM))

# Lilliefors test

###############################################################
# http://www.stat.umn.edu/geyer/5601/examp/kolmogorov.html












