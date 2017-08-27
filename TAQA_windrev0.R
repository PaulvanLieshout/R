# set up the following library packages

library(lubridate) # used to covert Date and Time info into a defined structure
library(ggplot2) # used for creating specific plots
library(openair) # used for creating specific plots
library(dplyr) #
library(moments) #
library(fBasics) #
library(fitdistrplus) # used for fitting distribution function
library(stats)
# evaluate which 'working directive' you work in; 
# is that the 'working directive' you want to work in?
getwd()
# C:\Users\Paul\Desktop\Taqa wind resource
# what folder is the required data in? copy it and reverse the 'slashes' from '\' to '/'
# C:\Users\pvanlieshout\Desktop\Project desktop\JESA\JESA TAQA 140MW\Taqa wind resource
setwd("C:/Users/Paul/Desktop/Taqa wind resource")
f <- list.files() # get list of files
f                 # print the full list
# pick the file to be evaluated
f1 <- f[4]        # pick the name of the required file(s)
f1                # print the file name
#
# use the following for a MERRA DAT file
f2 <- read.table(f1,header = FALSE,sep = "" , skip = 20) # read csv file; define if data set has a header (TRUE or FALSE) 
#
# use the following for a MERRA CSV file
# f2 <- read.table(f1,header = FALSE,sep = ",") # read csv file; define if data set has a header (TRUE or FALSE) 
head(f2)          #print the top 5 lines of the file with header
tail(f2)          #print the tail (bottom) 5 lines of the file with header
summary(f2)       #print a statistical summary of the file with header
#
# provide a header or column names for the data set
names(f2) <- c("DD/MM/YYYY","HH:MM:SS","speed_50mAGL","dir_50mAGL","temp","pressure","density")
#
# Initial checking of the data ##############################################################
head(f2)          #print the top 5 lines of the file with header
tail(f2)          #print the tail (bottom) 5 lines of the file with header
summary(f2)       #print a statistical summary of the file with header          #
#
# data checking using R-stats
summary(f2)
head(f2)
x<-(0:100) # define an x-axis variable
x
y <- f2$"speed_50mAGL"[0:101]
y1 <- sort(y, decreasing = FALSE) #Sort (or order) a vector or factor (partially) into ascending or descending order. 
plot(x,y1)
line(x,y1) # Fit a line robustly as recommended in Exploratory Data Analysis
coef(lm(y1~x)) # Coefficients extracted from the model object (in this case a linear model)
abline(line(x,y1)) # This function adds one or more straight lines through the current plot.
fitted.values(lm(y~x)) # fitted values i.e. te y-values of the lm for the x-values
residuals(lm(y~x)) # residuals i.e. the difference between actual y-values and model y-values
# for time series plots we have to convert the DD/MM/YYYY HH:MM:SS factors ###################
# using library(lubridate) to class "POSIXct" "POSIXt"
class(f2$`DD/MM/YYYY`) # this is a factor
class(f2$`HH:MM:SS`) # this is a factor
Pdaytime <- paste(f2$`DD/MM/YYYY`,f2$`HH:MM:SS`) # this is a factor or character
head(Pdaytime)
class(Pdaytime)
DateTime1 <- parse_date_time(Pdaytime,'ymd HMS') # this is where the conversion starts to class "POSIXct" "POSIXt"
head(DateTime1)
#
class(DateTime1)
DateTime2 <- as.numeric(DateTime1) 
head(DateTime2)
# add DateTime1 and DateTime2 to the existing f2 data.frame
f2$DateTime1 <- DateTime1  # new column in f2 called DataTime1 which contains the 'Coordinated Universal Time' (UTC) value
f2$DateTime2 <- DateTime2  # new column in f2 called DataTime2 which contains the number of seconds since 1jan1070 value
summary(f2)
head(f2)
tail(f2,50)
#
# combine the 2 columns 'dates' and 'time' into single DateTime1 column
# note that 'ymd HMS' might have to be 'dmy HM' depending on the two individual columns
# DateTime1 <- parse_date_time(paste(f2$`DD/MM/YYYY`,f2$`HH:MM:SS`),'ymd HMS') # note this is a seperate column
# class(DateTime1)  # this is class "POSIXct" "POSIXt", which can be split into seperate columns like YEAR or MONTHS etc.
# head(DateTime1)
f2$Year <- as.numeric(format(DateTime1,"%Y"))  # create an extra column unto 'f2' with just the Year data
# head(f2$Year)
# class(f2$Year)
# f2$Month <- as.numeric(format(DateTime1,"%m"))  # create an extra column with just the month data
# head(f2$Month)
# class(f2$Month)
# f2$Day <- as.numeric(format(DateTime1,"%d"))  # create an extra column with just the day data
# head(f2$Day)
# class(f2$Day)
# f2$Hour <- (format(DateTime1,"%HH"))  # create an extra column with just the hourly data
# head(f2$Hour)
# class(f2$Hour)
# head(f2,10)  # note numeric value sets the number of rows to be printed
#

# When creating a new data.frame with daily values use the following:
# create a new column 'Day' with the 'date' with class "POSIXct" "POSIXt"
f2$Day <- as.Date(format(f2$DateTime1))
head(f2,5)
# then create a new DataFrame 'f3' with the mean of a number of variables
f3 <- aggregate(cbind(speed_50mAGL, temp, pressure, density) ~ Day, f2, mean)
head(f3,5)
#
# or do a max value
f4 <- aggregate(cbind(speed_50mAGL, temp, pressure, density) ~ Day, f2, max)
head(f4,5)
summary(f3)
summary(f4)


#
# note that MERRA data goes from 0-180 degrees (from N to E to S)
# and from -0 to -180 (from N to W to S)
# this must be converted to an angle from 0 to 360 degrees (from N to E to S to W to N)
# this can be done to convert negative numbers to 360 + (-value)
#
nrow(f2)
for (n in 1:nrow(f2))
{
  if (f2[n, "dir_50mAGL"] >= 0)
  {
    # positive MERRA angle same as compass angle
    f2[n, "dir_50mAGL"] <- f2[n, "dir_50mAGL"]  
  } else if (f2[n, "dir_50mAGL"] < 0)
  {  
    # negative MERRA angle add 360
    (f2[n, "dir_50mAGL"] <- f2[n, "dir_50mAGL"] + 360)
  }   
}
# analysis using openair
head(f2)
names(f2)
class(f2$DateTime1)
# f2$date <- as.POSIXct(f2$date) # make sure that date class is POSIXct
# make sure that the "YYYY-MM-DD HH:MM:SS" is called "date" using the 'rename() from 'dplyr'
# library(dplyr)
f3 <- rename(f2, date = DateTime1, ws = speed_50mAGL, wd = dir_50mAGL)
head(f3)
names(f3)
f3$ws <- f3$ws*(10/6.2)
summary(f3)
class(f3$date)
#
############ Plotting variables ################

# a number of 'general' plots are constructed below:
hist(f2$`speed_50mAGL`)                # histogram of specified data set
hist(f2$`speed_50mAGL`,main="TAQA hyst wind", prob = TRUE)   # same but as probability density
hist(f2$`speed_50mAGL`,main="TAQA hyst wind", xlab = "wind speed in m/s", ylab = "density (0 to 1)",border="black", col="dodgerblue3",xlim=c(0,30), ylim=c(0,.2),breaks=seq(0, 30, 1) , prob = TRUE)
hist(f2$`speed_50mAGL`,main="TAQA hyst wind", xlab = "wind speed in m/s", ylab = "density (0 to 1)",border="black", col="dodgerblue3",xlim=c(0,30), ylim=c(0,.2),breaks=seq(0, 30, 1) , prob = TRUE)
lines(density(f2$`speed_50mAGL`))                                # and add probability density line in histogram plot
#
plot(speed_50mAGL ~ dir_50mAGL, data = f2)         # plot of speed as a function of direction
plot(f2$dir_50mAGL,f2$speed_50mAGL)                # same plot as above - note that variables in reverse order!
#
# plots using ggplot2 - tick ggplot2 to include as package!
ggplot(data=f2) + geom_histogram(aes(x=speed_50mAGL))
ggplot(data=f2) + geom_density(aes(x=speed_50mAGL))
ggplot(data=f2, aes(x=dir_50mAGL, y=speed_50mAGL))+geom_point()

plot(speed_50mAGL ~ DateTime1, data = f2)         # generic plot of speed as a function of UTC time
ggplot(data=f2, aes(x=DateTime1, y=speed_50mAGL))+geom_point()
ggplot(data=f2, aes(x=DateTime1, y=speed_50mAGL))+geom_point(aes(colour=dir_50mAGL))
g <- ggplot(data=f2, aes(x=DateTime1, y=speed_50mAGL))
g+geom_point(aes(colour=dir_50mAGL))+facet_wrap(~Year)
ggplot(data=f2, aes(x=speed_50mAGL), fill="deepskyblue3")+geom_histogram(fill="deepskyblue3") + facet_wrap(~Year)
summary(f2)

head(f3)
summaryPlot(subset(f3, select = c(date, wd, ws, temp , pressure)))
names(f3)
windRose(f3, key.footer = "m/s", angle = 10, breaks = c(0, 4, 12, 25), cols = c("yellow", "green", "green3", "black")) # default is m/s
windRose(f3, main = "Boujmil seasonal wind rose", type = "season", key.footer = "m/s", angle = 15, breaks = c(0, 4, 12, 25), cols = c("red1","red2","red3","red4")) # default is m/s
windRose(f3, key.header = "Boujmil seasonal wind rose", type = "season", key.footer = "m/s") # default is m/skey.footer = "m/s") # default is m/s
polarFreq(f3)
polarFreq(f3, type = "season")
polarFreq(f3, type = "month")
#
# scatter plots
#
names(f3)
plot(ws ~ wd, data = f3)         # plot of speed as a function of direction
ggplot(data=f3, aes(x=wd, y=ws))+geom_point()
scatterPlot(f3, x= "wd", y="ws")
scatterPlot(f3, x= "wd", y="ws", method = "hexbin", col = "jet")
scatterPlot(f3, x= "wd", y="ws", method = "density", col = "jet")
# some different scatter plots
scatterPlot(f3, x= "DateTime2", y="ws", method = "hexbin", col = "jet")
scatterPlot(f3, x= "temp", y="pressure", method = "hexbin", col = "jet")
scatterPlot(f3, x= "temp", y="pressure", method = "hexbin", col = "jet", type = "season", smooth = TRUE,linear = TRUE)

# some statistical values associated with the wind speed:
#plot box and wisker plot of wind speed
names(f3)
summaryPlot(subset(f3, select = c(date, wd, ws, temp, pressure, density)))

par(mfrow = c(3, 1)) # print the next 4 graps on one page as 4 graps in 1 column
plot(as.factor(format(f3$date,"%Y")),f3$ws, main = "Annual wind speed distribution", xlab = "year", ylab = "wind speed [m/s]", col = "lightblue")
plot(as.factor(format(f3$date,"%m")),f3$ws, main = "monthly wind speed distribution - full data set", xlab = "month", ylab = "wind speed [m/s]", col = "lightblue")
plot(as.factor(format(f3$date,"%H")),f3$ws, main = "diurnal wind speed", xlab = "hour of the day", ylab = "wind speed [m/s]", col = "lightblue")

par(mfrow = c(1, 1))
plot(as.factor(format(f3$date,"%Y-%m")),f3$ws, main = "monthly wind speed distribution - full data set", xlab = "year and month", ylab = "wind speed [m/s]", col = "lightblue")

windRose(f3, ws.int = 2, angle = 15, type = "month", key.footer = "m/s") # default is m/s key.footer = "m/s") # default is m/s

