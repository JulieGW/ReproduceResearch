# read it in
activity<-read.csv("./data/activity.csv")
# split by date and do column sums then histogram, overall mean and median
s<-split(activity,activity$date)
steps<-sapply(s,function(x) colSums(x[c("steps")],na.rm=T))
hist(steps, col="red",breaks = 20)
stepMean<- mean(steps)
stepMedian<-median(steps)

# to do the line chart you split by interval and do means
int<-split(activity,activity$interval)
fivemin<-sapply(int,function(x) colMeans(x[c("steps")],na.rm=T))
interval<-as.numeric(names(int))

plot(interval,fivemin,type="l",xlab="5 min interval",ylab="average steps")

# second group of overall statistics
which.max(fivemin)  # max(fivemin) = 206.1698
Totalna<-sum(is.na(activity$steps))

# then you need to create a separate set with NA replaced by something
activityNA<-activity
activityNA[is.na(activityNA$steps)=="TRUE",1]<-mean(fivemin)
sNA<-split(activityNA,activityNA$date)
stepsNA<-sapply(sNA,function(x) colSums(x[c("steps")]))

# different histogram to compare
hist(stepsNA, col="green", breaks=20)

# overallstatistics to see how they changed
stepsNAmean=mean(stepsNA)
stepsNAmedian=median(stepsNA)

# now go to labeling dates as weekend or weekday
activityNA$day<-weekdays(as.Date(activityNA$date))

for(i in 1:17568){
if(activityNA$day[i]=="Saturday"|activityNA$day[i]=="Sunday"){
  activityNA$week[i]<-"weekend"
}else{
  activityNA$week[i]<-"weekday"
}
}

# now I have to create a data frame that is set up to do the last graph
weekends<-subset(activityNA,week=="weekend")
weekday<-subset(activityNA,week=="weekday")

weekendint<-split(weekends,weekends$interval)
fiveweekend<-sapply(weekendint,function(x) colMeans(x[c("steps")]))
intervalweekend<-as.numeric(names(weekendint))

weekdayint<-split(weekday,weekday$interval)
fiveweekday<-sapply(weekdayint,function(x) colMeans(x[c("steps")]))
intervalweekday<-as.numeric(names(weekdayint))

weekenddf=data.frame(intervalweekend,fiveweekend)
weekdaydf=data.frame(intervalweekday,fiveweekday)

weekenddf$week<-c("weekend")
weekdaydf$week<-c("weekday")
names(weekenddf)<-c("interval","avgSteps","week")
names(weekdaydf)<-c("interval","avgSteps","week")

activitysummary<-rbind(weekdaydf,weekenddf)

# then create the graph
library(lattice)

xyplot(activitysummary$avgSteps~activitysummary$interval|activitysummary$week,type="l",layout=c(1,2),xlab="5 minute interval",ylab="average number of steps")