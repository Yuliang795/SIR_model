#read data
dat<-read.csv("covid19-download.csv", header=T)
#set all na to zeros
dat[is.na(dat)]<-0
str(dat)
#simply visualize the data
#plot number of recovered vs time
plot(seq(1,nrow(dat[dat$prname=="Ontario",])),
     dat[dat$prname=="Ontario",]$numrecover,
     type="l", col="5")
#add lines of number of confirmed
lines(seq(1,nrow(dat[dat$prname=="Ontario",])),
      dat[dat$prname=="Ontario",]$numconf, col=1)

#plot the number of case reported per day
plot(seq(1,nrow(dat[dat$prname=="Ontario",])),
     dat[dat$prname=="Ontario",]$numtoday,
     type="l", col="3")
#add lines of number of recovery per day
lines(seq(1,nrow(dat[dat$prname=="Ontario",])),
      dat[dat$prname=="Ontario",]$numrecoveredtoday, col=5)

#plot the total number of recovery
plot(seq(1,nrow(dat[dat$prname=="Ontario",])),
     dat[dat$prname=="Ontario",]$numrecover,
     type="l", col="3")
#add line of the total number of death
lines(seq(1,nrow(dat[dat$prname=="Ontario",])),
      dat[dat$prname=="Ontario",]$numdeaths, col=5)

#select the data in ontario and plot the total number of recovery.``
ont=dat[dat$pruid==35,]
plot(seq(1,length(ont$numrecover)), ont$numrecover, col=3, type="p", lwd=1)



######################
##Simple linear model#
######################

#Canada

fit=lm(numconf~numdeaths*numrecover*percentoday*numactive, data=dat)

fit2=lm(numconf~numdeaths+numrecover+percentoday+
          numactive+
          percentoday:numactive+
          numdeaths:numrecover:numactive, data=dat)

#Ontario
ont=dat[dat$pruid==35,]
ont_test_select=sample(nrow(ont), 30)
ont_test=dat[ont_test_select,]
ont_train=dat[-ont_test_select,]

ont_fit=lm(numconf~numdeaths*numrecover*percentoday*numactive*
             ratetotal*percentactive, data=dat)

ont_fit2=lm(numconf~numdeaths+numrecover+percentoday+
                       numactive+percentoday:numactive+
                       numdeaths:percentactive+percentoday:numactive:ratetotal+
                       numdeaths:numrecover:percentactive,
                     data=ont_train)
ont_pred<-predict(ont_fit2, data=ont_test)
ont_pred
##############################################################

#####################
######SIR MODEL######
#####################
sir_model <- function(beta, gamma, S0, I0, R0, times) {
  require(deSolve)
  
  #----------SIR
  sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S
      dI <-  beta * I * S - gamma * I
      dR <-  gamma * I
      return(list(c(dS, dI, dR)))
    })
  }
  
  #----------
  parameters_values <- c(beta  = beta, gamma = gamma)
  
  initial_values <- c(S = S0, I = I0, R = R0)
  
  #----------
  out <- ode(initial_values, times, sir_equations, parameters_values)
  
  #----------
  as.data.frame(out)
}
####################################################################
library(lubridate)
on_dat$date
date_start<-mdy(on_dat$date[1])
current_date<-mdy(on_dat$date)#current data matrix
days_from_beginning<-as.numeric(current_date-date_start)


#TOTAL POPULATION IN ONTARIO
population=14570000
#population = Toronto + Ottawa + Hamilton + Kitchener + London + Oshawa + Windsor
fit.pop=5429524+989567+693645+470015+383437+308875+287069
population=fit.pop
#FIT MODEL   duration days <- times
model_result<-sir_model(beta=0.1418525,gamma=0.0935716,S0=1-1/population,I0=1/population,R0=0,times =seq(1,600, 1))

#plot model results
library(ggplot2)
ggplot(data=model_result,aes(x=model_result$time,y=model_result$S))+
  geom_point(aes(x=model_result$time,y=model_result$I,colour="I"))+
  geom_point(aes(x=model_result$time,y=model_result$S,colour="S"))+
  geom_point(aes(x=model_result$time,y=model_result$R,colour="R"))+
  geom_point(data = on_dat,aes(x=days_from_beginning,y=on_dat$numactive/population,colour="actual data"))+
  labs(x="time / days", y="population %")

#plot predicted accumulated cases and actual accumulated cases
ggplot(data=model_result,aes(x=model_result$time,y=model_result$S))+
  geom_point(aes(x=model_result$time,y=1-model_result$S,colour="predicted accumulated cases"))+
  geom_point(data = on_dat,aes(x=days_from_beginning,y=on_dat$numtotal/population,colour="actual accumulated cases"))+
  labs(x="time / days", y="population %")
#plot predicted active cases and actual active cases
ggplot(data=model_result,aes(x=model_result$time,y=model_result$S))+
  geom_point(aes(x=model_result$time,y=model_result$I,colour="predicted active cases"))+
  geom_point(data = on_dat,aes(x=days_from_beginning,y=on_dat$numactive/population,colour="actual active cases"))+
  labs(x="time / days", y="population %")

#format date to class date
on_dat$date <- mdy(on_dat$date)

#percent confirmed per day
ggplot(data=on_dat, aes(x=on_dat$date, y=on_dat$numtotal))+
  scale_x_date(breaks="1 month", date_labels = "%m")+
  geom_point(aes(x=on_dat$date, y=(on_dat$numtoday / on_dat$numtestedtoday), colour=(on_dat$numtoday / on_dat$numtestedtoday)))+
  scale_colour_gradient(high = "#132B43", low = "#56B1F7", name="scale")+
  labs(x="time / month", y="population %")

#estimate beta and gamma
#avoiding 0 denominatro
on_dat$numtoday[1]=4

on_dat=dat[dat$pruid==35,]
fit.beta= on_dat$numtoday / ((on_dat$numactive-on_dat$numtoday) * 0.8)
fit.gamma = (on_dat$numrecoveredtoday) / (on_dat$numactive-on_dat$numtoday)
cat("beta: ", mean(fit.beta), "gm: ", mean(fit.gamma))

#estimated beta
ggplot(data=on_dat, aes(x=on_dat$date, y=on_dat$numtotal))+
  scale_x_date(breaks="1 month", date_labels = "%m")+
  geom_point(aes(x=on_dat$date, y=fit.beta, colour=fit.beta))+
  scale_colour_gradient(high = "#132B43", low = "#56B1F7" , name="scale")+
  labs(x="time / month", y=expression(gamma))

#estimated gamma
ggplot(data=on_dat, aes(x=on_dat$date, y=on_dat$numtotal))+
  scale_x_date(breaks="1 month", date_labels = "%m")+
  geom_point(aes(x=on_dat$date, y=fit.gamma, colour=fit.gamma))+
  scale_colour_gradient(high = "#132B43", low = "#56B1F7" , name="scale")+
  labs(x="time / month", y=expression(gamma))








