week <- nrow(ufc19) #number of observations, reported weekly
weeks <- seq(1,week, by=1) #vector of number of weeks of data

exponent.fit <- lm(log(ufc19$total)~weeks) #log transform data and fit linear model
modsum<-summary(exponent.fit)
plot(log(ufc19$total)~weeks) #plot transformed data and fitted line
abline(exponent.fit)
r2 <- modsum$adj.r.squared #store r2 and p from summary to add as text to plot
my.p <- modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n') #plot legend

three<-week + 3 #predict 3 weeks into future
threeweeks<-seq(1, three, by=1) #vector of number of weeks including 3 in future

#predict cases for next 3 weeks based on model exponent.fit
cases.exponential2 <- exp(predict(exponent.fit,list(weeks=threeweeks))) #calculate predicted variables and re-transform

#plot graph with fitted line
plot(weeks, ufc19$total, pch=16, xlim=c(1, three), ylim=c(0, max(cases.exponential2)), xlab = "Week(s)", ylab = "Total Cases")
lines(threeweeks, cases.exponential2, lwd=2, col = "red")
legend('topleft', legend = rp, bty = 'n') #same legend as above
