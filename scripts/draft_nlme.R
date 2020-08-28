CE.mod <- nls(obs ~ 
                SSlogis(time, Asym, xmid, scal), 
              data=subset(CEdat, time<=20))

nlme(circumference˜phi1/(1+exp(-(age-phi2)/phi3)),
     fixed=phi1+phi2+phi3˜1, random=phi1˜1|Tree,
     data=Orange, start=c(phi1=200,phi2=800,phi3=400))

newstart <- list(Asym = max(CEdat$obs),
                 xmid = mean(CEdat$time),
                 scal=1)
CE.fullmod <- update(CE.mod,data=CEdat,
                     start=newstart)

c.0 <- min(q24$cost.per.car) * 0.5
model.0 <- lm(log(cost.per.car - c.0) ~ reductions, data=q24)
start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)
model <- nls(cost.per.car ~ a * exp(b * reductions) + c, data = q24, start = start)

lin <- lm(length ~ temp_2, allLengthsEnv)

dat <- filter(allLengthsEnv, temp_2 != "NA", temp_2 !=0, length != "NA")

nls(length ~ SSlogis(temp_2, Asym, xmid, scal), data = dat, start = c(Asym = max(dat$length), xmid = mean(dat$temp_2), scal = 1))

