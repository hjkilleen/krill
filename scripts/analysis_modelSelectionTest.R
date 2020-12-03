library(lme4)
library(arm)
library(MuMIn)
library(olsrr)
# Tue Nov 17 17:10:11 2020 ------------------------------

#Based on example in https://onlinelibrary.wiley.com/doi/full/10.1111/j.1420-9101.2010.02210.x

#Euphausia pacifica
epc <- ep[complete.cases(ep),]#filter to only complete cases

#Optimize random effects structure using maximum likelihood
ep.int <- lmer(scale(length) ~ sex*temp_2_z + temp_100_z + sex:temp_100_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + sla_z + sex:sla_z + moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1|station), data = epc, na.action = na.fail, REML = FALSE) #model with random intercept 

ep.intSlope <- lmer(scale(length) ~ sex*temp_2_z + temp_100_z + sex:temp_100_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + sla_z + sex:sla_z + moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1+temp_2_z|station), data = epc, na.action = na.fail, REML = FALSE)#model with random intercept and slope

anova(ep.int, ep.intSlope)#compare models with different random effect structure
#Slope intercept is the optimal random effects structure

#Optimize fixed effect structure using AIC
ep.global.model <- lmer(scale(length) ~ sex*temp_2_z + temp_100_z + sex:temp_100_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + sla_z + sex:sla_z + moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1+temp_2_z|station), data = epc, na.action = na.fail, REML = FALSE)

ep.model.set <- dredge(ep.global.model)

ep.top.model <- get.models(ep.model.set, subset = 1)

ep.top.model

#top model with full dataset with random slope and intercept for SST
ep.model <- lmer(scale(length) ~ chla_z + moci_spring_z + sex +  sst_sd_z + temp_100_z + temp_2_z + (1 + temp_2_z | station) + moci_spring_z:sex +  sex:temp_100_z + sex:temp_2_z, data = ep)
car::vif(ep.model)#check for multicollinearilty using variance inflation factor <5 = ok
lattice::dotplot(ranef(ep.model,condVar=TRUE))
sjPlot::plot_model(ep.model)
sjPlot::tab_model(ep.model, 
                  show.re.var= TRUE, 
                  dv.labels= "Environmental Effects on E. pacifica Length")

#N and S models
epN <- filter(ep[complete.cases(ep),], latitude >= 34.5)
epS <- filter(ep[complete.cases(ep),], latitude < 34.5)
epN.global.model <- lmer(scale(length) ~ sex*temp_2_z + temp_100_z + sex:temp_100_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + sla_z + sex:sla_z + moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1+temp_2_z|station), data = epN, na.action = na.fail, REML = FALSE)
epN.model.set <- dredge(epN.global.model)

epN.top.model <- get.models(epN.model.set, subset = 1)

epN.top.model

#boundary fit is singular with random slope
epS.global.model <- lmer(scale(length) ~ sex*temp_2_z + temp_100_z + sex:temp_100_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + sla_z + sex:sla_z + moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1|station), data = epS, na.action = na.fail, REML = FALSE)

epS.model.set <- dredge(epS.global.model)

epS.top.model <- get.models(epS.model.set, subset = 1)

epS.top.model

#Thysanoessa spinifera
ts1 <- ts[,c(-12, -21)]
tsc <- ts1[complete.cases(ts1),]#filter to only complete cases

#Optimize random effects structure using maximum likelihood
ts.int <- lmer(scale(length) ~ sex*temp_2_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + sla_z + sex:sla_z + moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1|station), data = tsc, na.action = na.fail, REML = FALSE) #model with random intercept 

ts.intSlope <- lmer(scale(length) ~ sex*temp_2_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + sla_z + sex:sla_z + moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1+temp_2_z|station), data = tsc, na.action = na.fail, REML = FALSE)#model with random intercept and slope

anova(ts.int, ts.intSlope)#compare models with different random effect structure
#Slope intercept is the optimal random effects structure

#Optimize fixed effect structure using AIC
ts.global.model <- lmer(scale(length) ~ sex*temp_2_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + sla_z + sex:sla_z + moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1+temp_2_z|station), data = tsc, na.action = na.fail, REML = FALSE)

ts.model.set <- dredge(ts.global.model)

ts.top.model <- get.models(ts.model.set, subset = 4)

ts.top.model

#top model with full dataset with random slope and intercept for SST
ts.model <- lmer(scale(length) ~ chla_z + cuti_z + moci_spring_z + sex + sla_z + sst_sd_z + temp_2_z + (1 + temp_2_z | station) + chla_z:sex + sex:sla_z + sex:temp_2_z, data = ts)
car::vif(ts.model)#check for multicollinearilty using variance inflation factor <5 = ok
lattice::dotplot(ranef(ts.model,condVar=TRUE))
sjPlot::plot_model(ts.model)
sjPlot::tab_model(ts.model, 
                  show.re.var= TRUE, 
                  dv.labels= "Environmental Effects on T. spinifera Length")


#Nematocelis difficilis
ndc <- nd[complete.cases(nd),]#filter to only complete cases

#Optimize random effects structure using maximum likelihood
nd.int <- lmer(scale(length) ~ sex*temp_2_z + temp_100_z + sex:temp_100_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z +  moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1|station), data = ndc, na.action = na.fail, REML = FALSE) #model with random intercept 

nd.intSlope <- lmer(scale(length) ~ sex*temp_2_z + temp_100_z + sex:temp_100_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + moci_spring_z + sex:moci_spring_z + cuti_z + sex:cuti_z + (1+temp_2_z|station), data = ndc, na.action = na.fail, REML = FALSE)#model with random intercept and slope

#Random slope and intercept model is singular fit. Use random intercept model as global model. 

#Optimize fixed effect structure using AIC
nd.global.model <- lmer(scale(length) ~ sex*temp_2_z + temp_100_z + sex:temp_100_z + sst_sd_z + sex:sst_sd_z + chla_z + sex:chla_z + moci_spring_z + sex:moci_spring_z + sla_z + sex:sla_z + cuti_z + sex:cuti_z + (1|station), data = ndc, na.action = na.fail, REML = FALSE)

nd.model.set <- dredge(nd.global.model)

nd.top.model <- get.models(nd.model.set, subset = 1)

nd.top.model


#top model with full dataset with random slope and intercept for SST
nd.model <- lmer(scale(length) ~ chla_z + cuti_z + moci_spring_z + sex + temp_100_z + temp_2_z + (1 | station) + cuti_z:sex + moci_spring_z:sex, data = nd)
car::vif(nd.global.model)#check for multicollinearilty using variance inflation factor <5 = ok
kappa.mer(nd.global.model) #check condition index for model (mixed effect variation)
#kappa < 10 is reasonable collinearity,
# kappa < 30 is moderate collinearity,
# kappa >= 30 is troubling collinearity
lattice::dotplot(ranef(nd.model,condVar=TRUE))
sjPlot::plot_model(nd.model)
sjPlot::tab_model(nd.model, 
                  show.re.var= TRUE, 
                  dv.labels= "Environmental Effects on N. difficilis Length")
