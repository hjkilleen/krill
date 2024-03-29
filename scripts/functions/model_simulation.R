fsim.glmm <- function(m1, nsim = 1000, cont.pred.expansion = 1, n.sim.vals = 10, inv_link_function = NULL){ # takes a glm fit model object and an integer for the number of simulations to run.
  '%nin%' <- Negate('%in%')
  if("glmerMod" %in% class(m1) | "lmerMod" %in% class(m1)){
    mdata <- m1@frame
    vars <- c(as.character(attr(attr(mdata, 'terms'), 'predvars.fixed')),
              as.character(attr(attr(mdata, 'terms'), 'predvars.random'))[3:length(as.character(attr(attr(mdata, 'terms'), 'predvars.random')))] )
    model.residual.variance <- sd(residuals(m1))
  }
  if("glm" %in% class(m1) | "lm" %in% class(m1) & 'gam' %nin% class(m1)){
    mdata <- m1$data
    vars <- as.character(attr(m1$terms, 'variables'))
    model.residual.variance <- sd(residuals(m1))
  }
  if("gam" %in% class(m1)){
    mdata <- m1$model
    vars <- as.character(attr(m1$terms, 'variables'))
    model.residual.variance <- m1$sig2
  }
  vars <- vars[-which(vars == "list")]
  y <- vars[1] # response variable
  x <- vars[2:length(vars)] # predictor variable(s)
  
  dpred <- expand.grid(
    sapply(x,
           function(x.i){
             # print(class(mdata[[x.i]]))
             if(class(mdata[[x.i]])[1] == "numeric" | class(mdata[[x.i]])[1] == "integer"){
               out <- seq(range(mdata[[x.i]], na.rm = TRUE)[1] * cont.pred.expansion, range(mdata[[x.i]], na.rm = TRUE)[2] * cont.pred.expansion, length.out = n.sim.vals)
             }
             if(class(mdata[[x.i]])[1] == "character"){
               out <- unique(mdata[[x.i]])
             }
             if(class(mdata[[x.i]])[1] == "factor"){
               out <- unique(mdata[[x.i]])
             }
             if("POSIXct" %in% class(mdata[[x.i]])){
               out <- unique(mdata[[x.i]])
             }
             return(out)
           },
           simplify = FALSE
    )
  )
  dpred$predTC.respScale <- predict(m1, newdata = dpred[ , x], type = "response", allow.new.levels = TRUE) # model predictions for new data values (linear predictors only)
  dpred$predTC.linkScale <- predict(m1, newdata = dpred[ , x], type = "link", allow.new.levels = TRUE) # model predictions for new data values (linear predictors only)
  # dsim$simTC <- simulate(m1, nsim = 100)
  # Residuals are on the link scale (log link). Inverse link is exp(). Residuals are not really well defined anyway.
  # residuals.sim <- DHARMa::simulateResiduals(m1,  n = 500)
  dpred$simTC <- dpred$predTC.linkScale + rnorm(1, mean = 0, sd = model.residual.variance) # model simulation (just one sim) for new data values (linear predictors plus uncertainty)
  dsim <- matrix(NA, nrow = nrow(dpred), ncol = nsim)
  for(i in 1:nsim){
    dsim[,i] <- dpred$predTC.linkScale + rnorm(1, mean = 0, sd = model.residual.variance)
    if(is.null(inv_link_function) == FALSE){
      # get simulation vals back to response scale instead of link scale.
      dsim[ , i] <- do.call(inv_link_function, list(dsim[ , i]))
    }
  }
  return(list(dpred = dpred, dsim = dsim))
}


simsum <- function(sim.df){
  simsum.out <- data.frame(sim.mean = rowMeans(sim.df$dsim))
  simsum.out$lower.95 <- apply(sim.df$dsim, 1, function(x){max(sort(x)[1:round(ncol(sim.df$dsim)*0.025)])})
  simsum.out$upper.95 <- apply(sim.df$dsim, 1, function(x){max(sort(x)[1:round(ncol(sim.df$dsim)*0.975)])})
  simsum.out <- cbind(sim.df$dpred[,1:(ncol(sim.df$dpred) - 3)], simsum.out)
  return(simsum.out)
}


plot.simsum <- function(simsum.in, env_predictor){
  plot.simsum.out <- ggplot(simsum.in)  +
    geom_ribbon(data = simsum.in, aes(x = !!sym(env_predictor), ymin = lower.95, ymax = upper.95, fill = region), alpha = 0.2) +
    geom_line(data = simsum.in, aes(x = !!sym(env_predictor), y = sim.mean,
                                    color = region)) +
    # geom_point(data = m.jet1.sim$dpred, aes(x = env_predictor, y = predTC.respScale,
    # 	color = Location, shape = catDep)) +
    # geom_point(data = cbind(m.jet1.sim$dpred[,1:3], m.jet1.sim$dsim) %>% melt(id.vars = c('catDep', 'Location', 'env_predictor')), aes(x = env_predictor, y = value,
    # 	color = Location, shape = catDep)) +
    # facet_wrap(catDep ~ ., nrow = 2) +
    #facet_wrap(year ~ region, nrow = 2, scales = 'free') +
    ylab("GAM simulated NMDS value (mean +/- 95% CI)") +
    theme_bw()
  return(plot.simsum.out)
}


