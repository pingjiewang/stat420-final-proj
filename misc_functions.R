#Function to find the best transformation of the form considered by the Box-Cox method
get_boxcox_lambda = function(model1){
  boxcot_out=boxcox(model1, plotit = FALSE, lambda = seq(-0.5, 1.0, by = 0.05))
  lambda=boxcot_out$x[which.max(boxcot_out$y)]
  return(lambda)
}

#Function to subset the data into groups and considering groups that appear more than 300 times.
subset_autodata_with_boxcox =function(data,input_formula){
  
  autos_factor_groups=data %>%count(abtest,vehicleType,gearbox,fuelType,notRepairedDamage)
  autos_factor_groups=autos_factor_groups[order(autos_factor_groups$n,decreasing = TRUE),]
  autos_factor_groups=autos_factor_groups[autos_factor_groups$n>300,]
  nGroup=nrow(autos_factor_groups)
  #nGroup=5
  lambda_bc=rep(0,nGroup)
  
  for (g in 1:nGroup){
    group1=autos_factor_groups[g,]
    group1.size = group1$n
    group1=subset(group1, select = -c(n) )
    
    autos_1=data
    cols=names(group1)
    for (i in 1:ncol(group1)){
      idx = autos_1[,cols[i]]==group1[[i]]
      autos_1=autos_1[idx,]
    }
    model = lm(input_formula, data = autos_1,y=TRUE, qr=TRUE)
    lambda_bc[g] = get_boxcox_lambda(model)
  }
  return (lambda_bc)
}

#Function to remove influential points
remove_high_influential_points_and_refit_model = function (model, data1){
  ret = list()
  #finding influenctial
  cd = cooks.distance(model)
  n=length(resid(model))
  high_infl = cd > 4 / n
  ret[["removed.n"]]=sum(high_infl) 
  ret[["removed.fraction"]]=mean(high_infl)
  #Refit the multiple regression model without any influential points
  formula=as.formula(as.character(model$call[2]))
  model_new = lm(formula, data = data1, subset = !high_infl)
  ret[["new.model"]]=model_new
  par(mfrow=c(1,2))
  plot(fitted(model_new), resid(model_new), col = "dodgerblue", 
       xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals with Box-cox")
  abline(h = 0, col = "darkorange", lwd = 2)
  qqnorm(resid(model_new), main = "Normal Q-Q Plot with Box-cox", col = "dodgerblue")
  qqline(resid(model_new), col = "dodgerblue", lwd = 2)
  return(ret)
}

#Function to plot Fitted vs. Residuals and Q-Q plot
diagnostics = function(model, pcol="dodgerblue",lcol="orange",alpha=0.05,plotit=TRUE){
  if (plotit ){
    #fitted vs. residual
    par(mfrow=c(1,2))
    plot( model$fitted.values,
          model$residuals, 
          col=pcol,
          xlab="Fitted",
          ylab="Residuals",
          main="Fitted versus residuals" )
    abline(h=0,col=lcol,lwd=1)
    
    #QQ plot
    qqnorm(resid(model),main="Normal Q-Q Plot",col=pcol)
    qqline(resid(model),col=lcol,lwd=1)  
  }
}

#Function to calculate RMSE
calcrmse  = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
