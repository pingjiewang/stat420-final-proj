
get_boxcox_lambda = function(model1){
  boxcot_out=boxcox(model1, plotit = FALSE, lambda = seq(-0.5, 1.0, by = 0.05))
  lambda=boxcot_out$x[which.max(boxcot_out$y)]
  return(lambda)
}

subset_autodata_with_boxcox =function(data){
  
  autos_factor_groups=data %>% count (seller,offerType,abtest,vehicleType,gearbox,model,brand,fuelType,notRepairedDamage)
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
    model = lm(price ~ powerPS + kilometer , data = autos_1,y=TRUE, qr=TRUE)
    lambda_bc[g] = get_boxcox_lambda(model)
  }
  return (lambda_bc)
}

