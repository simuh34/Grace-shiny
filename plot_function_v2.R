pacman::p_load("tidyverse","openxlsx","Hmisc","rgl","splines","ggplot2")

############################################
#function#
###########################################

#####################################################################
#read-in data
#####################################################################
load("v_total_test.RData")

v_total_test_1 = v_total_test %>% 
  mutate(j = ifelse(j>0,ifelse(j>89,ifelse(j>178,j-2*89,j-89),j),j+89))
v_total_test_1$j = 90 - v_total_test_1$j


fit_data=data.frame(v_total_test_1$i,
                    v_total_test_1$j,
                    v_total_test_1$k,
                    v_total_test_1$fit,
                    v_total_test_1$r, 
                    v_total_test_1$omega)
colnames(fit_data) = c("i_value", "j_value", "k_value", "fit", "r", "omega_value")
#####################################################################
#function for plot
#####################################################################
##############omega=7807
wake_w_df_plot = function(iax, k){
  ustart0 = fit_data%>%filter(i_value==iax&k_value==k&omega_value==7808)
  ustart=c(ustart0[,4])
  dthetexa = 2 * pi /87/22
  thetexa = seq(pi/2, pi/2 + (87-1)*dthetexa, by = dthetexa)
  midthet = round(length(thetexa)/2)
  
  minval = min(ustart)
  tloc = which.min(ustart)
  
  #transform wake
  ishift = ifelse(tloc <= midthet, midthet - tloc, tloc - midthet)
  ustart1 = ustart[-c(1,89)]
  n = 87
  if (tloc <= midthet){
    ustart2 = ustart1[ c((n-ishift):n, 1:(n-ishift-1))]
  } else {
    if (ishift == 1){
      ustart2 = ustart1[c((ishift+1):n, 1)]
    }else{
      ustart2 = ustart1[c(ishift:n, 1:(ishift-1))]      
    }
  }
  
  #find right/left location
  fit1 = splinefun(thetexa, ustart2)
  
  d1 = fit1(thetexa, deriv = 1)
  d2 = fit1(thetexa, deriv = 2)
  
  j = midthet + 3
  while((j < 87) & sign(d1[j]) == sign(d1[midthet+3])){
    j = j + 1}
  if (j < 87){
    rightloc = j
  } else{
    while(abs(d1[j]-d1[j-1]) < 30 & j > midthet){
      j = j - 1
    }
    rightloc = j
  }
  
  i = midthet - 3
  while((i > 1) & sign(d1[i]) == sign(d1[midthet-3])){
    i = i - 1}
  leftloc = ifelse(i > 0, i, 1)
  if (i > 1){
    leftloc = i
  } else {
    while(abs(d1[i]-d1[i+1]) < 30 & i < midthet){
      i = i +1
    }
    leftloc = i
  }
  
  #  minval_iax = min(ustart2)
  locmin_iax = which.min(ustart2)
  
  #################
  #omega=11074
  #################
  
  ustart0_1 = fit_data%>%filter(i_value==iax&k_value==k&omega_value==11074)
  ustart_1=c(ustart0_1[,4])
  dthetexa = 2 * pi /87/22
  thetexa = seq(pi/2, pi/2 + (87-1)*dthetexa, by = dthetexa)
  midthet = round(length(thetexa)/2)
  
  minval_1 = min(ustart_1)
  tloc_1 = which.min(ustart_1)
  
  #transform wake
  ishift_1 = ifelse(tloc_1 <= midthet, midthet - tloc_1, tloc_1 - midthet)
  ustart1_1 = ustart_1[-c(1,89)]
  n = 87
  if (tloc_1 <= midthet){
    ustart2_1 = ustart1_1[ c((n-ishift_1):n, 1:(n-ishift_1-1))]
  } else {
    if (ishift_1 == 1){
      ustart2_1 = ustart1_1[c((ishift_1+1):n, 1)]
    }else{
      ustart2_1 = ustart1_1[c(ishift_1:n, 1:(ishift_1-1))]      
    }
  }
  
  #find right/left location
  fit1_1 = splinefun(thetexa, ustart2_1)
  
  d1_1 = fit1_1(thetexa, deriv = 1)
  
  j = midthet + 3
  while((j < 87) & sign(d1_1[j]) == sign(d1_1[midthet+3])){
    j = j + 1}
  if (j < 87){
    rightloc_1 = j
  } else{
    while(abs(d1_1[j]-d1_1[j-1]) < 30 & j > midthet){
      j = j - 1
    }
    rightloc_1 = j
  }
  
  i = midthet - 3
  while((i > 1) & sign(d1_1[i]) == sign(d1_1[midthet-3])){
    i = i - 1}
  leftloc_1 = ifelse(i > 0, i, 1)
  if (i > 1){
    leftloc_1 = i
  } else {
    while(abs(d1_1[i]-d1_1[i+1]) < 30 & i < midthet){
      i = i +1
    }
    leftloc_1 = i
  }
  
  
  locmin_iax_1 = which.min(ustart2_1)
  
  
  
  #############
  #omega=12657
  #############
  
  ustart0_2 = fit_data%>%filter(i_value==iax&k_value==k&omega_value==12657)
  ustart_2=c(ustart0_2[,4])
  dthetexa = 2 * pi /87/22
  thetexa = seq(pi/2, pi/2 + (87-1)*dthetexa, by = dthetexa)
  midthet = round(length(thetexa)/2)
  
  minval_2 = min(ustart_2)
  tloc_2 = which.min(ustart_2)
  
  #transform wake
  ishift_2 = ifelse(tloc_2 <= midthet, midthet - tloc_2, tloc_2 - midthet)
  ustart1_2 = ustart_2[-c(1,89)]
  n = 87
  if (tloc_2 <= midthet){
    ustart2_2 = ustart1_2[ c((n-ishift_2):n, 1:(n-ishift_2-1))]
  } else {
    if (ishift_2 == 1){
      ustart2_2 = ustart1_2[c((ishift_2+1):n, 1)]
    }else{
      ustart2_2 = ustart1_2[c(ishift_2:n, 1:(ishift_2-1))]      
    }
  }
  
  #find right/left location
  fit1_2 = splinefun(thetexa, ustart2_2)
  
  d1_2 = fit1_2(thetexa, deriv = 1)
  
  j = midthet + 3
  while((j < 87) & sign(d1_2[j]) == sign(d1_2[midthet+3])){
    j = j + 1}
  if (j < 87){
    rightloc_2 = j
  } else{
    while(abs(d1_2[j]-d1_2[j-1]) < 30 & j > midthet){
      j = j - 1
    }
    rightloc_2 = j
  }
  
  i = midthet - 3
  while((i > 1) & sign(d1_2[i]) == sign(d1_2[midthet-3])){
    i = i - 1}
  leftloc_2 = ifelse(i > 0, i, 1)
  if (i > 1){
    leftloc_2 = i
  } else {
    while(abs(d1_2[i]-d1_2[i+1]) < 30 & i < midthet){
      i = i +1
    }
    leftloc_2 = i
  }
  locmin_iax_2 = which.min(ustart2_2)

  
  
  plot_data = data.frame(a = c(1:87), b = ustart2$fit, c=ustart2_1$fit, d=ustart2_2$fit)
  
  highlight_p = data.frame(f = c(ustart2[leftloc,5]*ustart2[leftloc,7], 
                                 ustart2[locmin_iax,5]*ustart2[locmin_iax,7], 
                                 ustart2[rightloc,5]*ustart2[rightloc,7]), 
                           g = c(plot_data[leftloc,]$b, 
                                 plot_data[locmin_iax,]$b, 
                                 plot_data[rightloc,]$b))  
  
  highlight_p_1 = data.frame(f = c(ustart2_1[leftloc_1,5]*ustart2_1[leftloc_1,7], 
                                   ustart2_1[locmin_iax_1,5]*ustart2_1[locmin_iax_1,7], 
                                   ustart2_1[rightloc_1,5]*ustart2_1[rightloc_1,7]), 
                             g = c(plot_data[leftloc_1,]$c, 
                                   plot_data[locmin_iax_1,]$c, 
                                   plot_data[rightloc_1,]$c))
  
  highlight_p_2 = data.frame(f = c(ustart2_2[leftloc_2,5]*ustart2_2[leftloc_2,7], 
                                   ustart2_2[locmin_iax_2,5]*ustart2_2[locmin_iax_2,7], 
                                   ustart2_2[rightloc_2,5]*ustart2_2[rightloc_2,7]), 
                             g = c(plot_data[leftloc_2,]$d, 
                                   plot_data[locmin_iax_2,]$d, 
                                   plot_data[rightloc_2,]$d))  
  
  plot_data2=plot_data %>% tidyr::gather("omega", "value", 2:4)
  plot_data2$th[1:87] = ustart2$th
  plot_data2$th[88:174] = ustart2_1$th
  plot_data2$th[175:261] = ustart2_2$th
  plot_data2$r[1:87] = ustart2$r
  plot_data2$r[88:174] = ustart2_1$r
  plot_data2$r[175:261] = ustart2_2$r
  plot_data2$rth = plot_data2$r*plot_data2$th
  plot_data2$omega[plot_data2$omega=="b"]=7808
  plot_data2$omega[plot_data2$omega=="c"]=11074
  plot_data2$omega[plot_data2$omega=="d"]=12657
  plot_data2$omega = factor(plot_data2$omega, levels = c("7808","11074","12657"))
  
  ggplot(data = plot_data2, aes(x = rth, y = value,colour=omega)) +
    geom_point(pch = 1)+
    geom_point(data = highlight_p, aes(x=f,y=g), color='red', size = 2, pch = 19)+
    geom_point(data = highlight_p_1, aes(x=f,y=g), color='red', size = 2, pch = 19)+
    geom_point(data = highlight_p_2, aes(x=f,y=g), color='red', size = 2, pch = 19)+
    xlab("R*Theta") + ylab("Total Velocity") 
  
  
}
