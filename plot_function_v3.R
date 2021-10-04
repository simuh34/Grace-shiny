pacman::p_load("tidyverse","openxlsx","Hmisc","rgl","splines","ggplot2")

############################################
#function#
###########################################

#####################################################################
#read-in data
#####################################################################
load("D:/R project/Grace/shiny/v_total_test.RData")




fit_data=data.frame(v_total_test$i,
                    v_total_test$j,
                    v_total_test$k,
                    v_total_test$fit,
                    v_total_test$r, 
                    v_total_test$omega)
colnames(fit_data) = c("i_value", "j_value", "k_value", "fit", "r", "omega_value")
#####################################################################
#function for plot
#####################################################################
##############omega=7807
wake_w_df_plot = function(iax, k,omega){
  ustart0 = fit_data%>%filter(i_value==iax&k_value==k&omega_value==omega)
  ustart0$j_value = c(1:89)
  ustart=c(ustart0[c(1:89),4])
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
 
  
  
 
  
  
  
  
  
  
  plot_data = data.frame(a = c(1:87), b = ustart2)
  highlight_p = data.frame(f = c(leftloc, locmin_iax, rightloc), 
                           g = c(plot_data[leftloc,]$b, 
                                 plot_data[locmin_iax,]$b, 
                                 plot_data[rightloc,]$b))

  
  

  ggplot(data = plot_data, aes(x = a, y = b)) +
    geom_point(pch = 1)+
    geom_point(data = highlight_p, aes(x=f,y=g), color='red', size = 2, pch = 19)
  
  
  
}

