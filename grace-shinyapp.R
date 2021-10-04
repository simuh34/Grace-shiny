library(shiny)
library(plotly)
library(tidyverse)
library(DT) 
library(shinythemes)
library(shinydashboard)
library(magrittr)
library(shinyjs)
pacman::p_load("tidyverse","openxlsx","Hmisc","rgl","splines","ggplot2", "stringr", "plotrix", "RColorBrewer")
options(rgl.useNULL = TRUE)

# modeling part
# Data Import

setwd("D:\\R project\\Grace\\shiny\\final_plots")
load("prediction/r_test.RData")
load("prediction/r_train.RData")
load("prediction/th_test.RData")
load("prediction/th_train.RData")
load("prediction/v_test.RData")
load("prediction/v_train.RData")

#mars_model <- read.csv("data_test_mars.csv")

# Data recovery
n_blades <- 22
r <- rbind(r_train, r_test) %>%
  cbind(train = c(rep(T, nrow(r_train)), rep(F, nrow(r_test)))) %>%
  mutate(ik = paste(i, k, omega)) %>%
  ungroup() %>%
  select(fit.r = fit, res.r = res, train.r = train, ik)
th <- rbind(th_train, th_test) %>%
  cbind(train = c(rep(T, nrow(th_train)), rep(F, nrow(th_test)))) %>%
  mutate(ik = paste(i, k, omega)) %>%
  ungroup() %>%
  select(fit.th.low = fit, res.th.low = res, train.th = train, ik)
data <- rbind(V_train, V_test) %>%
  cbind(train = c(
    rep(T, nrow(V_train)),
    rep(F, nrow(V_test))
  )) %>%
  mutate(ik = paste(i, k, omega)) %>%
  ungroup() %>%
  select(
    i, j, k, omega, ik,
    x, r, th,
    percent.th = th_percent, V,
    fit.V = fit, res.V = res, train.V = train
  ) %>%
  right_join(r, by = "ik") %>%
  right_join(th, by = "ik") %>%
  mutate(
    fit.th = (percent.th - 0.5) * 2 * pi / n_blades + fit.th.low,
    res.th = fit.th - th
  ) %>%
  select(-ik)
rm(r, th)

# Plot of training (red) & testing (blue)
tt_plot_fun = function(y_tt, k_tt, o_tt, data){
  
  MD <- y_tt
  OM <- o_tt # input the speed of the rotor
  K <- k_tt
  
  dfp <- data %>% filter(omega == OM)
  # dfp$col <- ifelse(dfp[, paste("train.", MD, sep = "")], "red", "blue")
  if(K%in%seq(1,nrow(dfp),2))  
  {
    dfp$col <- "#87CEFA"
    dfp$col[dfp$k==K] <- "#008B00"
  }
  else
  {
    dfp$col <- "#87CEFA"
    dfp$col[dfp$k==K] <- "#CD3333"
  }
  dfp
}



tt_plot_fun_r = function(o_tt_a, data){
  OM <- o_tt_a
  viewpoint <- c(-45, 30, 30) # angle of the view
  zoom <- 1.1 # zoom times
  dfp <- data %>% filter(omega == OM)
  dfp$col <- color.scale(dfp$r, c(0, 1, 1), c(1, 1, 0), 0)
  plot3d(dfp$x, dfp$r * cos(dfp$th), dfp$r * sin(dfp$th),
         xlab = "", ylab = "", zlab = "",
         # main = paste("Omega =", OM),
         col = dfp$col, box = F, axes = F)
  box3d()
  rgl.viewpoint(viewpoint[1], viewpoint[2], viewpoint[3], zoom = zoom)
}

tt_plot_fun_th = function(o_tt_a, data){
  OM <- o_tt_a
  viewpoint <- c(-45, 30, 30) # angle of the view
  zoom <- 1.1 # zoom times
  dfp <- data %>% filter(omega == OM)
  dfp$col <- color.scale(dfp$th, c(0, 1, 1), c(1, 1, 0), 0)
  plot3d(dfp$x, dfp$r * cos(dfp$th), dfp$r * sin(dfp$th),
         xlab = "", ylab = "", zlab = "",
         # main = paste("Omega =", OM),
         col = dfp$col, box = F, axes = F)
  box3d()
  rgl.viewpoint(viewpoint[1], viewpoint[2], viewpoint[3], zoom = zoom)
}

tt_plot_fun_V = function(o_tt_a, data){
  OM <- o_tt_a
  viewpoint <- c(-45, 30, 30) # angle of the view
  zoom <- 1.1 # zoom times
  dfp <- data %>% filter(omega == OM)
  dfp$col <- color.scale(dfp$V, c(0, 1, 1), c(1, 1, 0), 0)
  plot3d(dfp$x, dfp$r * cos(dfp$th), dfp$r * sin(dfp$th),
         xlab = "", ylab = "", zlab = "",
         # main = paste("Omega =", OM),
         col = dfp$col, box = F, axes = F)
  box3d()
  rgl.viewpoint(viewpoint[1], viewpoint[2], viewpoint[3], zoom = zoom)
}

omega_app <- data %>% filter(omega == 7808)
omega_to  <- data %>% filter(omega == 11074)
omega_cb  <- data %>% filter(omega == 12657)

# define functions of plot
res_filter <- function(data, om, fix, value) {
  if (fix == "i") {
    data[data$omega == om & data$i == value, ]
  } else if (fix == "k") data[data$omega == om & data$k == value, ]
}
res_plot1 <- function(data) {
  f <- ifelse(length(unique(data$i)) == 1, "k", "i")
  g <- ifelse(length(unique(data$i)) == 1, unique(data$i), unique(data$k))
  gi <- ifelse(length(unique(data$i)) == 1, "i", "k")
  if (f == "k") {
    data <- data.frame(
      x = c(data$th*data$r, data$th*data$r),
      y = c(data$V, data$fit.V),
      f = c(data$k, data$k),
      c = c(rep("V", nrow(data)), rep("fit.V", nrow(data)))
    )
  }
  if (f == "i") {
    data <- data.frame(
      x = c(data$th*data$r, data$th*data$r),
      y = c(data$V, data$fit.V),
      f = c(data$i, data$i),
      c = c(rep("V", nrow(data)), rep("fit.V", nrow(data)))
    )
  }
  plot_ly(data) %>%
    add_lines(
      x = ~x, y = ~y, frame = ~f, color = ~c,
      type = "scatter", mode = "lines", color = I("blue")
    ) %>%
    animation_opts(100) %>%
    animation_slider(currentvalue = list(
      prefix = paste(f, ": "),
      font = list(color = "red")
    )) %>%
    layout(
      title = paste("Velocity VS r * Theta with", gi, "=", g),
      xaxis = list(title = "r * th, length of arc"),
      yaxis = list(title = "v, velocity, ft/s"),
      hovermode = "x unified"
    )
}
res_plot2 <- function(data) {
  f <- ifelse(length(unique(data$i)) == 1, "k", "i")
  g <- ifelse(length(unique(data$i)) == 1, unique(data$i), unique(data$k))
  gi <- ifelse(length(unique(data$i)) == 1, "i", "k")
  plot_ly(data,
          x = ~fit.V, y = ~V - res.V, frame = ~ get(f),
          type = "scatter", mode = "markers", color = I("blue"), size = I(1)
  ) %>%
    animation_opts(100) %>%
    animation_slider(currentvalue = list(
      prefix = paste(f, ": "),
      font = list(color = "red")
    )) %>%
    layout(
      title = paste("Residual Plots with", gi, "=", g),
      xaxis = list(title = "Fitted"),
      yaxis = list(title = "Residuals")
    )
  
}

# wake deficit part

dk <- get(load("wd_data.Rdata"))
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
                    v_total_test_1$omega,
                    v_total_test_1$th)
colnames(fit_data) = c("i_value", "j_value", "k_value", "fit", "r", "omega_value", "th")
#####################################################################
#function for plot
#####################################################################
##############omega=7807
wake_w_df_plot = function(iax, k){
  ustart0 = fit_data%>%filter(i_value==iax&k_value==k&omega_value==7808)
  ustart=ustart0
  dthetexa = 2 * pi /87/22
  thetexa = seq(pi/2, pi/2 + (87-1)*dthetexa, by = dthetexa)
  midthet = round(length(thetexa)/2)
  
  minval = min(ustart$fit)
  tloc = which.min(ustart$fit)
  
  #transform wake
  ishift = ifelse(tloc <= midthet, midthet - tloc, tloc - midthet)
  ustart1 = ustart[-c(1,89),]
  n = 87
  if (tloc <= midthet){
    ustart2 = ustart1[ c((n-ishift):n, 1:(n-ishift-1)),]
  } else {
    if (ishift == 1){
      ustart2 = ustart1[c((ishift+1):n, 1),]
    }else{
      ustart2 = ustart1[c(ishift:n, 1:(ishift-1)),]      
    }
  }
  
  #find right/left location
  fit1 = splinefun(thetexa, ustart2$fit)
  
  d1 = fit1(thetexa, deriv = 1)
  
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
  locmin_iax = which.min(ustart2$fit)
  
  #################
  #omega=11074
  #################
  
  ustart0_1 = fit_data%>%filter(i_value==iax&k_value==k&omega_value==11074)
  ustart_1=ustart0_1
  dthetexa = 2 * pi /87/22
  thetexa = seq(pi/2, pi/2 + (87-1)*dthetexa, by = dthetexa)
  midthet = round(length(thetexa)/2)
  
  minval_1 = min(ustart_1$fit)
  tloc_1 = which.min(ustart_1$fit)
  
  #transform wake
  ishift_1 = ifelse(tloc_1 <= midthet, midthet - tloc_1, tloc_1 - midthet)
  ustart1_1 = ustart_1[-c(1,89),]
  n = 87
  if (tloc_1 <= midthet){
    ustart2_1 = ustart1_1[ c((n-ishift_1):n, 1:(n-ishift_1-1)),]
  } else {
    if (ishift_1 == 1){
      ustart2_1 = ustart1_1[c((ishift_1+1):n, 1),]
    }else{
      ustart2_1 = ustart1_1[c(ishift_1:n, 1:(ishift_1-1)),]      
    }
  }
  
  #find right/left location
  fit1_1 = splinefun(thetexa, ustart2_1$fit)
  
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
  
  
  
  locmin_iax_1 = which.min(ustart2_1$fit)
  
  
  
  #############
  #omega=12657
  #############
  
  ustart0_2 = fit_data%>%filter(i_value==iax&k_value==k&omega_value==12657)
  ustart_2=ustart0_2
  dthetexa = 2 * pi /87/22
  thetexa = seq(pi/2, pi/2 + (87-1)*dthetexa, by = dthetexa)
  midthet = round(length(thetexa)/2)
  
  minval_2 = min(ustart_2$fit)
  tloc_2 = which.min(ustart_2$fit)
  
  #transform wake
  ishift_2 = ifelse(tloc_2 <= midthet, midthet - tloc_2, tloc_2 - midthet)
  ustart1_2 = ustart_2[-c(1,89),]
  n = 87
  if (tloc_2 <= midthet){
    ustart2_2 = ustart1_2[ c((n-ishift_2):n, 1:(n-ishift_2-1)),]
  } else {
    if (ishift_2 == 1){
      ustart2_2 = ustart1_2[c((ishift_2+1):n, 1),]
    }else{
      ustart2_2 = ustart1_2[c(ishift_2:n, 1:(ishift_2-1)),]      
    }
  }
  
  #find right/left location
  fit1_2 = splinefun(thetexa, ustart2_2$fit)
  
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
  locmin_iax_2 = which.min(ustart2_2$fit)
  
  
  
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

# shiny app
ui <- tagList(
  
  navbarPage(
    # Application title
    "GRACE",
    # Theme 
    theme = shinytheme("lumen"),
    position = "fixed-top",
    
    
    tabPanel("Plot of training & testing",
             br(),
             br(),
             br(),
             sidebarLayout(
               sidebarPanel(
                 style = "position:fixed;width:inherit;",
                 selectInput("set_y", label = "The Y of the model:",
                             choices = c("r",
                                         "th",
                                         "Velocity"
                             )),
                 helpText("Set omega value in the selected dataset."),
                 selectInput("set_o", label = "The value of omega(rpm):",
                             choices = c("7808-APP",
                                         "11074-TO",
                                         "12657-CB"
                             )),
                 helpText("Set k value in the selected dataset."),
                 sliderInput("k_value_tt", label = "The value of k:", min = 20, 
                             max = 50, value = 38),
                 
                 width = 2
                 
                 
                 
               ),
               
               mainPanel(
                 
                 column(7, rglwidgetOutput("tt_plot"),
                        verbatimTextOutput("tt_v")),
                 
                 column(3, rglwidgetOutput("tt_plot_k"))
                 
                 
               ))),
    
    
    tabPanel("Modeling",
             
             br(),
             br(),
             br(),
             tabsetPanel(type = "tabs",
                         tabPanel("Spline model", 
                                  helpText("The raw data is divided into 3 datesets according to different Revolutions Per Minute."),
                                  selectInput("set", label = h5("Choose the dataset:"),
                                              choices = c("rpm7808(APP)",
                                                          "rpm11074(TO)",
                                                          "rpm12657(CB)"
                                              )),
                                  
                                  helpText("We can choose to fix i value OR k value in the selected dataset."),
                                  sliderInput("i_value", label = h5("The value of i:"), min = 2, max = 153, value = 100), 
                                  
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Velocity VS r*theta Plot",br(), plotlyOutput("plot_v_i")),
                                              tabPanel("Residual Plot",br(), plotlyOutput("plot_v_r_i")),
                                              tabPanel("Table", DT::dataTableOutput("table_i"),
                                                       downloadButton("downloadData_i", "Download the Selected Dataset")),
                                              
                                              tabPanel("Summary", verbatimTextOutput("summary_i"))),            
                                  br(),            
                                  sliderInput("k_value", label = h5("The value of k:"), min = 20, max = 50, value = 33),
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Velocity VS r*theta Plot",br(), plotlyOutput("plot_v")),
                                              tabPanel("Residual Plot",br(), plotlyOutput("plot_v_r")),
                                              tabPanel("Table", DT::dataTableOutput("table_k"),
                                                       downloadButton("downloadData", "Download the Selected Dataset")),
                                              
                                              tabPanel("Summary", verbatimTextOutput("summary_k"))
                                              
                                              
                                  )),
                         
                         
                         tabPanel("Other model",
                                  fileInput('csv', h5('Input CSV:'), accept = '.csv'),
                                  helpText("The default size of uploaded file is limited by 500M."),
                                  selectInput("set_up", label = h5("Choose the dataset:"),
                                              choices = c("rpm7808(APP)",
                                                          "rpm11074(TO)",
                                                          "rpm12657(CB)"
                                              )),
                                  helpText("We can choose to fix i value OR k value in the selected dataset."),
                                  sliderInput("i_value_up", label = h5("The value of i:"), min = 2, max = 153, value = 60),
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Velocity VS r*theta Plot",br(), plotlyOutput("plot_v_i_up")),
                                              tabPanel("Residual Plot",br(), plotlyOutput("plot_v_r_i_up")),
                                              tabPanel("Table", DT::dataTableOutput("table_i_up")),
                                              tabPanel("Summary", verbatimTextOutput("summary_i_up"))),
                                  sliderInput("k_value_up", label = h5("The value of k:"), min = 20, max = 50, value = 33),
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Velocity VS r*theta Plot",br(), plotlyOutput("plot_v_up")),
                                              tabPanel("Residual Plot",br(), plotlyOutput("plot_v_r_up")),
                                              tabPanel("Table", DT::dataTableOutput("table_k_up")),
                                              
                                              tabPanel("Summary", verbatimTextOutput("summary_k_up"))
                                  )
                         )
             )),
    
    
    
    tabPanel("Wake deficit and width",
             
             br(),
             br(),
             br(),
             tabsetPanel(type = "tabs",
                         tabPanel("Plot", 
                                  sidebarLayout(
                                    sidebarPanel(
                                      
                                      
                                      helpText("Input i and k values to get the deficit and width of the wake."),
                                      sliderInput(inputId = "wake_k", "Input k value: ", min = 21, max = 49, value = 33, step = 2),
                                      
                                      sliderInput(inputId = "wake_i", "Input i value: ", min = 2, max = 153, value = 33)
                                    ),
                                    
                                    mainPanel(
                                      tableOutput("table_wake"),
                                      plotOutput("plot_wake")
                                    )
                                  )),
                         
                         tabPanel("Data",
                                  sidebarLayout(
                                    sidebarPanel (
                                      
                                      
                                      helpText("The value of i and k can be inputted as a specific value or a range."),
                                      
                                      sliderInput("s_w_i", "Set the range of i values:",
                                                  min = 2, max = 153, value = c(60, 90)),
                                      sliderInput("s_w_k", "Set the range of k values:",
                                                  min = 20, max = 50, value = c(30, 40)),
                                      
                                      
                                      downloadButton("downloadData_wake", "Download the Selected Dataset", class = "btn-primary")
                                    ),
                                    mainPanel(
                                      tableOutput("table_wake_r")
                                    )
                                  )                   
                         ),
                         tabPanel("Upload files", 
                                  sidebarLayout(
                                    sidebarPanel(
                                      
                                      fileInput('csv_wake', h5('Input CSV file of the wake deficit value:'), accept = '.csv'),
                                      fileInput('csv_wake_plot', h5('Input CSV file of the velocity:'), accept = '.csv'),
                                      helpText("The default size of uploaded file is limited by 500M."),
                                      
                                      helpText("Input i and k values to get the deficit and width of the wake."),
                                      sliderInput(inputId = "wake_k_up", "Input k value: ", min = 21, max = 49, value = 20),
                                      
                                      sliderInput(inputId = "wake_i_up", "Input i value: ", min = 2, max = 153, value = 2)
                                    ),
                                    
                                    mainPanel(
                                      tableOutput("table_wake_up"),
                                      plotOutput("plot_wake_up")
                                    )
                                  ))
                         
             )
    )
  ))

server <- function(input, output) {
  
  options(shiny.maxRequestSize=500*1024^2)
  
  # tt_plot
  output$tt_plot <- renderRglwidget({
    
    if (input$set_y== "r"){
      MD <- "r"
    }
    else if(input$set_y== "th"){
      MD <- "th"
    }
    else if(input$set_y== "Velocity"){
      MD <- "V"}
    
    if (input$set_o== "7808-APP"){
      OM <- 7808
    }
    else if(input$set_o== "11074-TO"){
      OM <- 11074
    }
    else if(input$set_o== "12657-CB"){
      OM <- 12657
    }
    dfp <- tt_plot_fun(MD, input$k_value_tt, OM, data)
    viewpoint <- c(-45, 30, 30) # angle of the view
    zoom <- 1.1 # zoom times
    close3d()
    plot3d(dfp$x, dfp$r * cos(dfp$th), dfp$r * sin(dfp$th),
           # xlab = "x", ylab = "y", zlab = "z",
           # main = paste(MD, ", K = ", K, ", Omega = ", OM),
           col = dfp$col, box = F, axes = F, xlab="", ylab="", zlab="")
    box3d()
    par3d(windowRect = c(0, 0, 512, 512))
    legend3d("topleft", legend = c('All data', 'Training set', 'Testing set'), pch = 16, col = c('#87CEFA', '#CD3333', '#008B00'))
    rgl.viewpoint(viewpoint[1], viewpoint[2], viewpoint[3], zoom = zoom)
    x=scene3d()
    close3d()
    rglwidget(x)
  })
  
  output$tt_v <- renderText({ 
    
    if(input$k_value_tt %% 2 == 0) {
      tt_k_tt <- "training set."
    }
    
    else 
    {tt_k_tt <- "testing set."}
    
    paste("The selected dataset is the", tt_k_tt)
    
  })
  
  output$tt_plot_k <- renderRglwidget({
    if (input$set_o== "7808-APP"){
      OM <- 7808
    }
    else if(input$set_o== "11074-TO"){
      OM <- 11074
    }
    else if(input$set_o== "12657-CB"){
      OM <- 12657
    }
    if (input$set_y== "r"){
      tt_plot_fun_r(OM, data)
      x=scene3d()
      close3d()
      rglwidget(x)
    }
    else if(input$set_y== "th"){
      tt_plot_fun_th(OM, data)
      x=scene3d()
      close3d()
      rglwidget(x)
    }
    else if(input$set_y== "Velocity"){
      tt_plot_fun_V(OM, data)
      x=scene3d()
      close3d()
      rglwidget(x)
    }
    
    
    
  })
  
  
  ##########################################################################
  # tt_k
  # wake modeling
  # modeling
  
  table_input <- reactive({
    if (input$set== "rpm7808(APP)"){
      omega_app <- filter(omega_app, k == input$k_value)
      omega_app
    }
    else if(input$set== "rpm11074(TO)"){
      omega_to <- filter(omega_to, k == input$k_value)
      omega_to
    }
    else if(input$set== "rpm12657(CB)"){
      omega_cb <- filter(omega_cb, k == input$k_value)
      omega_cb
    }
  })
  
  selec_k<- reactive({
    if (input$set== "rpm7808(APP)"){
      
      omega <- 7808
      fix <- "k"
      value <- input$k_value
      df_p <- res_filter(omega_app, omega, fix, value)
      
      res_plot1(df_p)
    }
    else if(input$set== "rpm11074(TO)"){
      
      omega <- 11074
      fix <- "k"
      value <- input$k_value
      df_p <- res_filter(omega_to, omega, fix, value)
      
      res_plot1(df_p)
      
    }else if(input$set== "rpm12657(CB)"){
      
      omega <- 12657
      fix <- "k"
      value <- input$k_value
      df_p <- res_filter(omega_cb, omega, fix, value)
      
      res_plot1(df_p)
    }
  })
  
  selec_k_r<- reactive({
    if (input$set== "rpm7808(APP)"){
      
      omega <- "7808"
      fix <- "k"
      value <- input$k_value
      df_p <- res_filter(omega_app, omega, fix, value)
      
      res_plot2(df_p)
    }
    else if(input$set== "rpm11074(TO)"){
      
      omega <- "11074"
      fix <- "k"
      value <- input$k_value
      df_p <- res_filter(omega_to, omega, fix, value)
      
      res_plot2(df_p)
      
    }else if(input$set== "rpm12657(CB)"){
      
      omega <- "12657"
      fix <- "k"
      value <- input$k_value
      df_p <- res_filter(omega_cb, omega, fix, value)
      
      res_plot2(df_p)
      
    }
  })
  
  output$plot_v <- renderPlotly({
    print(selec_k())
    
  })
  
  output$plot_v_r <- renderPlotly({
    print(selec_k_r())
    
  })
  
  output$summary_k <- renderPrint({
    summary(table_input())
  })
  
  output$table_k <- DT::renderDataTable(DT::datatable({
    table_input()
  }))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$set, " - [k=", input$k_value, "].csv", sep = "")
    },
    content = function(file) {
      write.csv(table_input(), file, row.names = FALSE)
    }
  )
  
  table_input_2 <- reactive({
    if (input$set== "rpm7808(APP)"){
      omega_app <- filter(omega_app, i == input$i_value)
      omega_app
    }
    else if(input$set== "rpm11074(TO)"){
      omega_to <- filter(omega_to, i == input$i_value)
      omega_to
    }
    else if(input$set== "rpm12657(CB)"){
      omega_cb <- filter(omega_cb, i == input$i_value)
      omega_cb
    }
  })
  
  selec_i<- reactive({
    if (input$set== "rpm7808(APP)"){
      
      omega <- "7808"
      fix <- "i"
      value <- input$i_value
      df_p <- res_filter(omega_app, omega, fix, value)
      
      
      res_plot1(df_p)
    }
    else if(input$set== "rpm11074(TO)"){
      
      omega <- "11074"
      fix <- "i"
      value <- input$i_value
      df_p <- res_filter(omega_to, omega, fix, value)
      
      
      res_plot1(df_p)
      
    }else if(input$set== "rpm12657(CB)"){
      
      omega <- "12657"
      fix <- "i"
      value <- input$i_value
      df_p <- res_filter(omega_cb, omega, fix, value)
      
      res_plot1(df_p)
    }
  })
  
  selec_i_r<- reactive({
    if (input$set== "rpm7808(APP)"){
      
      omega <- "7808"
      fix <- "i"
      value <- input$i_value
      df_p <- res_filter(omega_app, omega, fix, value)
      
      res_plot2(df_p)
    }
    else if(input$set== "rpm11074(TO)"){
      
      omega <- "11074"
      fix <- "i"
      value <- input$i_value
      df_p <- res_filter(omega_to, omega, fix, value)
      
      res_plot2(df_p)
      
    }else if(input$set== "rpm12657(CB)"){
      
      omega <- "12657"
      fix <- "i"
      value <- input$i_value
      df_p <- res_filter(omega_cb, omega, fix, value)
      
      res_plot2(df_p)
    }
  })
  
  
  output$plot_v_i <- renderPlotly({
    print(selec_i())
    
  })
  
  output$plot_v_r_i <- renderPlotly({
    print(selec_i_r())
    
  })
  
  output$summary_i <- renderPrint({
    summary( table_input_2())
  })
  
  output$table_i <- DT::renderDataTable(DT::datatable({
    table_input_2()
  }))
  
  output$downloadData_i <- downloadHandler(
    filename = function() {
      paste(input$set, " - [i=", input$i_value, "].csv", sep = "")
    },
    
    content = function(file) {
      write.csv(table_input_2(), file, row.names = FALSE)
    }
  )
  
  ###################################################################################
  # other model
  #i
  
  data_input <- reactive({
    infile <- input$csv
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  table_input_o <- reactive({
    if (input$set_up== "rpm7808(APP)"){
      df_mo_s <- filter(data_input(), i == input$i_value_up & omega == 7808)
      
    }
    else if(input$set_up== "rpm11074(TO)"){
      df_mo_s <- filter(data_input(), i == input$i_value_up & omega == 11074)
      
    }
    else if(input$set_up== "rpm12657(CB)"){
      df_mo_s <- filter(data_input(), i == input$i_value_up & omega == 12657)
      
    }
    df_mo_s
  })
  
  selec_i_up<- reactive({
    
    validate(
      need(input$csv, 'Please upload csv file.')
    )
    
    omega_app_u <- data_input() %>% filter(omega == 7808)
    omega_to_u  <- data_input() %>% filter(omega == 11074)
    omega_cb_u  <- data_input() %>% filter(omega == 12657)
    
    if (input$set_up== "rpm7808(APP)"){
      
      omega <- "7808"
      fix <- "i"
      value <- input$i_value_up
      df_p <- res_filter(omega_app_u, omega, fix, value)
      
      
      res_plot1(df_p)
    }
    else if(input$set_up== "rpm11074(TO)"){
      
      omega <- "11074"
      fix <- "i"
      value <- input$i_value_up
      df_p <- res_filter(omega_to_u, omega, fix, value)
      
      
      res_plot1(df_p)
      
    }else if(input$set_up== "rpm12657(CB)"){
      
      omega <- "12657"
      fix <- "i"
      value <- input$i_value_up
      df_p <- res_filter(omega_cb_u, omega, fix, value)
      
      res_plot1(df_p)
    }
  })
  
  selec_i_r_up<- reactive({
    
    validate(
      need(input$csv, 'Please upload csv file.')
    )
    
    omega_app_u <- data_input() %>% filter(omega == 7808)
    omega_to_u  <- data_input() %>% filter(omega == 11074)
    omega_cb_u  <- data_input() %>% filter(omega == 12657)
    
    if (input$set_up== "rpm7808(APP)"){
      
      omega <- "7808"
      fix <- "i"
      value <- input$i_value_up
      df_p <- res_filter(omega_app, omega, fix, value)
      
      res_plot2(df_p)
    }
    else if(input$set_up== "rpm11074(TO)"){
      
      omega <- "11074"
      fix <- "i"
      value <- input$i_value_up
      df_p <- res_filter(omega_to, omega, fix, value)
      
      res_plot2(df_p)
      
    }else if(input$set_up== "rpm12657(CB)"){
      
      omega <- "12657"
      fix <- "i"
      value <- input$i_value_up
      df_p <- res_filter(omega_cb, omega, fix, value)
      
      res_plot2(df_p)
    }
  })
  
  output$plot_v_i_up <- renderPlotly({
    print(selec_i_up())
    
  })
  
  output$plot_v_r_i_up <- renderPlotly({
    print(selec_i_r_up())
    
  })
  
  output$summary_i_up <- renderPrint({
    summary( table_input_o())
  })
  
  output$table_i_up <- DT::renderDataTable(DT::datatable({
    table_input_o()
  }))
  
  #k
  
  table_input_ok <- reactive({
    if (input$set_up== "rpm7808(APP)"){
      df_mo_s <- filter(data_input(), k == input$k_value_up & omega == 7808)
      
    }
    else if(input$set_up== "rpm11074(TO)"){
      df_mo_s <- filter(data_input(), k == input$k_value_up & omega == 11074)
      
    }
    else if(input$set_up== "rpm12657(CB)"){
      df_mo_s <- filter(data_input(), k == input$k_value_up & omega == 12657)
      
    }
    df_mo_s
  })
  
  selec_k_up<- reactive({
    
    validate(
      need(input$csv, 'Please upload csv file.')
    )
    
    omega_app_u <- data_input() %>% filter(omega == 7808)
    omega_to_u  <- data_input() %>% filter(omega == 11074)
    omega_cb_u  <- data_input() %>% filter(omega == 12657)
    
    if (input$set_up== "rpm7808(APP)"){
      
      omega <- "7808"
      fix <- "k"
      value <- input$k_value_up
      df_p <- res_filter(omega_app_u, omega, fix, value)
      
      
      res_plot1(df_p)
    }
    else if(input$set_up== "rpm11074(TO)"){
      
      omega <- "11074"
      fix <- "k"
      value <- input$k_value_up
      df_p <- res_filter(omega_to_u, omega, fix, value)
      
      
      res_plot1(df_p)
      
    }else if(input$set_up== "rpm12657(CB)"){
      
      omega <- "12657"
      fix <- "k"
      value <- input$k_value_up
      df_p <- res_filter(omega_cb_u, omega, fix, value)
      
      res_plot1(df_p)
    }
  })
  
  selec_k_r_up<- reactive({
    
    validate(
      need(input$csv, 'Please upload csv file.')
    )
    
    omega_app_u <- data_input() %>% filter(omega == 7808)
    omega_to_u  <- data_input() %>% filter(omega == 11074)
    omega_cb_u  <- data_input() %>% filter(omega == 12657)
    
    if (input$set_up== "rpm7808(APP)"){
      
      omega <- "7808"
      fix <- "k"
      value <- input$k_value_up
      df_p <- res_filter(omega_app_u, omega, fix, value)
      
      res_plot2(df_p)
    }
    else if(input$set_up== "rpm11074(TO)"){
      
      omega <- "11074"
      fix <- "k"
      value <- input$k_value_up
      df_p <- res_filter(omega_to_u, omega, fix, value)
      
      res_plot2(df_p)
      
    }else if(input$set_up== "rpm12657(CB)"){
      
      omega <- "12657"
      fix <- "k"
      value <- input$k_value_up
      df_p <- res_filter(omega_cb_u, omega, fix, value)
      
      res_plot2(df_p)
    }
  })
  
  output$plot_v_up <- renderPlotly({
    print(selec_k_up())
  })
  
  output$plot_v_r_up <- renderPlotly({
    print(selec_k_r_up())
  })
  
  output$summary_k_up <- renderPrint({
    summary( table_input_ok())
  })
  
  output$table_k_up <- DT::renderDataTable(DT::datatable({
    table_input_ok()
  }))
  
  
  #wake range  
  
  table_wake_i <- reactive({
    
    data <- dk
    
    data_dk <- subset(data, iax >= input$s_w_i[1] & iax <= input$s_w_i[2])
    
    data_dk <- subset(data_dk, k >=  input$s_w_k[1] & k <= input$s_w_k[2])
    
    data_dk
    
  })
  
  output$table_wake_r <- renderTable({
    
    table_wake_i()
    
  })
  
  
  output$downloadData_wake <- downloadHandler(
    filename = function() {
      paste("[i=", input$s_w_i[1],":", input$s_w_i[2],"] &", "[k=", input$s_w_k[1],":",input$s_w_k[2],"].csv", sep = "")
    },
    content = function(file) {
      write.csv(table_wake_i(), file, row.names = FALSE)
    }
  )
  
  
  output$table_wake <- renderTable({
    print(subset(dk, iax == input$wake_i & k == input$wake_k))
  })
  
  
  
  output$plot_wake <- renderPlot({
    print(wake_w_df_plot(input$wake_i, input$wake_k))
  })
  
# upload wake 
  
  data_wake <- reactive({
    infile <- input$csv_wake
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  data_wake_plot <- reactive({
    infile <- input$csv_wake_plot
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  
  output$table_wake_up <- renderTable({
    
    validate(
      need(input$csv_wake, 'Please upload csv file.')
    )
    
    
    print(subset(data_wake(), iax == input$wake_i_up & k == input$wake_k_up))
  })
  output$plot_wake_up <- renderPlot({
    
    validate(
      need(input$csv_wake_plot, ' ')
    )
    
   
    v_total_test_1 = data_wake_plot() %>% 
      mutate(j = ifelse(j>0,ifelse(j>89,ifelse(j>178,j-2*89,j-89),j),j+89))
    v_total_test_1$j = 90 - v_total_test_1$j
    
    fit_data=data.frame(v_total_test_1$i,
                        v_total_test_1$j,
                        v_total_test_1$k,
                        v_total_test_1$fit,
                        v_total_test_1$r, 
                        v_total_test_1$omega,
                        v_total_test_1$th)
    colnames(fit_data) = c("i_value", "j_value", "k_value", "fit", "r", "omega_value", "th")
    print(wake_w_df_plot(input$wake_i_up, input$wake_k_up))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

