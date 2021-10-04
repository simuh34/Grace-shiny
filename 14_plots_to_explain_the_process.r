pacman::p_load(tidyverse, openxlsx, rgl, plotrix)
### IMPORT DATA
load("data_full.RData")             # 'TSWIFT' velocity data of three stages
load("data_low_full.RData")         # 'TSWIFT' data (k 20:60) lowest v point
load("data_full_th.RData")          # 'TSWIFT' data (k 20:60) theta in percentage
### point i=2, j=33, k=35

n_blades=22
per_th <- 2 * pi / n_blades
l_th <- per_th * 0:(n_blades - 1)
dfp <- data_full %>%
  filter(k==35,i==2,j==33,omega==7808) %>%
  select(v_total, x, r, th, omega, i, j, k)
dfp$col <- "#000000"
open3d()
plot3d(dfp$x, dfp$r*cos(dfp$th), dfp$r*sin(dfp$th),xlab="dfp$x",ylab="dfp$y",zlab="dfp$z",  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

dfp <- data.frame(
  v_total = rep(dfp$v_total, n_blades),
  x = rep(dfp$x, n_blades),
  r = rep(dfp$r, n_blades),
  th = rep(dfp$th, n_blades) +
    rep(l_th, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  omega = rep(dfp$omega, n_blades),
  i = rep(dfp$i, n_blades),
  j = rep(dfp$j, n_blades) +
    rep((1:n_blades)-1, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  k = rep(dfp$k, n_blades),
  col = c(rep("#38a5f1", length(dfp$col)),rep(dfp$col, n_blades-1))
)
dfp$y=dfp$r*cos(dfp$th)
dfp$z=dfp$r*sin(dfp$th)
open3d()
plot3d(dfp$x, dfp$y, dfp$z,  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

### cycle i=2, j = 1 to 89, k=35 

n_blades=22
per_th <- 2 * pi / n_blades
l_th <- per_th * 0:(n_blades - 1)
dfp <- data_full %>%
  filter(k==35,i==2,omega==7808) %>%
  select(v_total, x, r, th, omega, i, j, k)
dfp$col <- color.scale(dfp$v_total, c(0, 1, 1), c(1, 1, 0), 0)
dfp$col[dfp$j%in%c(1:2,88:89)]="#000000"
open3d()
plot3d(dfp$x, dfp$r*cos(dfp$th), dfp$r*sin(dfp$th),xlab="dfp$x",ylab="dfp$y",zlab="dfp$z",  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

dfp <- data.frame(
  v_total = rep(dfp$v_total, n_blades),
  x = rep(dfp$x, n_blades),
  r = rep(dfp$r, n_blades),
  th = rep(dfp$th, n_blades) +
    rep(l_th, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  omega = rep(dfp$omega, n_blades),
  i = rep(dfp$i, n_blades),
  j = rep(dfp$j, n_blades) +
    rep((1:n_blades)-1, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  k = rep(dfp$k, n_blades),
  col = c(rep("#38a5f1", length(dfp$col)),rep(dfp$col, n_blades-1))
)
dfp$y=dfp$r*cos(dfp$th)
dfp$z=dfp$r*sin(dfp$th)
open3d()
plot3d(dfp$x, dfp$y, dfp$z,  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

### bucket i=2 to 153, j = 1 to 89, k=35 
n_blades=22
per_th <- 2 * pi / n_blades
l_th <- per_th * 0:(n_blades - 1)
dfp <- data_full %>%
  filter(k==35,omega==7808) %>%
  select(v_total, x, r, th, omega, i, j, k)
dfp$col <- color.scale(dfp$v_total, c(0, 1, 1), c(1, 1, 0), 0)
dfp$col[dfp$j%in%c(1:2,88:89)]="#000000"
open3d()
plot3d(dfp$x, dfp$r*cos(dfp$th), dfp$r*sin(dfp$th),xlab="dfp$x",ylab="dfp$y",zlab="dfp$z",  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

dfp <- data.frame(
  v_total = rep(dfp$v_total, n_blades),
  x = rep(dfp$x, n_blades),
  r = rep(dfp$r, n_blades),
  th = rep(dfp$th, n_blades) +
    rep(l_th, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  omega = rep(dfp$omega, n_blades),
  i = rep(dfp$i, n_blades),
  j = rep(dfp$j, n_blades) +
    rep((1:n_blades)-1, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  k = rep(dfp$k, n_blades),
  col = c(rep("#38a5f1", length(dfp$col)),rep(dfp$col, n_blades-1))
)
dfp$y=dfp$r*cos(dfp$th)
dfp$z=dfp$r*sin(dfp$th)
open3d()
plot3d(dfp$x, dfp$y, dfp$z,  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

### buckets in buckets  i=2 to 153, j = 1 to 89, k=20 to 60 
n_blades=22
per_th <- 2 * pi / n_blades
l_th <- per_th * 0:(n_blades - 1)
dfp <- data_full %>%
  filter(omega==7808) %>%
  select(v_total, x, r, th, omega, i, j, k)
dfp$col <- color.scale(dfp$v_total, c(0, 1, 1), c(1, 1, 0), 0)
dfp$col[dfp$j%in%c(1:2,88:89)]="#000000"
open3d()
plot3d(dfp$x, dfp$r*cos(dfp$th), dfp$r*sin(dfp$th),xlab="dfp$x",ylab="dfp$y",zlab="dfp$z",  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

dfp <- data.frame(
  v_total = rep(dfp$v_total, n_blades),
  x = rep(dfp$x, n_blades),
  r = rep(dfp$r, n_blades),
  th = rep(dfp$th, n_blades) +
    rep(l_th, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  omega = rep(dfp$omega, n_blades),
  i = rep(dfp$i, n_blades),
  j = rep(dfp$j, n_blades) +
    rep((1:n_blades)-1, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  k = rep(dfp$k, n_blades),
  col = c(rep("#38a5f1", length(dfp$col)),rep(dfp$col, n_blades-1))
)
dfp$y=dfp$r*cos(dfp$th)
dfp$z=dfp$r*sin(dfp$th)
open3d()
plot3d(dfp$x, dfp$y, dfp$z,  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

## initial one bucket with i=2 to 153, j = 1 to 89, k=35 
n_blades=22
per_th <- 2 * pi / n_blades
l_th <- per_th * 0:(n_blades - 1)
dfp <- data_full %>%
  filter(k==35,omega==7808) %>%
  select(v_total, x, r, th, omega, i, j, k)
dfp$col <- color.scale(dfp$v_total, c(0, 1, 1), c(1, 1, 0), 0)
dfp$col[dfp$j%in%c(1:2,88:89)]="#000000"
open3d()
plot3d(dfp$x, dfp$r*cos(dfp$th), dfp$r*sin(dfp$th),xlab="dfp$x",ylab="dfp$y",zlab="dfp$z",  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

dfp <- data.frame(
  v_total = rep(dfp$v_total, n_blades),
  x = rep(dfp$x, n_blades),
  r = rep(dfp$r, n_blades),
  th = rep(dfp$th, n_blades) +
    rep(l_th, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  omega = rep(dfp$omega, n_blades),
  i = rep(dfp$i, n_blades),
  j = rep(dfp$j, n_blades) +
    rep((1:n_blades)-1, each = length(unique(dfp$i)) * length(unique(dfp$j)) * length(unique(dfp$k))),
  k = rep(dfp$k, n_blades),
  col = c(rep("#38a5f1", length(dfp$col)),rep(dfp$col, n_blades-1))
)
dfp$y=dfp$r*cos(dfp$th)
dfp$z=dfp$r*sin(dfp$th)
open3d()
plot3d(dfp$x, dfp$y, dfp$z,  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

# step1: recalculate theat for the lowest points
n_blades=22
per_th <- 2 * pi / 22
l_th <- per_th * 0:(n_blades - 1)
dfp_low <- data_low_full %>% ungroup() %>%
  filter(k==35,omega==7808) %>%
  select(v_total, x, r, th, omega) %>%
  mutate(th_percent = 0.5) 
dfp <- data_full_th %>%
  filter(k==35,omega==7808) %>%
  select(v_total, x, r, omega, th_percent)
dfp$th <- (dfp$th_percent-rep(dfp_low$th_percent,each=89))*per_th+rep(dfp_low$th,each=89)
dfp$col <- color.scale(dfp$v_total, c(0, 1, 1), c(1, 1, 0), 0)
dfp$col[dfp$th_percent%in%c(0,1)]="#000000"
open3d()
plot3d(dfp$x, dfp$r*cos(dfp$th), dfp$r*sin(dfp$th),xlab="dfp$x",ylab="dfp$y",zlab="dfp$z",  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)
dfp <- data.frame(
  v_total = rep(dfp$v_total, n_blades),
  x = rep(dfp$x, n_blades),
  r = rep(dfp$r, n_blades),
  th = rep(dfp$th, n_blades) +
    rep(l_th, each = 89*152),
  omega = rep(dfp$omega, n_blades),
  col = c(rep("#38a5f1", length(dfp$col)),rep(dfp$col, n_blades-1))
)
dfp$y=dfp$r*cos(dfp$th)
dfp$z=dfp$r*sin(dfp$th)
open3d()
plot3d(dfp$x, dfp$y, dfp$z,  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

# step2: transform range of theta into %
n_blades=22
per_th <- 2 * pi / 22
l_th <- per_th * 0:(n_blades - 1)
dfp_low <- data_low_full %>% ungroup() %>%
  filter(k==35,omega==7808) %>%
  select(v_total, x, r, th, omega) %>%
  mutate(th_percent = 0.5) 
dfp <- data_full_th %>%
  filter(k==35,omega==7808) %>%
  select(v_total, x, r, omega, th_percent)
dfp$th <- (dfp$th_percent-rep(dfp_low$th_percent,each=89))*per_th + per_th*5.5
dfp$col <- color.scale(dfp$v_total, c(0, 1, 1), c(1, 1, 0), 0)
dfp$col[dfp$th_percent%in%c(0,1)]="#000000"
open3d()
plot3d(dfp$x, dfp$r*cos(dfp$th), dfp$r*sin(dfp$th),xlab="dfp$x",ylab="dfp$y",zlab="dfp$z",  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)

dfp <- data.frame(
  v_total = rep(dfp$v_total, n_blades),
  x = rep(dfp$x, n_blades),
  r = rep(dfp$r, n_blades),
  th = rep(dfp$th, n_blades) +
    rep(l_th, each = 89*152),
  omega = rep(dfp$omega, n_blades),
  col = c(rep("#38a5f1", length(dfp$col)),rep(dfp$col, n_blades-1))
)
dfp$y=dfp$r*cos(dfp$th)
dfp$z=dfp$r*sin(dfp$th)
open3d()
plot3d(dfp$x, dfp$y, dfp$z,  col = dfp$col, box=F)
rgl.viewpoint(-45,30,30,zoom=0.8)
