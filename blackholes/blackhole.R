library(tidyverse)
library(ambient)
library(ggforce)
library(ggfx)
source("galaxies/helper.R")
Rcpp::sourceCpp("galaxies/CDF.cpp")

colPal <- colorRamp(c("white","#FFE484","#FCCB33","#D1441B","#A43515"),bias = 2.3)

w <- 14

for(i in 1:100){
  cat(i,"\r")
  fname <- paste0("blackholes/results/original/blackhole_",str_pad(i,3,"left",pad="0"),".png")
  

  # constants ----
  radCore <- max(c(rnorm(1,4000,sd=1000),1000))
  radGalaxy <- max(c(rnorm(1,13000,sd=2000),10000))
  radFarField <- radGalaxy * 2
  elEx1 <- runif(1,min = 0.7,max = 1)
  elEx2 <- runif(1,min = 0.7,max = 1)
  amp <- max(c(rnorm(1,mean=40,sd=10),10))
  n <- sample(1:4,1)
  steps <- 100
  angleOffset <- runif(1,min=1e-5,max=1e-2)
  RBulge <- runif(1,min=0.001,max=0.5)
  nStar <- max(c(rnorm(1,mean=15000,sd=2000),7000))
  numDust <- floor(nStar/runif(1,min=1.5,max=5)) 
  numH2 <- max(c(50,rnorm(1,mean=300,sd=50)))
  
  cdf <- setupRealistic(I0 = 1,k = RBulge,a = radGalaxy/3,RBulge = radCore,
                        min = 0,max = radGalaxy*2,nSteps = 1000)
  
  
  n1 <- floor(numDust/sample(2:5,1))
  n2 <- numDust-n1
  
  rad1 <- unlist(sapply(runif(n1),valFromProb,cdf))
  rad2 <- sqrt((2*radGalaxy * runif(n2) - radGalaxy)^2+(2*radGalaxy * runif(n2) - radGalaxy)^2)
  rad <- c(rad1,rad2)
  
  dust <-  tibble(m_a = rad,
                  m_b = rad * sapply(rad,get_excentricity),
                  m_angle = rad*angleOffset) %>% 
    mutate(m_theta = 360*runif(nrow(.)),
           m_temp= 5000+rad/4.5,
           m_mag = 0.0153+0.01*runif(nrow(.)))
  
  xy <- matrix(compute_star_pos(dust$m_angle,dust$m_a,dust$m_b,dust$m_theta,n = n,amp = amp),ncol=2,byrow = F)
  
  dust$x <- xy[,1]
  dust$y <- xy[,2]
  dust$dist <- sqrt(dust$x^2+dust$y^2)
  dust$dist <- dust$dist/max(dust$dist)
  dust$col <- apply(colPal(dust$dist),1,function(x)rgb(x[1],x[2],x[3],maxColorValue = 255))
  
  ns <- 10
  bhole <- vector("list",length = ns)
  bhole[[1]] <- dust
  for(i in 1:ns){
    dust1 <- dust
    rot_ang <- 1+i*0.1
    rot_mat <- matrix(c(cos(rot_ang),-sin(rot_ang),
                        sin(rot_ang),cos(rot_ang)),2,2,byrow = TRUE)
    
    xy_rot <- t(apply(cbind(dust1$x,dust1$y),1,function(x) rot_mat%*%x))
    dust1$x <- xy_rot[,1] + 1500*gen_perlin(dust1$x,dust1$y)
    dust1$y <- xy_rot[,2] + 1500*gen_perlin(dust1$x,dust1$y)
    dust1$dist <- dust1$dist/max(dust1$dist)
    dust1$col <- apply(colPal(dust1$dist),1,function(x)rgb(x[1],x[2],x[3],maxColorValue = 255))
    bhole[[i]] <- dust1  
  }
  
  
  ggplot()+
    geom_point(data= bhole[[1]],aes(x,y, col=I(col)),alpha=0.2,size=0.5,shape=16)+
    geom_point(data= bhole[[2]],aes(x,y, col=I(col)),alpha=0.2,size=0.5,shape=16)+
    geom_point(data= bhole[[3]],aes(x,y, col=I(col)),alpha=0.2,size=0.5,shape=16)+
    geom_point(data= bhole[[4]],aes(x,y, col=I(col)),alpha=0.2,size=0.5,shape=16)+
    with_blur(geom_point(data= bhole[[1]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(data= bhole[[2]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(data= bhole[[3]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(data= bhole[[4]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(data= bhole[[5]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(data= bhole[[6]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(data= bhole[[7]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(data= bhole[[8]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(data= bhole[[9]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(data=bhole[[10]],aes(x,y, col=I(col)),alpha=0.05,size=2,shape=16),sigma = runif(1,2,5))+
    with_blur(geom_point(aes(x=0,y=0),shape=16,size=10,col="black"),sigma=2)+
    scale_x_continuous(limits=c(-30000,30000))+
    scale_y_continuous(limits=c(-30000,30000))+
    ggraph::theme_graph(background = "black")->p
  
  ggsave(fname,plot=p,width = w,height=w,bg="transparent")
  cmd <- paste0('convert ','"',fname,'"',' -set filename:base "%[base]" -trim +repage "blackholes/results/original/%[filename:base].png"')
  system(cmd)
  cmd <- paste0('convert ','"',fname,'[1000x]"',' -set filename:base "%[base]" "blackholes/results/scaled/%[filename:base].png"')
  system(cmd)
}
