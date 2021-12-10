library(tidyverse)
library(ggecho)
source("helper.R")
source("colors.R")
Rcpp::sourceCpp("CDF.cpp")
w <- 14

for(i in 1:100){
  cat(i,"\r")
  fname <- paste0("galaxies/results/original/galaxies_",str_pad(i,3,"left",pad="0"),".png")
  # color scheme ----
  sys <- c("NTSCsystem",  "EBUsystem",  "SMPTEsystem",  "HDTVsystem",  "CIEsystem",  "Rec709system")
  bbTemp <-  5000
  ncols  <- 200
  m_t0   <- 1000 #max(c(0,rnorm(1,mean = 1000,sd=500)))#
  m_t1   <- 10000#max(c(m_t0+1000,rnorm(1,mean = 10000,sd=1000)))#
  m_dt   <- (m_t1-m_t0) / ncols
  rand_cs <- eval(parse(text=sys[sample(1:6,1)]))
  
  cols <- matrix(0,ncols,3)
  for(i in 0:(ncols-1)){
    bbTemp <-  m_t0 + m_dt*i 
    cs <- rand_cs 
    xyz <- spectrum_to_xyz(bbTemp)
    rgbs <- xyz_to_rgb(cs, xyz[1], xyz[2], xyz[3])
    rgbs <- constrain_rgb(rgbs)
    cols[i+1,] <- norm_rgb(rgbs)
  }
  colPalette <- rgb(cols[,1],cols[,2],cols[,3])

  
  # constants ----
  radCore <- max(c(rnorm(1,4000,sd=1000),1000))
  radGalaxy <- max(c(rnorm(1,13000,sd=2000),10000))
  radFarField <- radGalaxy * 2
  elEx1 <- runif(1,0.5,1)#0-1
  elEx2 <- runif(1,0.5,1)#0-1
  amp <- max(c(rnorm(1,mean=40,sd=10),10))
  n <- sample(1:5,1) #15
  steps <- 100
  angleOffset <- runif(1,min=1e-5,max=1e-2)
  RBulge <- runif(1,min=0.001,max=0.5)
  nStar <- max(c(rnorm(1,mean=15000,sd=2000),7000))
  numDust <- floor(nStar/runif(1,min=1.5,max=5)) 
  numH2 <- max(c(50,rnorm(1,mean=300,sd=50)))
  
  cdf <- setupRealistic(I0 = 1,k = RBulge,a = radGalaxy/3,RBulge = radCore,
                        min = 0,max = radGalaxy*2,nSteps = 1000)
  
  # stars ----
  stars <- tibble(m_a = unlist(sapply(runif(nStar),valFromProb,cdf)),
                  m_b = m_a * sapply(m_a,get_excentricity),
                  m_angle = m_a*angleOffset) %>% 
    mutate(m_theta = 360*runif(nrow(.)),
           m_temp= 6000 + (4000*runif(nrow(.)))-2000,
           m_mag = 0.3+0.2*runif(nrow(.)))
  
  xy <- matrix(compute_star_pos(stars$m_angle,stars$m_a,stars$m_b,stars$m_theta,n = n,amp = amp),ncol=2,byrow = F)
  
  stars$x <- xy[,1]
  stars$y <- xy[,2]
  stars$col <- sapply(stars$m_temp,ColorFromTemperature,colPalette)
  
  # stars$col <- rgb(t(col2rgb(stars$col))/255 * stars$m_mag,maxColorValue=1)
  
  # dust ----
  n1 <- floor(numDust/4)
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
  dust$col <- sapply(dust$m_temp,ColorFromTemperature,colPalette)
  
  # dust$col <- rgb(t(col2rgb(dust$col))/255 * dust$m_mag)
  
  #H-II ----
  rad <- sqrt((2*radGalaxy * runif(numH2) - radGalaxy)^2+(2*radGalaxy * runif(numH2) - radGalaxy)^2)
  
  h2_1 <-  tibble(m_a = rad,
                  m_b = rad * sapply(rad,get_excentricity),
                  m_angle = rad*angleOffset) %>% 
    mutate(m_theta = 360*runif(nrow(.)),
           m_temp= 6000+(6000 * runif(nrow(.))),
           m_mag = 0.1+0.05*runif(nrow(.)))
  
  h2_2 <- h2_1 %>% mutate(m_a=m_a + 1000)
  
  xy <- matrix(compute_star_pos(h2_1$m_angle,h2_1$m_a,h2_1$m_b,h2_1$m_theta,n = n,amp = amp),ncol=2,byrow = F)
  h2_1$x <- xy[,1]
  h2_1$y <- xy[,2]
  h2_1$col <- sapply(h2_1$m_temp,ColorFromTemperature,colPalette)
  
  tmp <- t(col2rgb(sapply(h2_1$m_temp,ColorFromTemperature,colPalette))/255*c(2,0.5,0.5))
  
  for(i in 1:nrow(tmp)){
    a <- norm_rgb(tmp[i,])
    h2_1$col <- rgb(a[1],a[2],a[3])
  }
  
  
  xy <- matrix(compute_star_pos(h2_2$m_angle,h2_2$m_a,h2_2$m_b,h2_2$m_theta,n = n,amp = amp),ncol=2,byrow = F)
  h2_2$x <- xy[,1]
  h2_2$y <- xy[,2]
  
  dst <- sqrt((h2_1$x-h2_2$x)^2 + (h2_1$y-h2_2$y)^2)
  h2_1$size <- (1000-dst)/10-50
  
  h2_1 <- h2_1 %>% dplyr::filter(size>=1)
  
  #plot ----
  p <- ggplot()+
    geom_point(data=dust,aes(x,y),col=rep(dust$col,3),alpha=0.02,size=7,stat="echo",n=2,shape=16)+
    geom_point(data = stars,aes(x,y),col=stars$col,alpha=0.2,size=0.4,shape=16)+
    geom_point(data=h2_1,aes(x,y),size=rep(h2_1$size/10,3),col=rep(h2_1$col,3),alpha=0.1,stat="echo",n=2,shape=16)+
    geom_point(data=h2_1,aes(x,y),size=h2_1$size/60,col="white",alpha=0.4,shape=16)+
    scale_x_continuous(limits=c(-30000,30000))+
    scale_y_continuous(limits=c(-30000,30000))+
    ggraph::theme_graph(background = "black")
  
  ggsave(fname,plot=p,width = w,height=w,bg="transparent")
  cmd <- paste0('convert ','"',fname,'"',' -set filename:base "%[base]" -trim +repage "galaxies/results/original/%[filename:base].png"')
  system(cmd)
  cmd <- paste0('convert ','"',fname,'[1000x]"',' -set filename:base "%[base]" "galaxies/results/scaled/%[filename:base].png"')
  system(cmd)
}
