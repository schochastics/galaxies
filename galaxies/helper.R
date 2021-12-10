
# setup realistic cdf ----
setupRealistic <- function(I0,k,a,RBulge,min,max,nSteps){
  m_fMin <-  min
  m_fMax <-  max
  m_I0   <- I0
  m_k <- k
  m_a <- a
  m_RBulge <- RBulge
  
  BuildCDF(nSteps,m_fMax,m_fMin,m_RBulge,m_I0,m_k,m_a)
}

valFromProb <- function(fVal,cdf){
  h <- 1/(length(cdf$m_vY2)-1)
  i <- floor(fVal/h)
  
  remainder <- fVal - i*h
  
  if(i >= 0 & i < length(cdf$m_vM2)){
    return(cdf$m_vY2[i]+cdf$m_vM2[i]*remainder)
  } else{
    NA
  }
}

ellipse <- function(a,b,angle){
  beta <- -angle*pi/180
  sinbeta <- sin(beta)
  cosbeta <- cos(beta)
  x <- 0
  y <- 0
  map_dfr(seq(0,360,length.out = steps),function(i){
    alpha <- i*pi/180
    sinalpha <- sin(alpha)
    cosalpha <- cos(alpha)
    df <- tibble(fx=x+(a*cosalpha*cosbeta-b*sinalpha*sinbeta),fy=y+(a*cosalpha*sinbeta+b*sinalpha*cosbeta))
    # df$fx <- df$fx+(a/amp) * sin(alpha * 2 * n)
    # df$fy <- df$fy+(a/amp) * cos(alpha * 2 * n)
    df
  })
  
}

get_excentricity <- function(r){
  if(r<radCore){
    1 + (r/radCore) * (elEx1-1)
  } else if(r>radCore & r<= radGalaxy){
    elEx1+(r-radCore) / (radGalaxy-radCore) * (elEx2-elEx1)
  } else if(r<radGalaxy & r<radFarField){
    elEx2 + (r-radGalaxy) / (radFarField - radGalaxy) * (1-elEx2)
  } else{
    1
  }
}

density_waves <- function(num,rad){
  dr <- rad/num
  map_dfr(1:num,function(i){
    r <- (i+1)*dr
    df <- ellipse(r,r*get_excentricity(r),r*angleOffset*180/pi)
    df$grp <- i
    df
  })
}

compute_star_pos <- function(angle,a,b,theta,n=0,amp=0){
  beta <- -angle
  alpha <- theta * pi/180
  cosa <- cos(alpha)
  sina <- sin(alpha)
  cosb <- cos(beta)
  sinb <- sin(beta)
  
  xy <- c(a * cosa * cosb - b * sina * sinb,
          a * cosa * sinb + b * sina * cosb)
  
  if(amp>0 & n>0){
    xy <- xy + c((a/amp) * sin(alpha * 2 *n),(a/amp) * cos(alpha * 2 * n))
  }
  xy
}

# color stuff ----

# stars <- tibble(a = rep(0,numStars),
#                 b = rep(0,numStars),
#                 angle = rep(0,numStars),
#                 theta = rep(0,numStars),
#                 velTheta = rep(0,numStars),
#                 center = rep(0,numStars),
#                 temp = rep(6000,numStars))
# 
# stars$a[2] <- radCore
# stars$b[2] <- radCore * get_excentricity(radCore) 
# stars$angle[2] <- radCore * angleOffset * 180/pi
# 
# stars$a[3] <- radGalaxy
# stars$b[3] <- radGalaxy * get_excentricity(radGalaxy) 
# stars$angle[3] <- radGalaxy * angleOffset * 180/pi
# 
# dh <- radFarField/100
