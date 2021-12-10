IlluminantC    <- c(0.3101, 0.3162)
IlluminantD65  <- c(0.3127, 0.3291)
IlluminantE 	 <- c(0.33333333, 0.33333333)  
GAMMA_REC709	 <- 0
                    #xRed    yRed    xGreen  yGreen  xBlue  yBlue     White point      Gamma
NTSCsystem   <-  c(xRed = 0.67,   yRed = 0.33,   xGreen = 0.21,   yGreen = 0.71,   xBlue = 0.14,   yBlue = 0.08,   xWhite = IlluminantC[1],    yWhite = IlluminantC[2],    gamma = GAMMA_REC709)
EBUsystem    <-  c(xRed = 0.64,   yRed = 0.33,   xGreen = 0.29,   yGreen = 0.60,   xBlue = 0.15,   yBlue = 0.06,   xWhite = IlluminantD65[1],  yWhite = IlluminantD65[2],  gamma = GAMMA_REC709)
SMPTEsystem  <-  c(xRed = 0.630,  yRed = 0.340,  xGreen = 0.310,  yGreen = 0.595,  xBlue = 0.155,  yBlue = 0.070,  xWhite = IlluminantD65[1],  yWhite = IlluminantD65[2],  gamma = GAMMA_REC709)
HDTVsystem   <-  c(xRed = 0.670,  yRed = 0.330,  xGreen = 0.210,  yGreen = 0.710,  xBlue = 0.150,  yBlue = 0.060,  xWhite = IlluminantD65[1],  yWhite = IlluminantD65[2],  gamma = GAMMA_REC709)
CIEsystem    <-  c(xRed = 0.7355, yRed = 0.2645, xGreen = 0.2658, yGreen = 0.7243, xBlue = 0.1669, yBlue = 0.0085, xWhite = IlluminantE[1],    yWhite = IlluminantE[2],    gamma = GAMMA_REC709)
Rec709system <-  c(xRed = 0.64,   yRed = 0.33,   xGreen = 0.30,   yGreen = 0.60,   xBlue = 0.15,   yBlue = 0.06,   xWhite = IlluminantD65[1],  yWhite = IlluminantD65[2],  gamma = GAMMA_REC709)

upvp_to_xy <- function(up, vp){
  xc  <-  (9 * up) / ((6 * up) - (16 * vp) + 12)
  yc  <-  (4 * vp) / ((6 * up) - (16 * vp) + 12)
  c(xc,yc)
}

xy_to_upvp <- function(xc, yc){
  up  <-  (4 * xc) / ((-2 * xc) + (12 * yc) + 3);
  vp  <-  (9 * yc) / ((-2 * xc) + (12 * yc) + 3);
  c(up,vp)
}

xyz_to_rgb <- function(cs,xc, yc, zc){
  xr <- cs[["xRed"]]
  yr <- cs[["yRed"]]
  zr <- 1 - (xr + yr);
  xg <- cs[["xGreen"]]
  yg <- cs[["yGreen"]]
  zg <- 1 - (xg + yg);
  xb <- cs[["xBlue"]]
  yb <- cs[["yBlue"]]
  zb <- 1 - (xb + yb);
  
  xw <- cs[["xWhite"]]
  yw <- cs[["yWhite"]]
  zw <- 1 - (xw + yw)
  
  rx <- (yg * zb) - (yb * zg)  
  ry <- (xb * zg) - (xg * zb)  
  rz <- (xg * yb) - (xb * yg)
  gx <- (yb * zr) - (yr * zb)  
  gy <- (xr * zb) - (xb * zr)  
  gz <- (xb * yr) - (xr * yb)
  bx <- (yr * zg) - (yg * zr)  
  by <- (xg * zr) - (xr * zg)
  bz <- (xr * yg) - (xg * yr)
  
  rw <- ((rx * xw) + (ry * yw) + (rz * zw)) / yw
  gw <- ((gx * xw) + (gy * yw) + (gz * zw)) / yw
  bw <- ((bx * xw) + (by * yw) + (bz * zw)) / yw
  
  rx <- rx / rw
  ry <- ry / rw
  rz <- rz / rw
  gx <- gx / gw
  gy <- gy / gw
  gz <- gz / gw
  bx <- bx / bw
  by <- by / bw
  bz <- bz / bw
  
  r <- (rx * xc) + (ry * yc) + (rz * zc)
  g <- (gx * xc) + (gy * yc) + (gz * zc)
  b <- (bx * xc) + (by * yc) + (bz * zc)
  c(r,g,b)
}

inside_gamut <- function(col){
  r <- col[1]
  g <- col[2]
  b <- col[3]
  return (r >= 0) & (g >= 0) & (b >= 0)
}

constrain_rgb <- function(col){
  r <- col[1]
  g <- col[2]
  b <- col[3]
  w <-  ifelse(0 < r,0,r)
  w <-  ifelse(w < g, w,g)
  w <-  ifelse(w < b,w,b)
  w <-  -w
  
  if (w > 0) {
      r  <- r+w
      g  <- g+w
      b  <- g+w;
    }
  
  c(r,g,b)
}

gamma_correct <- function(cs, c){
  
  gamma  <-  cs$gamma
  
  if (gamma == GAMMA_REC709) {
      cc  <-  0.018
      
      if (c < cc) {
        c <-  c * ((1.099 * cc^0.45) - 0.099) / cc
      } else {
        c <- (1.099 * c^0.45) - 0.099
      }
  } else {
      c  <-  c^(1.0 / gamma)
  }
  c
}

gamma_correct_rgb <- function(cs, col){
  r <- gamma_correct(cs, col[1]);
  g <- gamma_correct(cs, col[2]);
  b <- gamma_correct(cs, col[3]);
  c(r,g,b)
}

norm_rgb <- function(col){
  #define Max(a, b)   (((a) > (b)) ? (a) : (b))
  greatest  <-  max(col)
  
  if (greatest > 0) {
    r <- col[1]/greatest
    g <- col[2]/greatest
    b <- col[3]/greatest
  }
  c(r,g,b)
}

bb_spectrum <- function(wavelength,bbTemp){
  wlm <-  wavelength * 1e-9
  (3.74183e-16 * wlm^-5.0) / (exp(1.4388e-2 / (wlm * bbTemp)) - 1.0)
}

spectrum_to_xyz <- function(bbTemp){
  X <- 0
  Y <- 0
  Z <- 0
  
  cie_colour_match <- matrix(c(
    0.0014,0.0000,0.0065, 0.0022,0.0001,0.0105, 0.0042,0.0001,0.0201,
    0.0076,0.0002,0.0362, 0.0143,0.0004,0.0679, 0.0232,0.0006,0.1102,
    0.0435,0.0012,0.2074, 0.0776,0.0022,0.3713, 0.1344,0.0040,0.6456,
    0.2148,0.0073,1.0391, 0.2839,0.0116,1.3856, 0.3285,0.0168,1.6230,
    0.3483,0.0230,1.7471, 0.3481,0.0298,1.7826, 0.3362,0.0380,1.7721,
    0.3187,0.0480,1.7441, 0.2908,0.0600,1.6692, 0.2511,0.0739,1.5281,
    0.1954,0.0910,1.2876, 0.1421,0.1126,1.0419, 0.0956,0.1390,0.8130,
    0.0580,0.1693,0.6162, 0.0320,0.2080,0.4652, 0.0147,0.2586,0.3533,
    0.0049,0.3230,0.2720, 0.0024,0.4073,0.2123, 0.0093,0.5030,0.1582,
    0.0291,0.6082,0.1117, 0.0633,0.7100,0.0782, 0.1096,0.7932,0.0573,
    0.1655,0.8620,0.0422, 0.2257,0.9149,0.0298, 0.2904,0.9540,0.0203,
    0.3597,0.9803,0.0134, 0.4334,0.9950,0.0087, 0.5121,1.0000,0.0057,
    0.5945,0.9950,0.0039, 0.6784,0.9786,0.0027, 0.7621,0.9520,0.0021,
    0.8425,0.9154,0.0018, 0.9163,0.8700,0.0017, 0.9786,0.8163,0.0014,
    1.0263,0.7570,0.0011, 1.0567,0.6949,0.0010, 1.0622,0.6310,0.0008,
    1.0456,0.5668,0.0006, 1.0026,0.5030,0.0003, 0.9384,0.4412,0.0002,
    0.8544,0.3810,0.0002, 0.7514,0.3210,0.0001, 0.6424,0.2650,0.0000,
    0.5419,0.2170,0.0000, 0.4479,0.1750,0.0000, 0.3608,0.1382,0.0000,
    0.2835,0.1070,0.0000, 0.2187,0.0816,0.0000, 0.1649,0.0610,0.0000,
    0.1212,0.0446,0.0000, 0.0874,0.0320,0.0000, 0.0636,0.0232,0.0000,
    0.0468,0.0170,0.0000, 0.0329,0.0119,0.0000, 0.0227,0.0082,0.0000,
    0.0158,0.0057,0.0000, 0.0114,0.0041,0.0000, 0.0081,0.0029,0.0000,
    0.0058,0.0021,0.0000, 0.0041,0.0015,0.0000, 0.0029,0.0010,0.0000,
    0.0020,0.0007,0.0000, 0.0014,0.0005,0.0000, 0.0010,0.0004,0.0000,
    0.0007,0.0002,0.0000, 0.0005,0.0002,0.0000, 0.0003,0.0001,0.0000,
    0.0002,0.0001,0.0000, 0.0002,0.0001,0.0000, 0.0001,0.0000,0.0000,
    0.0001,0.0000,0.0000, 0.0001,0.0000,0.0000, 0.0000,0.0000,0.0000
  ),byrow=T,ncol=3)
  
  i <- 0
  for(lambda in seq(380,780,5)){
    i <- i+1
    Me <-  bb_spectrum(lambda,bbTemp)
    X  <-  X + Me * cie_colour_match[i,1]
    Y  <-  Y + Me * cie_colour_match[i,2]
    Z  <-  Z + Me * cie_colour_match[i,3]
  }
  XYZ <-  (X + Y + Z);
  x <- X / XYZ
  y <- Y / XYZ
  z <- Z / XYZ
  return(c(x,y,z))
}

bbTemp <-  5000
ncols  <- 200
m_t0   <- 1000
m_t1   <- 10000
m_dt   <- (m_t1-m_t0) / ncols

cols <- matrix(0,ncols,3)
for(i in 0:(ncols-1)){
  bbTemp <-  m_t0 + m_dt*i 
  cs <- SMPTEsystem
  xyz <- spectrum_to_xyz(bbTemp)
  rgbs <- xyz_to_rgb(cs, xyz[1], xyz[2], xyz[3])
  rgbs <- constrain_rgb(rgbs)
  cols[i+1,] <- norm_rgb(rgbs)
}
colPalette <- rgb(cols[,1],cols[,2],cols[,3])

ColorFromTemperature <- function(temp,cols){
  idx <-  (temp - m_t0) / (m_t1-m_t0) * ncols
  idx <- min(c(ncols-1, idx));
  idx <- max(c(1, idx))
  cols[idx]
}
# glColor3f(col.r * pStars[i].m_mag,
#           col.g * pStars[i].m_mag,
#           col.b * pStars[i].m_mag);