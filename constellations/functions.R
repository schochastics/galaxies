suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ambient))
suppressPackageStartupMessages(library(ggforce))
suppressPackageStartupMessages(library(rgraph6))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(graphlayouts))
suppressPackageStartupMessages(library(ggraph))
suppressPackageStartupMessages(library(showtext))
suppressPackageStartupMessages(library(packcircles))
suppressPackageStartupMessages(library(patchwork))

# fonts ----
showtext_auto()

source("constellations/star_names.R")

# points in circle ----
circ_points <- function(n,R = 1){
  r  <-  R * sqrt(runif(n))
  theta  <-  runif(n) * 2 * pi
  x <-  r * cos(theta)
  y <-  r * sin(theta)
  data.frame(x,y)
}

# random random noise ----
randomize_noise <- function(dat2){
  xpar1 <- runif(1,0.1,2.7)
  ypar1 <- runif(1,0.1,2.7)
  xpar2 <- runif(1,0.5,3.5)
  ypar2 <- runif(1,0.5,5.5)
  xnoise <- sample(1:2,1)
  ynoise <- sample(1:2,1)
  # print(c(xpar1,ypar1,xpar2,ypar2,xnoise,ynoise))
  if(xnoise==1){
    dat2$x <- dat2$x+xpar1*gen_simplex(xpar2*dat2$x,ypar2*dat2$y)
  } else{
    dat2$x <- dat2$x+xpar1*gen_perlin(xpar2*dat2$x,ypar2*dat2$y)
  }
  if(ynoise==1){
    dat2$y <- dat2$y+ypar1*gen_simplex(xpar2*dat2$x,ypar2*dat2$y)
  } else{
    dat2$y <- dat2$y+ypar1*gen_perlin(xpar2*dat2$x,ypar2*dat2$y)
  }
  dat2 %>% dplyr::filter(sqrt(x^2+y^2)<1)
}

# sample networks ----
sample_nets <- function(n,nsizes=6:14){
  graphs <-  map(nsizes,function(x)readLines(paste0("constellations/trees_n",x,".txt")))
  nsample <- sample(nsizes,consts,replace = T)
  gsamp <- rep("",consts)
  k <- 0
  for(i in nsample){
    k <- k+1
    gsamp[k] <- sample(graphs[[i-5]][-1],1)
  }
  
  gsamp <- lapply(as_adjacency(gsamp),function(x) graph_from_adjacency_matrix(x,"undirected"))
  for(i in 1:length(gsamp)){
    V(gsamp[[i]])$name <- paste0(LETTERS[i],1:vcount(gsamp[[i]])) 
  }
  
  g <- Reduce("%u%",gsamp)
  V(g)$grp <- factor(components(g)$membership)
  V(g)$size <- sample(c(seq(1,3,length.out = ceiling(0.9*vcount(g))),
                        seq(4,7,length.out = floor(0.1*vcount(g)))))
  
  g  
}

# network layout ----
layout_const <- function(g,ar=50:300){
  xy <- layout_components(g,layout_with_stress)
  areas <- sample(ar,components(g)$no,replace = T)
  
  packing <- circleProgressiveLayout(areas) 
  
  xy <- layout_components(g,layout_with_stress)
  for(i in unique(V(g)$grp)){
    idx <- which(V(g)$grp==i)
    meanxy <- apply(xy[idx,],2,mean)
    tmp <- cbind(xy[idx,1]-meanxy[1]+packing$x[i],xy[idx,2]-meanxy[2]+packing$y[i])
    xy[idx,] <- tmp
  }
  xy[,1] <- normalise(xy[,1],to = c(-0.8,0.8))
  xy[,2] <- normalise(xy[,2],to = c(-0.8,0.8))
  xy
}

#------------------------------------------------------------------------------#
# constellation abstract ----
#------------------------------------------------------------------------------#

plot_const_1 <- function(g,xy,sbig=500,ssmall=5000,font="lora"){
  dat <- circ_points(sbig)
  dat2 <- circ_points(ssmall)
  dat2$x <- dat2$x+0.1*gen_simplex(dat2$x,dat2$y)
  dat2$y <- dat2$y+0.1*gen_simplex(dat2$x,dat2$y)
  dat2 <- dat2 %>% dplyr::filter(sqrt(x^2+y^2)<1)
  
  dfxy <- data.frame(x = xy[,1],y = xy[,2],grp = as.factor(V(g)$grp),star = stars[V(g)$grp])
  
  ggraph(g,"manual",x=xy[,1],y=xy[,2])+
    geom_circle(aes(x0=0,y0=0,r=1),fill="#203154",colour="white",size=1.0,linetype="longdash")+
    geom_point(data = dat,aes(x,y),col="white",size=0.7)+
    geom_point(data = dat2,aes(x,y),col="white",size=0.1,alpha=0.3)+
    geom_edge_link0(edge_colour="white",edge_width=0.3)+
    geom_node_point(aes(size=I(size+3)),shape=16,col="#203154")+
    geom_node_point(aes(size=I(size)),shape=16,col="white")+
    geom_mark_hull(data=dfxy,aes(x,y, group=grp, label = star),
                   expand = unit(10, "pt"),concavity = 2,
                   radius = unit(10, "pt"),
                   col=NA,
                   label.buffer = unit(0, 'mm'),
                   label.margin = margin(0, 0, 0, 0, "mm"),
                   label.fill = NA,
                   label.colour = "grey66",
                   label.fontsize = 44,
                   label.family = font,
                   label.fontface = "italic",
                   con.colour = NA,
                   con.type = "straight"
    )+
    ggraph::theme_graph(background = "#203154")+
    theme(plot.title = element_text(colour="white",family = font,face = "bold",size = 86,hjust=0.5),
          plot.subtitle = element_text(colour="white",family = font,face = "italic",size = 66,hjust=0.5))+
    coord_equal(clip="off")+
    labs(title=system_name,subtitle = subt)
}

#------------------------------------------------------------------------------#
# constellation real (no grid)----
#------------------------------------------------------------------------------#

plot_const_2 <- function(g,xy,sbig=150,font="lora"){
  dat <- circ_points(sbig)
  dat$size <- runif(nrow(dat),min=0.1,max=0.7)
  
  grid <- long_grid(seq(-2, 2, length.out = 1000), seq(-2, 2, length.out = 1000))
  grid$noise <- gen_simplex(2*grid$x, 2*grid$y)

  dfxy <- data.frame(x = xy[,1],y = xy[,2],grp = as.factor(V(g)$grp),star = stars[V(g)$grp])
  
  df_noise <- grid %>% 
    as_tibble() %>% 
    mutate(noise=normalise(noise)) %>% 
    dplyr::filter(noise>0.7) %>% 
    dplyr::filter(sqrt(x^2+y^2)<1)
  
  # no grid ----
  ggraph(g,"manual",x=xy[,1],y=xy[,2])+
    geom_circle(aes(x0=0,y0=0,r=1.02),fill="black",colour="#7DE0F3",size=0.4)+
    geom_circle(aes(x0=0,y0=0,r=1),fill=NA,colour="#7DE0F3",size=0.4)+
    geom_raster(data=df_noise,aes(x,y,fill=noise),alpha=0.5)+
    geom_point(data = dat,aes(x,y,size=I(size)),col="white")+
    geom_edge_link0(edge_colour="#7DE0F3",edge_width=0.3)+
    geom_node_point(aes(size=I(size)),shape=16,col="#7DE0F3")+
    geom_mark_hull(data=dfxy,aes(x,y, group=grp, label = star),
                   expand = unit(10, "pt"),concavity = 2,
                   radius = unit(10, "pt"),
                   col=NA,
                   label.buffer = unit(0, 'mm'),
                   label.margin = margin(0, 0, 0, 0, "mm"),
                   label.fill = NA,
                   label.colour = "grey66",
                   label.fontsize = 44,
                   label.family = font,
                   label.fontface = "italic",
                   con.colour = NA,
                   con.type = "straight"
    )+
    ggraph::theme_graph(background = "black")+
    theme(plot.title = element_text(colour="#7DE0F3",family = font,face = "bold",size = 86,hjust=0.5),
          plot.subtitle = element_text(colour="#7DE0F3",family = font,face = "italic",size = 66,hjust=0.5),
          legend.position = "none")+
    coord_equal(clip="off")+
    labs(title=system_name,subtitle = subt)
}

#------------------------------------------------------------------------------#
# constellation real (grid)----
#------------------------------------------------------------------------------#
plot_const_3 <- function(g,xy,sbig=150,font="lora"){
  
  dat <- circ_points(sbig)
  dat$size <- runif(nrow(dat),min=0.1,max=0.7)
  
  dfxy <- data.frame(x = xy[,1],y = xy[,2],grp = as.factor(V(g)$grp),star = stars[V(g)$grp])
  
  grid <- long_grid(seq(-2, 2, length.out = 1000), seq(-2, 2, length.out = 1000))
  grid$noise <- gen_simplex(2*grid$x, 2*grid$y)
  
  df_noise <- grid %>% 
    as_tibble() %>% 
    mutate(noise=normalise(noise)) %>% 
    dplyr::filter(noise>0.7) %>% 
    dplyr::filter(sqrt(x^2+y^2)<1)
  
  
  grid_lines <- data.frame(x=c(-1,1,0,0),y=c(0,0,-1,1),grp=c(1,1,2,2))
  grid_circles <- data.frame(r=seq(0.2,0.95,length.out = 5),x0=0,y0=0)
  grid_spoke <- tibble(angle=c(seq(0,pi,length.out=9)[-c(1,5,9)],-seq(0,pi,length.out=9)[-c(1,5,9)]),
                       r=0.8,x=0.2*cos(angle),y=0.2*sin(angle))
  
  ggraph(g,"manual",x=xy[,1],y=xy[,2])+
    geom_circle(aes(x0=0,y0=0,r=1.02),fill="black",colour="#7DE0F3",size=0.4)+
    geom_circle(aes(x0=0,y0=0,r=1),fill=NA,colour="#7DE0F3",size=0.4)+
    geom_circle(data=grid_circles,aes(x0=x0,y0=y0,r=r),col="grey66",size=0.2)+
    geom_spoke(data=grid_spoke,aes(x,y,angle=angle,radius=r),col="grey66",size=0.2)+
    geom_line(data=grid_lines,aes(x,y,group=grp),col="grey66",size=0.2)+
    geom_raster(data=df_noise,aes(x,y,fill=noise),alpha=0.5)+
    geom_point(data = dat,aes(x,y,size=I(size)),col="white")+
    geom_edge_link0(edge_colour="#7DE0F3",edge_width=0.3)+
    geom_node_point(aes(size=I(size)),shape=16,col="#7DE0F3")+
    geom_mark_hull(data=dfxy,aes(x,y, group=grp, label = star),
                   expand = unit(10, "pt"),concavity = 2,
                   radius = unit(10, "pt"),
                   col=NA,
                   label.buffer = unit(0, 'mm'),
                   label.margin = margin(0, 0, 0, 0, "mm"),
                   label.fill = NA,
                   label.colour = "grey66",
                   label.fontsize = 44,
                   label.family = font,
                   label.fontface = "italic",
                   con.colour = NA,
                   con.type = "straight"
    )+
    ggraph::theme_graph(background = "black")+
    theme(plot.title = element_text(colour="#7DE0F3",family = font,face = "bold",size = 86,hjust=0.5),
          plot.subtitle = element_text(colour="#7DE0F3",family = font,face = "italic",size = 66,hjust=0.5),
          legend.position = "none")+
    coord_equal(clip="off")+
    labs(title=system_name,subtitle = subt)-> p
  
}

#------------------------------------------------------------------------------#
# constellation vintage ----
#------------------------------------------------------------------------------#
plot_const_4 <- function(g,xy,sbig=150,cols=vintage1,font="fajardose"){
  dat <- circ_points(sbig)
  dat$size <- runif(nrow(dat),min=0.1,max=0.7)
  
  dfxy <- data.frame(x = xy[,1],y = xy[,2],grp = as.factor(V(g)$grp),star = stars[V(g)$grp])
  
  grid_spoke <- tibble(angle=c(seq(0,pi,length.out=5),-seq(0,pi,length.out=5)[-c(1,5)]),
                       r=1,x=0,y=0)
  
  ggraph(g,"manual",x=xy[,1],y=xy[,2])+
    geom_circle(aes(x0=0,y0=0,r=1),fill=cols[["cfill"]],colour=cols[["mcol"]],size=1)+
    geom_point(data = dat,aes(x,y,size=I(size)),col=cols[["mcol"]])+
    geom_spoke(data=grid_spoke,aes(x,y,angle=angle,radius=r),col=cols[["mcol"]],size=0.2)+
    geom_edge_link0(edge_colour=cols[["mcol"]],edge_width=0.3)+
    geom_node_point(aes(size=I(size)),shape=16,col=cols[["mcol"]])+
    geom_mark_hull(data=dfxy,aes(x,y, group=grp, label = star),
                   expand = unit(10, "pt"),concavity = 2,
                   radius = unit(10, "pt"),
                   col=NA,
                   label.buffer = unit(0, 'mm'),
                   label.margin = margin(0, 0, 0, 0, "mm"),
                   label.fill = NA,
                   label.colour = cols[["mcol"]],
                   label.fontsize = 74,
                   label.family = font,
                   label.fontface = "italic",
                   con.colour = NA,
                   con.type = "straight"
    )+
    ggraph::theme_graph(background = cols[["bg"]])+
    theme(plot.title = element_text(colour=cols[["mcol"]],family = font,face = "bold",size = 205,hjust=0.5),
          plot.subtitle = element_text(colour=cols[["mcol"]],family = font,size = 66,hjust=0.5),
          legend.position = "none",
          rect = element_rect(fill = "transparent"))+
    coord_equal(clip="off")+
    labs(title=system_name)
}

#------------------------------------------------------------------------------#
# constellation patched ----
#------------------------------------------------------------------------------#
plot_const_5 <- function(g,ssmall = 350,cols=vintage2,font="lora"){
  consts <- components(g)$no
  gs <- map(1:consts,function(x) induced_subgraph(g,which(components(g)$membership==x)))
  pList <- vector("list",consts)
  
  for(i in 1:consts){
    dat2 <- circ_points(ssmall)
    dat2$size <- normalise(gen_simplex(dat2$x,dat2$y),to=c(0.8,2.3))
    
    gxy <- layout_with_stress(gs[[i]])
    gxy[,1] <- gxy[,1]/(2*max(gxy[,1]))
    gxy[,2] <- gxy[,2]/(2*max(gxy[,2]))
    
    ggraph(gs[[i]],"manual",x=gxy[,1],y=gxy[,2])+
      geom_circle(aes(x0=0,y0=0,r=1),fill=cols[["cfill"]],colour=cols[["mcol"]],size=1)+
      geom_rect(aes(xmin=-1.05,xmax=1.05,ymin=-1.05,ymax=1.05),colour=cols[["mcol"]],fill=NA,size=1)+
      geom_rect(aes(xmin=-1.01,xmax=1.01,ymin=-1.01,ymax=1.01),colour=cols[["mcol"]],fill=NA,size=1)+
      annotate("text",x=-1.05,y=1.09,label=stars[i],colour=cols[["mcol"]],
               hjust=0,vjust=0,family=font,size = 27)+
      geom_point(data = dat2,aes(x,y,size=I(size)),col="white",alpha=0.25,shape=16)+
      geom_edge_link0(edge_colour=cols[["net"]],edge_width=0.7)+
      geom_node_point(aes(size=I(size+3)),shape=16,col=cols[["bg"]])+
      geom_node_point(aes(size=I(size)),shape=16,col=	cols[["net"]])+
      ggraph::theme_graph(background = cols[["bg"]])+
      theme(rect = element_rect(fill = "transparent"))+
      coord_equal(clip="off")+
      scale_x_continuous(limits=c(-1.1,1.1))+
      scale_y_continuous(limits=c(-1.1,1.1)) -> pList[[i]]
  }
  p <- Reduce("+",pList)+plot_layout(nrow=sqrt(length(pList)))
  p
}

#------------------------------------------------------------------------------#
# fix images ----
#------------------------------------------------------------------------------#
trim_png <- function(file,newfile){
  img <- magick::image_read(path = file)
  img <- magick::image_scale(img, "x1000")
  img <- magick::image_trim(img,fuzz=10)
  
  magick::image_write(img, path = newfile, format = "png")
}

#------------------------------------------------------------------------------#
# horoscope ----
#------------------------------------------------------------------------------#

# library(rvest)
# "https://generatorfun.com/code/model/generatorcontent.php?recordtable=generator&recordkey=28&gen=Y&itemnumber=16&randomoption=undefined&genimage=Yes&nsfw=No&keyword=undefined" %>%
#   read_html() %>%
#   html_nodes("p") %>% html_text() ->tst
# 
# # tst[1] <- paste(bold("TEST"),tst[1])
# tst <- sapply(tst,function(x) paste(word(x,1:5,sep="\\."),collapse="."))
# 
# df <- data.frame(x=rep(1,16),y=1,label=str_wrap(tst,40),grp=stars)
# ggplot(df,aes(x,y))+
#   geom_text(aes(label=label),hjust=0,vjust=1,col="white",family="lora",lineheight=0.6)+
#   scale_x_continuous(limits=c(1,2.4))+
#   scale_y_continuous(limits=c(0.9,1))+
#   facet_wrap(~grp,ncol=5,scales = "free")+
#   theme_graph(background = "#203154")+
#   theme(strip.text = element_text(colour="grey66",hjust=0,family="lora",face="italic"))->p1
# 
# p2 <- p+p1+plot_layout(ncol=1)
# ggsave("~/Documents/tmp/test.png",plot=p2,width = 14,height=20)
# 
# pList <- vector("list",length(tst))
# for(i in 1:length(tst)){
#   pList[[i]] <- ggplot(df[i,],aes(x,y))+
#     geom_text(aes(label=label),hjust=0,vjust=1,col="white",family="lora",lineheight=0.3,size=6)+
#     scale_x_continuous(limits=c(1,2.4))+
#     scale_y_continuous(limits=c(0.9,1))+
#     theme_graph(background = "#203154")+
#     theme(plot.title = element_text(colour="grey66",hjust=0,family="lora",face="italic",size=46),
#           plot.margin = margin(b=-50,unit="mm"))+
#     labs(title=df$grp[i])
# }
# 
# p+Reduce("+",pList[1:16])+
#   plot_layout(nrow=2) ->p2
# 
# 
# ggsave("~/Documents/tmp/test.png",plot=p2,width = 16,height=16)
