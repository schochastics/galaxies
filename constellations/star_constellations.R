source("constellations/functions.R")

# constants ----
vintage1 <- c(cfill="#FAEDCB",mcol="#9E4213",bg="#f6dd9c",net="#9E4213")
vintage2 <- c(cfill="black",mcol="#907D5F",bg="black",net="#ffff33")
mulberry <- c(cfill="#290F26",mcol="#B29E93",bg="#290F26",net="#fdb479")
colorfull <- c("#ffd27d","#ffa371","#a6a8ff","#fffa86","#a87bff")

font_add_google("Lora","lora")
font_add_google("Miss Fajardose","fajardose")
font_add_google("Parisienne","parisienne") # better readable handwritten

consts <- 16
# get names and title ----
stars <- get_starname(consts)
system_name <- get_systemname()
subt <- get_coord()

# create constellations ----
g <- sample_nets(consts)
xy <- layout_const(g,ar = 50:300)

# test constellations ----
p <- plot_const_1(g,xy,sbig=500,ssmall=5000,font = "lora")
ggsave("constellations/results/test1.png",plot=p,width = 14,height=14,bg="transparent")
system("eog constellations/results/test1.png")

p <- plot_const_2(g,xy,sbig=300,font = "lora")
ggsave("constellations/results/test2.png",plot=p,width = 14,height=14,bg="transparent")
system("eog constellations/results/test2.png")

p <- plot_const_3(g,xy,sbig=300,font = "lora")
ggsave("constellations/results/test3.png",plot=p,width = 14,height=14,bg="transparent")
system("eog constellations/results/test3.png")

p <- plot_const_4(g,xy,sbig=300,cols = vintage1,font = "fajardose")
ggsave("constellations/results/test4.png",plot=p,width = 14,height=14,bg="transparent")
system("eog constellations/results/test4.png")

p <- plot_const_4(g,xy,sbig=300,cols = vintage2,font = "fajardose")
ggsave("constellations/results/test5.png",plot=p,width = 14,height=14,bg="transparent")
system("eog constellations/results/test5.png")

p <- plot_const_5(g,ssmall=350,cols = vintage2,font = "lora")
ggsave("constellations/results/test6.png",plot=p,width = 20,height=20,bg="transparent")
system("eog constellations/results/test6.png")

system("rm constellations/results/test6.png")
# create many ----
for(i in 1:100){
  cat(i,"\r")
  fname <- paste0("constellations/results/original/constellation_",str_pad(i,3,"left",pad="0"),".png")

  consts <- sample(13:20,1)
  # get names and title ----
  stars <- get_starname(consts)
  system_name <- get_systemname()
  subt <- get_coord()
  # create constellations ----
  g <- sample_nets(consts)
  xy <- layout_const(g,ar = 50:300)
  
  
  style <- sample(1:5,1)
  if(consts%in%c(16,25,36)){
    style <- 6
  }
  w <- 14
  if(style==1){
    p <- plot_const_1(g,xy,sbig= max(c(rnorm(1,mean = 500,sd = 200),100)),
                      ssmall = max(c(rnorm(1,mean = 5000, sd = 1000),500)),font = "lora")
  } else if(style==2){
    p <- plot_const_2(g,xy,sbig= max(c(rnorm(1,mean = 300,sd = 200),100)),font = "lora")
  } else if(style==3){
    p <- plot_const_3(g,xy,sbig= max(c(rnorm(1,mean = 300,sd = 200),100)),font = "lora")
  } else if(style==4){
    p <- plot_const_4(g,xy,sbig= max(c(rnorm(1,mean = 300,sd = 200),100)),cols = vintage1,font = "parisienne")
  } else if(style==5){
    p <- plot_const_4(g,xy,sbig= max(c(rnorm(1,mean = 300,sd = 200),100)),cols = vintage2,font = "parisienne")
  } else if(style==6){
    p <- plot_const_5(g,ssmall = max(c(rnorm(1,mean = 350,sd = 200),100)),cols = mulberry,font = "lora")
    w <- 20
  }
  ggsave(fname,plot=p,width = w,height=w,bg="transparent")
  cmd <- paste0('convert ','"',fname,'"',' -set filename:base "%[base]" -trim +repage "constellations/results/original/%[filename:base].png"')
  system(cmd)
  cmd <- paste0('convert ','"',fname,'[1000x]"',' -set filename:base "%[base]" "constellations/results/scaled/%[filename:base].png"')
  system(cmd)
}

# convert "*.png" -set filename:base "%[base]" -trim +repage "scaled/%[filename:base].png"
# convert "*.png[500x]" -set filename:base "%[base]" "%[filename:base].png"