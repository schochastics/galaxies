rabbit <- jpeg::readJPEG("~/Downloads/rabbit.jpg")
tst <- reshape2::melt(rabbit)
tst %>% 
  group_by(Var1,Var2) %>% 
  dplyr::summarise(value=mean(value)) %>% 
  dplyr::filter(value<=0.05) -> tst

ggplot(tst,aes(Var2,800-Var1))+geom_raster(aes(fill=value))

rabchull <- concaveman::concaveman(cbind(tst$Var1,tst$Var2))
df_rabbit_chull <- as_tibble(rabchull) %>% 
  rowwise() %>% 
  mutate(p=runif(1)) %>%
  ungroup() %>% 
  dplyr::filter(p>=0.95) %>% 
  select(-p) %>% 
  rename(x=V2,y=V1)

df_rabbit_chull %>% 
  ggplot(aes(V1,V2))+geom_path()+geom_point()

df_rabbit_in <- tst %>% 
  sample_frac(0.0025) %>% 
  select(-value) %>% 
  rename(y=Var1,x=Var2)

df_rabbit <- bind_rows(df_rabbit_chull,df_rabbit_in) %>% distinct()

ggplot(df_rabbit,aes(x, y)) +
  # geom_point()+
  geom_voronoi_tile(alpha = 0.3, colour = 'black',fill=NA,bound=df_rabbit_chull)+
  geom_point(stat="delvor_summary")

dst_rab <- as.matrix(dist(df_rabbit))
diag(dst_rab) <- 1e7
adj <- lapply(1:nrow(dst_rab),function(x)order(dst_rab[x,])[1:4])
g <- graph_from_adj_list(adj,"in")
g <- as.undirected(g,"collapse")
V(g)$x <- df_rabbit$x
V(g)$y <- df_rabbit$y

dat2 <- data.frame(x=runif(800,-100,900),y=runif(800,-100,900))
dat2$size <- normalise(gen_simplex(dat2$x,dat2$y),to=c(0.8,2.3))
dat2$x <- dat2$x+150*gen_simplex(dat2$x,dat2$y)
dat2$y <- dat2$y+150*gen_simplex(dat2$x,dat2$y)

ggraph(g,"manual",x=V(g)$x,y=800-V(g)$y)+
  geom_edge_link0(edge_colour="white",edge_width=0.2)+
  geom_node_point(shape=16,color="white")+
  geom_point(data=dat2,aes(x,y,size=I(size)),col="white",alpha=0.7,shape=16)+
  theme_graph(background = "black")
