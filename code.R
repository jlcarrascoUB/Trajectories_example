load("trajectory_data.RData")

library(iccTraj)
library(readxl)
library(leaflet)
library(ggmap)
library(gridExtra)
library(ggplot2)
library(cccrm)
library(dplyr)
library(sp)


### Figure 1

# Subject 5107912
tr11<-dades %>% filter((ID == 5107912) & (trip=="V01"))
tr12<-dades %>% filter((ID == 5107912) & (trip=="V02"))
tr13<-dades %>% filter((ID == 5107912) & (trip=="V03"))


pp11 = SpatialPoints(tr11[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp12 = SpatialPoints(tr12[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp13 = SpatialPoints(tr13[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))

sp_colony<-data.frame(y=40.575,x=0.658)

aux<-data.frame(rbind(pp11,pp12,pp13),trip=c(rep("V01",length(pp11)),
                                             rep("V02",length(pp12)),
                                             rep("V03",length(pp13))
))


map <- get_googlemap(center = c(lon = 0.658, lat = 40.65), zoom=9, scale=1)
pbanya<-ggmap(map, extent = 'device')

p1<-pbanya + geom_path(mapping=aes(LONG,LAT,group=trip,colour=trip),linewidth=1.5,data=aux,
                       show.legend = FALSE) +
  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=5)

p1 <- p1 +  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=5)+
  ggtitle("Subject 1")


# Subject 5107913

tr21<-dades %>% filter((ID == 5107913) & (trip=="V01"))
tr22<-dades %>% filter((ID == 5107913) & (trip=="V02"))
tr23<-dades %>% filter((ID == 5107913) & (trip=="V03"))


pp21 = SpatialPoints(tr21[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp22 = SpatialPoints(tr22[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp23 = SpatialPoints(tr23[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))



aux<-data.frame(rbind(pp21,pp22,pp23),trip=c(rep("V01",length(pp21)),rep("V02",length(pp22)),
                                             rep("V03",length(pp23))))


p2<-pbanya + geom_path(mapping=aes(LONG,LAT,group=trip,colour=trip),size=1.5,data=aux,
                       show.legend = FALSE) +
  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=5)+
  ggtitle("Subject 2")




# Subject 5107916

tr31<-dades %>% filter((ID == 5107916) & (trip=="V01"))
tr32<-dades %>% filter((ID == 5107916) & (trip=="V02"))
tr33<-dades %>% filter((ID == 5107916) & (trip=="V03"))


pp31 = SpatialPoints(tr31[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp32 = SpatialPoints(tr32[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp33 = SpatialPoints(tr33[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))



aux<-data.frame(rbind(pp31,pp32,pp33),trip=c(rep("V01",length(pp31)),rep("V02",length(pp32)),
                                             rep("V03",length(pp33))))

p3<-pbanya + geom_path(mapping=aes(LONG,LAT,group=trip,colour=trip),size=1.5,data=aux,
                       show.legend = FALSE) +
  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=5)+
  ggtitle("Subject 3")


# Subject 5107917

tr41<-dades %>% filter((ID == 5107917) & (trip=="V01"))
tr42<-dades %>% filter((ID == 5107917) & (trip=="V02"))
tr43<-dades %>% filter((ID == 5107917) & (trip=="V03"))


pp41 = SpatialPoints(tr41[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp42 = SpatialPoints(tr42[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp43 = SpatialPoints(tr43[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))

aux<-data.frame(rbind(pp41,pp42,pp43),trip=c(rep("V01",length(pp41)),rep("V02",length(pp42)),
                                         rep("V03",length(pp43))))

p4<-pbanya + geom_path(mapping=aes(LONG,LAT,group=trip,colour=trip),size=1.5,data=aux,
                       show.legend = FALSE) +
  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=5)+
  ggtitle("Subject 4")


g <- grid.arrange(p1, p2, p3, p4,nrow = 2)

ggsave("Figure1.pdf",g ,width=960, height=960 , units="mm", dpi=720)



### Repeatability

out<-iccTraj(dades,"ID","trip","LONG","LAT","triptime", distance="H",nBoot = 1000)

# Summary medHD

summary(out$D$d)


# Figure 2
pden<-ggplot(data=out$D,aes(x=d))+geom_density(bw=10) + 
  theme_classic() +
  xlab("medHD") 


ggsave("Figure2.pdf",pden , units="mm", dpi=720)



# ICC estimate and standard error

# ICC estimate
out$est

# Empirical bootstrap CI
interval(out)


# Simulation study

library(adehabitatLT)
library(doParallel)


# Completament aleatori
# 20 individus
# 5 viatges
# concentration parameter rho=exp(-(sigma^2)/2)


sim_icc<-function(n,k,p){
  
  # Simulates base trips
  
  nc<-detectCores()
  cl <- makeCluster(getOption("cl.cores", nc))
  registerDoParallel(cl)
  
  
  base_traj<-foreach(j=(1:n), .packages = "adehabitatLT")  %dopar%{
    simm.crw(1:100, r = 0.6, burst = paste("ID",j,sep=""))
  }
  
  
  registerDoSEQ() 
  
  
  # Simulates trips  
  
  nc<-detectCores()
  cl <- makeCluster(getOption("cl.cores", nc))
  registerDoParallel(cl)
  
  
  trips_sim<-foreach(j=(1:n),.combine=rbind, .packages = "adehabitatLT") %:% 
    foreach(i=(1:k), .combine=rbind, .packages=c("dplyr","adehabitatLT")) %dopar%{
      if (i<k*p){
        u <- simm.crw(1:100, r = 0.6, burst = paste("ID",j,sep=""))
      }
      else u<-base_traj[[j]]
      u[[1]] %>% select(x,y,date) %>% mutate(trip=i,ID=j) %>%
        rename(LAT=y, LONG=x, daytime = date)
      
    }
  
  
  registerDoSEQ() 
  

  out<-iccTraj(trips_sim,"ID","trip","LONG","LAT","daytime",distance="H")
  
  return(data.frame(out$est$r,mean(out$boot$r),sd(out$boot$r)))
  
}


library(purrr)


set.seed(2023)
sim0_0<-1:100 %>% map_df(function(i) sim_icc(20,5,1))
sim1_0<-1:100 %>% map_df(function(i) sim_icc(20,5,0.7))
sim2_0<-1:100 %>% map_df(function(i) sim_icc(20,5,0.5))
sim3_0<-1:100 %>% map_df(function(i) sim_icc(20,5,0.3))
sim4_0<-1:100 %>% map_df(function(i) sim_icc(20,5,0))


set.seed(2023)
sim0_1<-1:100 %>% map_df(function(i) sim_icc(20,10,1))
sim1_1<-1:100 %>% map_df(function(i) sim_icc(20,10,0.7))
sim2_1<-1:100 %>% map_df(function(i) sim_icc(20,10,0.5))
sim3_1<-1:100 %>% map_df(function(i) sim_icc(20,10,0.3))
sim4_1<-1:100 %>% map_df(function(i) sim_icc(20,10,0))



set.seed(2023)
sim0_2<-1:100 %>% map_df(function(i) sim_icc(100,10,1))
sim1_2<-1:100 %>% map_df(function(i) sim_icc(100,10,0.7))
sim2_2<-1:100 %>% map_df(function(i) sim_icc(100,10,0.5))
sim3_2<-1:100 %>% map_df(function(i) sim_icc(100,10,0.3))
sim4_2<-1:100 %>% map_df(function(i) sim_icc(100,10,0))

set.seed(2023)
sim0_3<-1:100 %>% map_df(function(i) sim_icc(100,5,1))
sim1_3<-1:100 %>% map_df(function(i) sim_icc(100,5,0.7))
sim2_3<-1:100 %>% map_df(function(i) sim_icc(100,5,0.5))
sim3_3<-1:100 %>% map_df(function(i) sim_icc(100,5,0.3))
sim4_3<-1:100 %>% map_df(function(i) sim_icc(100,5,0))



sim0 <- bind_rows(sim0_0,sim1_0,sim2_0,sim3_0,sim4_0) %>% 
  mutate(n=20,k=5,P=c(rep(1,100),rep(0.7,100),rep(0.5,100),rep(0.3,100),rep(0,100))) %>%
  rename(r = est.r, mboot= mean.H_boot.r., sdboot=sd.H_boot.r. )


sim1 <- bind_rows(sim0_1,sim1_1,sim2_1,sim3_1,sim4_1) %>% 
  mutate(n=20,k=10,P=c(rep(1,100),rep(0.7,100),rep(0.5,100),rep(0.3,100),rep(0,100))) %>%
  rename(r = est.r, mboot= mean.H_boot.r., sdboot=sd.H_boot.r. )

sim2 <- bind_rows(sim0_2,sim1_2,sim2_2,sim3_2,sim4_2) %>% 
  mutate(n=100,k=10,P=c(rep(1,100),rep(0.7,100),rep(0.5,100),rep(0.3,100),rep(0,100))) %>%
  rename(r = est.r, mboot= mean.H_boot.r., sdboot=sd.H_boot.r. )

sim3 <- bind_rows(sim0_3,sim1_3,sim2_3,sim3_3,sim4_3) %>% 
  mutate(n=100,k=5,P=c(rep(1,100),rep(0.7,100),rep(0.5,100),rep(0.3,100),rep(0,100))) %>%
  rename(r = est.r, mboot= mean.H_boot.r., sdboot=sd.H_boot.r. )


sim <- bind_rows(sim0,sim1,sim2,sim3) %>% arrange(n,k,P)


results_sim <- sim_res %>% group_by(n,k,P) %>% summarize(mr=mean(r),
                                                     mb=mean(mboot),
                                                     sdr=sd(r), 
                                                     sdb=sqrt(mean(sdboot^2))
)

library(ggplot2)

ggplot(sim,aes(x=r,y=mboot)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1) +  facet_grid(n + k ~ P)

ggplot(sim,aes(y=r)) + geom_boxplot() + 
  facet_grid(n + k ~ P)


results_20_5<-sim %>% filter(n==20, k==5)
results_20_10<-sim %>% filter(n==20, k==10)
results_100_5<-sim %>% filter(n==100, k==5)
results_100_10<-sim %>% filter(n==100, k==10)

labP <- function(x) paste("P =",x,sep=" ")


ggplot(results_20_5,aes(y=r)) + geom_boxplot() + 
  facet_grid(. ~ P, labeller = labeller(P=labP)) + geom_hline(yintercept=c(0,1)) +
  ylab("ICC") + scale_x_discrete() + theme_bw()

ggplot(results_20_10,aes(y=r)) + geom_boxplot() + 
  facet_grid(. ~ P, labeller = labeller(P=labP)) + geom_hline(yintercept=c(0,1)) +
  ylab("ICC") + scale_x_discrete() + theme_bw()

ggplot(results_100_5,aes(y=r)) + geom_boxplot() + 
  facet_grid(. ~ P, labeller = labeller(P=labP)) + geom_hline(yintercept=c(0,1)) +
  ylab("ICC") + scale_x_discrete() + theme_bw()

ggplot(results_100_10,aes(y=r)) + geom_boxplot() + 
  facet_grid(. ~ P, labeller = labeller(P=labP)) + geom_hline(yintercept=c(0,1)) +
  ylab("ICC") + scale_x_discrete() + theme_bw()



# Standard error

results_sim_20_5 <- results_sim %>% select(n,k,P,sdr,sdb) %>% 
  filter(n==20,k==5)

results_sim <- results_sim %>% mutate(k=factor(k),n=n, P=factor(P))

ggplot(results_sim,aes(x=sdr,y=sdb, color = k, shape=P, size=factor(n))) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + theme_bw() +
  ylab("Bootstrap SE") + xlab("Estimates SE")

library(knitr)
library(kableExtra)


results_sim %>% select(n,k,P,sdr,sdb) %>% 
  kable(col.names=c("n","k","P","SD Estimates","Bootstrap SE"),
        digits=c(0,0,0,4,4), format="latex") %>%
  kable_classic_2(full_width = F)




