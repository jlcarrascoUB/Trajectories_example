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



# Figure 1 ----------------------------------------------------------------



map <- get_googlemap(center = c(lon = 0.65, lat = 40.46), zoom=10, scale=2,color="bw")
pbanya<-ggmap(map, extent = 'device')

map2 <- get_googlemap(center = c(lon = 0.6, lat = 40.4), zoom=10, scale=2,color="bw")
pbanya2<-ggmap(map2, extent = 'device')



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


p1<-pbanya + geom_path(mapping=aes(LONG,LAT,group=trip,colour=trip),linewidth=4,data=aux,
                       show.legend = FALSE) +
  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=20)

p1 <- p1 +  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=5)+
  ggtitle("Subject 1")+ 
  theme(plot.title = element_text(size = 40, face = "bold"))

# Subject 5107913

tr21<-dades %>% filter((ID == 5107913) & (trip=="V01"))
tr22<-dades %>% filter((ID == 5107913) & (trip=="V02"))
tr23<-dades %>% filter((ID == 5107913) & (trip=="V03"))


pp21 = SpatialPoints(tr21[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp22 = SpatialPoints(tr22[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp23 = SpatialPoints(tr23[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))



aux<-data.frame(rbind(pp21,pp22,pp23),trip=c(rep("V01",length(pp21)),rep("V02",length(pp22)),
                                             rep("V03",length(pp23))))


p2<-pbanya + geom_path(mapping=aes(LONG,LAT,group=trip,colour=trip),linewidth=4,data=aux,
                       show.legend = FALSE) +
  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=20)+
  ggtitle("Subject 2")+ 
  theme(plot.title = element_text(size = 40, face = "bold"))





# Subject 5107916

tr31<-dades %>% filter((ID == 5107916) & (trip=="V01"))
tr32<-dades %>% filter((ID == 5107916) & (trip=="V02"))
tr33<-dades %>% filter((ID == 5107916) & (trip=="V03"))


pp31 = SpatialPoints(tr31[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp32 = SpatialPoints(tr32[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp33 = SpatialPoints(tr33[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))



aux<-data.frame(rbind(pp31,pp32,pp33),trip=c(rep("V01",length(pp31)),rep("V02",length(pp32)),
                                             rep("V03",length(pp33))))

p3<-pbanya2 + geom_path(mapping=aes(LONG,LAT,group=trip,colour=trip),linewidth=4,data=aux,
                       show.legend = FALSE) +
  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=20)+
  ggtitle("Subject 3")+ 
  theme(plot.title = element_text(size = 40, face = "bold"))



# Subject 5107917

tr41<-dades %>% filter((ID == 5107917) & (trip=="V01"))
tr42<-dades %>% filter((ID == 5107917) & (trip=="V02"))
tr43<-dades %>% filter((ID == 5107917) & (trip=="V03"))


pp41 = SpatialPoints(tr41[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp42 = SpatialPoints(tr42[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))
pp43 = SpatialPoints(tr43[c("LONG","LAT")], proj4string=CRS("+proj=longlat"))

aux<-data.frame(rbind(pp41,pp42,pp43),trip=c(rep("V01",length(pp41)),rep("V02",length(pp42)),
                                         rep("V03",length(pp43))))

p4<-pbanya + geom_path(mapping=aes(LONG,LAT,group=trip,colour=trip),linewidth=4,data=aux,
                       show.legend = FALSE) +
  geom_point(data=sp_colony,mapping=aes(x=x,y=y),shape="*",col="red",cex=20)+
  ggtitle("Subject 4")+ 
  theme(plot.title = element_text(size = 40, face = "bold"))



g <- grid.arrange(p1, p2, p3, p4,nrow = 2)

g
ggsave("Figure1.pdf",g ,width=960, height=960 , units="mm", dpi=720)



# Repeatability Example ---------------------------------------------------


out<-iccTraj(dades,"ID","trip","LONG","LAT","triptime", distance="H",nBoot = 1000)



# Summary medHD

summary(out$D$d)



# Figure 2 ----------------------------------------------------------------


pden<-ggplot(data=out$D,aes(x=d))+geom_density(bw=10) + 
  theme_classic(base_size = 20) +  xlab("medHD (km)") 


ggsave("Figure2.pdf",pden , units="mm", dpi=720)



# ICC estimate and standard error

# ICC estimate
out$est

# Empirical bootstrap CI
interval(out)


# Using discrete Fréchet distance



out_F<-iccTraj(dades,"ID","trip","LONG","LAT","triptime", distance="F",nBoot = 1000)

# ICC estimate
out_F$est

# Empirical bootstrap CI
interval(out_F)

# Application to standard normal data -------------------------------------


library(rptR)
data(BeetlesBody)


out<-rpt(BodyL ~ (1 | Population), grname = "Population", data = BeetlesBody, datatype = "Gaussian", 
         nboot = 0, npermut = 0)

out$R

library(cccrm)

est<-icc(BeetlesBody, ry="BodyL", rind="Population")
est$icc

# Estimate with ICC by distances
# Number of data
n<-nrow(BeetlesBody)
# Number of subjects
ns<-length(unique(BeetlesBody$Population))
# Number of data by subject
nt<-BeetlesBody %>% group_by(Population) %>% summarise(n=n())
k<-mean(nt$n)

# Euclidean distance among data with data ordered by subject
X<-as.matrix(dist(BeetlesBody$BodyL))

# Sample estimate
est.r<-ICC(X,nt)
est.r



# Simulation Study --------------------------------------------------------


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

### Number of replicates different by subject

sim_icc2<-function(n,k,p){
  
  # Number of trips by subject
  nk<-round(runif(n,2,2*k-2),0)
  
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
    foreach(i=(1:nk[j]), .combine=rbind, .packages=c("dplyr","adehabitatLT")) %dopar%{
      if (i<nk[j]*p){
        u <- simm.crw(1:100, r = 0.6, burst = paste("ID",j,sep=""))
      }
      else u<-base_traj[[j]]
      u[[1]] %>% select(x,y,date) %>% mutate(trip=paste("V",i,sep=""),ID=j) %>%
        rename(LAT=y, LONG=x, daytime = date)
      
    }
  
  
  registerDoSEQ() 
  
  
  out<-try(iccTraj(trips_sim,"ID","trip","LONG","LAT","daytime",distance="H"))
  
  if (sum(class(out)=="try-error")==0) out_ret<-data.frame(out$est$r,mean(out$boot$r),sd(out$boot$r))
  if (sum(class(out)=="try-error")>0) out_ret<-data.frame(rep(NA,3))
  return(out_ret)
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


sim_res <- bind_rows(sim0,sim1,sim2,sim3) %>% arrange(n,k,P)


results <- sim_res %>% group_by(n,k,P) %>% summarize(mr=mean(r),
                                                     mb=mean(mboot),
                                                     sdr=sd(r), 
                                                     sdb=sqrt(mean(sdboot^2))
)



set.seed(2023)
sim0_0b<-1:100 %>% map_df(function(i) sim_icc2(20,5,1))
sim1_0b<-1:100 %>% map_df(function(i) sim_icc2(20,5,0.7))
sim2_0b<-1:100 %>% map_df(function(i) sim_icc2(20,5,0.5))
sim3_0b<-1:100 %>% map_df(function(i) sim_icc2(20,5,0.3))
sim4_0b<-1:100 %>% map_df(function(i) sim_icc2(20,5,0))


set.seed(2023)
sim0_1b<-1:100 %>% map_df(function(i) sim_icc2(20,10,1))
sim1_1b<-1:100 %>% map_df(function(i) sim_icc2(20,10,0.7))
sim2_1b<-1:100 %>% map_df(function(i) sim_icc2(20,10,0.5))
sim3_1b<-1:100 %>% map_df(function(i) sim_icc2(20,10,0.3))
sim4_1b<-1:100 %>% map_df(function(i) sim_icc2(20,10,0))



set.seed(2023)
sim0_2b<-1:100 %>% map_df(function(i) sim_icc2(100,10,1))
sim1_2b<-1:100 %>% map_df(function(i) sim_icc2(100,10,0.7))
sim2_2b<-1:100 %>% map_df(function(i) sim_icc2(100,10,0.5))
sim3_2b<-1:100 %>% map_df(function(i) sim_icc2(100,10,0.3))
sim4_2b<-1:100 %>% map_df(function(i) sim_icc2(100,10,0))

set.seed(2023)
sim0_3b<-1:100 %>% map_df(function(i) sim_icc2(100,5,1))
sim1_3b<-1:100 %>% map_df(function(i) sim_icc2(100,5,0.7))
sim2_3b<-1:100 %>% map_df(function(i) sim_icc2(100,5,0.5))
sim3_3b<-1:100 %>% map_df(function(i) sim_icc2(100,5,0.3))
sim4_3b<-1:100 %>% map_df(function(i) sim_icc2(100,5,0))



sim0b <- bind_rows(sim0_0b,sim1_0b,sim2_0b,sim3_0b,sim4_0b) %>% 
  mutate(n=20,k=5,P=c(rep(1,100),rep(0.7,100),rep(0.5,100),rep(0.3,100),rep(0,100))) %>%
  rename(r = est.r, mboot= mean.H_boot.r., sdboot=sd.H_boot.r. )


sim1b <- bind_rows(sim0_1b,sim1_1b,sim2_1b,sim3_1b,sim4_1b) %>% 
  mutate(n=20,k=10,P=c(rep(1,100),rep(0.7,100),rep(0.5,100),rep(0.3,100),rep(0,100))) %>%
  rename(r = est.r, mboot= mean.H_boot.r., sdboot=sd.H_boot.r. )

sim2b <- bind_rows(sim0_2b,sim1_2b,sim2_2b,sim3_2b,sim4_2b) %>% 
  mutate(n=100,k=10,P=c(rep(1,100),rep(0.7,100),rep(0.5,100),rep(0.3,100),rep(0,100))) %>%
  rename(r = est.r, mboot= mean.H_boot.r., sdboot=sd.H_boot.r. )

sim3b <- bind_rows(sim0_3b,sim1_3b,sim2_3b,sim3_3b,sim4_3b) %>% 
  mutate(n=100,k=5,P=c(rep(1,100),rep(0.7,100),rep(0.5,100),rep(0.3,100),rep(0,100))) %>%
  rename(r = est.r, mboot= mean.H_boot.r., sdboot=sd.H_boot.r. )


sim_resb <- bind_rows(sim0b,sim1b,sim2b,sim3b) %>% arrange(n,k,P)


resultsb <- sim_resb %>% group_by(n,k,P) %>% summarize(mr=mean(r),
                                                       mb=mean(mboot),
                                                       sdr=sd(r), 
                                                       sdb=sqrt(mean(sdboot^2))
)





###### Removing intermediate data

temp <- dades %>% group_by(ID,trip) %>%
  filter(row_number() %% 2 == 1)

out_10<-iccTraj(temp,"ID","trip","LONG","LAT","triptime", distance="H",nBoot = 1000)


temp <- dades %>% group_by(ID,trip) %>%
  filter(row_number() %% 4 == 1)

out_20<-iccTraj(temp,"ID","trip","LONG","LAT","triptime", distance="H",nBoot = 1000)


temp <- dades %>% group_by(ID,trip) %>%
  filter(row_number() %% 6 == 1)

out_30<-iccTraj(temp,"ID","trip","LONG","LAT","triptime", distance="H",nBoot = 1000)



# ICC estimate
out$est
out_10$est
out_20$est

# Empirical bootstrap CI
interval(out)
diff(interval(out_10))
diff(interval(out_20))



# Example of Hausdorff's distance -----------------------------------------


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

aux<-data.frame(rbind(pp11,pp12),trip=c(rep("V01",length(pp11)),
                                             rep("V02",length(pp12))
                                        )
)



map <- get_googlemap(center = c(lon = 0.658, lat = 40.65), zoom=11, scale=1,color="bw")
pbanya<-ggmap(map, extent = 'device')


p1<-pbanya + geom_path(mapping=aes(LONG,LAT,group=trip,colour=trip),linewidth=4,data=aux,
                       show.legend = FALSE) 

HD<-function(pp1,pp2,q=1){
  ds<-spDists(pp1, pp2, longlat = TRUE)
  m1<-quantile(apply(ds,1,min),q)
  m2<-quantile(apply(ds,2,min),q)
  max(m1,m2)
}

HD(pp11,pp12)
ds<-spDists(pp11, pp12, longlat = TRUE)

m1<-quantile(apply(ds,1,min),1)
m2<-quantile(apply(ds,2,min),1)

which(ds == m1, arr.ind = TRUE)

sp_1<-data.frame(data.frame(pp11[30,]))
sp_2<-data.frame(data.frame(pp12[6,]))
sp_p1<-rbind(sp_1,sp_2)

which(ds == m2, arr.ind = TRUE)
sp_3<-data.frame(data.frame(pp11[6,]))
sp_4<-data.frame(data.frame(pp12[11,]))
sp_p2<-rbind(sp_3,sp_4)


p1<-p1 + geom_point(data=sp_p1,mapping=aes(x=LONG,y=LAT),col="black",cex=20) + 
  geom_point(data=sp_p2,mapping=aes(x=LONG,y=LAT),col="black",cex=20)+
  geom_segment(data=sp_p1,aes(x=LONG[1], y=LAT[1], xend=LONG[2], yend=LAT[2]), 
               arrow = arrow(length=unit(0.1,"inches")),linetype=5,cex=4) +
  geom_segment(data=sp_p2,aes(x=LONG[2], y=LAT[2], xend=LONG[1], yend=LAT[1]), 
               arrow = arrow(length=unit(0.1,"inches")),cex=4)


p1
ggsave("Figure_HD.pdf",p1 ,width=960, height=960 , units="mm", dpi=720)


# Individual within-subjects  ---------------------------------------------

indR <- out$indW %>% mutate(sb=out$est$sb, r=sb/(sb+w))
indR

