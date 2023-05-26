library(readxl)
library(tidyverse)
library(janitor)
library(RColorBrewer)
library(vegan)
library(writexl)

# read data
besigtigelse_arter_raw <- janitor::clean_names(read_xlsx("data/besigtigelse_ellenberg_SIR_name_corrected.xlsx"))
meta_data <- janitor::clean_names(read_xlsx("C:/Users/Tskja/Desktop/Speciale/source_data/combined_data_biodiv_indices.xlsx"))
# besigtigelse_arter_raw <- janitor::clean_names(read_xlsx("data/besigtigelse_arter_data.xlsx"))
besigtigelse_ref <- janitor::clean_names(read_xlsx("data/hojnaturkval_ref.xlsx"))


# Merge datasets
simple_ref<- besigtigelse_ref %>%select(aktid,location_id,site_type,ler_sand,location_id)
simple_art<- besigtigelse_arter_raw%>%select(akt_id,art_latin,f,n,l,r,s,species_level)
besigtigelse<-merge(simple_ref,simple_art,by.x="aktid",by.y="akt_id")

besigtigelse[besigtigelse$location_id=="TS8",]

# Calculate Richness 
richness_quick<-besigtigelse%>%group_by(aktid)%>%summarise(jordtype = mean(ler_sand),location_id=unique(location_id), site_type=unique(site_type),richness=length(unique(art_latin)))%>%ungroup()

richness_quick_grouped<-besigtigelse%>%group_by(location_id,site_type)%>%summarise(jordtype = mean(ler_sand),location_id=unique(location_id), site_type=unique(site_type),richness=length(unique(art_latin)))%>%mutate(., group_id = paste0(location_id,"_",site_type))%>%ungroup()

ellenberg_SIR<- besigtigelse%>%group_by(aktid)%>%summarise(ellenberg_N = mean(n, na.rm=TRUE),ellenberg_F = mean(f, na.rm=TRUE), SIR=sum(1/species_level))%>%ungroup()

ellenberg_SIR_grouped<- besigtigelse%>%group_by(location_id,site_type)%>%distinct()%>%summarise(ellenberg_N = mean(n, na.rm=TRUE),ellenberg_F = mean(f, na.rm=TRUE), SIR=sum(1/species_level))%>%mutate(., group_id = paste0(location_id,"_",site_type))%>%ungroup()

Besigtigelser_combined<- merge(richness_quick,ellenberg_SIR, by.x="aktid",by.y="aktid")

Besigtigelser_combined_grouped<- merge(richness_quick_grouped,ellenberg_SIR_grouped[,3:6], by.x="group_id",by.y="group_id")


# Richness for soiltype
boxplot(richness~jordtype, data=richness_quick)    

selected_color <- scale_fill_brewer(palette = "Paired")

#Calculate samplesize

sample_sizes<- Besigtigelser_combined%>% group_by(location_id,site_type)%>%summarize(samplesize = length(unique(aktid)))
Besigtigelser_combined%>% group_by(jordtype,site_type)%>%summarize(samplesize = length(unique(aktid)))
# Plot all richness for sites 
Besigtigelser_combined%>% 
  ggplot(
    aes(
      x = location_id,
      y = richness,
      fill = site_type
    )
  )+ 
  geom_boxplot() +
  labs(title = "Species richness restored sites and near-natural sites")+
  theme_classic()+ selected_color+xlab("")+ylab("Species richness")+
  annotate("text",
          x = aggregate(richness ~ site_type+location_id, richness_quick, median)[ , 2],
          y = rep(c(1,3),times=10),
          label = paste0(sample_sizes$site_type," n=",sample_sizes$samplesize),
          col = "black")

# correct the one datapoint with mixed soil
Besigtigelser_combined[Besigtigelser_combined$jordtype==23,]$jordtype <- 45
Besigtigelser_combined[Besigtigelser_combined$jordtype==1,]$jordtype <- "Sand dominated soil"
Besigtigelser_combined[Besigtigelser_combined$jordtype==45,]$jordtype <- "Clay dominated soil"
# Plot soiltype between sites
Besigtigelser_combined%>% 
  ggplot(
    aes(
      x = as.factor(jordtype),
      y = richness,
      fill = site_type
    )
  )+ 
  geom_boxplot() +
  labs(title = "Species richness restored sites and Near-natural sites")+
  theme_classic()+ selected_color+xlab("")+ylab("Species richness")

Besigtigelser_combined%>% 
  ggplot(
    aes(
      x = as.factor(jordtype),
      y = log(SIR),
      fill = site_type
    )
  )+ 
  geom_boxplot() +
  labs(title = "Sum of inverse range size restored sites and Near-natural sites")+
  theme_classic()+ selected_color+xlab("")+ylab("Log(SIR)")


# Violoin plot nrichness restored 
Besigtigelser_combined%>% 
  ggplot(
    aes(x = site_type, y = richness,fill = site_type)) + 
  geom_violin() + 
  geom_boxplot(width=0.1)+
  labs(title = "Species richness")+
  theme_classic()+
  annotate("text",
         x = 1:length(table(richness_quick$site_type)),
         y = aggregate(richness ~ site_type, richness_quick, median)[ , 2],
         label = paste("n =",table(richness_quick$site_type)),
         col = "black",
         hjust = +1.5)+
  ylab("Species richness")+xlab("")+ selected_color

# violin SIR
Besigtigelser_combined%>% 
  ggplot(
    aes(x = site_type, y = log(SIR),fill = site_type)) + 
  geom_violin() + 
  geom_boxplot(width=0.1)+
  labs(title = "Log(SIR)")+
  theme_classic()+ selected_color+
  annotate("text",
           x = 1:length(table(richness_quick$site_type)),
           y = aggregate(log(SIR) ~ site_type, Besigtigelser_combined, median)[ , 2],
           label = paste("n =",table(richness_quick$site_type)),
           col = "black",
           hjust = +1.5)+
  ylab("Log(SIR)")+xlab("")+ selected_color
  
# All SIR
Besigtigelser_combined%>% 
  ggplot(
    aes(
      x = location_id,
      y = log(SIR),
      fill = site_type
    )
  )+ 
  geom_boxplot() +
  labs(title = "Log(SIR) restored sites and Near-natural sites")+
  theme_classic()+ selected_color+
  annotate("text",
           x = aggregate(richness ~ site_type+location_id, richness_quick, median)[ , 2],
           y = rep(c(-5,-5.2),times=10),
           label = paste0(sample_sizes$site_type," n=",sample_sizes$samplesize),
           col = "black")

ggplot(data=Besigtigelser_combined,aes(x=ellenberg_N,y=ellenberg_F,col=site_type))+
  geom_point(aes(size=SIR))+stat_ellipse(level=0.95)+theme_classic()+ggtitle("Restored and Near-natural Ellenberg values")+xlab("Mean Ellenberg N")+ylab("Mean Ellenberg F")


ggplot(data=Besigtigelser_combined_grouped,aes(x=ellenberg_N,y=ellenberg_F,col=site_type))+
  geom_point()+stat_ellipse(level=0.95)+theme_classic()+ggtitle("Restored and Near-natural Ellenberg values")

ggplot(data=Besigtigelser_combined_grouped,aes(x=site_type,y=ellenberg_F, fill=site_type))+geom_boxplot()+theme_classic()+selected_color

ggplot(data=Besigtigelser_combined_grouped,aes(x=site_type,y=ellenberg_N, fill=site_type))+geom_boxplot()+theme_classic()+selected_color


shapiro.test(Besigtigelser_combined[Besigtigelser_combined$site_type=="Restored",]$ellenberg_F)

hist(Besigtigelser_combined[Besigtigelser_combined$site_type=="Near-natural",]$ellenberg_F)
shapiro.test(Besigtigelser_combined[Besigtigelser_combined$site_type=="Near-natural",]$ellenberg_F)

# testing relationships 
wilcox.test(ellenberg_N~site_type, data=Besigtigelser_combined)
wilcox.test(ellenberg_F~site_type, data=Besigtigelser_combined)
wilcox.test(richness~site_type, data=Besigtigelser_combined)
wilcox.test(SIR~site_type, data=Besigtigelser_combined)


summary(Besigtigelser_combined[Besigtigelser_combined$site_type=="Restored",])
summary(Besigtigelser_combined[Besigtigelser_combined$site_type=="Near-natural",])

sd(Besigtigelser_combined[Besigtigelser_combined$site_type=="Restored",]$ellenberg_F)
sd(Besigtigelser_combined[Besigtigelser_combined$site_type=="Near-natural",]$ellenberg_F)

# distance besigtigelser bray curtis
besigtigelse_absence_presents<- besigtigelse%>%select(location_id,art_latin,site_type)%>%group_by(location_id,site_type)%>%distinct()%>%mutate(occurence=1,location=paste0(location_id,"_",site_type))%>%pivot_wider(names_from=art_latin,values_from = occurence, values_fill=0)%>%arrange(site_type,location_id)

# create matrix
m.besigtigelse_absence_presents<- besigtigelse_absence_presents[4:ncol(besigtigelse_absence_presents)]

# make a empty tibble for depositing data
besigtigelse_distance<- tibble(site_restored=c(""),site_nearnat=c(""),jaccard=c(NULL),bray=c(NULL))

# loop throug and calculate distances
for(i in 1:10){
  besigtigelse_distance[i,1:4]<-tibble(site_restored=c(besigtigelse_absence_presents$location_id[i][1]),
                                       site_nearnat= c(besigtigelse_absence_presents$location_id[i+10][1]),
                                       jaccard=c(vegdist(m.besigtigelse_absence_presents[c(i,i+10),], method="jaccard")[1]),
                                       bray=c(vegdist(m.besigtigelse_absence_presents[c(i,i+10),], method="bray")[1]))}
besigtigelse_distance

Besigtigelser_combined<- merge(Besigtigelser_combined,besigtigelse_distance[,2:4], by.x="location_id",by.y="site_nearnat")

meta_data_selected<- meta_data%>%select(ts_sites_location_id,jord_n_percent,jord_p_h,management_wide)

Besigtigelser_combined<- merge(Besigtigelser_combined,meta_data_selected, by.x="location_id",by.y="ts_sites_location_id")

boxplot(jaccard~jordtype, data=Besigtigelser_combined)
boxplot(jaccard~management_wide, data=Besigtigelser_combined)
plot(jaccard~jord_p_h,col=as.factor(management_wide), data=Besigtigelser_combined)
plot(jaccard~jord_n_percent,col=as.factor(management_wide),pch=16, data=Besigtigelser_combined)

# write_xlsx(Besigtigelser_combined,path="C:/Users/Tskja/Desktop/Speciale/source_data/besigtigelser_combined.xlsx")
Besigtigelser_combined%>%group_by(site_type)%>%summarize(sd_SIR = sd(SIR),mean_SIR = mean(SIR))
wilcox.test(SIR~site_type, data=Besigtigelser_combined)
