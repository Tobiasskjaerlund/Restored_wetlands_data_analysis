library(tidyverse)
library(readxl)
library(janitor)
library(writexl)
library(ggplot2)

setwd("C:/Users/Tskja/Desktop/Speciale/sum_inv_range")

# selected_sites<- janitor::clean_names(read_xlsx("data/Rank_based_select_ref_final.xlsx"))
selected_sites<- janitor::clean_names(read_xlsx("data/Rank_based_select_ref_update_30_03.xlsx"))
novana_plant_data_raw <-janitor::clean_names(read_xlsx("data/Novana_plant_data.xlsx"))
vegetations_register_raw <-janitor::clean_names(read_xlsx("data/vegetations_register_12marts.xlsx"))
# range_size_old <- read.table("data/Rangesizes_space_cor.csv", sep=";",header =T)
range_size <- janitor::clean_names(read_xlsx("data/Rangesizes_AFD_condensed_species.xlsx"))%>% select(.,sync_name,species_level)
remove_novana <-read_xlsx("data/remove_novana.xlsx")

# _______ Novana sites _______
# creating subset
novana_plant_subset<- novana_plant_data_raw %>%
  select(akt_id,del_akt_navn,art_latin,er_fundet,c(pinpoint1:pinpoint16))%>%
  filter(akt_id %in% selected_sites$akt_id, del_akt_navn=="Floraregistrering, pinpoint"|del_akt_navn=="Floraregistrering, 5 m cirkel"|del_akt_navn=="Floraregistrering, prøvefelt")



# subspecies and var. habve been removed in excel 
# write_xlsx(novana_plant_subset,path = "data/novana_plant_subset_30_03.xlsx")

novana_plant_subset<- janitor::clean_names(read_xlsx("data/novana_plant_subset_30_03.xlsx"))
# remove bryophytes and some genus
novana_plant_subset<- novana_plant_subset %>%
  filter(.,!art_latin %in% c(remove_novana$remove_this))
# merge with the rangesize data
novana_plant_subset <- merge(novana_plant_subset, range_size, by.x = "art_latin", 
                   by.y = "sync_name", all.x = TRUE, all.y = FALSE)
# check for missing values
novana_missing<-novana_plant_subset[is.na(novana_plant_subset$species_level),]%>%
  select(art_latin,species_level)
# check for missing values
unique(novana_missing$art_latin)

#_____ restored sites: ______
restored_plant_subset<- vegetations_register_raw %>%
  select(scientific_name,plot)

restored_plant_subset<-restored_plant_subset%>%
  filter(.,!scientific_name %in% c(remove_novana$remove_this))

restored_plant_subset <- merge(restored_plant_subset, range_size, by.x = "scientific_name", by.y = "sync_name", all.x = TRUE, all.y = FALSE)
# check for missing values
restored_missing<-restored_plant_subset[is.na(restored_plant_subset$species_level),]%>%
  select(scientific_name,species_level)

# check for missing values
unique(restored_missing$scientific_name)

# Create a tibble with missing species names
update_species_names <- tibble(species_not_found = unique(c(novana_missing$art_latin,restored_missing$scientific_name)))
# Write to excel: 
# write_xlsx(update_species_names,path="C:/Users/Tskja/Desktop/Speciale/sum_inv_range/data/update_species_names.xlsx")
update_species_names_solved <- janitor::clean_names(read_xlsx("data/update_species_names_solved.xlsx"))

# replace names with the list
for(i in 1:length(update_species_names_solved$new_species_name)){
  range_size$sync_name[range_size$sync_name == update_species_names_solved$old_species_name[i]]<-update_species_names_solved$new_species_name[i]
}

# Create a column with the inversed range sízes
restored_rangesize <- restored_plant_subset %>%
  group_by(plot)%>%
  summarize(.,inv_rng_size = sum(1/species_level))

restored_rangesize_normalized<- restored_plant_subset %>%
  group_by(plot)%>%
  summarize(.,inv_rng_size = sum(1/(species_level/1300)))

# calculate rangesizes
novana_rangesize <- novana_plant_subset%>%
  group_by(akt_id)%>%
  summarize(.,inv_rng_size = sum(1/(species_level)))

novana_rangesize_normalized <- novana_plant_subset%>%
  group_by(akt_id)%>%
  summarize(.,inv_rng_size = sum(1/((species_level/1300))))

novana_plant_subset%>%group_by(art_latin)%>%summarise(count=n())%>%arrange(by=desc(count))
# se lige på de tungest vægtede planter
arrange(novana_plant_subset,by=species_level)%>% select(art_latin,akt_id,species_level)
arrange(restored_plant_subset,by=species_level)%>% select(scientific_name,plot,species_level)

# change the column names to match for appending 
colnames(restored_rangesize) <- c("akt_id","inv_rng_size")

# create a collected list
sum_inv_rangesize<- rbind(restored_rangesize,novana_rangesize)

# make a group index
sum_inv_rangesize <- cbind(sum_inv_rangesize,tibble(site_type =c(replicate(30,"restored"),replicate(28,"near-natural"))))

# Save data 
write_xlsx(sum_inv_rangesize,path = "data/sum_inv_rangesize.xlsx")

# Have a quick look at rangesize 
boxplot_inv_rng_size <- ggplot(sum_inv_rangesize, aes(y=inv_rng_size, x=site_type)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=T)+theme_classic()+ylab("Inversed range size")+xlab("")
boxplot_inv_rng_size

# test for difference between means
t.test(novana_rangesize$inv_rng_size,restored_rangesize$inv_rng_size)

# Sort to have get an overview
sum_inv_rangesize<- arrange(sum_inv_rangesize,by=inv_rng_size)

# Plot from smallest to largest
scatterplot_inv_range_size<- ggplot(sum_inv_rangesize, aes(y=inv_rng_size, x =c(1:58)))+
  geom_point(aes(size=inv_rng_size, col=site_type))
scatterplot_inv_range_size

# Calculate richness for Novana: 
novana_plant_richness<- novana_plant_subset%>%
  select(.,akt_id,art_latin)%>%
  group_by(akt_id)%>%
  summarise(richness =length(unique(art_latin)))

# Calculate richness for restored
restored_plant_richness<-restored_plant_subset%>%
  select(.,plot,scientific_name)%>%
  group_by(plot)%>%
  summarise(length(unique(scientific_name)))

# Change column names to match
colnames(restored_plant_richness) <- c("akt_id","richness")

# Append tibble to contain both
plant_richness<- rbind(restored_plant_richness,novana_plant_richness)

# Merge by akt ID with SIR
plant_richness <- merge(plant_richness, sum_inv_rangesize, by.x = "akt_id", by.y = "akt_id", all.x = TRUE, all.y = FALSE)

# Plot SIR against richness
richness_inv_rng_size<- ggplot(plant_richness, aes(y=inv_rng_size, x =richness,color=site_type, label =akt_id))+
  geom_point()+
  geom_smooth(method = "lm", fill = NA)+
  geom_text(check_overlap=T)+
  theme_classic()+
  ggtitle("Site richness against Site Inversed Range Size")

# Save file
write_xlsx(plant_richness,path = "data/SIR_richness_04_04.xlsx")



# Take a look at the species that occurs on most plots

vegetations_register_raw%>%group_by(scientific_name)%>%summarize(occurence=sum(inner_circle)+sum(outer_circle))%>%arrange(desc(occurence))

novana_plant_data_raw%>%group_by(art_dansk)%>%summarize(occurence=n())%>%arrange(desc(occurence))%>%View()

