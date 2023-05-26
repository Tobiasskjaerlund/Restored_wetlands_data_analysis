library(readxl)
library(tidyverse)
library(janitor)
library(writexl)


# read data
besigtigelse_arter_raw <- janitor::clean_names(read_xlsx("data/besigtigelse_arter_data.xlsx"))
besigtigelse_ref <- janitor::clean_names(read_xlsx("data/hojnaturkval_ref.xlsx"))
ellenberg_values <- janitor::clean_names(read_xlsx("data/ellenberg_registre.xlsx"))
danish_bryophytes <- janitor::clean_names(read_delim("data/redlist_extract_mos.csv"))
name_update <- janitor::clean_names(read_xlsx("data/manual_name_corrections.xlsx"))
AFD_range_sizes <-janitor::clean_names(read_xlsx("data/Rangesizes_AFD_condensed_species.xlsx"))%>%select(.,sync_name,species_level)
update_names_SIR <-janitor::clean_names(read_xlsx("data/update_species_names_solved.xlsx"))


# see species that is not in the ellenberg values
aev1<- unique(besigtigelse_arter_raw$art_latin[!besigtigelse_arter_raw$art_latin %in% ellenberg_values$species])
sort(aev1)
# find single word genus to remove
genus_to_remove<- aev1[!str_detect(aev,". .")]

# remove genus from observations
besigtigelse_arter_raw <- besigtigelse_arter_raw[!besigtigelse_arter_raw$art_latin %in% genus_to_remove,]
                                                 
# remove bryophytes
Sphagnum_to_remove <- besigtigelse_arter_raw$art_latin[str_detect(besigtigelse_arter_raw$art_latin, "Sphagnum.")]
besigtigelse_arter_raw <- besigtigelse_arter_raw[!besigtigelse_arter_raw$art_latin %in% danish_bryophytes$scientific_name,]
besigtigelse_arter_raw <- besigtigelse_arter_raw[!besigtigelse_arter_raw$art_latin %in% Sphagnum_to_remove,]

# remove species of other groups
besigtigelse_arter_raw <- besigtigelse_arter_raw[!besigtigelse_arter_raw$art_latin %in% name_update$delete,]

# Find and replace all Variants and subspecies
var <- besigtigelse_arter_raw$art_latin[str_detect(besigtigelse_arter_raw$art_latin, ". var. ")]
art_var <- str_extract(var, "^[A-Z][a-z]+ [a-z]+")
besigtigelse_arter_raw$art_latin[str_detect(besigtigelse_arter_raw$art_latin, ". var.")]<-art_var

ssp <- besigtigelse_arter_raw$art_latin[str_detect(besigtigelse_arter_raw$art_latin, ". subsp. .")]
art_ssp <- str_extract(ssp, "^[A-Z][a-z]+ [a-z]+")
besigtigelse_arter_raw$art_latin[str_detect(besigtigelse_arter_raw$art_latin, ". subsp. .")]<-art_ssp

# update names for ellenberg
for(i in 1:length(besigtigelse_arter_raw$art_latin)){
  if(besigtigelse_arter_raw$art_latin[i] %in% name_update$besigt_name){
    besigtigelse_arter_raw$art_latin[i]<-(name_update$ellenberg_name[match(besigtigelse_arter_raw$art_latin[i],name_update$besigt_name)])}
}

# list of species lacking ellenberg values
aev2<- unique(besigtigelse_arter_raw$art_latin[!besigtigelse_arter_raw$art_latin %in% ellenberg_values$species])

# merge with ellenberg
besigtigelse_ellenberg<-merge(besigtigelse_arter_raw,ellenberg_values,by.x="art_latin",by.y="species")


unique(besigtigelse_ellenberg[!besigtigelse_ellenberg$art_latin %in% AFD_range_sizes$sync_name,]$art_latin)

for(i in 1:length(update_names_SIR$new_species_name)){
  AFD_range_sizes$sync_name[AFD_range_sizes$sync_name == update_names_SIR$old_species_name[i]]<-update_names_SIR$new_species_name[i]
}

besigtigelse_ellenberg_SIR<-merge(besigtigelse_ellenberg,AFD_range_sizes,by.x="art_latin",by.y="sync_name")

# write_xlsx(besigtigelse_ellenberg_SIR, path="data/besigtigelse_ellenberg_SIR_name_corrected.xlsx")

