install.packages("vctrs")
library(tidyverse)
library(readxl)
library(janitor)
library(corrr)
library(ggcorrplot)
library("FactoMineR")
library("factoextra")

raw_abiotic_variables <- janitor::clean_names(read_xls("data/Abiotic_variables.xls"))

# select data and remove NA
abiotic_full <- raw_abiotic_variables%>%
  select(.,!c(planteprove_n_percent,jord_p_percent,akt_id))%>%
  na.omit()

# Check all data for NA
colSums(is.na(abiotic_full))

# see data
str(abiotic_full)

# Create numeric only subset
numerical_data <- abiotic_full[,3:17]

# scale data
data_normalized <- scale(numerical_data)
head(data_normalized)

#create correlation plot
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)

# create a PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)

fviz_eig(data.pca, addlabels = TRUE)

fviz_pca_biplot(data.pca, repel = TRUE)

