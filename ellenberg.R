# create N, F and L values
N_table <- table_ellenberg %>%
  group_by(plot)%>%
  mutate(meanN = mean(N))%>%
  summarise(meanN=unique(meanN))

F_table <- table_ellenberg %>%
  group_by(plot)%>%
  mutate(meanF = mean(F))%>%
  summarise(meanF=unique(meanF))

L_table <- table_ellenberg %>%
  group_by(plot)%>%
  mutate(meanL = mean(L))%>%
  summarise(meanL=unique(meanL))

## explanatory variables ---
plot_parameters <- table_ellenberg%>%
  group_by(plot)%>%
  summarise(species_count= length(unique(`scientific name`)), east_gradient= unique(coordinate_x), end_date= as.integer(unique(end)),management_type =unique(management))%>%
  ungroup()

# Create old/young catagory
plot_parameters <- plot_parameters %>%
  mutate(., age_group = if_else(2008<end_date,"young","old"))

# Create multiple age catagories
plot_parameters <- plot_parameters %>%
  mutate(., age = if_else(2004>end_date,"2002-2003 n=6",if_else(2003 < end_date & 2009>end_date,"2004-2006 n=9",if_else(2008 < end_date & 2011>end_date,"2009-2011=n=8","2011-2017 n=7"))))

# enter the ellenberg values in the tibble
plot_parameters<- merge(plot_parameters,N_table,by.x ="plot", 
                        by.y = "plot", all.x = TRUE, all.y = FALSE)
plot_parameters<- merge(plot_parameters,F_table,by.x ="plot", 
                        by.y = "plot", all.x = TRUE, all.y = FALSE)
plot_parameters<- merge(plot_parameters,L_table,by.x ="plot", 
                        by.y = "plot", all.x = TRUE, all.y = FALSE)
# Simplify NP
unique_TotalNP<- TotalNP%>%group_by(plot)%>%mutate(unique_p = mean(total_p),unique_n = mean(total_n))%>% select(plot,unique_p,unique_n)%>%distinct()
# merge NP
plot_parameters<- merge(plot_parameters,unique_TotalNP,by.x ="plot", all.x = TRUE, all.y = FALSE)


# plot ellenberg vs. richness
ggplot(data=plot_parameters, aes(y=species_count, x=meanN))+geom_point()+labs(x="Mean Ellenberg N", y = "Species richness")+ggtitle("Restored sites")+geom_smooth(method = "lm")

model_species_EIVN <- lm(species_count~meanN,data=plot_parameters)
summary(model_species_EIVN)

plot_parameters

# Fit the mixed effect model
fit <- lm( species_count ~ meanN + management_type, data = plot_parameters)

# Print the summary of the model
summary(fit)

# calculate shannon index
shannon<- both_circle%>%group_by(plot)%>%mutate(.,Shannon =diversity(freq_plot))%>%select(Shannon,plot)%>% distinct()%>% ungroup()

# needs both circle thing to create shannon
plot_parameters<- merge(plot_parameters,shannon,by.x ="plot")
