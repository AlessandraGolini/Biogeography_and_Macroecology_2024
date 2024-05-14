# 14/05/2024

#install.packages("sars")
#install.packages("tidyverse")
library(sars) 
library(tidyverse)
library(terra)

#upload the database of Mediterranean island
#https://onlinelibrary.wiley.com/doi/full/10.1111/geb.13855
medis <- vect("data/medis.gpkg") #import spatial object thanks to terra package
plot(medis)

medis_df <- as.data.frame(medis)

#data from GIFT - Global Inventory of Floras and Traits
#they are already processed for the mediterranean islands
data <- read.csv("data/islands_flora.csv")

#how the species are dinstributed across the islands? we compute the frequencies 
freq <- rowSums(data>0)
head(freq)

#new dataframe with species frequencies
species_freq <- data.frame(
  "taxa" = data$accepted_species,
  "freq" = freq
)
#at this point we have the number of island in which the species is present, for each species

#which species occur in more than 50 islands
species_freq[species_freq$freq>50, ]

#ordering the dataset according to the frequency values
sorted_freq <- species_freq[order(species_freq$freq, decreasing = T), ]
## the rare species are making difference among islands, the ones that are very common are 
## reducing the beta diversity

plot(sorted_freq$freq)
##we can have an idea of the frequencies. There are few species that are very common in
##several islands, while the majority of species are present in one or two islands:
##there are many rare species, while few species very aboundant
##this is called RANK ABOUNDANCE/FREQUENCY DISTRIBUTION (depending on what you consier on the y axis)
plot(sorted_freq$freq, ylab = "Number of islands")

# species richness per island
isl_rich <- colSums(data[-1]>0)
head(isl_rich)

# dataframe with the number of species for each island
island_sr <- data.frame(
  "island_id" = colnames(data[-1]),
  "sr" = isl_rich
)

# we control which columns contain the data we're intereste to and we select them
medis_df <- medis_df[c(1, 3:5)]

# link the name of the island to the number of species
medis_df <- left_join(medis_df, island_sr, join_by(id==island_id)) 
# with the left_join command, we can join elements form a dataframe to another one: the first
# is the one to which we add a column, and the second one is the dataframe from which we get the data
# there also are the right_join and the foot_join...

# we want to remove the islands for which we don't have any species (tidyverse package):
medis_df_clean <- medis_df %>%
  filter(sr > 1500)  #all the islands that have more than 1500 species

medis_df_clean2 <- medis_df %>%
  filter(!is.na(sr)) #we select all the values that are not NA

# calculate the species areal relationship: generally, the larger area the richer in species 
fit_power <- sar_power(medis_df_clean2[c(4,5)])
plot(fit_power)
#the power function is the way to use the Arrhenius model
## most of the islands are small. there is a pretty rich island which is quite rare for its area: Corsica
## 
