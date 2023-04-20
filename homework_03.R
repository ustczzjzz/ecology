rm(list = ls())
# Load necessary libraries
library(tidyverse)
library(ade4)
library(adespatial)
library(vegan)
library(gclus)
library(cluster)
library(FD)


# Read in the doubs data
data(doubs,package="ade4")
head(doubs)

# Remove site 8 which has no fishes
env<-doubs$env
spe<-doubs$species 
spa<-doubs$xy 
fish<-doubs$fish
env <- env[-8, ]
spe <- spe[-8, ]
spa <- spa[-8, ]
fish <- fish[-8, ]

# Which site has the most species (and how many species)?
# site 29, 26 species
site_rich<- apply(fish > 0, 1, sum) 
sort(site_rich) 
# Which species is the most widespread (i.e., found in the most sites)?
# Lece, found in 25 sites
species_widespread <- apply(fish > 0, 2, sum) 
sort(species_widespread) #Lece 

# Select a suitable association measure of species
# R Mode
# write code for clusters and answer: In terms of the fish community composition, which groups of species can you identify? Which groups of species are related to these groups of sites?
# Transpose matrix of species abundances
spe.t <- t(fish)
# Chi-square pre-transformation followed by Euclidean distance
spe.t.chi <- decostand(spe.t, "chi.square")
spe.t.D16 <- dist(spe.t.chi)
# Color plots of a dissimilarity matrix, without and with ordering
"coldiss" <- function(D,
                      nc = 4,
                      byrank = TRUE,
                      diag = FALSE) {
  require(gclus)
  
  D <- as.dist(as.matrix(D))
  
  if (max(D) > 1)
    D <- D / max(D)
  
  if (byrank) {
    spe.color <- dmat.color(1 - D, cm.colors(nc))
  }
  else {
    spe.color <- dmat.color(1 - D, byrank = FALSE, cm.colors(nc))
  }
  
  spe.o <- order.single(1 - D)
  speo.color <- spe.color[spe.o, spe.o]
  
  op <- par(mfrow = c(1, 2), pty = "s")
  
  if (diag) {
    plotcolors(
      spe.color,
      rlabels = attributes(D)$Labels,
      main = "Dissimilarity Matrix",
      dlabels = attributes(D)$Labels
    )
    plotcolors(
      speo.color,
      rlabels = attributes(D)$Labels[spe.o],
      main = "Ordered Dissimilarity Matrix",
      dlabels = attributes(D)$Labels[spe.o]
    )
  }
  else {
    plotcolors(spe.color, rlabels = attributes(D)$Labels,
               main = "Dissimilarity Matrix")
    plotcolors(speo.color,
               rlabels = attributes(D)$Labels[spe.o],
               main = "Ordered Dissimilarity Matrix")
  }
  
  par(op)
}

coldiss(spe.t.D16, diag = TRUE)



# Do RDA analysis, and then write code and answer: 
# Which environmental variables cause a community to vary across a landscape?

# Set aside the variable 'dfs' (distance from the source) for 
# later use
dfs <- env[, 1]

# Remove the 'dfs' variable from the env data frame
env2 <- env[, -1]
# Recode the slope variable (slo) into a factor (qualitative) 
# variable to show how these are handled in the ordinations
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, 
               levels = c(".low", ".moderate", ".steep", ".very_steep"))
table(slo2)
# Create an env3 data frame with slope as a qualitative variable
env3 <- env2
env3$slo <- slo2

# Hellinger-transform the species dataset
# RDA of the Hellinger-transformed fish species data, constrained
decorana(fish)
RDA <- rda(fish,env,scale = T) 
plot(RDA)
spe_hel <- decostand(fish, method = 'hellinger')
rda_tb <- rda(spe_hel~., env, scale = FALSE)
plot(rda_tb)
summary(rda_tb)

