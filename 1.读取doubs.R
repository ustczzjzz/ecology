#周桢婕 生态学 SA22008309
install.packages("ade4") 
library(ade4)

rm(list = ls()) # clean the environment space3 
data(doubs,package = "ade4") # read the doubs dataset from ade4 package
class(doubs) # identify the data class
doubs#view the data profile
spe <- doubs$fish # extract data elements

environment <- doubs$env
spa <- doubs$xy

head(spe) # view the species data
names(spe)
nrow(spe)
ncol(spe)
dim(spe)

plot(spa,asp = 1,type = "n",xlab = "x (km)", ylab ="y (km)")
lines(spa, col = "light blue")
text(spa,row.names(spa), cex = 0.5,col = "red")
text(70,10,"upstream", cex = 0.8,col = "red")
text(20,120,"downstream",cex = 0.8,col = "red")

range(spe) # species abundance range
ab <- table(unlist(spe))
barplot(ab, las = 1, xlab = "Abundance degree", ylab = "frequency", col = gray(5:0/5))

par(mfrow = c(2,2)) # split several window to vlew
plot(spa, asp = 1, col = "brown", cex = spe$Satr, xlab = "x(km)", ylab = "y (km)")
lines(spa, col = "light blue")
text(150,10,"upstream",cex = 0.8,col = "blue")
text(20,120,"downstream",cex=0.8,col="blue")

spe<-spe[-8,]
env<-environment[-8,]
spa<-spa[-8,]

