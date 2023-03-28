#周桢婕 生态学 SA22008309
#1. Loading libraries of tidyverse and ade4, as well as the doubs data into R, and checking what the data looks like and the class of the data. 
#Loading libraries of tidyverse and ade4
library(tidyverse)
library(ade4)

#Loading the doubs data into R.
dobus<- data("doubs",package = "ade4")

#Checking what the data looks like and the class of the data.
head(doubs)
class(doubs)

#2. Turning the row names into a column called site, then convert the data frame to a tibble, named it env_tb.
# Turning the row names into a column called site.
env<-doubs$env   
env_tb <- rownames_to_column(env, var = "site")

# Convert the data frame to a tibble, named it env_tb.
env_tb <- as_tibble(env_tb)


#3. Concatenating several steps with %>% pipe, and name the final variable as env_final.
  # 3.1	One of the columns is dfs. It indicates the distance from sources. Extract and remain the data of the dfs with more than 1000 km.
  env_final<-env_tb[env_tb$dfs>1000,] %>%
  # 3.2	Only interested in these columns: site, dfs, slo, flo, pH, nit, oxy. Select these columns for further analysis.
  select(site, dfs, slo, flo, pH, nit, oxy)%>% 
  # 3.3	Some column names are not intuitive. Rename them as follows: dfs to distsour, slo to slope, flo to flowrate, nit to nitrogen, oxy to oxygen.
  rename(distsour = dfs, slope =slo , flowrate =flo ,  nitrogen =nit , oxygen =oxy )%>%
  # 3.4	Order the data. Arrange the data first by slope in ascending order, and then by pH in descending order.
  arrange(slope,desc(pH))

# View the results.
head(env_final)


