condo <- read.csv("~/xport/csv/condo.csv")
coop <- read.csv("~/xport/csv/coop.csv")
options(scipen = 999)
install.packages("sp")
install.packages("tigris")
library(sp)
library(tigris)
library(ggplot2)
library(dplyr)
library(tidyr)

##### CONDO UNRELATED #######
colnames(condo1)
condo1 <- condo[,1:12]
condo1 <- condo1[,-2]
colnames(condo2) <- colnames(condo1)
condo2 <- condo[,13:23]


colnames(condo3) <- colnames(condo1)
condo3 <- condo[,25:35]

condonew <- rbind(condo1, condo2, condo3)
colnames(condonew) <- c("bor_block_lot", "address", 
                        "neighborhood", "building_class",
                        "units", "year_built", "gross_sqf", 
                        "gross_income","income_persqf", 
                        "market_value", "value_persqf")

condonew %>% filter(year_built > 0 & gross_sqf < 1000000) %>% 
  ggplot(aes(x = year_built, y = gross_sqf/1000)) + 
  geom_point()
condonew %>% filter(year_built > 0 & gross_sqf < 1000000) %>% 
  ggplot(aes(x = gross_sqf, y = income_persqf)) + 
  geom_point()

str(condonew)
condonew <- condonew %>% 
  filter(year_built > 0 & income_persqf > 0)
summary(lm(data = condonew, income_persqf ~ gross_sqf + year_built +
             units + neighborhood))

#######################################

install.packages("acs")
library(acs)

key1 <- "ada405bf8fc62b3fda4767166b3761e198ed6f61"

api.key.install(key = key1)
tracts <- read.csv("/Users/ilyaperepelitsa/quant/census/tracts_nyc.csv", row.names = NULL)
dfw <- tracts(state = "NY", county = c("Bronx", "Queens", "Kings", "New York"))
geog <- geo.make(state="NY", county=c("Bronx", "Queens", "Kings", "New York"), tract="*")
# geo.lookup(state="NY", county="Bronx", tract="*")

# Total population B01003
# Disability sex age B18101
# Marital B12001
# Household size B11016
# Means of Tansportation B08101
# Family and nonfamily households B09019

data  <- acs.fetch(geography = geog, endyear = 2015, span = 5,
                               table.number = "B08101", col.names = "pretty")

# data@geography$county <- as.character(data@geography$county)
# for (i in 1:length(data@geography$county)){
#   if (nchar(data@geography$county[i]) == 1) {
#     data@geography$county[i] <- paste("00", data@geography$county[i], sep = "")
#   } else {
#     data@geography$county[i] <- paste("0", data@geography$county[i], sep = "")
#   }
#   
# } 

class(data@geography$county)

data <- as.data.frame(estimate(data))
disab1 <- data[,c(seq(4,19,3))]
disab2 <- data[,c(seq(23,39,3))]
disab3 <- vector

disab1[,7] <- disab1[,1] + disab1[,2] + disab1[,3] + disab1[,4] + disab1[,5] + disab1[,6]
disab2[,7] <- disab2[,1] + disab2[,2] + disab2[,3] + disab2[,4] + disab2[,5] + disab2[,6]
disab_total <- disab1[,7] + disab2[,7]
data[,2] <- disab_total
disability <- data.frame(rownames(data), data[,1], data[,2], row.names = NULL)
colnames(disability) <- c("census_tract", "total", "disability")
disability$disability <- disability$disability/disability$total
data <- left_join(tracts, disability, by = "census_tract")

data <- data.frame(rownames(data), data, row.names = NULL)
# tracts[,2] <- as.character(tracts[,2])
# data[,1] <- as.character(data[,1])

colnames(data) <- c("census_tract", "data")
data <- left_join(tracts, data, by = "census_tract")
data <- data %>% filter(disability != "NaN") %>% select(GEOID, disability)

write.csv(data, "/Users/ilyaperepelitsa/quant/census/disability.csv", row.names = FALSE)

# popdf <- data.frame(paste0(as.character(data@geography$state),
#                            as.character(data@geography$county),
#                            as.character(data@geography$tract)),
#                     rownames(as.data.frame(estimate(data))),
#                     unlist(strsplit(rownames(as.data.frame(estimate(data))), ","))[seq(2, length( unlist(strsplit(rownames(as.data.frame(estimate(data))), ","))), 3)],
#                     data@estimate, row.names = NULL)
# colnames(popdf) <- c("GEOID", "census_tract", "borough", "total_population")
# write.csv(popdf, "/Users/ilyaperepelitsa/quant/census/tracts_nyc.csv", row.names = FALSE)
# 
# columns <- unlist(strsplit(rownames(as.data.frame(estimate(population))), ","))[seq(2, length( unlist(strsplit(rownames(as.data.frame(estimate(population))), ","))), 3)]

# boroughs <- population@geography[,c(1, 4)]
# colnames(boroughs) <- c("full", "tract")
tracts <- read.csv("/Users/ilyaperepelitsa/quant/census/tracts_nyc.csv")
# population@geography
# population@span
population <- as.data.frame(estimate(population))
population <- data.frame(rownames(population), population, row.names = NULL)

columns <- rownames(population)
full <- rownames(population)
tract <-  columns[seq(1, length(columns), 3)]
county <- columns[seq(2, length(columns), 3)]
state <-  columns[seq(3, length(columns), 3)]
where <- data.frame(full, tract, county, state)
ctract <- left_join(where, boroughs, by = "full")
write.csv(ctract, "/Users/ilyaperepelitsa/quant/census/tracts_nyc.csv", row.names = FALSE)

population <- data.frame(full, tract, county, state, population, row.names = NULL) 
colnames(population) <- c("full", "census_tract", "county", "state", "population")

write.csv(population, "/Users/ilyaperepelitsa/population.csv")




pew <- acs.fetch(geography = geog, endyear = 2015,
                 table.number = "B12001", col.names = "pretty")
View(as.data.frame(estimate(pew)))

pew <- as.data.frame(estimate(pew))
data <- data[,c(1,3,4,10,12,13,19)]
data <- data %>%
  mutate(never_married = (data[,2]+data[,5])/data[,1],
         now_married   = (data[,3]+data[,6])/data[,1],
         divorced      = (data[,4]+data[,7])/data[,1])
data <- data[,8:10]

data$census_tract <- rownames(pew)

data <- data %>% select(c(census_tract, never_married, now_married, divorced))
# data <- read.csv("/Users/ilyaperepelitsa/quant/census/disability.csv", row.names = NULL)
data <- left_join(tracts, data)
write.csv(data, "/Users/ilyaperepelitsa/quant/census/marital.csv", row.names = FALSE)
# 
# columns <- unlist(strsplit(rownames(as.data.frame(estimate(pew))), ","))
# tract <- bronnn[seq(1, length(bronnn), 3)]
# county <- bronnn[seq(2, length(bronnn), 3)]
# state <- bronnn[seq(3, length(bronnn), 3)]
#data.frame(tract, county, state)

colnames(bronxnew) = c("census_tract", "county", "state", "population")








data  <- acs.fetch(geography = geog, endyear = 2015, span = 5,
                   table.number = "B11016", col.names = "pretty")
data <- as.data.frame(estimate(data))


# household (roommates)
data <- data.frame(rownames(data), data[, 9]/data[, 1])

data <- data %>% filter(data[,2] != "NaN")

colnames(data) <- c("census_tract", "non_family")
data <- left_join(tracts, data)
write.csv(data, "/Users/ilyaperepelitsa/quant/census/non_family.csv", row.names = FALSE)



data  <- acs.fetch(geography = geog, endyear = 2015, span = 5,
                   table.number = "B11016", col.names = "pretty")

data <- as.data.frame(estimate(data))


# household (roommates)
two_three <- (data[,3] + data[,4]) / data[ ,2]
four_five <- (data[,5] + data[,6]) / data[ ,2]
sixplus <-   (data[,7] + data[,8]) / data[ ,2]
data <- data_frame(rownames(data), two_three, four_five, sixplus)
data <- data %>% filter(data[,2] != "NaN")

colnames(data)[1] <- c("census_tract")
data <- left_join(tracts, data)
write.csv(data, "/Users/ilyaperepelitsa/quant/census/family_size.csv", row.names = FALSE)






data  <- acs.fetch(geography = geog, endyear = 2015, span = 5,
                   table.number = "B08101", col.names = "pretty")

data <- as.data.frame(estimate(data))

total <- data[,1]
car_alone <- data[,9]
car_pooled <- data[,17]
public <- data[,25]
walked <- data[,33]

data <- data.frame(rownames(data), total, car_alone,
                   car_pooled, public, walked,
                   row.names = NULL)
data$car_alone <- car_alone/total
data$car_pooled <- car_pooled/total 
data$public <- public/total
data$walked <- walked/total
colnames(data)[1] <- "census_tract"

data <- data %>% filter(walked != "NaN")
data <- left_join(tracts, data)
write.csv(data, "/Users/ilyaperepelitsa/quant/census/transportation.csv", row.names = FALSE)


### TRANSPORTATION
data1 <- data %>% filter(!is.na(car_alone)) %>% 
  gather("transportation", "proportion", 6:9)

for (i in 1:length(unique(data1$transportation))){
  
  plot <- data1 %>%  filter(transportation == unique(data1$transportation)[i]) %>% 
  ggplot(aes(x = reorder(borough, -proportion),
             y = proportion,
             fill = borough)) +
  geom_boxplot() +
    ggtitle(unique(data1$transportation)[i])
  print(plot)
  ggsave(paste("/Users/ilyaperepelitsa/quant/census/",
               unique(data1$transportation)[i],
               ".jpg", sep = ""),
         plot, height = 10, width = 7)
}



### DISABILITY
data <- read.csv("/Users/ilyaperepelitsa/quant/census/disability.csv")
data1 <- data %>% filter(!is.na(disability)) 

  
  plot <- data1 %>%  
    ggplot(aes(x = reorder(borough, -disability),
               y = disability,
               fill = borough)) +
    geom_boxplot() +
    ggtitle("disability")
  print(plot)
  ggsave("/Users/ilyaperepelitsa/quant/census/disability.jpg",
         plot, height = 10, width = 7)

### FAMILY SIZE
data <- read.csv("/Users/ilyaperepelitsa/quant/census/family_size.csv")
data1 <- data %>% filter(!is.na(sixplus)) %>% 
  gather("family_size", "proportion", 5:7)

for (i in 1:length(unique(data1$family_size))){
  
  plot <- data1 %>%  filter(family_size == unique(data1$family_size)[i]) %>% 
    ggplot(aes(x = reorder(borough, -proportion),
               y = proportion,
               fill = borough)) +
    geom_boxplot() +
    ggtitle(unique(data1$family_size)[i])
  print(plot)
  ggsave(paste("/Users/ilyaperepelitsa/quant/census/",
               unique(data1$family_size)[i],
               ".jpg", sep = ""),
         plot, height = 10, width = 7)
}


### NON-FAMILY
data <- read.csv("/Users/ilyaperepelitsa/quant/census/non_family.csv")
data1 <- data %>% filter(!is.na(non_family)) 


plot <- data1 %>%  
  ggplot(aes(x = reorder(borough, -non_family),
             y = non_family,
             fill = borough)) +
  geom_boxplot() +
  ggtitle("non_family")
print(plot)
ggsave("/Users/ilyaperepelitsa/quant/census/non_family.jpg",
       plot, height = 10, width = 7)


### MARITAL
data <- read.csv("/Users/ilyaperepelitsa/quant/census/marital.csv")
data1 <- data %>% filter(!is.na(divorced)) %>% 
  gather("status", "proportion", 5:7)

for (i in 1:length(unique(data1$status))){
  
  plot <- data1 %>%  filter(status == unique(data1$status)[i]) %>% 
    ggplot(aes(x = reorder(borough, -proportion),
               y = proportion,
               fill = borough)) +
    geom_boxplot() +
    ggtitle(unique(data1$status)[i])
  print(plot)
  ggsave(paste("/Users/ilyaperepelitsa/quant/census/",
               unique(data1$status)[i],
               ".jpg", sep = ""),
         plot, height = 10, width = 7)
}
