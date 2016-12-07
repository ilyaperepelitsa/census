condo <- read.csv("~/xport/csv/condo.csv")
coop <- read.csv("~/xport/csv/coop.csv")
options(scipen = 999)
library(ggplot2)
library(dplyr)

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

bronx <- geo.make(state="NY", county=c("Bronx", "Queens", "Kings", "New York"), tract="*")
geo.lookup(state="NY", county="Bronx", tract="*")

pew <- acs.fetch(geography=bronx, endyear=2014,
                 table.number="B12001", col.names="pretty")
View(as.data.frame(estimate(pew)))

bronxnew <- as.data.frame(estimate(pew))
bronxmarital <- bronxnew[,c(1,3,4,10,12,13,19)]
bronxmarital <- bronxmarital %>%
  mutate(never_married = (bronxmarital[,2]+bronxmarital[,5])/bronxmarital[,1],
         now_married   = (bronxmarital[,3]+bronxmarital[,6])/bronxmarital[,1],
         divorced      = (bronxmarital[,4]+bronxmarital[,7])/bronxmarital[,1])
bronxmarital <- bronxmarital[,8:10]


bronnn <- unlist(strsplit(rownames(as.data.frame(estimate(pew))), ","))
tract <- bronnn[seq(1, length(bronnn), 3)]
county <- bronnn[seq(2, length(bronnn), 3)]
state <- bronnn[seq(3, length(bronnn), 3)]
#data.frame(tract, county, state)

colnames(bronxnew) = c("census_tract", "county", "state", "population")

                       


acs.lookup(endyear=2014, keyword="marital")

