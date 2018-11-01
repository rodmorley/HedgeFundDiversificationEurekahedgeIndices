###
###    Hedge Fund Diversification using the Eurekahedge Hedge Fund Indices
###
###    h/t  Multi Dimensional Diversification - Dr Rufus G Rankin.
###

# load libraries
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(xts)
library(ggcorrplot)
library(psych)
library(d3heatmap)

# download the Eurekahedge indices
eurekahedge_url <- "http://www.eurekahedge.com/df/Eurekahedge_indices.zip"
download.file(eurekahedge_url, destfile = "EurekaHedge_Indices.zip", mode = 'wb')
unzip("EurekaHedge_Indices.zip")
eh       <- read.csv("EurekaHedge_Indices.csv", header = TRUE, stringsAsFactors = F)
eh$Date  <- as.Date(eh$Date, format = "%d-%b-%Y")

str(eh)

# start with just the 'fund of funds' indices
eh %>% filter(str_detect(Index.Name, pattern = "Fund of Funds Index"))  %>% group_by(EHIID, Index.Name) %>% select(Index.Name, EHIID) %>% summarise(Total = n()) %>% as.data.frame()
eh_FoF <- eh %>% filter(str_detect(Index.Name, pattern = "Fund of Funds Index"))  %>% group_by(EHIID, Index.Name) %>% select(Index.Name, EHIID) %>% summarise(Total = n()) %>% as.data.frame()
eh_FoF_EHIIDs <- eh_FoF$EHIID %>% as.numeric()

eh_dataset_FoF_longform   <- eh %>% filter(EHIID %in% eh_FoF_EHIIDs) %>% select(Index.Name, Date, NAV)
eh_dataset_FoF_wide       <- spread(eh_dataset_FoF_longform, key = Index.Name, value = NAV)
dim(eh_dataset_FoF_wide)

ehColsFoF = dim(eh_dataset_FoF_wide)[2]
ehColsFoF

# convert Date to date format
eh_dataset_FoF_wide$Date  <- as.Date(eh_dataset_FoF_wide$Date, format = "%Y-%m-%d")
# convert all FoF indicies to xts format
eh_dataset_FoF.xts        <- xts(eh_dataset_FoF_wide[,2:ehColsFoF]/100, order.by = eh_dataset_FoF_wide[,1])[-1]

colnames(eh_dataset_FoF.xts) <- gsub(pattern = "Eurekahedge ", "", colnames(eh_dataset_FoF.xts))

#ggcorrplot(cor(eh_dataset_FoF.xts))

pcaFoF = principal(cor(eh_dataset_FoF.xts), nfactors = 10, rotate = 'varimax')
pcaFoF

pcaFoF$Vaccounted["Cumulative Var",] > 0.75
grep(TRUE, pcaFoF$Vaccounted["Cumulative Var",] > 0.75) %>% min()

pcaFoF$Structure[1:length(pcaFoF$values), 1:pcaFoF$factors] %>%
  d3heatmap(dendrogram = "row", colors = "Blues")

# add narative....

# first PC  = Rel Val, Evnt Drn, Arbitrage
# sec PC    = Emerging markets & Asia (inc. long short and MS)
# third PC  = all things Japanese - FoF, LS, MS
# foutrh PC = global macro and CTA


###   select all NON fund of funds and the Mizuho indices as the EHIID is not numeric

eh %>% filter(!str_detect(Index.Name, pattern = "Fund of Funds Index"))  %>% group_by(EHIID, Index.Name) %>% select(Index.Name, EHIID) %>% summarise(Total = n()) %>% as.data.frame()
eh_nonFoF <- eh %>% filter(!str_detect(Index.Name, pattern = "Fund of Funds Index"), !str_detect(EHIID, pattern = "^[a-zA-Z]"), !str_detect(Index.Name, pattern = "^CBOE"),!str_detect(Index.Name, pattern = "UCITS"))  %>% 
                    group_by(EHIID, Index.Name) %>% 
                    select(Index.Name, EHIID) %>% 
                    summarise(Total = n()) %>% 
                    as.data.frame()

eh_nonFoF$EHIID
eh_nonFoF_EHIIDs <- eh_nonFoF$EHIID %>% as.numeric()
eh_nonFoF_EHIIDs


eh_dataset_nonFoF_longform   <- eh %>% filter(EHIID %in% eh_nonFoF_EHIIDs) %>% select(Index.Name, Date, NAV)
eh_dataset_nonFoF_wide       <- spread(eh_dataset_nonFoF_longform, key = Index.Name, value = NAV)
dim(eh_dataset_nonFoF_wide)

ehColsnonFoF = dim(eh_dataset_nonFoF_wide)[2]
ehColsnonFoF

# convert Date to date format
eh_dataset_nonFoF_wide$Date  <- as.Date(eh_dataset_nonFoF_wide$Date, format = "%Y-%m-%d")
# convert all non-FoF indicies to xts format
eh_dataset_nonFoF.xts        <- xts(eh_dataset_nonFoF_wide[,2:ehColsnonFoF]/100, order.by = eh_dataset_nonFoF_wide[,1])[-1]

colnames(eh_dataset_nonFoF.xts) <- gsub(pattern = "Eurekahedge ", "", colnames(eh_dataset_nonFoF.xts))

colnames(eh_dataset_nonFoF.xts)

# remove some indicieswith short track records
short_track_record    <- apply(eh_dataset_nonFoF.xts, 2, function(x) sum(!is.na(x))) < 120
eh_dataset_nonFoF.xts <- na.omit(eh_dataset_nonFoF.xts[,!short_track_record])

#
#  Simulate random combinations to ten hedge fund indices
#  run PCA analysis and calculate the number of bets making up 75% of the total variance
#  

# create a helper function
myCount <- function(x) grep(TRUE, x$Vaccounted["Cumulative Var",] > 0.75) %>% min()

# create 1000 random samplings of ten hedge fund indices
random_col_sampling = t(replicate(1000, sample(1:51, 10)))

# create vector to hold results
results = vector(mode = 'numeric', length = 1000)

# loop over each portfolio combination (of ten randomly selected) of hedge fund indices and calculate the number of bets

for(i in 1:1000){
  results[i] <- eh_dataset_nonFoF.xts[, random_col_sampling[i,]] %>% cor() %>% principal(nfactors = 10, rotate = 'varimax') %>% myCount
}

table(results)
prop.table(table(results))
max(results)

which(results == max(results))
which(results == 7)

names(eh_dataset_nonFoF.xts)

r1 <- eh_dataset_nonFoF.xts[, random_col_sampling[168,]] %>% cor() %>% principal(nfactors = 10, rotate = 'varimax')
r1
r1$Structure[1:length(r1$values), 1:r1$factors] %>%
  d3heatmap(dendrogram = "row", colors = "Blues")


r2 <- eh_dataset_nonFoF.xts[, random_col_sampling[231,]] %>% cor() %>% principal(nfactors = 10, rotate = 'varimax')
r2
r2$Structure[1:length(r2$values), 1:r2$factors] %>%
  d3heatmap(dendrogram = "row", colors = "Blues")

