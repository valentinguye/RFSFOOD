

##### 0. PACKAGES, WD, OBJECTS #####


### WORKING DIRECTORY SHOULD BE CORRECT IF THIS SCRIPT IS RUN WITHIN R_project_for_individual_runs
### OR CALLED FROM LUCFP PROJECT master.do FILE.
### IN ANY CASE IT SHOULD BE (~/LUCFP/data_processing) 


### PACKAGES ###
# see this project's README for a better understanding of how packages are handled in this project. 

# These are the packages needed in this particular script. *** these are those that we now not install: "rlist","lwgeom","htmltools", "iterators", 
neededPackages <- c("data.table", "plyr", "tidyr", "dplyr",  "Hmisc", "sjmisc", "stringr",
                    "here", "readstata13", "foreign", "readxl", "writexl",
                    "raster", "rgdal", "sp",  "sf",
                    "knitr", "kableExtra",
                    "DataCombine", 
                    "fixest", 
                    "boot",  "sandwich",# "fwildclusterboot",
                    "ggplot2", "dotwhisker", "leaflet", "htmltools")
# Install them in their project-specific versions
renv::restore(packages = neededPackages)

# Load them
lapply(neededPackages, library, character.only = TRUE)

# /!\/!\ IF renv::restore(neededPackages) FAILS TO INSTALL SOME PACKAGES /!\/!\ 

# For instance sf could cause trouble https://github.com/r-spatial/sf/issues/921 
# or magick, as a dependency of raster and rgdal. 

# FOLLOW THESE STEPS:
# 1. Remove these package names from neededPackages above, and rerun renv::restore(packages = neededPackages)
# 2. Write them in troublePackages below, uncomment, and run the following code chunk: 

# # /!\ THIS BREAKS THE PROJECT REPRODUCIBILITY GUARANTY /!\
# troublePackages <- c() 
# # Attempt to load packages from user's default libraries.
# lapply(troublePackages, library, lib.loc = default_libraries, character.only = TRUE)

# 3. If the troubling packages could not be loaded ("there is no package called ...") 
#   you should try to install them, preferably in their versions stated in the renv.lock file. 
#   see in particular https://rstudio.github.io/renv/articles/renv.html 


# # # /!\ THIS BREAKS THE PROJECT REPRODUCIBILITY GUARANTY /!\
# troublePackages <- c("leaflet", "leaflet.providers", "png")
# # Attempt to load packages from user's default libraries.
# lapply(troublePackages, library, lib.loc = default_libraries, character.only = TRUE)

### WORKING DIRECTORY SHOULD BE CORRECT IF THIS SCRIPT IS RUN WITHIN R_project_for_individual_runs
### OR CALLED FROM LUCFP PROJECT master.do FILE.
### IN ANY CASE IT SHOULD BE (~/LUCFP/data_processing

### NEW FOLDERS USED IN THIS SCRIPT 
dir.create(here("temp_data"))
dir.create(here("temp_data","country_nourishment"))





####  UNDERNOURISHMENT #### 
# Share of people who are undernourished in thte total population
# "Undernourishment measures the share of the population that has a caloric intake which is insufficient to meet the
# minimum energy requirements necessary for a given individual." 
# See FAOSTAT-foodsecurity_Descriptions_and_Metadata.xlsx for detailed definitions
undn <- read.csv(here("input_data", "outcome_variables", "FAOSTAT-foodsecurity_FAOall_value_20092021.csv"))
head(undn)

# the domain is the common to the whole data set "Food Balances (-2013, old methodology and population)" so we can remove it
unique(undn$Domain) 

undn <- undn[, !grepl("Domain", names(undn))]

## Area and area code are bijective 
length(unique(undn$Area)) == length(unique(undn$Area.Code..FAO.)) 
undn <- dplyr::select(undn, -Area.Code..FAO.)

## Element is Value
unique(undn$Element)
undn <- undn[, !grepl("Element", names(undn))]

## Item 
unique(undn$Item)
# [1] "Prevalence of undernourishment (percent) (3-year average)"                                                 
# [2] "Prevalence of severe food insecurity in the total population (percent) (3-year average)"                   
# [3] "Prevalence of severe food insecurity in the male adult population (percent) (3-year average)"              
# [4] "Prevalence of severe food insecurity in the female adult population (percent) (3-year average)"            
# [5] "Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)"       
# [6] "Prevalence of moderate or severe food insecurity in the male adult population (percent) (3-year average)"  
# [7] "Prevalence of moderate or severe food insecurity in the female adult population (percent) (3-year average)"
# [8] "Prevalence of undernourishment (percent) (annual value)" 

# Items and Item.Code are bijective
length(unique(undn$Item)) == length(unique(undn$Item.Code))
undn <- dplyr::select(undn, -Item.Code)


# We keep only [1] [2] and [5] ([8] is feature but actually there is no data for it, only China has it, and its empty)
undn$Item[undn$Item=="Prevalence of undernourishment (percent) (3-year average)"] <- "undernourished"
undn$Item[undn$Item=="Prevalence of severe food insecurity in the total population (percent) (3-year average)"] <- "undernourished_severe"
undn$Item[undn$Item=="Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)"] <- "undernourished_modsevere"

undn <- dplyr::filter(undn, Item %in% c("undernourished", "undernourished_severe", "undernourished_modsevere"))#

head(undn)

## Year
undn <- dplyr::select(undn, -Year.Code)

names(undn)[names(undn) == "Year"] <- "year"

unique(undn$year)
for(year in 2009:2021){ 
  # determine moving average bounds
  mab <- paste0(year-1,"-",year+1)
  undn$year[undn$year == mab] <- year
}
undn$year <- as.numeric(undn$year)
unique(undn$year)

## Unit 
unique(undn$Unit)
undn <- dplyr::select(undn, -Unit)

## For the moment, do not bother Flags,nor notes
undn <- undn[, !grepl("Flag", names(undn))]
undn <- dplyr::select(undn, -Note)


# Now split data by Item
unique(undn$Item)

wide_ds <- stats::reshape(undn,
                          # varying = unique(long_ds$Item),
                          # v.names = c("Value"),
                          sep = ".",
                          timevar = "Item",
                          idvar = c("Area", "year"),
                          direction = "wide",
                          new.row.names = NULL)  

vars_slct <- grepl("Value.", names(wide_ds))

# those variables that have been reshaped, give the Item identifier to their names
names(wide_ds)[vars_slct] <- gsub(pattern = "Value.", replacement = "", names(wide_ds)[vars_slct])

head(undn, 20)
head(wide_ds, 20)


## Handle missing values 
wide_ds[grepl("NA", wide_ds$undernourished), "undernourished"] <- ""
wide_ds[grepl("NA", wide_ds$undernourished_severe), "undernourished_severe"] <- ""
wide_ds[grepl("NA", wide_ds$undernourished_modsevere), "undernourished_modsevere"] <- ""

wide_ds[is.na(wide_ds$undernourished), "undernourished"] <- ""
wide_ds[is.na(wide_ds$undernourished_severe), "undernourished_severe"] <- ""
wide_ds[is.na(wide_ds$undernourished_modsevere), "undernourished_modsevere"] <- ""


## Coerce character strings into numeric, and in particular "<2.5". 
# Most countries that have <2.5 have no temporal variation in it. Therefore, they will be removed automatically from analysis. 
# Hence, the choice of the value given to 2.5 is not very important. We attribute the mid-point 1.25 by default. 
wide_ds[wide_ds$undernourished=="<2.5", "undernourished"] <- "1.25"
wide_ds[wide_ds$undernourished_severe=="<2.5", "undernourished_severe"] <- "1.25"
wide_ds[wide_ds$undernourished_modsevere=="<2.5", "undernourished_modsevere"] <- "1.25"

wide_ds$undernourished <- as.numeric(wide_ds$undernourished)
wide_ds$undernourished_severe <- as.numeric(wide_ds$undernourished_severe)
wide_ds$undernourished_modsevere <- as.numeric(wide_ds$undernourished_modsevere)

summary(wide_ds$undernourished_modsevere)



#### STUNTING #### 
# https://ourworldindata.org/hunger-and-undernourishment#too-little-height-for-age-stunting
# "Children who are stunted are determined as having a height which falls two standard deviations below 
# the median height-for-age of the World Health Organization’s Child Growth Standards.
# Stunting is an indicator of severe malnutrition. Unlike wasting and low weight-for-age, the impacts of stunting on 
# child development are considered to be largely irreversible beyond the first 1000 days of a child’s life."

# Here there is only one variable: the share of the under-5 population who are defined as stunted. 
# "Note that many countries report stunting prevalence through periodic health and demographic surveys, 
# meaning that this data is often not available on an annual basis." 

stun <- read.csv(here("input_data", "outcome_variables", "share-of-children-younger-than-5-who-suffer-from-stunting.csv"))

head(stun)

names(stun) <- c("Area", "Code", "year", 
                 "stunting")

stun$year <- as.numeric(stun$year)
unique(stun$year) %>% sort()
stun <- dplyr::filter(stun, year >= 2009)
length_ctry <- ddply(stun, "Area", summarise, length_ctry=length(unique(year)))
summary(length_ctry$length_ctry)

stun <- dplyr::select(stun, -Code)


#### WASTING #### 
# https://ourworldindata.org/hunger-and-undernourishment#too-little-weight-for-height-wasting
# "Wasting is defined as being dangerously thin for one’s height, and is generally a sign (especially in children) of rapid weight loss."
# Here there is only one variable: share of children under-5 suffering from wasting.  

wast <- read.csv(here("input_data", "outcome_variables", "share-of-children-with-a-weight-too-low-for-their-height-wasting.csv"))

head(wast)

names(wast) <- c("Area", "Code", "year", 
                "wasting")

wast$year <- as.numeric(wast$year)

unique(wast$year) %>% sort()
wast <- dplyr::filter(wast, year >= 2009)
length_ctry <- ddply(wast, "Area", summarise, length_ctry=length(unique(year)))
summary(length_ctry$length_ctry)

wast <- dplyr::select(wast, -Code)


#### MERGE OUTCOME VARIABLES ----------------------------------------------------
undn_c <- unique(undn$Area)
stun_c <- unique(stun$Area)
wast_c <- unique(wast$Area)

length(undn_c) # 204
length(stun_c) # 138
length(wast_c) # 150

length(unique(c(stun_c, wast_c))) # 151 

stun_c[!(stun_c %in% undn_c)]
wast_c[!(wast_c %in% undn_c)]

# adjust stunting data country names
stun$Area[stun$Area=="China"] <- "China, mainland"
stun$Area[stun$Area=="Bolivia"] <- "Bolivia (Plurinational State of)"
stun$Area[stun$Area=="Brunei"] <- "Brunei Darussalam"
stun$Area[stun$Area=="Cote d'Ivoire"] <- "Ivory Coast"
stun$Area[stun$Area=="Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
stun$Area[stun$Area=="Congo"] <- "Republic of the Congo"
stun$Area[stun$Area=="Iran"] <- "Iran (Islamic Republic of)"
stun$Area[stun$Area=="Laos"] <- "Lao People's Democratic Republic"
stun$Area[stun$Area=="Moldova"] <- "Republic of Moldova"
stun$Area[stun$Area=="North Korea"] <- "Democratic People's Republic of Korea"
stun$Area[stun$Area=="South Korea"] <- "Republic of Korea"
stun$Area[stun$Area=="Syria"] <- "Syrian Arab Republic"
stun$Area[stun$Area=="Tanzania"] <- "United Republic of Tanzania"
stun$Area[stun$Area=="Timor"] <- "Timor-Leste"
stun$Area[stun$Area=="United States"] <- "United States of America"
stun$Area[stun$Area=="Venezuela"] <- "Venezuela (Bolivarian Republic of)"
stun$Area[stun$Area=="Vietnam"] <- "Viet Nam"

# adjust wasting data country names
wast$Area[wast$Area=="China"] <- "China, mainland"
wast$Area[wast$Area=="Bolivia"] <- "Bolivia (Plurinational State of)"
wast$Area[wast$Area=="Brunei"] <- "Brunei Darussalam"
wast$Area[wast$Area=="Cote d'Ivoire"] <- "Ivory Coast"
wast$Area[wast$Area=="Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
wast$Area[wast$Area=="Congo"] <- "Republic of the Congo"
wast$Area[wast$Area=="Iran"] <- "Iran (Islamic Republic of)"
wast$Area[wast$Area=="Laos"] <- "Lao People's Democratic Republic"
wast$Area[wast$Area=="Moldova"] <- "Republic of Moldova"
wast$Area[wast$Area=="North Korea"] <- "Democratic People's Republic of Korea"
wast$Area[wast$Area=="South Korea"] <- "Republic of Korea"
wast$Area[wast$Area=="Syria"] <- "Syrian Arab Republic"
wast$Area[wast$Area=="Tanzania"] <- "United Republic of Tanzania"
wast$Area[wast$Area=="Timor"] <- "Timor-Leste"
wast$Area[wast$Area=="United States"] <- "United States of America"
wast$Area[wast$Area=="Venezuela"] <- "Venezuela (Bolivarian Republic of)"
wast$Area[wast$Area=="Vietnam"] <- "Viet Nam"


# this is to match modif made in prepare_exposures too 
wide_ds$Area[wide_ds$Area=="CÃ´te d'Ivoire"] <- "Ivory Coast" 
wide_ds$Area[wide_ds$Area=="C?te d'Ivoire"] <- "Ivory Coast"
wide_ds$Area[wide_ds$Area=="TÃ¼rkiye"] <- "Turkey"
wide_ds$Area[wide_ds$Area=="T?rkiye"] <- "Turkey"
wide_ds$Area[wide_ds$Area=="Congo"] <- "Republic of the Congo"

# China 
wide_ds$Area[wide_ds$Area == "China, Taiwan Province of"] <- "Taiwan"
# Remove China (which aggregates China mainland and Taiwan), and keep only China mainland 
wide_ds <- dplyr::filter(wide_ds, Area != "China")
# and remove Hong Kong and Macao 
wide_ds <- dplyr::filter(wide_ds, Area != "China, Hong Kong SAR")
wide_ds <- dplyr::filter(wide_ds, Area != "China, Macao SAR")

wide_ds <- dplyr::filter(wide_ds, Area != "French Polynesia")
wide_ds <- dplyr::filter(wide_ds, Area != "Netherlands Antilles (former)")
wide_ds <- dplyr::filter(wide_ds, Area != "Bermuda")
wide_ds <- dplyr::filter(wide_ds, Area != "New Caledonia")


wide_ds_c <- unique(wide_ds$Area)
stun_c <- unique(stun$Area)
wast_c <- unique(wast$Area)
# the discrepancies that remain are only those with regions of the world
stun_c[!(stun_c %in% wide_ds_c)]
wast_c[!(wast_c %in% wide_ds_c)]


# wide_ds_c[grepl("ongo", wide_ds_c)]
# wast_c[grepl("ongo", wast_c)]

outcomes <- left_join(wide_ds, stun, by = c("Area", "year")) 

outcomes <- left_join(outcomes, wast, by = c("Area", "year"))

#### Handle Serbia and Montenegro and Sudan and South Sudan ####
# We aggregate these countries because they were not distinct in the pre-treatment period from which we build the exposures
# We aggregate by average with population weights. 
# For Serbia and Montenegro, set weights at 9/10 and 1/10 resp. 
sm <- outcomes[outcomes$Area%in%c("Serbia", "Montenegro"),] 
sm <- dplyr::mutate(sm, 
                     w_undernourished = if_else(Area=="Montenegro", 
                                                true = undernourished*0.1, 
                                                false = undernourished*0.9), 
                     w_undernourished_severe = if_else(Area=="Montenegro", 
                                                       true = undernourished_severe*0.1, 
                                                       false = undernourished_severe*0.9), 
                     w_undernourished_modsevere = if_else(Area=="Montenegro", 
                                                          true = undernourished_modsevere*0.1, 
                                                          false = undernourished_modsevere*0.9), 
                     w_stunting = if_else(Area=="Montenegro", 
                                          true = stunting*0.1, 
                                          false = stunting*0.9), 
                     w_wasting = if_else(Area=="Montenegro", 
                                         true = wasting*0.1, 
                                         false = wasting*0.9))

# Aggregate within years
sm <- ddply(.data = sm, .variables = "year", summarise, 
             undernourished = if_else(is.na(sum(undernourished, na.rm = TRUE)),  # if one of the two country is NA this year
                                      true = mean(undernourished, na.rm = FALSE), # then take the value from the non-NA one (if any)
                                      false = sum(w_undernourished, na.rm = TRUE)), # else (none is NA), make the weighted average
             
             undernourished_severe = if_else(is.na(sum(undernourished_severe, na.rm = TRUE)),  
                                             true = mean(undernourished_severe, na.rm = FALSE), 
                                             false = sum(w_undernourished_severe, na.rm = TRUE)), 
             
             undernourished_modsevere = if_else(is.na(sum(undernourished_modsevere, na.rm = TRUE)), 
                                                true = mean(undernourished_modsevere, na.rm = FALSE), 
                                                false = sum(w_undernourished_modsevere, na.rm = TRUE)), 
             
             stunting = if_else(is.na(sum(stunting, na.rm = TRUE)),  
                                true = mean(stunting, na.rm = FALSE), 
                                false = sum(w_stunting, na.rm = TRUE)), 
             
             wasting = if_else(is.na(sum(wasting, na.rm = TRUE)),
                               true = mean(wasting, na.rm = FALSE), 
                               false = sum(w_wasting, na.rm = TRUE))
)

# replace the two countries by the group of them two
sm$Area <- "Serbia and Montenegro"
outcomes <- rbind(outcomes, sm)
outcomes <- dplyr::filter(outcomes, 
                          Area != "Serbia" & Area != "Montenegro")

# For Sudan and South Sudan, set weights at 34.5/44 and 9.5/44 resp. 
sss <- outcomes[outcomes$Area%in%c("Sudan", "South Sudan"),] 
sss <- dplyr::mutate(sss, 
                     w_undernourished = if_else(Area=="South Sudan", 
                                                     true = undernourished*9.5/44, 
                                                     false = undernourished*34.5/44), 
                     w_undernourished_severe = if_else(Area=="South Sudan", 
                                                true = undernourished_severe*9.5/44, 
                                                false = undernourished_severe*34.5/44), 
                     w_undernourished_modsevere = if_else(Area=="South Sudan", 
                                                true = undernourished_modsevere*9.5/44, 
                                                false = undernourished_modsevere*34.5/44), 
                     w_stunting = if_else(Area=="South Sudan", 
                                                true = stunting*9.5/44, 
                                                false = stunting*34.5/44), 
                     w_wasting = if_else(Area=="South Sudan", 
                                                true = wasting*9.5/44, 
                                                false = wasting*34.5/44))

# Aggregate within years
sss <- ddply(.data = sss, .variables = "year", summarise, 
            undernourished = if_else(is.na(sum(undernourished, na.rm = TRUE)),  # if one of the two country is NA this year
                                     true = mean(undernourished, na.rm = FALSE), # then take the value from the non-NA one (if any)
                                     false = sum(w_undernourished, na.rm = TRUE)), # else (none is NA), make the weighted average

            undernourished_severe = if_else(is.na(sum(undernourished_severe, na.rm = TRUE)),  
                                            true = mean(undernourished_severe, na.rm = FALSE), 
                                            false = sum(w_undernourished_severe, na.rm = TRUE)), 

            undernourished_modsevere = if_else(is.na(sum(undernourished_modsevere, na.rm = TRUE)), 
                                               true = mean(undernourished_modsevere, na.rm = FALSE), 
                                               false = sum(w_undernourished_modsevere, na.rm = TRUE)), 

            stunting = if_else(is.na(sum(stunting, na.rm = TRUE)),  
                               true = mean(stunting, na.rm = FALSE), 
                               false = sum(w_stunting, na.rm = TRUE)), 

            wasting = if_else(is.na(sum(wasting, na.rm = TRUE)),
                              true = mean(wasting, na.rm = FALSE), 
                              false = sum(w_wasting, na.rm = TRUE))
            )


# replace the two countries by the group of them two
sss$Area <- "Sudan (former)"
outcomes <- rbind(outcomes, sss)
outcomes <- dplyr::filter(outcomes, 
                          Area != "Sudan" & Area != "South Sudan")


saveRDS(outcomes, file = here("temp_data", "country_nourishment", paste0("undernourished_stun_wast_",
                                                                         min(unique(outcomes$year)),
                                                                         max(unique(outcomes$year)),
                                                                         ".Rdata")))




#### OLD STUFFS, NOT USED -------------------------------------------------------
#### depth of the food deficit #### 
dfd <- read.csv(here("input_data", "outcome_variables", "depth-of-the-food-deficit.csv"))
# Here there is only one variable: the depth of the food deficit in kilocalories per person per day. 
# "The depth of the food deficit indicates how many calories would be needed to lift all undernourished people from their
# status, everything else being constant."
head(dfd)

names(dfd) <- c("Entity", "Code", "year", 
                "deficit_kcal_cpt_day")

length(unique(dfd$Entity))
unique(dfd$year)

#### supply of cereal kcal/capita/day #### 
cereals <- read.csv(here("input_data", "outcome_variables", "per-capita-consumption-of-cereals-by-commodity-type-daily-kilocalories.csv"))
# Variable names are "Food Balance Sheets: Oats - Food supply (kcal/capita/day) (FAO (2017))" 
# with the cereal taking values: Oats, Rye and products, Barley and products, Sorghum and products, Maize and products, Wheat and products, Rice Milled Equivalent
# They re all expressed in kcal/capita/day.
# " Breakdown of the average per capita intake of cereals by specific cereal-based commodity types, measured in kilocalories per
# person per day. This figure measures the primary equivalent of all food products derived from a given commodity (e.g. "wheat"
# represents the primary equivalent of its derived products). Data refers to cereal food supply at the consumer level but does not
# account for consumer wastage."

names(cereals)

unique(cereals$Entity)

names(cereals) <- c("Entity", "Code", "year", 
                    "oat_kcal_cpt_day",
                    "rye_kcal_cpt_day",
                    "barley_kcal_cpt_day",
                    "sorghum_kcal_cpt_day",
                    "maize_kcal_cpt_day",
                    "wheat_kcal_cpt_day", 
                    "rice_kcal_cpt_day")

cereals <- mutate(cereals, cereal_kcal_cpt_day = rowSums(across(.cols = contains("kcal_cpt_day")), na.rm = TRUE))

# cereal_kcal_cpt_day is an outcome to be used as such. Particular intakes from maize, for instance, can be looked at to see whether the effect hits directly through corn market



#### global hunger index ####
# The index score comprises of four key hunger indicators: prevalence of undernourishment; childhood wasting; childhood
# stunting; and child mortality. It is measured on a 100-point scale where 0 is the best score (no hunger) and 100 the worst.
ghi <- read.csv(here("input_data", "outcome_variables", "global-hunger-index.csv"))

head(ghi)
length(unique(ghi$Entity))
unique(ghi$Year) # there are only 4 years of data for every country 

names(ghi) <- c("Entity", "Code", "year", "GHI", "annotations")

unique(ghi$annotations)
ghi[ghi$annotations != "", ] # only 2021 observations

ghi <- dplyr::select(ghi, annotations)


#### staple food prices #### 
sfp <- read_excel(here("input_data", "FEWS_NET_Staple_Food_Price_Data.xlsx"))
head(sfp)
class(sfp)
sfp <- as.data.frame(sfp)
head(sfp)
length(unique(sfp$country))
# IT'S ONLY 13 COUNTRIES 

sfp <- dplyr::select(sfp, -source_document, -longitude, -latitude, admin_1)
unique(sfp$unit_type) # Weight, Itam, Volume 
unique(sfp$product_source) # Local, Import
unique(sfp$price_type) # Retail, Wage, Wholesale 




# manage time var
# sub(pattern = "\-(.*)", replacement = "", x= sfp$period_date)

nrow(sfp)
sfp$year <- sapply(str_split( sfp$period_date, pattern = "-"), FUN = function(date){date[1]}) 
head(sfp)
sfp$value <- as.numeric(sfp$value)
summary(sfp$value)
# this averages local prices over months and markets within (i.e. for every) a country, year, product type, currency, unit and source. 
sfp2 <- ddply(sfp, c("country", "year", "cpcv2", "currency", "unit", "product_source"), summarise, 
              staple_price = mean(value, na.rm = TRUE))

head(sfp2)
summary(sfp2$staple_price)

multicurrency <- ddply(sfp2, c("country", "cpcv2"), summarise, 
                       ncurrency = length(unique(currency)))


# there may be different 










