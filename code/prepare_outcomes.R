

##### 0. PACKAGES, WD, OBJECTS #####


### WORKING DIRECTORY SHOULD BE CORRECT IF THIS SCRIPT IS RUN WITHIN R_project_for_individual_runs
### OR CALLED FROM LUCFP PROJECT master.do FILE.
### IN ANY CASE IT SHOULD BE (~/LUCFP/data_processing) 


### PACKAGES ###
# see this project's README for a better understanding of how packages are handled in this project. 

# These are the packages needed in this particular script. *** these are those that we now not install: "rlist","lwgeom","htmltools", "iterators", 
neededPackages <- c("data.table", "plyr", "tidyr", "dplyr",  "Hmisc", "sjmisc", "stringr",
                    "here", "readstata13", "foreign", "readxl", "writexl",
                    "knitr", "kableExtra",
                    "DataCombine", 
                    "fixest", 
                    "boot", "fwildclusterboot", "sandwich",
                    "ggplot2", "dotwhisker")
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



 
#### SUPPLY OF CEREAL KCAL / CAPITA / DAY #### 
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


#### DEPTH OF THE FOOD DEFICIT #### 
dfd <- read.csv(here("input_data", "outcome_variables", "depth-of-the-food-deficit.csv"))
# Here there is only one variable: the depth of the food deficit in kilocalories per person per day. 
# "The depth of the food deficit indicates how many calories would be needed to lift all undernourished people from their
# status, everything else being constant."
head(dfd)

names(dfd) <- c("Entity", "Code", "year", 
                "deficit_kcal_cpt_day")

length(unique(dfd$Entity))

####  UNDERNOURISHMENT #### 
# Share of people who are undernourished
# "Undernourishment measures the share of the population that has a caloric intake which is insufficient to meet the
# minimum energy requirements necessary for a given individual." 
undernourish <- read.csv(here("input_data", "outcome_variables", "share-undernourished-region.csv"))
head(undernourish)

length(unique(undernourish$Entity))

#### GLOBAL HUNGER INDEX, 2000 to 2021 ####
# The index score comprises of four key hunger indicators: prevalence of undernourishment; childhood wasting; childhood
# stunting; and child mortality. It is measured on a 100-point scale where 0 is the best score (no hunger) and 100 the worst.
ghi <- read.csv(here("input_data", "outcome_variables", "global-hunger-index.csv"))

head(ghi)
length(unique(ghi$Entity))

names(ghi) <- c("Entity", "Code", "year", "GHI", "annotations")

unique(ghi$annotations)
ghi[ghi$annotations != "", ] # only 2021 observations

ghi <- dplyr::select(ghi, annotations)



#### STAPLE FOOD PRIES #### 
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
sub(pattern = "\-(.*)", replacement = "", x= sfp$period_date)

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










