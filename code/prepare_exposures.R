

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
dir.create(here("temp_data","exposures"))

### ITEM GROUPS
broadest <- c("Vegetal Products", 
              "Animal Products")

# adding up nutritional contents of these categories yields the grand total. 
categories <- c("Cereals - Excluding Beer", 
                "Starchy Roots", 
                "Sugar Crops",
                "Sugar & Sweeteners",
                "Pulses",
                "Treenuts",
                "Oilcrops",
                "Vegetable Oils",
                "Vegetables",
                "Fruits - Excluding Wine",
                "Stimulants",
                "Spices",
                "Alcoholic Beverages",
                "Meat", # from there, adding up yields the animal products contents
                "Offals",
                "Animal fats",
                "Eggs",
                "Milk - Excluding Butter",
                "Fish, Seafood",
                "Aquatic Products, Other"
                # "Miscellaneous" exclude Miscellaneous category because blind nutrient conversion not relevant for it, and the item is not available for all elements (missing on Production)
                )


cereals <- c("Wheat and products", 
             "Rice (Milled Equivalent)", 
             "Barley and products",
             "Maize and products",
             "Rye and products", 
             "Oats", 
             "Millet and products", 
             "Sorghum and products", 
             "Cereals, Other")

oilcrops <- c("Soyabeans",
              "Groundnuts (Shelled Eq)",
              "Rape and Mustardseed", 
              "Sunflower seed", 
              "Cottonseed",
              "Coconuts - Incl Copra",
              "Sesame seed",
              "Palm kernels",
              "Olives (including preserved)",
              "Oilcrops, Other")

vegetable_oils <- c("Soyabean Oil",
                    "Groundnut Oil",
                    "Sunflowerseed Oil", 
                    "Rape and Mustard Oil",
                    "Cottonseed Oil",
                    "Palm Oil", 
                    "Sesameseed Oil",
                    "Olive Oil",
                    "Maize Germ Oil",
                    "Oilcrops Oil, Other")

all_items <- c("Grand Total", 
                      broadest, 
                        categories,  
                          cereals, 
                          oilcrops,
                          vegetable_oils)

selected_items <- c("Cereals - Excluding Beer",
                    # Major cereals
                    "Wheat and products", 
                    "Rice (Milled Equivalent)", 
                    "Barley and products",
                    "Maize and products",
                    
                    # Major oil crop seed (for feed)
                    "Soyabeans",
                    
                    "Vegetable Oils",
                    # Major veg oils
                    "Soyabean Oil",
                    "Sunflowerseed Oil", 
                    "Rape and Mustard Oil",
                    "Palm Oil")


#### MAKE DATA WIDE --------------------------------------------------------------------------------------------

pretreatment_years <- c(2001, 2002, 2003, 2004, 2005)
year <- 2001
# list to store annual data sets prepared
wide_fb_list <- list()

for(year in pretreatment_years){
  fb <- read.csv(here("input_data", "exposure_variables", paste0("FAOSTAT-oldfoodbalances_allcountries_aggritems_",year,".csv")))

  # unique(fb$Item)[grepl("meal", unique(fb$Item))]
  
  # the first row names gets weird "ï.." prefixe
  # names(fb)[1] <- "Domain.Code"
  
  # the domain is the common to the whole data set "Food Balances (-2013, old methodology and population)" so we can remove it
  unique(fb$Domain) 

  fb <- fb[, !grepl("Domain", names(fb))]
  
  ## Area and area code are bijective 
  length(unique(fb$Area)) == length(unique(fb$Area.Code)) 
  fb <- dplyr::select(fb, -Area.Code)
  
  ## Element
  unique(fb$Element)
  # "Food supply (kcal/capita/day)"          
  # "Protein supply quantity (g/capita/day)" 
  # "Fat supply quantity (g/capita/day)"   
  # "Production"
  # "Import Quantity"      
  # "Stock Variation" 
  # "Export Quantity" 
  # "Domestic supply quantity"               
  # "Food"                                  
  # "Food supply quantity (kg/capita/yr)"   
  
  # Element and Element.Code are bijective 
  length(unique(fb$Element)) == length(unique(fb$Element.Code))
  fb <- dplyr::select(fb, -Element.Code)
  
  ## Item 
  unique(fb$Item)
  
  # there are some Items that have more than one Item.Code
  length(unique(fb$Item)) == length(unique(fb$Item.Code))

  spot_doublones <- sapply(unique(fb$Item), function(itm){itm_length <- fb[fb$Item==itm,"Item.Code"] %>% unique() %>% length() 
                                itm_spot <- if_else(itm_length > 1, true = itm, false = "")
                                return(itm_spot) })
  spot_doublones[spot_doublones != ""]
  
  fb[fb$Item == "Eggs","Item.Code"] %>% unique()
  fb[fb$Item == "Milk - Excluding Butter","Item.Code"] %>% unique()
  fb[fb$Item == "Miscellaneous","Item.Code"] %>% unique()
  
  fb[fb$Item == "Eggs",] 
  fb[fb$Item == "Miscellaneous",] 
  
  # they seem to be the same figures, or almost, but with different Flags (not always) --> let's keep only one instance
  
  # Items with several item codes are duplicates (within the same country and Element)
  fb[duplicated(fb[,c("Area", "Element", "Item")]), ] %>% nrow() # 2951 obs. in 2001
  # we want those that are not duplicates
  fb <- fb[!duplicated(fb[,c("Area", "Element", "Item")]), ]
  
  if(!(length(unique(fb$Item)) == length(unique(fb$Item.Code)))){
    stop("there are still duplicates in Item variable")
  }
  
  fb <- dplyr::select(fb, -Item.Code)
  
  # keep only Items specified
  fb <- dplyr::filter(fb, Item %in% all_items)
  
  ## Year
  fb <- dplyr::select(fb, -Year.Code)
  # Year is not useful either
  fb <- dplyr::select(fb, -Year)

    
  ## Unit 
  # For "Import Quantity", "Domestic supply quantity" & "Food", the unit is not given in the Element. 
  
  # some checks that the units are expressed in a sound way
  prod_u <- fb[fb$Element=="Production", "Unit"] %>% unique()
  import_u <- fb[fb$Element=="Import Quantity", "Unit"] %>% unique()
  export_u <- fb[fb$Element=="Export Quantity", "Unit"] %>% unique()
  stock_u <- fb[fb$Element=="Stock Variation", "Unit"] %>% unique()
  dom_supply_u <- fb[fb$Element=="Domestic supply quantity", "Unit"] %>% unique()
  food_u <- fb[fb$Element=="Food", "Unit"] %>% unique()
  

  if(length(import_u) > 1 | length(dom_supply_u) > 1 | length(food_u) > 1 | 
     length(prod_u) > 1 | length(export_u) > 1 | length(stock_u) > 1 ){
    stop("different units used within Elements")
  }
  if(!all.equal(import_u, dom_supply_u, food_u, prod_u, export_u, stock_u)){
    stop("different units used across Elements")
  }
  if(import_u != "1000 tonnes"){
    stop("different units used across YEARS")
  }
  
  u_less_slct <- fb$Element %in% c("Production", "Import Quantity", "Export Quantity", "Stock Variation", 
                                   "Domestic supply quantity", "Food")
  fb[u_less_slct,] <- mutate(fb[u_less_slct,], Element = paste0(Element, " (",Unit,")"))

  fb <- dplyr::select(fb, -Unit)
  
  ## For the moment, do not bother Flags
  fb <- fb[, !grepl("Flag", names(fb))]
  
  # Now split data by Element
  unique(fb$Element)
  # "Food (1000 tonnes)" is the annual quantity available
  # "Food supply quantity (kg/capita/yr)" is the annual quantity available, but divided by population
  elmt_wide_ds_list <- list()
  for(elmt in unique(fb$Element)){
    long_ds <- fb[fb$Element==elmt, c("Area", "Item", "Value")]
    
    wide_ds <- stats::reshape(long_ds,
                              # varying = unique(long_ds$Item),
                              # v.names = c("Value"),
                              sep = ".",
                              timevar = "Item",
                              idvar = "Area", 
                              direction = "wide",
                              new.row.names = NULL)  
    
                vars_slct <- grepl("Value.", names(wide_ds))
                
                # those variables that have been reshaped, give the Element identifier to their names
                names(wide_ds)[vars_slct] <- paste0(names(wide_ds)[vars_slct], " -- ", elmt)
                # checked that " -- " is not used in names already 
                # any(grepl(" -- ", names(wide_ds)))
                
                # remove "Value." part in names
                names(wide_ds)[vars_slct] <- gsub("Value.", "", 
                                                  x = names(wide_ds)[vars_slct])
  
    elmt_wide_ds_list[[elmt]] <- wide_ds
  }
  rm(wide_ds)
  
  # and then join them back based on country key
  wide_fb <- elmt_wide_ds_list[[1]]
  for(i in 2:length(elmt_wide_ds_list)){
    wide_fb <- left_join(wide_fb, elmt_wide_ds_list[[i]], by = "Area")
  }
  # at this point, 175 rows, one for each country, and, if no Item has been removed, 808 columns, one for each type Element*Item 
  
  length(all_items)*7 == ncol(wide_fb) - 1
  # not all selected items are available for every 7 elements. 
  # in particular, grand total and vegetable and animal product totals are available only in nutrient, not in weights 
  
  ### Clean some area related things
  unique(wide_fb$Area)
  # Handle China: get Taiwan apart (makes sense in food security context)
  
  wide_fb$Area[wide_fb$Area == "China, Taiwan Province of"] <- "Taiwan"
  
  # Remove China (which aggregates China mainland and Taiwan), and keep only China mainland 
  wide_fb <- dplyr::filter(wide_fb, Area != "China")
  
  # and remove Hong Kong and Macao 
  wide_fb <- dplyr::filter(wide_fb, Area != "China, Hong Kong SAR")
  wide_fb <- dplyr::filter(wide_fb, Area != "China, Macao SAR")

  wide_fb[grepl("China", wide_fb$Area), c("Area", "Rice (Milled Equivalent) -- Production (1000 tonnes)")]
  
  # Remove oversea territories
  # wide_fb[grepl("Fr", wide_fb$Area), c("Area", "Rice (Milled Equivalent) -- Production (1000 tonnes)")]
  wide_fb <- dplyr::filter(wide_fb, Area != "French Polynesia")
  wide_fb <- dplyr::filter(wide_fb, Area != "Netherlands Antilles (former)")
  wide_fb <- dplyr::filter(wide_fb, Area != "Bermuda")
  wide_fb <- dplyr::filter(wide_fb, Area != "New Caledonia")
  
  # Handle some weird names 
  # "TÃ¼rkiye" and "CÃ´te d'Ivoire" 
  wide_fb$Area[wide_fb$Area == "TÃ¼rkiye"] <- "Turkey"
  wide_fb$Area[wide_fb$Area == "T?rkiye"] <- "Turkey"
  # wide_fb[grepl("Tur", wide_fb$Area), c("Area", "Rice (Milled Equivalent) -- Production (1000 tonnes)")]
  # wide_fb[grepl("?", wide_fb$Area), c("Area", "Rice (Milled Equivalent) -- Production (1000 tonnes)")]
  wide_fb$Area[wide_fb$Area == "CÃ´te d'Ivoire"] <- "Ivory Coast"
  wide_fb$Area[wide_fb$Area == "C?te d'Ivoire"] <- "Ivory Coast"
  
  # CONGO - this is special: there is only one Congo in the data, named simply "Congo". 
  # Checking by the population size in the data, it is the Republic of the Congo (i.e. Congo Brazzaville)
  wide_fb$Area[wide_fb$Area == "Congo"] <- "Republic of the Congo"
  
  # Keep track of the year 
  wide_fb$year <- year
  
  wide_fb_list[[match(year, pretreatment_years)]] <- wide_fb
} # Close loop over years here, because we will need imputations from other years for NAs in the next steps

# stack to make a panel of food balance data  
pfb <- bind_rows(wide_fb_list)
# rm(wide_fb_list, wide_fb)
# unique(pfb$Area)




#### RDC, LIBYA, SYRIA & PAPUA NEW GUINEA -----------------------------------------------------------

# Let us handle here the fact that the Democratic Republic of Congo is missing in FAOSTAT Food Balance data before 2010. 
wide_rdc_list <- list()
missing_country_years <- c(2010, 2011)
for(year in missing_country_years){
  rdc <- read.csv(here("input_data", "exposure_variables", paste0("FAOSTAT-foodbalances_missingcountries_aggritems_",year,".csv")))

  # unique(rdc$Item)[grepl("meal", unique(rdc$Item))]
  
  # the first row names gets weird "ï.." prefixe
  # names(rdc)[1] <- "Domain.Code"
  
  # the domain is the common to the whole data set "Food Balances (-2013, old methodology and population)" so we can remove it
  unique(rdc$Domain) 
  
  rdc <- rdc[, !grepl("Domain", names(rdc))]
  
  ## Area and area code are bijective 
  length(unique(rdc$Area)) == length(unique(rdc$Area.Code..FAO.)) # NOTICE it's a different name for area code than in FB -2013 data 
  rdc <- dplyr::select(rdc, -Area.Code..FAO.)
  
  ## Element
  unique(rdc$Element)
  # "Food supply (kcal/capita/day)"          
  # "Protein supply quantity (g/capita/day)" 
  # "Fat supply quantity (g/capita/day)"   
  # "Production"
  # "Import Quantity"      
  # "Stock Variation" 
  # "Export Quantity" 
  # "Domestic supply quantity"               
  # "Food"                                  
  # "Food supply quantity (kg/capita/yr)"   
  
  # Element and Element.Code are bijective 
  length(unique(rdc$Element)) == length(unique(rdc$Element.Code))
  rdc <- dplyr::select(rdc, -Element.Code)
  
  ## Item 
  unique(rdc$Item)
  
  # there are some Items that have more than one Item.Code
  length(unique(rdc$Item)) == length(unique(rdc$Item.Code))
  
  spot_doublones <- sapply(unique(rdc$Item), function(itm){itm_length <- rdc[rdc$Item==itm,"Item.Code"] %>% unique() %>% length() 
  itm_spot <- if_else(itm_length > 1, true = itm, false = "")
  return(itm_spot) })
  spot_doublones[spot_doublones != ""]
  
  rdc[rdc$Item == "Eggs","Item.Code"] %>% unique()
  rdc[rdc$Item == "Milk - Excluding Butter","Item.Code"] %>% unique()
  rdc[rdc$Item == "Miscellaneous","Item.Code"] %>% unique()
  
  rdc[rdc$Item == "Eggs",] 
  rdc[rdc$Item == "Miscellaneous",] 
  
  # they seem to be the same figures, or almost, but with different Flags (not always) --> let's keep only one instance
  
  # Items with several item codes are duplicates (within the same country and Element)
  rdc[duplicated(rdc[,c("Area", "Element", "Item")]), ] %>% nrow() # 2951 obs. in 2001
  # we want those that are not duplicates
  rdc <- rdc[!duplicated(rdc[,c("Area", "Element", "Item")]), ]
  
  if(!(length(unique(rdc$Item)) == length(unique(rdc$Item.Code)))){
    stop("there are still duplicates in Item variable")
  }
  
  rdc <- dplyr::select(rdc, -Item.Code)
  
  # And some items have different names than in main (old) FB (-2013) 
  rdc$Item[!(rdc$Item %in% fb$Item)] %>% unique()
  rdc$Item[rdc$Item=="Rice and products"] <- "Rice (Milled Equivalent)"
  rdc$Item[rdc$Item=="Groundnuts"] <- "Groundnuts (Shelled Eq)"
  
  # keep only Items specified
  rdc <- dplyr::filter(rdc, Item %in% all_items)
  
  ## Year
  rdc <- dplyr::select(rdc, -Year.Code)
  # Year is not useful either
  rdc <- dplyr::select(rdc, -Year)
  
  
  ## Unit 
  # For "Import Quantity", "Domestic supply quantity" & "Food", the unit is not given in the Element. 
  
  # some checks that the units are expressed in a sound way
  prod_u <- rdc[rdc$Element=="Production", "Unit"] %>% unique()
  import_u <- rdc[rdc$Element=="Import Quantity", "Unit"] %>% unique()
  export_u <- rdc[rdc$Element=="Export Quantity", "Unit"] %>% unique()
  stock_u <- rdc[rdc$Element=="Stock Variation", "Unit"] %>% unique()
  dom_supply_u <- rdc[rdc$Element=="Domestic supply quantity", "Unit"] %>% unique()
  food_u <- rdc[rdc$Element=="Food", "Unit"] %>% unique()
  
  
  if(length(import_u) > 1 | length(dom_supply_u) > 1 | length(food_u) > 1 | 
     length(prod_u) > 1 | length(export_u) > 1 | length(stock_u) > 1 ){
    stop("different units used within Elements")
  }
  if(!all.equal(import_u, dom_supply_u, food_u, prod_u, export_u, stock_u)){
    stop("different units used across Elements")
  }
  if(import_u != "1000 tonnes"){
    stop("different units used across YEARS")
  }
  
  u_less_slct <- rdc$Element %in% c("Production", "Import Quantity", "Export Quantity", "Stock Variation", 
                                   "Domestic supply quantity", "Food")
  rdc[u_less_slct,] <- mutate(rdc[u_less_slct,], Element = paste0(Element, " (",Unit,")"))
  
  rdc <- dplyr::select(rdc, -Unit)
   
  ## For the moment, do not bother Flags
  rdc <- rdc[, !grepl("Flag", names(rdc))]
  
  # Now split data by Element
  unique(rdc$Element)
  # "Food (1000 tonnes)" is the annual quantity available
  # "Food supply quantity (kg/capita/yr)" is the annual quantity available, but divided by population
  elmt_wide_ds_list <- list()
  for(elmt in unique(rdc$Element)){
    long_ds <- rdc[rdc$Element==elmt, c("Area", "Item", "Value")]
    
    wide_ds <- stats::reshape(long_ds,
                              # varying = unique(long_ds$Item),
                              # v.names = c("Value"),
                              sep = ".",
                              timevar = "Item",
                              idvar = "Area", 
                              direction = "wide",
                              new.row.names = NULL)  
    
    vars_slct <- grepl("Value.", names(wide_ds))
    
    # those variables that have been reshaped, give the Element identifier to their names
    names(wide_ds)[vars_slct] <- paste0(names(wide_ds)[vars_slct], " -- ", elmt)
    # checked that " -- " is not used in names already 
    # any(grepl(" -- ", names(wide_ds)))
    
    # remove "Value." part in names
    names(wide_ds)[vars_slct] <- gsub("Value.", "", 
                                      x = names(wide_ds)[vars_slct])
    
    elmt_wide_ds_list[[elmt]] <- wide_ds
  }
  rm(wide_ds)
  
  # and then join them back based on country key
  wide_rdc <- elmt_wide_ds_list[[1]]
  for(i in 2:length(elmt_wide_ds_list)){
    wide_rdc <- left_join(wide_rdc, elmt_wide_ds_list[[i]], by = "Area")
  }
  
  
  # Keep track of the year 
  wide_rdc$year <- year
  
  wide_rdc_list[[match(year, missing_country_years)]] <- wide_rdc
  
}
prdc <- bind_rows(wide_rdc_list)

### MERGE RDC CONGO DATA WITH MAIN ONE 

# Not as many Element*Item data rows are available, as for the global, old, food balance data (-2013). 
# This prevents from row binding with panel of all countries
# Simply add the columns missing, filled with NAs 
missing_col_names <- names(pfb)[!(names(pfb) %in% names(prdc))]
# (all names in rdc data are in global data)
names(prdc)[!(names(prdc) %in% names(pfb))]

missing_cols <- matrix(data = NA, ncol = length(missing_col_names), nrow = nrow(prdc))
colnames(missing_cols) <- missing_col_names
missing_cols <- as.data.frame(missing_cols)
prdc <- cbind(prdc, missing_cols)
 
pfb <- rbind(prdc, pfb)

# for convenience
row.names(pfb) <- dplyr::mutate(pfb, Area_year = paste0(Area, "_", year))$Area_year

#### PREPARE CONVERSION FACTORS -----------------------------------------------------------------------------------------

# Retrieve conversion factors (weights to nutritional contents), for aggregated categories, and individual commodities (main ones only)

# pfb[, names(pfb) %in% paste0(categories, " -- ","Food supply (kcal/capita/day)")] %>% head()
# pfb[, names(pfb) %in% paste0(categories, " -- ","Protein supply quantity (g/capita/day)")] %>% head()
# pfb[, names(pfb) %in% paste0(categories, " -- ","Fat supply quantity (g/capita/day)")] %>% head()

for(item in c(categories, cereals, oilcrops, vegetable_oils)){# 
  
  pfb[, paste0("kcal_per_kg -- ",item)] <- pfb[, paste0(item," -- ","Food supply (kcal/capita/day)")] * 365 / 
                                           pfb[, paste0(item," -- ","Food supply quantity (kg/capita/yr)")]  
  
  pfb[, paste0("gprot_per_kg -- ",item)] <- pfb[, paste0(item," -- ","Protein supply quantity (g/capita/day)")] * 365 / 
                                            pfb[, paste0(item," -- ","Food supply quantity (kg/capita/yr)")]  
  
  pfb[, paste0("gfat_per_kg -- ",item)] <- pfb[, paste0(item," -- ","Fat supply quantity (g/capita/day)")] * 365 / 
                                           pfb[, paste0(item," -- ","Food supply quantity (kg/capita/yr)")]  
  
  # note that the potential country-year specificity of these conversion factors is maintained - but they are missing sometimes

  ## NON FINITE / MISSING CONVERSION FACTORS 
  
  # First, handle Inf, due to null (0) food supply quantity (i.e. attempting to divide by 0)
  pfb[is.infinite(pfb[,paste0("kcal_per_kg -- ", item)]), c(paste0("kcal_per_kg -- ", item))] <- NA
  pfb[is.infinite(pfb[,paste0("gprot_per_kg -- ", item)]), c(paste0("gprot_per_kg -- ", item))] <- NA
  pfb[is.infinite(pfb[,paste0("gfat_per_kg -- ", item)]), c(paste0("gfat_per_kg -- ", item))] <- NA
  
  # There ARE missings, due to NAs in food supply (nutrient) or food supply quantity. 
  # This is the case for individual commodities, but also for categories, like Oilcrops
  
  # We can impute by using the average values across years for the same country, 
  avg_convfact <- ddply(.data = pfb, .variables = "Area", .fun = summarise, 
                            !!as.symbol(paste0("timeavg_kcal_per_kg -- ", item)) := mean(!!as.symbol(paste0("kcal_per_kg -- ",item)), na.rm = TRUE), 
                            !!as.symbol(paste0("timeavg_gprot_per_kg -- ", item)) := mean(!!as.symbol(paste0("gprot_per_kg -- ",item)), na.rm = TRUE), 
                            !!as.symbol(paste0("timeavg_gfat_per_kg -- ", item)) := mean(!!as.symbol(paste0("gfat_per_kg -- ",item)), na.rm = TRUE))

  # head(avg_convfact)
  
  # and then, taking average values across countries if the conversion factor is still missing.
  for(var in names(avg_convfact)[names(avg_convfact) != "Area"]){
    avg_convfact[,var][is.na(avg_convfact[,var])] <- mean(avg_convfact[,var], na.rm = TRUE)
  }
  
  # Finally, make the imputations
  pfb <- left_join(pfb, avg_convfact, by = "Area")
  
  # countries that have missings, for this item 
  # cntry_wmiss <- pfb[!is.finite(pfb[,paste0("kcal_per_kg -- ", item)]), c("Area")] %>% unique()
  
  # pfb[!is.finite(pfb[,paste0("kcal_per_kg -- ", item)]), c("Area", grep("Oilcrops", names(pfb), value = TRUE))] %>% head()

  pfb <- dplyr::mutate(pfb, 
                       !!as.symbol(paste0("kcal_per_kg -- ", item)) := if_else(!is.finite(!!as.symbol(paste0("kcal_per_kg -- ", item))), 
                                                                               true = !!as.symbol(paste0("timeavg_kcal_per_kg -- ", item)), 
                                                                               false = !!as.symbol(paste0("kcal_per_kg -- ", item))), 
                       
                       !!as.symbol(paste0("gprot_per_kg -- ", item)) := if_else(!is.finite(!!as.symbol(paste0("gprot_per_kg -- ", item))), 
                                                                               true = !!as.symbol(paste0("timeavg_gprot_per_kg -- ", item)), 
                                                                               false = !!as.symbol(paste0("gprot_per_kg -- ", item))), 
                       
                       !!as.symbol(paste0("gfat_per_kg -- ", item)) := if_else(!is.finite(!!as.symbol(paste0("gfat_per_kg -- ", item))), 
                                                                               true = !!as.symbol(paste0("timeavg_gfat_per_kg -- ", item)), 
                                                                               false = !!as.symbol(paste0("gfat_per_kg -- ", item)))
                       )
  
  
  # pfb[pfb$Area %in% cntry_wmiss, c("Area", grep("Oilcrops", names(pfb), value = TRUE))]
  
  # for food supply and import variables, we don't impute across years or countries, this would not make much sense, 
  # and implied missings in annual dependency measures will be partly handled by their averaging across pre-treatment years.  

}

# unique(fb$Element)
# pfbsave <- pfb 





#### MAKE STATISTICS OF INTEREST ---------------------------------------------------------------------------------

### Convert imports, exports, and domestic supply into their nutrient contents (they are in 1000 tonnes and we first convert them to kg, to match conversion factors)
for(item in c(categories, cereals, oilcrops, vegetable_oils)){# 
  for(nutrient in c("kcal", "gprot", "gfat")){
    pfb[, paste0("import_",nutrient," -- ",item)] <- pfb[, paste0(item," -- ","Import Quantity (1000 tonnes)")] * 1e6 * 
                                                     pfb[, paste0(nutrient,"_per_kg -- ",item)]  
    
    pfb[, paste0("export_",nutrient," -- ",item)] <- pfb[, paste0(item," -- ","Export Quantity (1000 tonnes)")] * 1e6 * 
                                                     pfb[, paste0(nutrient,"_per_kg -- ",item)]  
    
    pfb[, paste0("domsupply_",nutrient," -- ",item)] <- pfb[, paste0(item," -- ","Domestic supply quantity (1000 tonnes)")] * 1e6 * 
                                                        pfb[, paste0(nutrient,"_per_kg -- ",item)]  
    # casing is correct here: capital Q for import and export, not for domestic supply
    
    # And add up export and production
    pfb[, paste0("gross_supply_",nutrient," -- ",item)] <- pfb[, paste0("domsupply_",nutrient," -- ",item)] + 
                                                           pfb[, paste0("export_",nutrient," -- ",item)]
    
    # pfb <- dplyr::mutate(pfb, !!as.symbol(paste0("gross_supply2_", nutrient," -- ",item)) := !!as.symbol(paste0("domsupply_",nutrient," -- ",item)) + 
    #                                                                                          !!as.symbol(paste0("export_",nutrient," -- ",item)) )
    # all.equal(pfb[, paste0("gross_supply_",nutrient," -- ",item)], pfb[, paste0("gross_supply2_",nutrient," -- ",item)])
  }
}
# pfb[,grep("Miscellaneous", names(pfb), value = TRUE)] %>% head()


### SUM OVER CATEGORIES OF ITEMS 
# (this is the operation that requires converting to commodities to nutrient in the first place)
for(nutrient in c("kcal", "gprot", "gfat")){
  pfb <- dplyr::mutate(pfb, !!as.symbol(paste0("import_",nutrient,"_total")) := base::rowSums(across(.cols = any_of(paste0("import_",nutrient," -- ", categories)) ), na.rm = TRUE))
  pfb <- dplyr::mutate(pfb, !!as.symbol(paste0("gross_supply_",nutrient,"_total")) := base::rowSums(across(.cols = any_of(paste0("gross_supply_",nutrient," -- ", categories)) ), na.rm = TRUE))
}
# pfb <- dplyr::mutate(pfb, import_gprot_total = base::rowSums(across(.cols = any_of(paste0("gross_supply_",nutrient," -- ", categories)) ), na.rm = TRUE))

### MAKE RATIOS 
# Grand Total and animal/vegetable product categories are not available for import and domestic supply directly from FAOSTAT
# Hence, we computed imports by nutrient values, so that we can then relate to nutrient supply quantities  
pfb$dependency_calorie <- pfb[, paste0("import_kcal_total")] /
                          pfb[, paste0("gross_supply_kcal_total")]

pfb$dependency_protein <- pfb[, paste0("import_gprot_total")] /
                          pfb[, paste0("gross_supply_gprot_total")]

pfb$dependency_fat <- pfb[, paste0("import_gfat_total")] /
                      pfb[, paste0("gross_supply_gfat_total")]

summary(pfb$dependency_calorie)

# Dependency through some selected crops
for(item in selected_items){
  pfb[, paste0("dependency_calorie -- ",item)] <- pfb[, paste0("import_kcal -- ",item)] /
                                                  pfb[, "gross_supply_kcal_total"]
  
  pfb[, paste0("dependency_protein -- ",item)] <- pfb[, paste0("import_gprot -- ",item)] /
                                                  pfb[, "gross_supply_gprot_total"]
  
  pfb[, paste0("dependency_fat -- ",item)] <- pfb[, paste0("import_gfat -- ",item)] /
                                              pfb[, "gross_supply_gfat_total"]
}


csfb5 <- ddply(pfb, "Area", summarise, 
              # Main dependency variables: 
              dependency_calorie = mean(dependency_calorie, na.rm = TRUE), 
              dependency_protein = mean(dependency_protein, na.rm = TRUE), 
              dependency_fat = mean(dependency_fat, na.rm = TRUE), 
              # import, total: 
              import_kcal_total = mean(import_kcal_total, na.rm = TRUE), 
              import_gprot_total = mean(import_gprot_total, na.rm = TRUE), 
              import_gfat_total = mean(import_gfat_total, na.rm = TRUE), 
              # gross supply, total: 
              gross_supply_kcal_total = mean(gross_supply_kcal_total, na.rm = TRUE), 
              gross_supply_gprot_total = mean(gross_supply_gprot_total, na.rm = TRUE), 
              gross_supply_gfat_total = mean(gross_supply_gfat_total, na.rm = TRUE), 
              
              # Dependency through specific crops
              !!as.symbol("dependency_calorie -- Cereals - Excluding Beer") := mean(!!as.symbol("dependency_calorie -- Cereals - Excluding Beer"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Wheat and products") := mean(!!as.symbol("dependency_calorie -- Wheat and products"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Rice (Milled Equivalent)") := mean(!!as.symbol("dependency_calorie -- Rice (Milled Equivalent)"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Barley and products") := mean(!!as.symbol("dependency_calorie -- Barley and products"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Maize and products") := mean(!!as.symbol("dependency_calorie -- Maize and products"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Soyabeans") := mean(!!as.symbol("dependency_calorie -- Soyabeans"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Vegetable Oils") := mean(!!as.symbol("dependency_calorie -- Vegetable Oils"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Soyabean Oil") := mean(!!as.symbol("dependency_calorie -- Soyabean Oil"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Sunflowerseed Oil") := mean(!!as.symbol("dependency_calorie -- Sunflowerseed Oil"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Rape and Mustard Oil") := mean(!!as.symbol("dependency_calorie -- Rape and Mustard Oil"), na.rm = TRUE), 
              !!as.symbol("dependency_calorie -- Palm Oil") := mean(!!as.symbol("dependency_calorie -- Palm Oil"), na.rm = TRUE),
              
              # import of specific crops 
              !!as.symbol("import_kcal -- Cereals - Excluding Beer") := mean(!!as.symbol("import_kcal -- Cereals - Excluding Beer"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Wheat and products") := mean(!!as.symbol("import_kcal -- Wheat and products"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Rice (Milled Equivalent)") := mean(!!as.symbol("import_kcal -- Rice (Milled Equivalent)"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Barley and products") := mean(!!as.symbol("import_kcal -- Barley and products"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Maize and products") := mean(!!as.symbol("import_kcal -- Maize and products"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Soyabeans") := mean(!!as.symbol("import_kcal -- Soyabeans"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Vegetable Oils") := mean(!!as.symbol("import_kcal -- Vegetable Oils"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Soyabean Oil") := mean(!!as.symbol("import_kcal -- Soyabean Oil"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Sunflowerseed Oil") := mean(!!as.symbol("import_kcal -- Sunflowerseed Oil"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Rape and Mustard Oil") := mean(!!as.symbol("import_kcal -- Rape and Mustard Oil"), na.rm = TRUE), 
              !!as.symbol("import_kcal -- Palm Oil") := mean(!!as.symbol("import_kcal -- Palm Oil"), na.rm = TRUE),

              # gross supply of specific crops 
              !!as.symbol("gross_supply_kcal -- Cereals - Excluding Beer") := mean(!!as.symbol("gross_supply_kcal -- Cereals - Excluding Beer"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Wheat and products") := mean(!!as.symbol("gross_supply_kcal -- Wheat and products"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Rice (Milled Equivalent)") := mean(!!as.symbol("gross_supply_kcal -- Rice (Milled Equivalent)"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Barley and products") := mean(!!as.symbol("gross_supply_kcal -- Barley and products"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Maize and products") := mean(!!as.symbol("gross_supply_kcal -- Maize and products"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Soyabeans") := mean(!!as.symbol("gross_supply_kcal -- Soyabeans"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Vegetable Oils") := mean(!!as.symbol("gross_supply_kcal -- Vegetable Oils"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Soyabean Oil") := mean(!!as.symbol("gross_supply_kcal -- Soyabean Oil"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Sunflowerseed Oil") := mean(!!as.symbol("gross_supply_kcal -- Sunflowerseed Oil"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Rape and Mustard Oil") := mean(!!as.symbol("gross_supply_kcal -- Rape and Mustard Oil"), na.rm = TRUE), 
              !!as.symbol("gross_supply_kcal -- Palm Oil") := mean(!!as.symbol("gross_supply_kcal -- Palm Oil"), na.rm = TRUE),
              
              # conversion factors, for some major items: 
              !!as.symbol("kcal_per_kg -- Cereals - Excluding Beer") := mean(!!as.symbol("kcal_per_kg -- Cereals - Excluding Beer"), na.rm = TRUE), 
              !!as.symbol("gprot_per_kg -- Cereals - Excluding Beer") := mean(!!as.symbol("gprot_per_kg -- Cereals - Excluding Beer"), na.rm = TRUE), 
              !!as.symbol("gfat_per_kg -- Cereals - Excluding Beer") := mean(!!as.symbol("gfat_per_kg -- Cereals - Excluding Beer"), na.rm = TRUE), 
              !!as.symbol("kcal_per_kg -- Vegetable Oils") := mean(!!as.symbol("kcal_per_kg -- Vegetable Oils"), na.rm = TRUE), 
              !!as.symbol("gprot_per_kg -- Meat") := mean(!!as.symbol("gprot_per_kg -- Meat"), na.rm = TRUE), 
              !!as.symbol("gprot_per_kg -- Fish, Seafood") := mean(!!as.symbol("gprot_per_kg -- Fish, Seafood"), na.rm = TRUE), 
              !!as.symbol("gfat_per_kg -- Vegetable Oils") := mean(!!as.symbol("gfat_per_kg -- Vegetable Oils"), na.rm = TRUE)
            )

# alternatively, average over the two most recent years only
csfb2 <- ddply(pfb[pfb$year == 2004 | pfb$year == 2005 | pfb$year == 2010 | pfb$year == 2011,], 
               "Area", 
               summarise, 
               # Main dependency variables: 
               dependency_calorie = mean(dependency_calorie, na.rm = TRUE), 
               dependency_protein = mean(dependency_protein, na.rm = TRUE), 
               dependency_fat = mean(dependency_fat, na.rm = TRUE), 
               # import, total: 
               import_kcal_total = mean(import_kcal_total, na.rm = TRUE), 
               import_gprot_total = mean(import_gprot_total, na.rm = TRUE), 
               import_gfat_total = mean(import_gfat_total, na.rm = TRUE), 
               # gross supply, total: 
               gross_supply_kcal_total = mean(gross_supply_kcal_total, na.rm = TRUE), 
               gross_supply_gprot_total = mean(gross_supply_gprot_total, na.rm = TRUE), 
               gross_supply_gfat_total = mean(gross_supply_gfat_total, na.rm = TRUE), 
               
               # Dependency through specific crops
               !!as.symbol("dependency_calorie -- Cereals - Excluding Beer") := mean(!!as.symbol("dependency_calorie -- Cereals - Excluding Beer"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Wheat and products") := mean(!!as.symbol("dependency_calorie -- Wheat and products"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Rice (Milled Equivalent)") := mean(!!as.symbol("dependency_calorie -- Rice (Milled Equivalent)"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Barley and products") := mean(!!as.symbol("dependency_calorie -- Barley and products"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Maize and products") := mean(!!as.symbol("dependency_calorie -- Maize and products"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Soyabeans") := mean(!!as.symbol("dependency_calorie -- Soyabeans"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Vegetable Oils") := mean(!!as.symbol("dependency_calorie -- Vegetable Oils"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Soyabean Oil") := mean(!!as.symbol("dependency_calorie -- Soyabean Oil"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Sunflowerseed Oil") := mean(!!as.symbol("dependency_calorie -- Sunflowerseed Oil"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Rape and Mustard Oil") := mean(!!as.symbol("dependency_calorie -- Rape and Mustard Oil"), na.rm = TRUE), 
               !!as.symbol("dependency_calorie -- Palm Oil") := mean(!!as.symbol("dependency_calorie -- Palm Oil"), na.rm = TRUE),
               
               # import of specific crops 
               !!as.symbol("import_kcal -- Cereals - Excluding Beer") := mean(!!as.symbol("import_kcal -- Cereals - Excluding Beer"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Wheat and products") := mean(!!as.symbol("import_kcal -- Wheat and products"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Rice (Milled Equivalent)") := mean(!!as.symbol("import_kcal -- Rice (Milled Equivalent)"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Barley and products") := mean(!!as.symbol("import_kcal -- Barley and products"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Maize and products") := mean(!!as.symbol("import_kcal -- Maize and products"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Soyabeans") := mean(!!as.symbol("import_kcal -- Soyabeans"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Vegetable Oils") := mean(!!as.symbol("import_kcal -- Vegetable Oils"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Soyabean Oil") := mean(!!as.symbol("import_kcal -- Soyabean Oil"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Sunflowerseed Oil") := mean(!!as.symbol("import_kcal -- Sunflowerseed Oil"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Rape and Mustard Oil") := mean(!!as.symbol("import_kcal -- Rape and Mustard Oil"), na.rm = TRUE), 
               !!as.symbol("import_kcal -- Palm Oil") := mean(!!as.symbol("import_kcal -- Palm Oil"), na.rm = TRUE),
               
               # gross supply of specific crops 
               !!as.symbol("gross_supply_kcal -- Cereals - Excluding Beer") := mean(!!as.symbol("gross_supply_kcal -- Cereals - Excluding Beer"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Wheat and products") := mean(!!as.symbol("gross_supply_kcal -- Wheat and products"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Rice (Milled Equivalent)") := mean(!!as.symbol("gross_supply_kcal -- Rice (Milled Equivalent)"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Barley and products") := mean(!!as.symbol("gross_supply_kcal -- Barley and products"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Maize and products") := mean(!!as.symbol("gross_supply_kcal -- Maize and products"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Soyabeans") := mean(!!as.symbol("gross_supply_kcal -- Soyabeans"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Vegetable Oils") := mean(!!as.symbol("gross_supply_kcal -- Vegetable Oils"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Soyabean Oil") := mean(!!as.symbol("gross_supply_kcal -- Soyabean Oil"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Sunflowerseed Oil") := mean(!!as.symbol("gross_supply_kcal -- Sunflowerseed Oil"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Rape and Mustard Oil") := mean(!!as.symbol("gross_supply_kcal -- Rape and Mustard Oil"), na.rm = TRUE), 
               !!as.symbol("gross_supply_kcal -- Palm Oil") := mean(!!as.symbol("gross_supply_kcal -- Palm Oil"), na.rm = TRUE),
               
               # conversion factors, for some major items: 
               !!as.symbol("kcal_per_kg -- Cereals - Excluding Beer") := mean(!!as.symbol("kcal_per_kg -- Cereals - Excluding Beer"), na.rm = TRUE), 
               !!as.symbol("gprot_per_kg -- Cereals - Excluding Beer") := mean(!!as.symbol("gprot_per_kg -- Cereals - Excluding Beer"), na.rm = TRUE), 
               !!as.symbol("gfat_per_kg -- Cereals - Excluding Beer") := mean(!!as.symbol("gfat_per_kg -- Cereals - Excluding Beer"), na.rm = TRUE), 
               !!as.symbol("kcal_per_kg -- Vegetable Oils") := mean(!!as.symbol("kcal_per_kg -- Vegetable Oils"), na.rm = TRUE), 
               !!as.symbol("gprot_per_kg -- Meat") := mean(!!as.symbol("gprot_per_kg -- Meat"), na.rm = TRUE), 
               !!as.symbol("gprot_per_kg -- Fish, Seafood") := mean(!!as.symbol("gprot_per_kg -- Fish, Seafood"), na.rm = TRUE), 
               !!as.symbol("gfat_per_kg -- Vegetable Oils") := mean(!!as.symbol("gfat_per_kg -- Vegetable Oils"), na.rm = TRUE)
               
              )

summary(csfb5$dependency_calorie)
summary(csfb5$dependency_protein)
summary(csfb5$dependency_fat)


#### SAVE OUT -------------------------------------------------------------------------------------------------------
saveRDS(csfb5, file = here("temp_data", "exposures", "dependency_5ya.Rdata"))
saveRDS(csfb2, file = here("temp_data", "exposures", "dependency_2ya.Rdata"))

#### Data exploration  -----------------------------------------------------------------------------------------------

sffb <- st_read(here("input_data", "Global_LSIB_Polygons_Detailed"))

# necessary for simplifying below, and better for unioning 
sffb <- st_transform(sffb, crs = 4088)

names(sffb)[names(sffb) == "COUNTRY_NA"] <- "Area"
# sffb[grepl("ongo", sffb$Area), ]
# pfb[grepl("ongo", pfb$Area), "Area"]

### Match country names to those from FAOSTAT 
## /!\ NEXT TIME CONSIDER USING A FAOSTAT ADMIN BOUNDARIES MAP DIRECTLY ! 
# (but some advantages to adjust manually some things here)

# Note that some FAOSTAT names have been modified above already (they are on top here)
# Note also that we do not associate islands to their main country, for simplicity (by commenting their section out)
sffb$Area[sffb$Area=="Cote d'Ivoire"] <- "Ivory Coast" # NOTICE THIS (FAOSTAT name is Côte d'Ivoire)
sffb$Area[sffb$Area=="Congo, Rep of the"] <- "Republic of the Congo" # This name is not from FAOSTAT, I changed it above

# Political discrepancies
sffb$Area[sffb$Area=="China"] <- "China, mainland" # NOTICE THIS 
sffb$Area[sffb$Area=="Hong Kong (Ch)"] <- "China, Hong Kong SAR"
sffb$Area[sffb$Area=="Macau (Ch)"] <- "China, Macao SAR"
# sffb$Area[sffb$Area=="Taiwan"] 

# for Sudan and South Sudan, they will be splitted in the anaysis period. They will have outcome data separately. 
# But they will have exactly the same treatment. So we can treat them as within the same cluster, 
# or merge them into a single country, with average outcome. 
sffb[sffb$Area=="Sudan", "geometry"] <- st_union(sffb[sffb$Area == "Sudan", "geometry"], 
                                                       sffb[sffb$Area == "South Sudan", "geometry"]) # %>% dplyr::select(geometry)
sffb$Area[sffb$Area=="Sudan"] <- "Sudan (former)" # South Sudan independent since 2005, after our data period

sffb[sffb$Area=="Serbia", "geometry"] <- st_union(sffb[sffb$Area == "Serbia", "geometry"], 
                                                  sffb[sffb$Area == "Montenegro", "geometry"]) # %>% dplyr::select(geometry)
sffb$Area[sffb$Area=="Serbia"] <- "Serbia and Montenegro" # Montenegro independent since 2006, after our data period

# Countries for which it's simply a matter of different way to write the name down
sffb$Area[sffb$Area=="Congo, Dem Rep of the"] <- "Democratic Republic of the Congo"
sffb$Area[sffb$Area=="Macedonia"] <- "North Macedonia"
sffb$Area[sffb$Area=="United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland"
sffb$Area[sffb$Area=="United States"] <- "United States of America"
sffb$Area[sffb$Area=="Russia"] <- "Russian Federation"
sffb$Area[sffb$Area=="Syria"] <- "Syrian Arab Republic"
sffb$Area[sffb$Area=="Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
sffb$Area[sffb$Area=="Korea, North"] <- "Democratic People's Republic of Korea"
sffb$Area[sffb$Area=="Korea, South"] <- "Republic of Korea"
sffb$Area[sffb$Area=="Moldova"] <- "Republic of Moldova"
sffb$Area[sffb$Area=="Gambia, The"] <- "Gambia"
sffb$Area[sffb$Area=="Swaziland"] <- "Eswatini"
sffb$Area[sffb$Area=="Iran"] <- "Iran (Islamic Republic of)"
sffb$Area[sffb$Area=="Burma"] <- "Myanmar"
sffb$Area[sffb$Area=="Bahamas, The"] <- "Bahamas"
sffb$Area[sffb$Area=="Vietnam"] <- "Viet Nam"
sffb$Area[sffb$Area=="Laos"] <- "Lao People's Democratic Republic"
sffb$Area[sffb$Area=="Antigua & Barbuda"] <- "Antigua and Barbuda"
sffb$Area[sffb$Area=="St Kitts & Nevis"] <- "Saint Kitts and Nevis"
sffb$Area[sffb$Area=="St Lucia"] <- "Saint Lucia"
sffb$Area[sffb$Area=="St Vincent & the Grenadines"] <- "Saint Vincent and the Grenadines"
sffb$Area[sffb$Area=="Venezuela"] <- "Venezuela (Bolivarian Republic of)"
sffb$Area[sffb$Area=="Trinidad & Tobago"] <- "Trinidad and Tobago"
sffb$Area[sffb$Area=="Central African Rep"] <- "Central African Republic"
sffb$Area[sffb$Area=="Brunei"] <- "Brunei Darussalam"
sffb$Area[sffb$Area=="Sao Tome & Principe"] <- "Sao Tome and Principe"
sffb$Area[sffb$Area=="Tanzania"] <- "United Republic of Tanzania"
sffb$Area[sffb$Area=="Solomon Is"] <- "Solomon Islands"
sffb$Area[sffb$Area=="Bolivia"] <- "Bolivia (Plurinational State of)"
# sffb$Area[sffb$Area=="Micronesia, Fed States of"]

# ISLANDS
# sffb$Area[sffb$Area=="Niue (NZ)"] <- "New Zealand"
# sffb$Area[sffb$Area=="Turks & Caicos Is (UK)"] <- "United Kingdom of Great Britain and Northern Ireland"
# sffb$Area[sffb$Area=="Montserrat (UK)"] <- "United Kingdom of Great Britain and Northern Ireland"
# sffb$Area[sffb$Area=="Br Virgin Is (UK)"] <- "United Kingdom of Great Britain and Northern Ireland"
# sffb$Area[sffb$Area=="Pitcairn Is (UK)"] <- "United Kingdom of Great Britain and Northern Ireland"
# sffb$Area[sffb$Area=="Cayman Is (UK)"] <- "United Kingdom of Great Britain and Northern Ireland"
# sffb$Area[sffb$Area=="Puerto Rico (US)"] <- "United States of America"
# sffb$Area[sffb$Area=="Northern Mariana Is (US)"] <- "United States of America"
# sffb$Area[sffb$Area=="American Samoa (US)"] <- "United States of America"
# sffb$Area[sffb$Area=="Guam (US)"] <- "United States of America"
# sffb$Area[sffb$Area=="US Virgin Is (US)"] <- "United States of America"
# sffb$Area[sffb$Area=="Christmas I (Aus)"] <- "Australia"
# sffb$Area[sffb$Area=="Spain [Canary Is]"] <- "Spain"
# sffb$Area[sffb$Area=="Guadeloupe (Fr)"] <- "France"
# sffb$Area[sffb$Area=="Martinique (Fr)"] <- "France"
# sffb$Area[sffb$Area=="French Guiana (Fr)"] <- "France"
# sffb$Area[sffb$Area=="French Polynesia (Fr)"] <- "France"
# sffb$Area[sffb$Area=="Mayotte (Fr)"] <- "France"
# sffb$Area[sffb$Area=="Wallis & Futuna (Fr)"] <- "France"
# sffb$Area[sffb$Area=="New Caledonia (Fr)"] <- "France"
# sffb$Area[sffb$Area=="Reunion (Fr)"] <- "France"

# length(unique(sffb$Area))
# sffb$Area[duplicated(sffb$Area)]
# length(unique(csfb5$Area))
# sfctry <- unique(sffb$Area)
# fbctry <- unique(csfb5$Area)
# extra_names <- sfctry[!(sfctry %in% fbctry)]
# # (all names in rdc data are in global data)
# fbctry[!(fbctry %in% sfctry)]
# sfctry[grepl("yri", sfctry)]
# fbctry[grepl("apua", fbctry)]
pfbnames[grepl("uantity", pfbnames)]


# simplify before plotting
sffb$geometry <- st_simplify(sffb$geometry, dTolerance = 1000)

sffb <- st_transform(sffb, crs = 4326)

csfb5_sf <- left_join(csfb5, sffb, by = "Area") %>% st_as_sf()

csfb5_sf$Area[st_is_empty(csfb5_sf$geometry)]
  
#### PLOT TOTAL DEPENDENCY #### 
pal_dep <- colorNumeric("viridis", # "viridis" (green-purple), "magma" (yellow-purple), "inferno" (like magma), or "plasma", "BuPu", "Greens"
                        domain = st_drop_geometry(csfb5_sf[,"dependency_calorie"]),
                        #bins = 4, 
                        na.color = "transparent", 
                        reverse = F)


# popup
csfb5_sf$popup_total <- paste0(csfb5_sf$Area, "<br/>",
                               "Imports: ", formatC(csfb5_sf$import_kcal_total/1e6, format = "e", digits = 2), " bn cal", "<br/>",
                               "Supply + Exports: ", formatC(csfb5_sf$gross_supply_kcal_total/1e6,format = "e", digits = 2), " bn cal", "<br/>"
)



leaflet() %>% 
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group ="ESRI") %>%
  setView(lat = 0, 
          lng = 0, 
          zoom = 1) %>% 
  addPolygons(data = csfb5_sf, 
              opacity = 0, color = "black", weight = 2, 
              fill = TRUE, fillColor = ~pal_dep(csfb5_sf$dependency_calorie), fillOpacity = 0.5,
              popup = ~csfb5_sf$popup_total, 
              popupOptions = popupOptions(riseOnHover = TRUE, 
                                          bringToFront = TRUE),
              highlightOptions = highlightOptions(bringToFront = TRUE)
              ) %>% 
  # addMarkers(data = csfb5_sf, 
  #            popup = ~csfb5_sf$popup_total,
  #            options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegend(pal = pal_dep,  
            values = csfb5_sf$dependency_calorie, 
            bins = 5, opacity = 0.4,
            title = "Import dependency <br/> for calories, total",
            position = "bottomright") 



#### PLOT CONVERSION FACTORS #### 
pal_dep <- colorNumeric("viridis", # "viridis" (green-purple), "magma" (yellow-purple), "inferno" (like magma), or "plasma", "BuPu", "Greens"
                        domain = st_drop_geometry(csfb5_sf[,"kcal_per_kg -- Cereals - Excluding Beer"]),
                        #bins = 4, 
                        na.color = "transparent", 
                        reverse = F)


# popup
csfb5_sf$popup_convfact <- paste0(csfb5_sf$Area, "<br/>",
                                  "kcal per kg cereals (excl. beer): ", round(csfb5_sf$`kcal_per_kg -- Cereals - Excluding Beer`,1), "<br/>",
                                  "g protein per kg cereals (excl. beer): ", round(csfb5_sf$`gprot_per_kg -- Cereals - Excluding Beer`,1), "<br/>",
                                  "g fat per kg cereals (excl. beer): ", round(csfb5_sf$`gfat_per_kg -- Cereals - Excluding Beer`,1), "<br/>",
                                  "kcal per kg vegetable oils: ", round(csfb5_sf$`kcal_per_kg -- Vegetable Oils`,1), "<br/>",
                                  "gprot per kg meat: ", round(csfb5_sf$`gprot_per_kg -- Meat`,1), "<br/>",
                                  "gprot per kg fish & seafood: ", round(csfb5_sf$`gprot_per_kg -- Fish, Seafood`,1), "<br/>"
)

leaflet() %>% 
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group ="ESRI") %>%
  setView(lat = 0, 
          lng = 0, 
          zoom = 1) %>% 
  addPolygons(data = csfb5_sf, 
              opacity = 0, color = "black", weight = 2, 
              fill = TRUE, fillColor = ~pal_dep(csfb5_sf$`kcal_per_kg -- Cereals - Excluding Beer`), fillOpacity = 0.5,
              popup = ~csfb5_sf$popup_convfact, 
              popupOptions = popupOptions(riseOnHover = TRUE, 
                                          bringToFront = TRUE),
              highlightOptions = highlightOptions(bringToFront = TRUE)) %>% 
  # addMarkers(data = csfb5_sf, 
  #            popup = ~csfb5_sf$popup_convfact,
  #            options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegend(pal = pal_dep,  
            values = csfb5_sf$`kcal_per_kg -- Cereals - Excluding Beer`, 
            bins = 5, opacity = 0.4,
            title = "Nutritional density <br/> of cereals (kcal/kg) <br/> (2001-2005)",
            position = "bottomright") 

plot(csfb5_sf[,"dependency_calorie"])
plot(csfb5_sf[,"dependency_protein"])
plot(csfb5_sf[,"gross_supply_kcal_total"])
plot(csfb5_sf[,"import_kcal_total"])
plot(csfb5_sf[,"dependency_calorie -- Cereals - Excluding Beer"])
plot(csfb5_sf[,"dependency_calorie -- Wheat and products"])
plot(csfb5_sf[,"dependency_calorie -- Rice (Milled Equivalent)"])
plot(csfb5_sf[,"dependency_calorie -- Maize and products"])
plot(csfb5_sf[,"dependency_calorie -- Soyabeans"])
plot(csfb5_sf[,"dependency_calorie -- Vegetable Oils"])
plot(csfb5_sf[,"dependency_calorie -- Soyabean Oil"])
plot(csfb5_sf[,"dependency_calorie -- Sunflowerseed Oil"])
plot(csfb5_sf[,"dependency_calorie -- Palm Oil"])

plot(csfb5_sf[,"kcal_per_kg -- Cereals - Excluding Beer"])
plot(csfb5_sf[,"kcal_per_kg -- Vegetable Oils"])
plot(csfb5_sf[,"gprot_per_kg -- Meat"])
plot(csfb5_sf[,"gprot_per_kg -- Fish, Seafood"])
plot(csfb5_sf[,"gfat_per_kg -- Vegetable Oils"])


csfb5_sf[csfb5_sf$Area=="Niger", c("dependency_calorie", "geometry")] %>% plot()










#### PREPARE POPULATION DATA #### 
# This is necessary to scale nutrient food supply, which are expressed per capita.
pop <- read.csv(here("input_data", "exposure_variables", "FAOSTAT-oldfoodbalances_allcountries_population_20012005.csv"))
head(pop) 

# note that Unit is 1000 persons
unique(pop$Unit)
# Make it unitary 
pop$Value <- pop$Value * 1000

pop <- pop[, !grepl("Domain", names(pop))]

pop <- dplyr::select(pop, -Area.Code, -Element.Code, -Element, -Item.Code, -Item, -Year.Code, -Unit, -Flag, -Flag.Description)
head(pop)

# No imputations needed bc no missings
pop[!is.finite(pop$Value)]
names(pop) <- c("Area", "year", "population")

pfb <- left_join(pfb, pop, by = c("Area", "year"))

for(item in c(cereals, oilcrops, vegetable_oils)){
  
  pfb <- dplyr::mutate(!!as.symbol(paste0("Food supply (kcal/capita/day)")))
  
}



### MAKE RATIOS
# Most simple one is Import Quantity (1000 tonnes) / Domestic supply quantity (1000 tonnes) 
for(item in c(cereals, oilcrops, vegetable_oils)){
  pfb[, paste0("dependency -- ",item)] <- pfb[, paste0(item," -- ","Import Quantity (1000 tonnes)")] / 
                                          pfb[, paste0(item," -- ","Domestic supply quantity (1000 tonnes)")]  
}

# Grand Total and animal/vegetable product categories are not available for import and domestic supply directly from FAOSTAT
# Hence, we computed imports by nutrient values, so that we can then relate to nutrient supply quantities  
pfb <- dplyr::mutate(pfb, dependency_kcal_total = import_kcal_total / Food_supply_kcal_total)
pfb <- dplyr::mutate(pfb, dependency_gprot_total = import_gprot_total / Food_supply_kcal_total)
pfb <- dplyr::mutate(pfb, dependency_gfat_total = import_gfat_total / Food_supply_kcal_total)



names(fb)

# CHECK IF MISSING IN ANY COUNTRY 
unique(pfb$`kcal_per_kg -- Cereals - Excluding Beer`) %>% summary()

pfb
pfb$`kcal_per_kg -- Oilcrops` %>% summary()

pfb[is.na(pfb$`kcal_per_kg -- Oilcrops`), grepl("Food supply quantity", names(pfb))]# supply (kcal/capita/day)

unique(pfb$`kcal_per_kg -- Vegetable Oils`) %>% summary()