

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
dir.create(here("temp_data","exposures"))


year <- 2001
for(year in pretreatment_years){
  fb <- read.csv(here("input_data", "exposure_variables", paste0("FAOSTAT_",year,"_allcountries_foodbalances_aggritems.csv")))

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
  # "Import Quantity"                        
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
  
  ## Year
  fb <- dplyr::select(fb, -Year.Code)
  # Year is not useful either
  fb <- dplyr::select(fb, -Year)

    
  ## Unit 
  # For "Import Quantity", "Domestic supply quantity" & "Food", the unit is not given in the Element. 
  
  # some checks that the units are expressed in a sound way
  import_u <- fb[fb$Element=="Import Quantity", "Unit"] %>% unique()
  dom_supply_u <- fb[fb$Element=="Domestic supply quantity", "Unit"] %>% unique()
  food_u <- fb[fb$Element=="Food", "Unit"] %>% unique()

  if(length(import_u) > 1 | length(dom_supply_u) > 1 | length(food_u) > 1 ){
    stop("different units used within Elements")
  }
  if(!all.equal(import_u, dom_supply_u, food_u)){
    stop("different units used across Elements")
  }
  if(import_u != "1000 tonnes"){
    stop("different units used across YEARS")
  }
  
  u_less_slct <- fb$Element %in% c("Import Quantity", "Domestic supply quantity", "Food")
  fb[u_less_slct,] <- mutate(fb[u_less_slct,], Element = paste0(Element, " (",Unit,")"))

  fb <- dplyr::select(fb, -Unit)
  
  ## As of now, do not bother Flags
  fb <- fb[, !grepl("Flag", names(fb))]
  
  # Now split data by Element
  unique(fb$Element)
  food_s_kcalcapday <- fb[fb$Element=="Food supply (kcal/capita/day)", c("Area", "Item", "Value")]
  prot_s_gcapday <- fb[fb$Element=="Protein supply quantity (g/capita/day)", c("Area", "Item", "Value")]
  fat_s_gcapday <- fb[fb$Element=="Fat supply quantity (g/capita/day)", c("Area", "Item", "Value")]
  
  import_kt <- fb[fb$Element=="Import Quantity (1000 tonnes)", c("Area", "Item", "Value")]
  dom_s_kt <- fb[fb$Element=="Domestic supply quantity (1000 tonnes)", c("Area", "Item", "Value")]
  # annual quantity available
  food_kt <- fb[fb$Element=="Food (1000 tonnes)", c("Area", "Item", "Value")]

  # the annual quantity available, but divided by population
  food_s_kgcapyr <- fb[fb$Element=="Food supply quantity (kg/capita/yr)", c("Area", "Item", "Value")]
  
  ## RESHAPE  
  
  # let's reshape element-datasets separately
  # and then join them back based on country key
  
  
  varying_vars <- list(names(losscropland), 
                       names(lossoilpalmboth),
                       names(lossoilpalmindus),
                       names(losspasture))
  #varying_vars <- names(drivenloss_gaez)[grep(".", names(drivenloss_gaez), fixed = TRUE)]
  
  # reshape to long.
  wide_fb <- stats::reshape(fb,
                            varying = varying_vars,
                            v.names = c("loss_cropland", "loss_oilpalm_both", "loss_oilpalm_indus", "loss_pasture"),
                            sep = ".",
                            timevar = "year",
                            idvar = "grid_id", # don't put "lon" and "lat" in there, otherwise memory issue (see https://r.789695.n4.nabble.com/reshape-makes-R-run-out-of-memory-PR-14121-td955889.html)
                            ids = "grid_id", # lonlat is our cross-sectional identifier.
                            direction = "wide",
                            new.row.names = NULL)#seq(from = 1, to = nrow(ibs_msk_df)*length(years), by = 1)
  rm(wide_df)
  names(long_df)
  unique(fb$Flag.Description)
  
 
}


names(fb)
