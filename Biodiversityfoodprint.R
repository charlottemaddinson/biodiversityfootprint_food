# Author: Charlotte Maddinson
# Date: 25.8.2025
# Aim: Calculate location-specific biodiversity impacts of food using a combination of FAOstat data, Resource Trade Earth data
# and BIOVALENT footprint values

# Editing code instructions: 
# The source file for FAOSTAT data and Resource Trade Earth data needs to be changed (lines 26 and 30).
# If using a year other than 2022, the year value must be changed (Year==2022) (line 31)
# The remainder of the code does not change. 

# ------ 1. Downloading data and packages

# packages
library(dplyr)
library(readxl)
library(ggplot2)
library(openxlsx)
library(reshape2)
library(tidyr)
library(stringr)
library(writexl)

# FAOSTAT data. Download from https://www.fao.org/faostat/en/#data/FBS, select desired country.  
# Data must include Production Quantity, Import Quantity, Export Quantity  
# and Food supply quantity (kg/cap/yr). Choose year (here, 2022) and country (here, Brazil). Select all Items *except* population.
FAOSTAT_data<- read_excel("FAOSTAT_data.xls")   

# Resource trade earth data. Download from https://resourcetrade.earth/. 
# Set 'Importer' to desired country, 'Exporter' to all, and 'commodities' to agricultural products. Select year (here, 2022)
RTE_data <- read_excel("RTE_data.xlsx", sheet="Trades")
RTE_data <- RTE_data %>% filter(Year == 2022) 

# BIOVALENT data. No changes needed
BIOVALENT <- BIOVALENT <- read_excel("BIOVALENT.xlsx")

# Matching criteria: FAOSTAT and Resource Trade Earth. No changes needed.
FAO_RTE_matching<- read_excel("FAO_RTE_matching.xlsx")

# Matching criteria: FAOSTAT and BIOVALENT. No changes needed.
biovalent_fao_match_product <- read_excel("FAO_BIOVALENT_matching.xlsx", sheet= "Product")
biovalent_fao_match_location <- read_excel("FAO_BIOVALENT_matching.xlsx", sheet= "Location")

# ---- 2. Using FAOSTAT data to determine food consumption from imports and domestic food

# cleaning data
FAOSTAT_data_clean <- FAOSTAT_data %>%
  pivot_wider(
    id_cols = c(Item, Area),
    names_from = Element,
    values_from = Value,
    values_fn = sum,                 
    values_fill = 0                 
  ) 

# calculating food consumption from imports and domestic food.
FAOSTAT_data_clean <- FAOSTAT_data_clean %>%
  mutate(IDR = `Import quantity` / (Production + `Import quantity` - `Export quantity`)) %>% mutate(Domestic_food = (1-`IDR`)* `Food supply quantity (kg/capita/yr)` ) %>% 
  mutate(imported_food=`IDR`*`Food supply quantity (kg/capita/yr)`)

# ---- 3. Combining FAOSTAT data with Resource Trade Earth Data. 

# 3.a) Preparing FAOSTAT data
# creating a function to clean text, so that FAOSTAT and RTE data match
clean_text <- function(x) {
  x %>%
    str_replace_all("[\u00A0]", " ") %>%  # replace non-breaking space with normal space
    str_squish() %>%                      # collapse multiple spaces
    str_trim()                            # trim leading/trailing spaces
}

# applying function to all of the required sheets to clean
FAOSTAT_data_clean <- FAOSTAT_data_clean %>%
  mutate(Item = clean_text(as.character(Item)))
FAOSTAT_data_clean <- FAOSTAT_data_clean %>%
  mutate(Item = as.character(Item))
FAO_RTE_matching <- FAO_RTE_matching %>%
  mutate(FAOSTAT_name = clean_text(as.character(FAOSTAT_name)))
FAO_RTE_matching <- FAO_RTE_matching %>%
  mutate(FAOSTAT_name = as.character(FAOSTAT_name))

# Left joining FAOSTAT and RTE data matching criteria
# Left join FAOSTAT data with matching criteria, so each product has a corresponding RTE name
FAOSTAT_data_clean <- FAOSTAT_data_clean %>%
  left_join(FAO_RTE_matching, by = c("Item" = "FAOSTAT_name"))

# Separating FAOSTAT imported and domestic data. We only need to apply RTE values to imported data
FAO_imports <- FAOSTAT_data_clean %>% select(Item, Area, RTE_name, `imported_food`)
FAO_domestic<- FAOSTAT_data_clean %>% select(Item, Area, RTE_name, `Domestic_food`)

# For domestic supply quantity, clarify that the Exporter is the same country as consumption and that it is produced domestically.
FAO_domestic <- FAO_domestic %>% mutate(Exporter = Area)
domestic_products <- FAO_domestic %>%
  transmute(
    Item,
    Area,
    RTE_name,
    Exporter,
    Supply = `Domestic_food`,
    imported_or_domestic = "Domestic"
  )

# 3.b) Preparing RTE data
# Filter Resource Trade Earth data so it uses item weight, not financial value.
RTE_data_clean <- RTE_data %>%
  select(Exporter, Resource, `Weight (1000kg)`) 

# For each food product, calculate % of total supply (product weight) coming from each location
RTE_data_clean <- RTE_data_clean %>%
  group_by(Resource, Exporter) %>%
  summarise(Total_Weight = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop")

RTE_data_clean <- RTE_data_clean  %>%
  group_by(Resource) %>%
  mutate(Percentage = (Total_Weight / sum(Total_Weight)) * 100) %>%
  ungroup()

# 3.c) Combining FAOSTAT and RTE data. 
# This is only needed for imported products, not domestic ones.
imported_products <- FAO_imports %>%
  left_join(
    RTE_data_clean, 
    by = c("RTE_name" = "Resource"),
    relationship = "many-to-many"
  )

# Calculating the total weight of product for each location and food item based on percentage of total supply
imported_products <- imported_products %>%
  mutate(totalweight_perlocation = `imported_food` * Percentage*0.01)

# Clarify that imported products are imports
imported_products <- imported_products %>%
  transmute(
    Item,
    Area,
    RTE_name,
    Exporter,
    Supply = totalweight_perlocation,
    imported_or_domestic = "Imported"
  )

# Recombining imported and domestic product tables
finaltable<- bind_rows(domestic_products, imported_products)

# Data should now include total quantity of product consumed (FAOSTAT)
# How much from domestic vs imported sources  (FAOSTAT)
# The original location of imported product (from Resource Trade Earth) 
# And the proportion of product from each location (from Resource Trade Earth) .

# --- 4. Combining FAOSTAT, RTE and BIOVALENT data

# Cleaning matching criteria before matching datasets
finaltable <- finaltable %>%
  mutate(Item = clean_text(as.character(Item))) %>%   mutate(Exporter = clean_text(as.character(Exporter)))
biovalent_fao_match_product <- biovalent_fao_match_product %>%
  mutate(FAOSTAT_product_name = clean_text(as.character(FAOSTAT_product_name))) %>% mutate(BIOVALENT_product_name = clean_text(as.character(BIOVALENT_product_name)))
biovalent_fao_match_location <- biovalent_fao_match_location %>%
  mutate(RTE_location_name = clean_text(as.character(RTE_location_name))) %>% mutate(BIOVALENT_location_name = clean_text(as.character(BIOVALENT_location_name)))
BIOVALENT <- BIOVALENT %>%
  mutate(factor_name = clean_text(as.character(factor_name))) %>%   mutate(food_location = clean_text(as.character(food_location)))

# Joining matching criteria
finaltable <- finaltable %>%
  left_join(biovalent_fao_match_product, by = c("Item" = "FAOSTAT_product_name"))
finaltable <- finaltable %>%
  left_join(biovalent_fao_match_location, by = c("Exporter" = "RTE_location_name"))

# Matching datasets. This table can be used for location analysis if desired

biovalent_table<- finaltable %>%   left_join(BIOVALENT, by = c("BIOVALENT_product_name" = "factor_name", "BIOVALENT_location_name"="food_location")) %>% mutate(totalimpact=Supply*impact_factor)


# Calculating summary values and exporting to Excel. 
biovalent_table_summary <- biovalent_table %>% group_by(Item) %>% summarise(totalimpact=sum(totalimpact,na.rm=TRUE), totalconsumption=sum(`Supply`, na.rm=TRUE))
write_xlsx(biovalent_table_summary,"biovalent_table_summary.xlsx") 

