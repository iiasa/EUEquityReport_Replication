# Equity considerations - data collection --------------------------------------
# Contact for clarifications: Setu Pelz (TISS, ECE, pelz@iiasa.ac.at)

# LOAD PACKAGES ----------------------------------------------------------------

#install.packages("pacman")
library(pacman)

# processing
p_load(dplyr, tidyr, readr, readxl, writexl, purrr)

# data   
p_load(WDI)

# misc
p_load(here, tidylog, countrycode)

# COUNTRY GROUPINGS ------------------------------------------------------------

# Standard ordering and labeling of regions for visualisation
rgnorder <- tibble(
  r11eu27 = c("EU27", "REU", "CPA", "FSU", "LAM", "MEA", "NAM", "PAS", "PAO", 
              "SAS", "AFR"),
  r11eu27_long = c("EU27", "Rest of Europe", "Centrally Planned Asia", 
                   "Former Soviet Union", "Latin America and The Caribbean", 
                   "Middle East and North Africa", "North America", 
                   "Other Pacific Asia", "Pacific OECD", "South Asia", 
                   "Sub-Saharan Africa")
  )

# Read in ISO3C-R11EU27 grouping
isorgn <- read_csv(here("Data", "countrygrouping", "isorgn.csv")) %>% 
  select(iso3c, r11eu27)

# SET INDICATOR NAMES ----------------------------------------------------------

# Set standard equity consideration indicator names and ordering
indicatornames <- read.csv(here("Data", "equity_data", "indicatormapping.csv"))
  
# RES --------------------------------------------------------------------------

# Historical CO2-FFI emissions 1850-2014 - Global Carbon Budget 2022
# https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2022
emiss_1850co2 <- read_xlsx(here("data", "equity_data", "responsibility",
                               "National_Fossil_Carbon_Emissions_2022v1.0.xlsx"),
                           sheet = "Territorial Emissions", skip = 11) %>% 
  rename("year" = `...1`) %>% 
  pivot_longer(-year, names_to = "country.name", values_to = "CO2") %>% 
  # Data provided in million tonnes of carbon per year. For values in million 
  # tonnes of CO2 per year, we multiply the values by 3.664 (ref: .xlsx file)
  mutate(CO2 = CO2 * 3.664,
         iso3c = countrycode(country.name, origin = "country.name", 
                             destination = "iso3c")) %>% 
  filter(!is.na(iso3c)) %>% 
  full_join(isorgn) %>% 
  filter(year >= 1850, year <= 2014)

# Check which iso3c are missing in GCB emissions data in each year
miss_1850co2 <- emiss_1850co2 %>% 
  group_by(year) %>% 
  filter(is.na(CO2) | is.na(r11eu27)) %>% 
  summarise(missing = paste(iso3c, collapse = ", "))

# After checking missing iso3c above, confirm least-worst option is to remove
# for our regional analysis purposes and aggregate to regional level
emiss_1850co2 <- emiss_1850co2 %>% 
  group_by(r11eu27, year) %>% 
  summarise(CO2 = sum(CO2, na.rm = T)) %>% 
  # Original units at MtCO2, convert to GtCO2
  transmute(r11eu27 = r11eu27, year = year, emiss_1850co2 = CO2 / 1e3)

# Historical Consumption CO2 FFI emissions 1990-2014 - Global Carbon Budget 2022
# https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2022
emiss_1990co2cons <- read_xlsx(here("data", "equity_data", "responsibility",
                                "National_Fossil_Carbon_Emissions_2022v1.0.xlsx"),
                           sheet = "Consumption Emissions", skip = 8) %>% 
  rename("year" = `...1`) %>% 
  pivot_longer(-year, names_to = "country.name", values_to = "CO2") %>% 
  # Data provided in million tonnes of carbon per year. For values in million 
  # tonnes of CO2 per year, we multiply the values by 3.664
  mutate(CO2 = CO2 * 3.664,
         iso3c = countrycode(country.name, origin = "country.name", 
                             destination = "iso3c")) %>% 
  filter(!is.na(iso3c)) %>% 
  full_join(isorgn) %>% 
  filter(year >= 1990, year <= 2014)

# Check which iso3c are missing in GCB consumption emissions data
miss_1990co2cons <- emiss_1990co2cons %>% 
  group_by(year) %>% 
  filter(is.na(CO2) | is.na(r11eu27)) %>% 
  summarise(missing = paste(iso3c, collapse = ", "))

# After checking missing iso3c above, confirm least-worst option is to remove
# for our regional analysis purposes and aggregate to regional level
emiss_1990co2cons <- emiss_1990co2cons %>% 
  group_by(r11eu27, year) %>% 
  summarise(CO2 = sum(CO2, na.rm = T)) %>% 
  # Original units at MtCO2, convert to GtCO2
  transmute(r11eu27 = r11eu27, year = year, emiss_1990co2cons = CO2 / 1e3)

equity_1850co2 <- emiss_1850co2 %>% 
  group_by(r11eu27) %>% 
  summarise(RES_emiss_1850co2_1989 = sum(ifelse(year < 1990, emiss_1850co2, NA), na.rm = T),
            RES_emiss_1850co2_2014 = sum(ifelse(year < 2015, emiss_1850co2, NA), na.rm = T))

equity_1990co2 <- emiss_1850co2 %>% 
  group_by(r11eu27) %>% 
  summarise(RES_emiss_1990co2_2014 = sum(ifelse(year >= 1990 & year < 2015, emiss_1850co2, NA), na.rm = T))

equity_1990co2cons <- emiss_1990co2cons %>% 
  group_by(r11eu27) %>% 
  summarise(RES_emiss_1990co2cons_2014 = sum(ifelse(year >= 1990 & year < 2015, emiss_1990co2cons, NA), na.rm = T))

equity_res <- list(equity_1850co2, equity_1990co2, equity_1990co2cons) %>% 
  reduce(left_join, by = "r11eu27")

# EQL --------------------------------------------------------------------------

# Historical population 1850-2015 (Our World in Data)
# https://ourworldindata.org/population-sources
pop <- read_csv(here("Data", "equity_data", "population", "population.csv")) %>% 
  select(iso3c = Code, year = Year, totalpop = `Population (historical estimates)`) %>% 
  filter(year >= 1850 & year <= 2015) %>% 
  full_join(isorgn) %>% 
  filter(iso3c %in% isorgn$iso3c)

# Check which iso3c are missing historical population data
miss_pop <- pop %>% 
  group_by(year) %>% 
  filter(is.na(totalpop)) %>% 
  summarise(missing = paste(iso3c, collapse = ", "))

# After checking missing iso3c above, confirm least-worst option is to remove
# for our regional analysis purposes and aggregate to regional level
pop <- pop %>%
  filter(!is.na(r11eu27), !is.na(totalpop)) %>% 
  group_by(r11eu27, year) %>% 
  summarise(totalpop = sum(totalpop))

# Save population data for later use
write_csv(pop, here("Data", "equity_data", "population", "popr11eu271850.csv"))

# Prepare population estimates in 1990 and 2015
equity_popyear <- pop %>% 
  group_by(r11eu27) %>% 
  summarise(EQL_capita_1990 = sum(ifelse(year == 1990, totalpop, NA), na.rm = T),
            EQL_capita_2015 = sum(ifelse(year == 2015, totalpop, NA), na.rm = T))

# Prepare cumulative per capita emissions estimates
equity_popcmltv <- pop %>% 
  group_by(r11eu27) %>% 
  # First determine cumulative populations. Variable names reflect subsequent operation.
  summarise(RES_cmltvcapita_1850co2_1989 = sum(ifelse(year <=  1989, totalpop, NA), na.rm = T),
            RES_cmltvcapita_1850co2_2014 = sum(ifelse(year <=  2014, totalpop, NA), na.rm = T),
            RES_cmltvcapita_1990co2_2014 = sum(ifelse(year >= 1990 & year <=  2014, totalpop, NA), na.rm = T),
            RES_cmltvcapita_1990co2cons_2014 = sum(ifelse(year >= 1990 & year <=  2014, totalpop, NA), na.rm = T)) %>%
  left_join(equity_res) %>% 
  # Determine cumulative per capita emissions as per given variable names.
  mutate(RES_cmltvcapita_1850co2_1989 = RES_emiss_1850co2_1989 * 1e9 / RES_cmltvcapita_1850co2_1989,
         RES_cmltvcapita_1850co2_2014 = RES_emiss_1850co2_2014 * 1e9 / RES_cmltvcapita_1850co2_2014,
         RES_cmltvcapita_1990co2_2014 = RES_emiss_1990co2_2014 * 1e9 / RES_cmltvcapita_1990co2_2014,
         RES_cmltvcapita_1990co2cons_2014 = RES_emiss_1990co2cons_2014 * 1e9 / RES_cmltvcapita_1990co2cons_2014
  ) %>% 
  select(r11eu27, matches("RES") & matches("cmltv"))

# Note, forward-looking cumulative population projections are determined in the
# analysis script - EQL_cmltvcapita_19902050 and EQL_cmltvcapita_20152050
equity_eql = left_join(equity_popyear, equity_popcmltv)

# CAP --------------------------------------------------------------------------

# GDP (WDI) --------------------------------------------------------------------

# See https://data.worldbank.org
#
# Set population and GDP indicator names (in $2017 international PPP) from WDI
gdp_indicators <- tibble(
  long = c("gdp2017ppp", "wdipop"), 
  indicator = c("NY.GDP.MKTP.PP.KD", "SP.POP.TOTL"))

# Source data from WDI (requires active internet connection to retrieve original)
# gdp_iso3c <- WDI::WDI(country = isorgn$iso3c, indicator = gdp_indicators$indicator)
# write_csv(gdp_iso3c, here("data", "equity_data", "capability", "gdp_iso3c_wdiextract.csv"))
gdp_iso3c <- read_csv(here("data", "equity_data", "capability", "gdp_iso3c_wdiextract.csv"))

gdp_iso3c <- gdp_iso3c %>% 
  rename(!!!tibble::deframe(gdp_indicators)) %>% 
  filter(year >= 1960, year <= 2014) %>% 
  full_join(isorgn)

# Check which iso3c are missing in WDI data for the years 1990 and 2014
miss_gdp1990 <- gdp_iso3c %>% 
  filter((year %in% c(1990) & (is.na(gdp2017ppp) | is.na(wdipop))) | is.na(country)) %>% 
  arrange(iso3c) %>%
  select(iso3c) %>% 
  mutate(country = countrycode(iso3c, origin = "iso3c", 
                               destination = "country.name")) %>% 
  filter(!is.na(country)) %>% 
  pull(country)

miss_gdp2014 <- gdp_iso3c %>% 
  filter((year %in% c(2014) & (is.na(gdp2017ppp) | is.na(wdipop))) | is.na(country)) %>% 
  arrange(iso3c) %>%
  select(iso3c) %>% 
  mutate(country = countrycode(iso3c, origin = "iso3c", 
                               destination = "country.name")) %>% 
  filter(!is.na(country)) %>% 
  pull(country)

# After checking missing iso3c above, confirm least-worst option is to remove
# these for our regional analysis purposes, and then aggregate to regional level
gdp_r11eu27 <- gdp_iso3c %>% 
  filter(!is.na(wdipop), !is.na(gdp2017ppp), !is.na(country)) %>% 
  # Aggregate population and GDP to regional level
  group_by(r11eu27, year) %>% 
  summarise(
    across(c(wdipop, gdp2017ppp), ~sum(.))) %>% 
  # Calculate regional GDP per capita
  mutate(gdp2017pppcapita = gdp2017ppp / wdipop,
         # Convert GDP to trillions for visualisation
         gdp2017ppp = gdp2017ppp / 1e12) %>% 
  # Set region ordering
  mutate(r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>% 
  ungroup()

# Extract GDP and GDP per capita in 1990 and 2014
equity_gdp <- gdp_r11eu27 %>% 
  filter(year %in% c(1990, 2014)) %>% 
  rename_with(.cols = c(gdp2017ppp, gdp2017pppcapita), ~paste0("CAP_",.)) %>% 
  select(r11eu27, year, matches("CAP")) %>% 
  pivot_wider(names_from = year, names_sep = "_", 
              values_from = c(CAP_gdp2017ppp, CAP_gdp2017pppcapita))

# Remove unnecessary variable(s)
rm(gdp_indicators, gdp_r11eu27)

# Capital stock (Penn World Tables) --------------------------------------------

# See Feenstra, Inklaar, & Timmer (2015). https://doi.org/10.1257/aer.20130954
# www.ggdc.net/pwt
# 
capstock <- 
  read_xlsx(here("data", "equity_data", "capability",
                 "PWT_100capitalstock.xlsx"), sheet = 3) %>%
  # cn = 'Capital stock at current PPPs (in mil. 2017US$)'
  select(iso3c = countrycode, year, capstock2017ppp = cn) %>% 
  filter(year >= 1989)

# Check mislabeled iso3c codes in pwt dataset (none)
misslabel_capstock <- full_join(capstock %>% distinct(iso3c), isorgn) %>%
  filter(is.na(r11eu27))

# Add population and regional country groupings for aggregation
capstock <- capstock %>%
  mutate(year = as.numeric(year)) %>%
  full_join(isorgn %>% select(iso3c, r11eu27)) %>% 
  full_join(gdp_iso3c %>% select(iso3c, country, year, wdipop))

# Check and report which iso3c missing for the year 1990 and 2014 in PWT data
miss_capstock1990 <- capstock %>%
  filter((year == 1990 | is.na(year)) & 
           (is.na(capstock2017ppp) | is.na(wdipop) | is.na(country))) %>%
  arrange(iso3c) %>%
  select(iso3c) %>% 
  mutate(country = countrycode(iso3c, origin = "iso3c", 
                               destination = "country.name")) %>% 
  filter(!is.na(country)) %>% 
  distinct(country) %>% 
  pull(country)

miss_capstock2014 <- capstock %>%
  filter((year == 2014 | is.na(year)) &
           (is.na(capstock2017ppp) | is.na(wdipop) | is.na(country))) %>%
  arrange(iso3c) %>%
  select(iso3c) %>% 
  mutate(country = countrycode(iso3c, origin = "iso3c", 
                               destination = "country.name")) %>% 
  filter(!is.na(country)) %>% 
  distinct(country) %>% 
  pull(country)

# After checking missing iso3c above, confirm least-worst option is to remove
# for our regional analysis purposes and aggregate to regional level
capstock_r11 <- capstock %>%
  filter(!is.na(capstock2017ppp), !is.na(wdipop)) %>%
  # Aggregate to regional level
  group_by(r11eu27, year) %>%
  summarise(capstock2017ppp = sum(capstock2017ppp, na.rm = T),
            wdipop = sum(wdipop)) %>%
  ungroup() %>% 
  mutate(
    # Original data in 1e6, convert to 1e9 (billion $)
    capstock2017ppp = capstock2017ppp / 1e3,
    # Calculate capstock per capita in $PPP2017
    capstock2017pppcapita = capstock2017ppp * 1e9 / wdipop,
    r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27))

# Extract capital stock in 1990 and 2014
equity_capstock <- capstock_r11 %>%
  select(year, r11eu27, capstock2017ppp, capstock2017pppcapita) %>% 
  filter(year %in% c(1990, 2014)) %>%
  arrange(r11eu27) %>%
  ungroup() %>%
  rename_with(.cols = c(capstock2017ppp, capstock2017pppcapita), ~paste0("CAP_",.)) %>% 
  pivot_wider(names_from = year, names_sep = "_", values_from = c(CAP_capstock2017ppp, CAP_capstock2017pppcapita))

rm(misslabel_capstock)

equity_cap <- list(equity_gdp, equity_capstock) %>% 
  reduce(left_join)

# COMBINE EQUITY CONSIDERATION INDICATORS --------------------------------------

equity <- list(equity_res,
               equity_eql,
               equity_cap
               ) %>% 
  reduce(left_join, by = "r11eu27") %>%
  select(-matches("year")) %>% 
  mutate(r11eu27 = factor(r11eu27, rgnorder$r11eu27)) %>% 
  select(r11eu27, c(any_of(indicatornames$indicator)))

write_csv(equity, here::here("data", "equity_data", "indicators_r11eu27.csv"))

# Save missing indicators table
miss_tbl <- tibble(miss_pop = paste(miss_pop, collapse = "; "),
                   miss_gdp1990 = paste(miss_gdp1990, collapse = "; "),
                   miss_gdp2014 = paste(miss_gdp2014, collapse = "; "),
                   miss_capstock1990 = paste(miss_capstock1990, collapse = "; "),
                   miss_capstock2014 = paste(miss_capstock2014, collapse = "; ")
                   ) %>% 
  pivot_longer(everything(), names_to = "indicator", values_to = "countries") %>% 
  mutate(countries = iconv(countries, "UTF-8"))

write_xlsx(
  list(miss_1850co2 = miss_1850co2,
       miss_1990co2cons = miss_1990co2cons,
       miss_others = miss_tbl), 
       here("data", "equity_data", "appendix_missingindicators_r11eu27.xlsx"))
