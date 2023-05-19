# Equity considerations - allocation approaches --------------------------------
# Contact for clarifications: Setu Pelz (TISS, ECE, pelz@iiasa.ac.at)

# LOAD PACKAGES ----------------------------------------------------------------

#install.packages("pacman")
library(pacman)

# processing
p_load(dplyr, tidyr, readr, readxl, writexl, purrr, ggplot2, patchwork, forcats,
       ggrepel)

# data   
p_load(WDI)

# misc
p_load(here, tidylog, countrycode, zoo)

# options
options(scipen = 999)

# COUNTRY GROUPINGS ------------------------------------------------------------

isorgn <- read_csv(here("Data", "countrygrouping", "isorgn.csv")) %>% 
  select(iso3c, r11eu27)

rgnorder <- tibble(
  r11eu27 = c("EU27", "REU", "CPA", "FSU", "LAM", "MEA", "NAM", "PAS", "PAO", 
              "SAS", "AFR"),
  r11eu27_long = c("EU27", "Rest of Europe", "Centrally Planned Asia", 
                   "Former Soviet Union", "Latin America and The Caribbean", 
                   "Middle East and North Africa", "North America", 
                   "Other Pacific Asia", "Pacific OECD", "South Asia", 
                   "Sub-Saharan Africa")
)

# LOAD DATA --------------------------------------------------------------------

# Indicators -------------------------------------------------------------------
indicators <- read_csv(here("data", "equity_data", 
                            "indicators_r11eu27.csv")) 

# Set standard equity consideration indicator names and ordering
indicatornames <- read.csv(here("Data", "equity_data", "indicatormapping.csv"))

# Future population projections 2030-2050 (IIASA SSP2) -------------------------

popssp2 <- read_csv(here("data", "equity_data", "population", 
                         "SspDb_country_data_2013-06-12.csv")) %>%
  filter(MODEL == "IIASA-WiC POP", SCENARIO %in% c("SSP2_v9_130115"), 
         VARIABLE == "Population") %>% 
  select(iso3c = REGION, `2030`, `2040`, `2050`) %>% 
  mutate(across(-c(iso3c), ~ . * 1e6))  %>% 
  pivot_longer(-c(iso3c), names_to = "year", values_to = "totalpop") %>% 
  mutate(year = as.numeric(year)) %>% 
  full_join(isorgn) %>% 
  group_by(r11eu27, year) %>% 
  summarise(totalpop = sum(totalpop, na.rm = T)) %>% 
  filter(!is.na(year))

# Interpolate between 2014 and each SSP data point (2030, 2040, 2050)
pop_projected <- 
  rbind(
    popssp2 %>% select(r11eu27, year, pop = totalpop),
    indicators %>% select(r11eu27, pop = EQL_capita_1990) %>% mutate(year = 1990)
    ) %>% 
  rbind(
    indicators %>% select(r11eu27, pop = EQL_capita_2015) %>% mutate(year = 2015)
    ) %>% 
  group_by(r11eu27) %>% 
  complete(year = 1990:2050) %>% 
  arrange(r11eu27, year) %>% 
  mutate(pop = zoo::na.approx(pop))

# Plot interpolation
pop_projected %>% 
  ggplot(aes(year, pop / 1e9, group = r11eu27)) +
  geom_line(linetype = 2) +
  geom_point(data = . %>% filter(year %in% c(1990, 2015, popssp2$year))) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  facet_wrap(~r11eu27) +
  labs(x = NULL, y = "Population (1e9)",
       title = "SSP2 population projections") +
  theme_bw()

# Determine cumulative population aggregates
pop_1990_2050 <- pop_projected %>% 
  group_by(r11eu27) %>% 
  summarise(cmltvcapita_19902014 = 
              sum(ifelse(year >=  1990 & year <= 2014, pop, NA), na.rm = T),
            cmltvcapita_20152050 = 
              sum(ifelse(year >=  2015 & year <= 2050, pop, NA), na.rm = T))

rm(popssp2, pop_projected)

# Determine aggregate CO2-FFI emissions from 1990 to 2019 and 2015 to 2019 -----
# This is used to compare against allocated budgets from each year of allocation

# Historical CO2 FFI emissions - Global Carbon Budget 2022
# https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2022
emiss_19902020co2 <- read_xlsx(here("data", "equity_data", "responsibility",
                                "National_Fossil_Carbon_Emissions_2022v1.0.xlsx"),
                           sheet = "Territorial Emissions", skip = 11) %>% 
  rename("year" = `...1`) %>% 
  pivot_longer(-year, names_to = "country.name", values_to = "CO2") %>% 
  # Data provided in million tonnes of carbon per year. For values in million 
  # tonnes of CO2 per year, we multiply the values by 3.664
  mutate(CO2 = CO2 * 3.664,
         iso3c = countrycode(country.name, origin = "country.name", 
                             destination = "iso3c")) %>% 
  filter(!is.na(iso3c)) %>% 
  full_join(isorgn) %>% 
  filter(year >= 1990, year <= 2020)

# After checking missing iso3c above, confirm least-worst option is to remove
# for our regional analysis purposes and aggregate to regional level
emiss_19902020co2 <- emiss_19902020co2 %>% 
  group_by(r11eu27, year) %>% 
  summarise(CO2 = sum(CO2, na.rm = T)) %>% 
  # Original units at MtCO2, convert to GtCO2
  transmute(r11eu27 = r11eu27, year = year, emiss_19902020co2 = CO2 / 1e3)

emiss_19902015co2 <- emiss_19902020co2 %>% 
  group_by(r11eu27) %>% 
  summarise(
    emiss_19902019co2 = 
      sum(ifelse(year >= 1990 & year < 2020, emiss_19902020co2, NA), na.rm = T),
    emiss_20152019co2 = 
      sum(ifelse(year >= 2015 & year < 2020, emiss_19902020co2, NA), na.rm = T))

# Set remaining global carbon budget from allocation year to net zero ----------

# From 2020 with a temperature target of 1.5C with a likelihood of 50% from the
# Contribution of Working Group I to the Sixth Assessment Report of the 
# Intergovernmental Panel on Climate Change. Cambridge University Press. 
# https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter_05.pdf
grcb2020_nz = 500

# From 1990 to net zero (addition taken from Friedlingsteine et al (2022). 
# Global Carbon Budget 2021. Earth System Science Data, 14(4), 1917–2005. 
# https://doi.org/10.5194/essd-14-1917-2022)
grcb1990_nz = 1030 + grcb2020_nz

# From 2015 to net zero (addition taken from Friedlingsteine et al (2022). 
# Global Carbon Budget 2021. Earth System Science Data, 14(4), 1917–2005. 
# https://doi.org/10.5194/essd-14-1917-2022)
grcb2015_nz = 204 + grcb2020_nz

# ALLOCATION FUNCTIONS ---------------------------------------------------------

# Set up possible penalty functions to apply
penaltyfunc1 <- function(x) {1 / x^2}
penaltyfunc2 <- function(x) {1 / x}
penaltyfunc3 <- function(x) {(1 / sqrt(x))}
penaltyfunc4 <- function(x) {asinh(x)^(-1)}

# Visualise penalty functions
penaltyfuncplot <- tibble(range = seq(0, 100, 1)) %>%
  mutate(
    "1/x^2" = penaltyfunc1(range),
    "1/x" = penaltyfunc2(range),
    "1/sqrt(x)" = penaltyfunc3(range),
    "asinh(x)^(-1)" = penaltyfunc4(range)
  ) %>%
  pivot_longer(-range, names_to = "pf") %>%
  mutate(pf = forcats::fct_reorder(pf, value)) %>% 
  ggplot(aes(range, value, colour = pf)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = c(0.8,0.7)) +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = "Original value", y = "Transformed value", colour = "Function")

penaltyfuncplot

# Set up penalising function
penalise <- function(.data, penaltyfunc) {
  
  .data %>% 
    mutate(across(matches("RES_") & !(ends_with("1989") | ends_with("1990")), ~ 
                    penaltyfunc(.))) %>% 
    mutate(across(matches("RES_") & (ends_with("1989") | ends_with("1990")), ~ 
                    penaltyfunc(.))) %>% 
    mutate(across(matches("CAP_") & !(ends_with("1989") | ends_with("1990")), ~ 
                    penaltyfunc(.))) %>% 
    mutate(across(matches("CAP_") & (ends_with("1989") | ends_with("1990")), ~ 
                    penaltyfunc(.)))
  
}

# Set up PC allocation function with allocation start year (ONLY 1990 or 2015)
allocationPC <- function(.data, year1990or2015) {
  
  if (year1990or2015 == 1990) {
    
    rcb_pc <- .data %>% 
      transmute(r11eu27 = r11eu27,
                across((matches("RES_") | matches("NED_") | matches("CAP_")) & 
                         (ends_with("1989") | ends_with("1990")), 
                       ~ (. /  sum(. * EQL_capita_1990)) * grcb1990_nz * 1e9),
                EQL_capita_1990 = (1 /  sum(EQL_capita_1990)) * grcb1990_nz * 1e9) %>% 
      pivot_longer(-c(r11eu27), names_to = "indicator", values_to = "rcb_capita_1990") %>% 
      left_join(indicatornames) %>%
      left_join(indicators %>% select(r11eu27, pop1990 = EQL_capita_1990)) %>% 
      select(r11eu27, indicator, indicatorfull, rcb_capita_1990, pop1990) %>% 
      mutate(rcb = rcb_capita_1990 * pop1990 / 1e9,
             indicatorfull = factor(indicatorfull, levels = indicatornames$indicatorfull),
             r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>% 
      arrange(indicatorfull, r11eu27)
    
  }
  
  if (year1990or2015 == 2015) {
    
    rcb_pc <- .data %>% 
      transmute(r11eu27 = r11eu27,
                across((matches("RES_") | matches("NED_") | matches("CAP_")) & 
                         !(ends_with("1989") | ends_with("1990") | matches("cmltvcapita_19902014")), 
                       ~ (. /  sum(. * EQL_capita_2015)) * grcb2015_nz * 1e9),
                EQL_capita_2015 = (1 /  sum(EQL_capita_2015)) * grcb2015_nz * 1e9) %>% 
      pivot_longer(-c(r11eu27), names_to = "indicator", values_to = "rcb_capita_2015") %>% 
      left_join(indicatornames) %>%
      left_join(indicators %>% select(r11eu27, pop2015 = EQL_capita_2015)) %>% 
      select(r11eu27, indicator, indicatorfull, rcb_capita_2015, pop2015) %>% 
      mutate(rcb = rcb_capita_2015 * pop2015 / 1e9,
             indicatorfull = factor(indicatorfull, levels = indicatornames$indicatorfull),
             r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>% 
      arrange(indicatorfull, r11eu27)
    
  }
  
  return(rcb_pc)
  
}

# Set up CPC allocation function with allocation start year (ONLY 1990 or 2015)
allocationCPC <- function(.data, year1990or2015) {
  
  if (year1990or2015 == 1990) {
    
    rcb_cpc <- .data %>% 
      left_join(pop_1990_2050) %>% 
      mutate(cmltvcapita_19902050 = cmltvcapita_19902014 + cmltvcapita_20152050) %>% 
      transmute(r11eu27 = r11eu27,
                across((matches("RES_") | matches("NED_") | matches("CAP_")) & 
                         (ends_with("1989") | ends_with("1990")), 
                       ~ (. /  sum(. * cmltvcapita_19902050)) * grcb1990_nz * 1e9),
                cmltvcapita_19902050 = cmltvcapita_19902050,
                EQL_cmltvcapita_19902050 = (1 /  sum(cmltvcapita_19902050)) * grcb1990_nz * 1e9) %>% 
      pivot_longer(-c(r11eu27, cmltvcapita_19902050), names_to = "indicator", values_to = "rcb_cmltvcapita_19902050") %>% 
      left_join(indicatornames) %>%
      select(r11eu27, indicator, indicatorfull, rcb_cmltvcapita_19902050, cmltvcapita_19902050) %>% 
      mutate(rcb = rcb_cmltvcapita_19902050 * cmltvcapita_19902050 / 1e9,
             indicatorfull = factor(indicatorfull, levels = indicatornames$indicatorfull),
             r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>% 
      arrange(indicatorfull, r11eu27)
    
  }
  
  if (year1990or2015 == 2015) {
    
    rcb_cpc <- .data %>% 
      left_join(pop_1990_2050) %>% 
      transmute(r11eu27 = r11eu27,
                across((matches("RES_") | matches("NED_") | matches("CAP_")) & 
                         !(ends_with("_1989") | ends_with("_1990")), 
                       ~ (. /  sum(. * cmltvcapita_20152050)) * grcb2015_nz * 1e9),
                cmltvcapita_20152050 = cmltvcapita_20152050,
                EQL_cmltvcapita_20152050 = (1 /  sum(cmltvcapita_20152050)) * grcb2015_nz * 1e9) %>% 
      pivot_longer(-c(r11eu27, cmltvcapita_20152050), names_to = "indicator", values_to = "rcb_cmltvcapita_20152050") %>% 
      left_join(indicatornames) %>%
      select(r11eu27, indicator, indicatorfull, rcb_cmltvcapita_20152050, cmltvcapita_20152050) %>% 
      mutate(rcb = rcb_cmltvcapita_20152050 * cmltvcapita_20152050 / 1e9,
             indicatorfull = factor(indicatorfull, levels = indicatornames$indicatorfull),
             r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>% 
      arrange(indicatorfull, r11eu27)
    
  }
  
  return(rcb_cpc)
  
}
            
# VISUALISE ALLOCATIONS --------------------------------------------------------

# Function to compare effects of different penalty functions
pfcompareCPC <- function(.data, penaltyfunc, year1990or2015) {
  
  .data %>% 
    penalise(., penaltyfunc) %>% 
    allocationCPC(., year1990or2015)

}

pfcomparePC <- function(.data, penaltyfunc, year1990or2015) {
  
  .data %>% 
    penalise(., penaltyfunc) %>% 
    allocationPC(., year1990or2015)
  
}

# Visualise RCB_CPC allocations as share of RCB_ECPC relative to indicator ratio
# for each penalty function

indicators %>% 
  pfcompareCPC(., penaltyfunc1, 2015) %>% 
  mutate(pf = "1/x^2") %>% 
  rbind(
    indicators %>% 
      pfcompareCPC(., penaltyfunc2, 2015) %>% 
      mutate(pf = "1/x")
  ) %>% 
  rbind(
    indicators %>% 
      pfcompareCPC(., penaltyfunc3, 2015) %>% 
      mutate(pf = "1/sqrt(x)")
  ) %>% 
  rbind(
    indicators %>% 
      pfcompareCPC(., penaltyfunc4, 2015) %>% 
      mutate(pf = "asinh(x)^(-1)")
  ) %>% 
  group_by(pf) %>% 
  mutate(rcb_ecpc = sum(rcb * 1e9 / sum(cmltvcapita_20152050)),
         rcb_ratio = rcb_cmltvcapita_20152050 / rcb_ecpc) %>% 
  left_join(
    indicators %>% 
      transmute(r11eu27 = r11eu27,
                EQL_capita_2015 = EQL_capita_2015,
                across((matches("RES_") | matches("NED_") | matches("CAP_")) & 
                         !(ends_with("1989") | ends_with("1990")), 
                       ~ (. /  weighted.mean(., w = EQL_capita_2015)))) %>% 
      pivot_longer(-c(r11eu27, EQL_capita_2015), names_to = "indicator", 
                   values_to = "indicator_ratio") %>% 
      select(r11eu27, indicator, indicator_ratio)) %>% 
  mutate(pf = factor(pf, levels = levels(penaltyfuncplot$data$pf))) %>% 
  filter(indicator %in% c("RES_emiss_1850co2_2014", "RES_cmltvcapita_1850co2_2014",
                          "CAP_gdp2017pppcapita_2014", "CAP_capstock2017pppcapita_2014"), 
         !grepl(indicator, pattern = "NED")) %>% 
  ggplot(aes(indicator_ratio, rcb_ratio, colour = pf, group = pf)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_line() +
  geom_point(data = . %>% filter(r11eu27 == "EU27"), colour = "darkblue", size = 3) +
  geom_point() +
  geom_text_repel(aes(label = r11eu27),
            data = . %>% filter(r11eu27 %in% c("NAM", "SAS", "AFR"),
                                pf == "1/x^2"),
            colour = "grey", size = 3) +
  facet_wrap(~indicatorfull) +
  theme_bw() +
  theme(legend.position = "top") +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = "Ratio of original indicator to weighted global average",
       y = "Ratio of allocated PC budget to global EPC budget",
       colour = NULL)

ggsave(here("Figures", "penaltyfunctioncomp_ratios.png"), height = 4.5, width = 4.5)

# Visualise RCB_PC allocations as share of RCB_ECPC relative to penalised indicator ratio
# for each penalty function

indicators %>% 
      pfcomparePC(., penaltyfunc2, 2015) %>% 
      mutate(pf = "1/x") %>% 
  group_by(pf) %>% 
  mutate(rcb_epc = sum(rcb * 1e9 / sum(pop2015)),
         rcb_ratio = rcb_capita_2015 / rcb_epc) %>% 
  left_join(
    indicators %>% 
      transmute(r11eu27 = r11eu27,
                EQL_capita_2015 = EQL_capita_2015,
                across((matches("RES_") | matches("NED_") | matches("CAP_")) & 
                         !(ends_with("1989") | ends_with("1990")), 
                       ~ ((1/.) /  weighted.mean((1/.), w = EQL_capita_2015)))) %>% 
      pivot_longer(-c(r11eu27, EQL_capita_2015), names_to = "indicator", 
                   values_to = "indicator_ratio") %>% 
      select(r11eu27, indicator, indicator_ratio)) %>% 
  mutate(pf = factor(pf, levels = levels(penaltyfuncplot$data$pf))) %>% 
  filter(indicator %in% c("RES_emiss_1850co2_2014"), 
         !grepl(indicator, pattern = "NED")) %>% 
  ggplot(aes(indicator_ratio, rcb_ratio)) +
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1), 
               arrow = arrow(length = unit(3, "mm")), colour = "darkblue") +
  geom_segment(aes(x = 1, y = 1, xend = 0, yend = 1), 
               arrow = arrow(length = unit(3, "mm")), colour = "darkblue") +
  geom_line() +
  geom_point() +
  geom_point(data = . %>% filter(r11eu27 == "EU27"), colour = "black", size = 2) +
  geom_text_repel(aes(label = r11eu27),
                  data = . %>% filter(pf == "1/x"),
                  colour = "darkgrey", size = 3) +
  facet_wrap(~indicatorfull, ncol = 1) +
  theme_bw() +
  theme(legend.position = "top") +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = "Ratio of transformed indicator to weighted global average",
       y = "Rato of regional PC budget to global EPC budget",
       colour = NULL)

ggsave(here("Figures", "penaltyfunctioncomp_ratios_transformed.png"), height = 4.5, width = 4.5)

# Apply each allocation approach -----------------------------------------------

rcb_1990pf2 <- indicators %>% 
  penalise(., penaltyfunc2) %>% 
  allocationPC(., 1990) %>% 
  transmute(r11eu27 = r11eu27, indicator = indicator, rcb = rcb, pop = pop1990,
            allocation = "Allocated per capita in 1990",
            pf = "1/x") %>% 
  group_by(indicator) %>% 
  mutate(equalcapita = sum(rcb * 1e9) / sum(pop))

rcb_1990pf3 <- indicators %>% 
  penalise(., penaltyfunc3) %>% 
  allocationPC(., 1990) %>% 
  transmute(r11eu27 = r11eu27, indicator = indicator, rcb = rcb, pop = pop1990,
            allocation = "Allocated per capita in 1990",
            pf = "1/sqrt(x)") %>% 
  group_by(indicator) %>% 
  mutate(equalcapita = sum(rcb * 1e9) / sum(pop))

rcb_2015pf2 <- indicators %>% 
  penalise(., penaltyfunc2) %>% 
  allocationPC(., 2015) %>% 
  transmute(r11eu27 = r11eu27, indicator = indicator, rcb = rcb, pop = pop2015,
            allocation = "Allocated per capita in 2015",
            pf = "1/x") %>% 
  group_by(indicator) %>% 
  mutate(equalcapita = sum(rcb * 1e9) / sum(pop))

rcb_2015pf3 <- indicators %>% 
  penalise(., penaltyfunc3) %>% 
  allocationPC(., 2015) %>% 
  transmute(r11eu27 = r11eu27, indicator = indicator, rcb = rcb, pop = pop2015,
            allocation = "Allocated per capita in 2015",
            pf = "1/sqrt(x)") %>% 
  group_by(indicator) %>% 
  mutate(equalcapita = sum(rcb * 1e9) / sum(pop))

rcb_1990cmltvpf2 <- indicators %>% 
  penalise(., penaltyfunc2) %>% 
  allocationCPC(., 1990) %>% 
  transmute(r11eu27 = r11eu27, indicator = indicator, rcb = rcb, pop = cmltvcapita_19902050,
            allocation = "Allocated cumulative per capita 1990-2050",
            pf = "1/x") %>% 
  group_by(indicator) %>% 
  mutate(equalcapita = sum(rcb * 1e9) / sum(pop))

rcb_1990cmltvpf3 <- indicators %>% 
  penalise(., penaltyfunc3) %>% 
  allocationCPC(., 1990) %>% 
  transmute(r11eu27 = r11eu27, indicator = indicator, rcb = rcb, pop = cmltvcapita_19902050,
            allocation = "Allocated cumulative per capita 1990-2050",
            pf = "1/sqrt(x)") %>% 
  group_by(indicator) %>% 
  mutate(equalcapita = sum(rcb * 1e9) / sum(pop))

rcb_2015cmltvpf2 <- indicators %>% 
  penalise(., penaltyfunc2) %>% 
  allocationCPC(., 2015) %>% 
  transmute(r11eu27 = r11eu27, indicator = indicator, rcb = rcb, pop = cmltvcapita_20152050,
            allocation = "Allocated cumulative per capita 2015-2050",
            pf = "1/x") %>% 
  group_by(indicator) %>% 
  mutate(equalcapita = sum(rcb * 1e9) / sum(pop))

rcb_2015cmltvpf3 <- indicators %>% 
  penalise(., penaltyfunc3) %>% 
  allocationCPC(., 2015) %>% 
  transmute(r11eu27 = r11eu27, indicator = indicator, rcb = rcb, pop = cmltvcapita_20152050,
            allocation = "Allocated cumulative per capita 2015-2050",
            pf = "1/sqrt(x)") %>% 
  group_by(indicator) %>% 
  mutate(equalcapita = sum(rcb * 1e9) / sum(pop))

# Visualise EU 27 RCB under each allocation approach ---------------------------

a <- list(rcb_1990pf2, rcb_1990pf3, rcb_1990cmltvpf2, rcb_1990cmltvpf3) %>% 
  reduce(rbind) %>% 
  left_join(indicatornames) %>% 
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_19902019co2)) %>% 
  filter(!is.na(principle), r11eu27 == "EU27") %>% 
  mutate(indicator = factor(indicator, levels = indicatornames$indicator, 
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle, 
                            labels = indicatornames$principle), ) %>% 
  ggplot(aes(fct_rev(indicator), rcb, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  geom_hline(aes(yintercept = emiss_19902019co2), linetype = 2) +
  geom_text(aes(x = "ECPC 1990-2050", y = emiss_19902019co2, 
                label = "Known CO2-FFI 1990-2019"), size = 2,
            angle = -90, vjust = -1, hjust = 0, 
            data = . %>% filter(indicator == "CO2 1850-1989")) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(r11eu27~fct_rev(allocation)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Allocated remaining carbon budget (GtCO2)", x = NULL, 
       fill = NULL)

b <- list(rcb_1990pf2, rcb_1990pf3, rcb_1990cmltvpf2, rcb_1990cmltvpf3) %>%
  reduce(rbind) %>%
  left_join(indicatornames) %>%
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_19902019co2)) %>%
  filter(!is.na(principle), r11eu27 == "EU27") %>%
  mutate(indicator = factor(indicator, levels = indicatornames$indicator, 
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle, 
                            labels = indicatornames$principle)) %>%
  ggplot(aes(fct_rev(indicator), rcb * 1e9 / pop / equalcapita, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(r11eu27~fct_rev(allocation), scales = "free_x") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Ratio of regional to global per capita budgets", x = NULL, 
       fill = NULL)

wrap_plots(b,a, ncol = 1) + plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(here("Figures", "PC_CPC_1990_EU27.png"), height = 6, width = 8)
write.csv(b$data, here("Figures", "PC_CPC_1990_EU27_top.csv"))
write.csv(a$data, here("Figures", "PC_CPC_1990_EU27_bot.csv"))

a <- list(rcb_2015pf2, rcb_2015pf3,
     rcb_2015cmltvpf2, rcb_2015cmltvpf3) %>% 
  reduce(rbind) %>% 
  left_join(indicatornames) %>% 
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_20152019co2)) %>% 
  filter(!is.na(principle), r11eu27 == "EU27") %>% 
  mutate(indicator = factor(indicator, levels = indicatornames$indicator, 
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle, 
                            labels = indicatornames$principle), ) %>% 
  ggplot(aes(fct_rev(indicator), rcb, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  geom_hline(aes(yintercept = emiss_20152019co2), linetype = 2) +
  geom_text(aes(x = "ECPC 2015-2050", y = emiss_20152019co2, 
                label = "Known CO2-FFI 2015-2019"), size = 2,
            angle = -90, vjust = -1, hjust = 0, 
            data = . %>% filter(indicator == "CO2 1850-2014")) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(r11eu27~fct_rev(allocation)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Allocated remaining carbon budget (GtCO2)", x = NULL, 
       fill = NULL)

b <- list(rcb_2015pf2, rcb_2015pf3,
     rcb_2015cmltvpf2, rcb_2015cmltvpf3) %>% 
  reduce(rbind) %>% 
  left_join(indicatornames) %>% 
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_20152019co2)) %>% 
  filter(!is.na(principle), r11eu27 == "EU27") %>% 
  mutate(indicator = factor(indicator, levels = indicatornames$indicator, 
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle, 
                            labels = indicatornames$principle), ) %>% 
  ggplot(aes(fct_rev(indicator), rcb * 1e9 / pop / equalcapita, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(r11eu27~fct_rev(allocation), scales = "free_x") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Ratio of regional to global per capita budgets", x = NULL, 
       fill = NULL)

wrap_plots(b,a, ncol = 1) + plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(here("Figures", "PC_CPC_2015_EU27.png"), height = 6, width = 8)
write.csv(b$data, here("Figures", "PC_CPC_2015_EU27_top.csv"))
write.csv(a$data, here("Figures", "PC_CPC_2015_EU27_bot.csv"))

# Compare EU27 net zero pathways with 'fair' allocations from 1990 -------------

# Prior to net zero pathways being available, we assume a linear reduction in
# total emissions from 2020 to global net zero in 2050

emiss_19902050co2_eu27 <- emiss_19902020co2 %>% 
  select(r11eu27, year, emiss_co2 = emiss_19902020co2) %>% 
  filter(r11eu27 == "EU27") %>% 
  group_by(r11eu27) %>% 
  complete(year = 1990:2050) %>% 
  mutate(
    emiss_co2_nz2050 = ifelse(year >= 2050, 0, emiss_co2),
    emiss_co2_nz2030_70pc1990 = case_when(
      year >= 2050 ~ 0, 
      year == 2030 ~ emiss_19902020co2[emiss_19902020co2$r11eu27 == "EU27" & 
                                         emiss_19902020co2$year == 1990,]$emiss_19902020co2 * 0.3,
      TRUE ~ emiss_co2),
    emiss_co2_nz2040 = ifelse(year >= 2040, 0, emiss_co2)) %>% 
  mutate(
    across(matches("emiss_co2_"), ~zoo::na.approx(.), .names = "{.col}_fill"),
    across(matches("_fill"), ~cumsum(.), .names = "{.col}_cmltv")) %>% 
  select(r11eu27, year, matches("_fill"), matches("fill_cmltv"))

emiss_19902050co2_eu27 %>% 
  ggplot(aes(emiss_co2_nz2050_fill_cmltv, emiss_co2_nz2050_fill)) +
  geom_line(linetype = 2, size = 0.1) +
  geom_point(size = 1) +
  geom_text(aes(label = year), angle = 45, hjust = -0.1, vjust = 0.5,
            colour = "grey", size = 3,
            data = . %>% 
              filter(year %in% seq(1990,2050,5))) +
  geom_vline(aes(xintercept = rcb, linetype = pf),
             data = list(rcb_1990pf2, rcb_1990pf3) %>% 
               reduce(rbind) %>% 
               filter(r11eu27 == "EU27") %>% 
               left_join(indicatornames) %>% 
               mutate(principle = 
                        factor(principle, levels = unique(indicatornames$principle))) %>% 
               filter(!is.na(principle))) +
  geom_text(aes(x = rcb, y = 0, label = indicatorfull), angle = 90, hjust = 0, 
            vjust = 1, size = 3,
             data = list(rcb_1990pf2, rcb_1990pf3) %>%
              reduce(rbind) %>% 
              filter(r11eu27 == "EU27") %>% 
              left_join(indicatornames) %>% 
              group_by(principle) %>% 
              filter(rcb == min(rcb) | rcb == max(rcb))%>% 
              mutate(principle = 
                       factor(principle, levels = unique(indicatornames$principle)))%>% 
              filter(!is.na(principle))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0,140)) +
  facet_wrap(~principle) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(subtitle = "Assuming linear reduction to NZ-2050, with approximate 55% reduction from 1990 levels in 2030",
       x = "Cumulative emissions since first year of allocation (Gt CO2-FFI)",
       y = "Annual emissions (Gt CO2-FFI)",
       colour = NULL, linetype = "Transformation")

ggsave(here("Figures", "emiss_19902050_alloc_PC_nz2050.png"), height = 4, width = 10)

emiss_19902050co2_eu27 %>% 
  ggplot(aes(emiss_co2_nz2040_fill_cmltv, emiss_co2_nz2040_fill)) +
  geom_line(linetype = 2, size = 0.1) +
  geom_point(size = 1) +
  geom_text(aes(label = year), angle = 45, hjust = -0.1, vjust = 0.5,
            colour = "grey", size = 3,
            data = . %>% 
              filter(year %in% seq(1990,2040,5))) +
  geom_vline(aes(xintercept = rcb, linetype = pf),
             data = list(rcb_1990pf2, rcb_1990pf3) %>% 
               reduce(rbind) %>% 
               filter(r11eu27 == "EU27") %>% 
               left_join(indicatornames) %>% 
               mutate(principle = factor(principle, 
                                         levels = unique(indicatornames$principle))) %>% 
               filter(!is.na(principle))) +
  geom_text(aes(x = rcb, y = 0, label = indicatorfull), angle = 90, hjust = 0, 
            vjust = 1, size = 3,
            data = list(rcb_1990pf2, rcb_1990pf3) %>%
              reduce(rbind) %>% 
              filter(r11eu27 == "EU27") %>% 
              left_join(indicatornames) %>% 
              group_by(principle) %>% 
              filter(rcb == min(rcb) | rcb == max(rcb))%>% 
              mutate(principle = factor(principle, 
                                        levels = unique(indicatornames$principle)))%>% 
              filter(!is.na(principle))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits = c(0,140)) +
  facet_wrap(~principle) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(subtitle = "Assuming linear reduction to NZ-2040, with approximate 65% reduction from 1990 levels in 2030",
       x = "Cumulative emissions since first year of allocation (Gt CO2-FFI)",
       y = "Annual emissions (Gt CO2-FFI)",
       colour = NULL, linetype = "Transformation")

ggsave(here("Figures", "emiss_19902050_alloc_PC_nz2040.png"), height = 4, width = 10)

# Compare EU27 net zero pathways with 'fair' allocations from 2015 -------------

# Prior to net zero pathways being available, we assume a linear reduction in
# total emissions from 2020 to global net zero in 2050

emiss_20152050co2_eu27 <- emiss_19902020co2 %>% 
  select(r11eu27, year, emiss_co2 = emiss_19902020co2) %>% 
  filter(r11eu27 == "EU27") %>% 
  group_by(r11eu27) %>% 
  complete(year = 1990:2050) %>%  
  mutate(
    emiss_co2_nz2050 = ifelse(year >= 2050, 0, emiss_co2),
    emiss_co2_nz2030_70pc1990 = case_when(
      year >= 2050 ~ 0, 
      year == 2030 ~ emiss_19902020co2[emiss_19902020co2$r11eu27 == "EU27" & 
                                         emiss_19902020co2$year == 1990,]$emiss_19902020co2 * 0.3,
      TRUE ~ emiss_co2),
    emiss_co2_nz2040 = ifelse(year >= 2040, 0, emiss_co2)) %>% 
  filter(year >= 2015) %>% 
  mutate(
    across(matches("emiss_co2_"), ~zoo::na.approx(.), .names = "{.col}_fill"),
    across(matches("_fill"), ~cumsum(.), .names = "{.col}_cmltv")) %>% 
  select(r11eu27, year, matches("_fill"), matches("fill_cmltv"))

emiss_20152050co2_eu27 %>% 
  ggplot(aes(emiss_co2_nz2050_fill_cmltv, emiss_co2_nz2050_fill)) +
  geom_line(linetype = 2, size = 0.1) +
  geom_point(size = 1) +
  geom_text(aes(label = year), angle = 45, hjust = -0.1, vjust = 0.5,
            colour = "grey", size = 3,
            data = . %>% 
              filter(year %in% seq(1990,2050,5))) +
  geom_vline(aes(xintercept = rcb, linetype = pf),
             data = list(rcb_2015pf2, rcb_2015pf3) %>% 
               reduce(rbind) %>% 
               filter(r11eu27 == "EU27") %>% 
               left_join(indicatornames) %>% 
               mutate(principle = factor(principle, 
                                         levels = unique(indicatornames$principle))) %>% 
               filter(!is.na(principle))) +
  geom_text(aes(x = rcb, y = 0, label = indicatorfull), angle = 90, hjust = 0, 
            vjust = 1, size = 3,
            data = list(rcb_2015pf2, rcb_2015pf3) %>%
              reduce(rbind) %>% 
              filter(r11eu27 == "EU27") %>% 
              left_join(indicatornames) %>% 
              group_by(principle) %>% 
              filter(rcb == min(rcb) | rcb == max(rcb))%>% 
              mutate(principle = factor(principle, 
                                        levels = unique(indicatornames$principle)))%>% 
              filter(!is.na(principle))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0,55)) +
  facet_wrap(~principle) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(subtitle = "Assuming linear reduction to NZ-2050, with approximate 55% reduction from 1990 levels in 2030",
       x = "Cumulative emissions since first year of allocation (Gt CO2-FFI)",
       y = "Annual emissions (Gt CO2-FFI)",
       colour = NULL, linetype = "Transformation")

ggsave(here("Figures", "emiss_20152050_alloc_PC_nz2050.png"), height = 4, width = 10)

emiss_20152050co2_eu27 %>% 
  ggplot(aes(emiss_co2_nz2040_fill_cmltv, emiss_co2_nz2040_fill)) +
  geom_line(linetype = 2, size = 0.1) +
  geom_point(size = 1) +
  geom_text(aes(label = year), angle = 45, hjust = -0.1, vjust = 0.5,
            colour = "grey", size = 3,
            data = . %>% 
              filter(year %in% seq(1990,2040,5))) +
  geom_vline(aes(xintercept = rcb, linetype = pf),
             data = list(rcb_2015pf2, rcb_2015pf3) %>% 
               reduce(rbind) %>% 
               filter(r11eu27 == "EU27") %>% 
               left_join(indicatornames) %>% 
               mutate(principle = factor(principle, 
                                         levels = unique(indicatornames$principle))) %>% 
               filter(!is.na(principle))) +
  geom_text(aes(x = rcb, y = 0, label = indicatorfull), angle = 90, hjust = 0, 
            vjust = 1, size = 3,
            data = list(rcb_2015pf2, rcb_2015pf3) %>%
              reduce(rbind) %>% 
              filter(r11eu27 == "EU27") %>% 
              left_join(indicatornames) %>% 
              group_by(principle) %>% 
              filter(rcb == min(rcb) | rcb == max(rcb))%>% 
              mutate(principle = factor(principle, 
                                        levels = unique(indicatornames$principle)))%>% 
              filter(!is.na(principle))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits = c(0,55)) +
  facet_wrap(~principle) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(subtitle = "Assuming linear reduction to NZ-2040, with approximate 65% reduction from 1990 levels in 2030",
       x = "Cumulative emissions since first year of allocation (Gt CO2-FFI)",
       y = "Annual emissions (Gt CO2-FFI)",
       colour = NULL, linetype = "Transformation")

ggsave(here("Figures", "emiss_20152050_alloc_PC_nz2040.png"), height = 4, width = 10)

# Comparing allocations across all regions in 1990 -----------------------------

regionrcb1990_pc_sum <- list(rcb_1990pf2, rcb_1990pf3) %>%
  reduce(rbind) %>%
  left_join(indicatornames) %>%
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_19902019co2)) %>% 
  mutate(indicator = factor(indicator, levels = indicatornames$indicator,
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle,
                            labels = indicatornames$principle), 
         r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>%
  ggplot(aes(r11eu27, rcb, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  geom_point(aes(y = emiss_19902019co2), shape = 3, show.legend = F) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(~indicator, ncol = 5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Allocated remaining carbon budget (GtCO2)", x = NULL,
       fill = NULL)

regionrcb1990_pc_ratio <- list(rcb_1990pf2, rcb_1990pf3) %>%
  reduce(rbind) %>%
  left_join(indicatornames) %>%
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_19902019co2)) %>% 
  mutate(indicator = factor(indicator, levels = indicatornames$indicator,
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle,
                            labels = indicatornames$principle), 
         r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>%
  ggplot(aes(r11eu27, rcb * 1e9 / pop / equalcapita, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0,3)) +
  facet_wrap(~indicator, ncol = 5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Ratio of allocated regional PC budgets to global EPC budget", x = NULL,
       fill = NULL)

wrap_plots(regionrcb1990_pc_ratio, regionrcb1990_pc_sum, ncol = 1) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(here("Figures", "regionrcb1990_pc.png"), height = 10, width = 12)
write.csv(regionrcb1990_pc_ratio$data, here("Figures", "regionrcb1990_pc_top.csv"))
write.csv(regionrcb1990_pc_sum$data, here("Figures", "regionrcb1990_pc_bot.csv"))

regionrcb1990_cpc_sum <- list(rcb_1990cmltvpf2, rcb_1990cmltvpf3) %>%
  reduce(rbind) %>%
  left_join(indicatornames) %>%
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_19902019co2)) %>% 
  mutate(indicator = factor(indicator, levels = indicatornames$indicator,
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle,
                            labels = indicatornames$principle), 
         r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>%
  ggplot(aes(r11eu27, rcb, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  geom_point(aes(y = emiss_19902019co2), shape = 3, show.legend = F) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(~indicator, ncol = 5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Allocated remaining carbon budget (GtCO2)", x = NULL,
       fill = NULL)

regionrcb1990_cpc_ratio <- list(rcb_1990cmltvpf2, rcb_1990cmltvpf3) %>%
  reduce(rbind) %>%
  left_join(indicatornames) %>%
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_19902019co2)) %>% 
  mutate(indicator = factor(indicator, levels = indicatornames$indicator,
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle,
                            labels = indicatornames$principle), 
         r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>%
  ggplot(aes(r11eu27, rcb * 1e9 / pop / equalcapita, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0,3)) +
  facet_wrap(~indicator, ncol = 5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Ratio of allocated regional PC budgets to global ECPC budget", x = NULL,
       fill = NULL)

wrap_plots(regionrcb1990_cpc_ratio, regionrcb1990_cpc_sum, ncol = 1) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "top") 

ggsave(here("Figures", "regionrcb1990_cpc.png"), height = 10, width = 12)

# Comparing allocations across all regions in 2015 -----------------------------

regionrcb2015_pc_sum <- list(rcb_2015pf2, rcb_2015pf3) %>%
  reduce(rbind) %>%
  left_join(indicatornames) %>%
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_20152019co2)) %>% 
  filter(!is.na(principle)) %>%
  mutate(indicator = factor(indicator, levels = indicatornames$indicator,
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle,
                            labels = indicatornames$principle), 
         r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>%
  ggplot(aes(r11eu27, rcb, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  geom_point(aes(y = emiss_20152019co2), shape = 3, show.legend = F) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(~indicator, ncol = 5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Allocated remaining carbon budget (GtCO2)", x = NULL,
       fill = NULL)

regionrcb2015_pc_ratio <- list(rcb_2015pf2, rcb_2015pf3) %>%
  reduce(rbind) %>%
  left_join(indicatornames) %>%
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_20152019co2)) %>% 
  filter(!is.na(principle)) %>%
  mutate(indicator = factor(indicator, levels = indicatornames$indicator,
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle,
                            labels = indicatornames$principle), 
         r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>%
  ggplot(aes(r11eu27, rcb * 1e9 / pop / equalcapita, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0,3)) +
  facet_wrap(~indicator, ncol = 5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Ratio of allocated regional PC budgets to global EPC budget", x = NULL,
       fill = NULL)

wrap_plots(regionrcb2015_pc_ratio, regionrcb2015_pc_sum, ncol = 1) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(here("Figures", "regionrcb2015_pc.png"), height = 15, width = 12)
write.csv(regionrcb2015_pc_ratio$data, here("Figures", "regionrcb2015_pc_top.csv"))
write.csv(regionrcb2015_pc_sum$data, here("Figures", "regionrcb2015_pc_bot.csv"))

regionrcb2015_cpc_sum <- list(rcb_2015cmltvpf2, rcb_2015cmltvpf3) %>%
  reduce(rbind) %>%
  left_join(indicatornames) %>%
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_20152019co2)) %>% 
  filter(!is.na(principle)) %>%
  mutate(indicator = factor(indicator, levels = indicatornames$indicator,
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle,
                            labels = indicatornames$principle), 
         r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>%
  ggplot(aes(r11eu27, rcb, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  geom_point(aes(y = emiss_20152019co2), shape = 3, show.legend = F) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(~indicator, ncol = 5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Allocated remaining carbon budget (GtCO2)", x = NULL,
       fill = NULL)

regionrcb2015_cpc_ratio <- list(rcb_2015cmltvpf2, rcb_2015cmltvpf3) %>%
  reduce(rbind) %>%
  left_join(indicatornames) %>%
  left_join(emiss_19902015co2 %>% select(r11eu27, emiss_20152019co2)) %>% 
  filter(!is.na(principle)) %>%
  mutate(indicator = factor(indicator, levels = indicatornames$indicator,
                            labels = indicatornames$indicatorfull),
         principle = factor(principle, levels = indicatornames$principle,
                            labels = indicatornames$principle), 
         r11eu27 = factor(r11eu27, levels = rgnorder$r11eu27)) %>%
  ggplot(aes(r11eu27, rcb * 1e9 / pop / equalcapita, fill = principle)) +
  geom_col(width = 0.2, data = . %>% filter(pf != "1/x"), alpha = 1) +
  geom_col(width = 0.6, data = . %>% filter(pf == "1/x"), alpha = 0.6) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0,3)) +
  facet_wrap(~indicator, ncol = 5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Ratio of allocated regional PC budgets to global EPC budget", x = NULL,
       fill = NULL)

wrap_plots(regionrcb2015_cpc_ratio, regionrcb2015_cpc_sum, ncol = 1) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(here("Figures", "regionrcb2015_cpc.png"), height = 15, width = 12)
