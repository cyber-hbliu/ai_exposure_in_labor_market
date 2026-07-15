library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(scales)
library(ggrepel)
library(ggalt)

options(tigris_use_cache = TRUE)
setwd("/Users/cyberhbliu/Desktop/PERSONAL/2026portfolio/ai_impact")
dir.create("outputs", showWarnings = FALSE)

naics3_names <- tibble::tribble(
  ~naics3, ~industry_name,
  "111", "Crop Production", "112", "Animal Production & Aquaculture", "113", "Forestry & Logging", "114", "Fishing, Hunting & Trapping", "115", "Support Activities for Agriculture",
  "211", "Oil & Gas Extraction", "212", "Mining (Coal, Metal, Mineral)", "213", "Support Activities for Mining",
  "221", "Utilities (Electric, Gas, Water)", "22S", "Utilities (Not Specified)",
  "23",  "Construction",
  "311", "Food Manufacturing", "312", "Beverage & Tobacco Mfg", "313", "Textile Mills", "314", "Textile Product Mills", "315", "Apparel Manufacturing (Cut & Sew)", "316", "Leather & Allied Product Mfg", "31M", "Knitting Mills & Apparel Knitting",
  "321", "Wood Product Manufacturing", "322", "Paper Manufacturing", "323", "Printing & Related Support", "324", "Petroleum & Coal Products Mfg", "325", "Chemical Mfg (Pharma & Basic)", "326", "Plastics & Rubber Products", "327", "Nonmetallic Mineral Products (Glass/Concrete)",
  "331", "Primary Metal Manufacturing", "332", "Fabricated Metal Product Mfg", "333", "Machinery Manufacturing", "334", "Computer & Electronic Product Mfg", "335", "Electrical Equipment & Appliance Mfg", "336", "Transportation Equipment (Autos/Aerospace)", "337", "Furniture & Related Product Mfg", "339", "Medical Equipment & Misc Manufacturing", "33M", "Fabricated Metal & Machinery (Aggregated)", "3MS", "Manufacturing (Not Specified)",
  "423", "Wholesale: Durable Goods", "424", "Wholesale: Nondurable Goods (Drugs/Food)", "425", "Wholesale: Agents & Brokers", "42S", "Wholesale Trade (Not Specified)",
  "441", "Retail: Motor Vehicle & Parts", "444", "Retail: Building Material & Garden", "445", "Retail: Food & Beverage Stores", "449", "Retail: Furniture, Electronics & Appliances", "455", "Retail: General Merchandise (Dept Stores)", "456", "Retail: Health & Personal Care", "457", "Retail: Gas Stations & Fuel", "458", "Retail: Clothing, Shoes, Jewelry", "459", "Retail: Sporting Goods, Hobby, Books, Misc", "4MS", "Retail Trade (Not Specified)",
  "481", "Air Transportation", "482", "Rail Transportation", "483", "Water Transportation", "484", "Truck Transportation", "485", "Transit & Ground Passenger Transport", "486", "Pipeline Transportation", "487", "Scenic & Sightseeing Transport", "488", "Support Activities for Transportation",
  "491", "Postal Service", "492", "Couriers & Messengers", "493", "Warehousing & Storage",
  "512", "Motion Picture & Sound Recording", "513", "Publishing Industries", "516", "Broadcasting & Content Providers", "517", "Telecommunications", "518", "Data Processing, Hosting & Related", "519", "Libraries, Search Portals & Other Info",
  "522", "Credit Intermediation (Banks)", "523", "Securities, Investments & Funds", "524", "Insurance Carriers & Related", "525", "Funds & Trusts", "52M", "Finance & Insurance (Misc/Aggregated)",
  "531", "Real Estate", "532", "Rental & Leasing Services", "533", "Lessors of Intangible Assets", "53M", "Real Estate & Rental (Misc)",
  "541", "Professional, Scientific & Technical Svcs",
  "55",  "Management of Companies (HQ)", "551", "Management of Companies (HQ)",
  "561", "Administrative & Support Services", "562", "Waste Management & Remediation",
  "611", "Educational Services",
  "621", "Ambulatory Health Care (Doctors/Clinics)", "622", "Hospitals", "623", "Nursing & Residential Care Facilities", "624", "Social Assistance (Childcare/Food Relief)", "62M", "Health Care Services (Aggregated)",
  "711", "Performing Arts & Spectator Sports", "712", "Museums, Historical Sites & Zoos", "713", "Amusement, Gambling & Recreation",
  "721", "Accommodation (Hotels/RV Parks)", "722", "Food Services & Drinking Places",
  "811", "Repair & Maintenance", "812", "Personal & Laundry Services", "813", "Religious, Civic & Professional Orgs", "814", "Private Households",
  "921", "Public Admin: Exec/Legislative/General", "923", "Public Admin: Human Resource Programs", "928", "Public Admin: National Security & Military", "92M", "Public Admin: Justice/Env/Econ/Other",
  "999", "Unemployed / No Work Experience", "bbb", "N/A (Less than 16 / NILF)")

aei_url  <- "https://huggingface.co/datasets/Anthropic/EconomicIndex/resolve/main/labor_market_impacts/job_exposure.csv"
aei_file <- "job_exposure.csv"
if (!file.exists(aei_file)) download.file(aei_url, aei_file, mode = "wb")

soc_exposure <- readr::read_csv(aei_file, show_col_types = FALSE) %>%
  dplyr::mutate(socp6 = gsub("-", "", occ_code)) %>%
  dplyr::select(socp6, occ_title = title, observed_exposure)

stopifnot(nrow(soc_exposure) > 600, all(c("socp6", "observed_exposure") %in% names(soc_exposure)))
tbl_file <- "ai-labor-market-tracker_all-data_2026-06-15/crosswalks/soc_data.csv"

soc_usage <- readr::read_csv(tbl_file, show_col_types = FALSE) %>%
  dplyr::mutate(socp6 = gsub("-", "", soc2018)) %>%
  dplyr::select(socp6, usage = total, aioe = AIOE,
                eloundou_beta = dv_rating_beta,
                ms_applicability = ai_applicability_score)

stopifnot(nrow(soc_usage) > 800)

soc_metrics <- dplyr::full_join(soc_exposure, soc_usage, by = "socp6")

metric_cols <- c("observed_exposure", "usage", "aioe", "eloundou_beta", "ms_applicability")

prefix_tbl <- function(df, n) {
  df %>%
    dplyr::mutate(prefix = substr(socp6, 1, n)) %>%
    dplyr::group_by(prefix) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(metric_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(metric_cols), ~ ifelse(is.nan(.x), NA_real_, .x)))
}

soc_p5 <- prefix_tbl(soc_metrics, 5) %>% dplyr::rename_with(~ paste0(.x, "_p5"), dplyr::all_of(metric_cols))
soc_p4 <- prefix_tbl(soc_metrics, 4) %>% dplyr::rename_with(~ paste0(.x, "_p4"), dplyr::all_of(metric_cols))
soc_p3 <- prefix_tbl(soc_metrics, 3) %>% dplyr::rename_with(~ paste0(.x, "_p3"), dplyr::all_of(metric_cols))
soc_p2 <- prefix_tbl(soc_metrics, 2) %>% dplyr::rename_with(~ paste0(.x, "_p2"), dplyr::all_of(metric_cols))

philly_msa_counties <- c("42017", "42029", "42045", "42091", "42101", "34005", "34007", "34015", "34033", "10003", "24015")
target_states <- c("PA", "NJ", "DE", "MD")

msa_shape <- map_df(target_states, ~counties(state = .x, cb = TRUE, year = 2024)) %>%
  mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  filter(FIPS %in% philly_msa_counties) %>%
  st_transform(3857)

puma_shape_all <- map_df(target_states, ~pumas(state = .x, cb = TRUE, year = 2020)) %>%
  st_transform(3857)

msa_pumas_list <- st_filter(st_centroid(puma_shape_all), msa_shape) %>%
  st_drop_geometry() %>%
  mutate(valid_geo_id = paste0(STATEFP20, PUMACE20)) %>%
  select(valid_geo_id)

pums_vars <- c("PUMA", "SOCP", "NAICSP", "ESR", "PWGTP", "WAGP", "SEX")

pums_raw <- get_pums(variables = pums_vars, state = target_states, survey = "acs1", year = 2024, rep_weights = NULL)

philly_workforce <- pums_raw %>%
  mutate(STATE_CODE = if ("ST" %in% names(.)) ST else if ("state" %in% names(.)) state else if ("STATE" %in% names(.)) STATE else NA) %>%
  filter(ESR %in% c("1", "2"), !is.na(SOCP)) %>%
  mutate(geo_id = paste0(STATE_CODE, PUMA)) %>%
  inner_join(msa_pumas_list, by = c("geo_id" = "valid_geo_id")) %>%
  mutate(
    gender = if_else(SEX == "2", "Female", "Male"),
    naics3 = substr(NAICSP, 1, 3),
    soc2   = substr(SOCP, 1, 2),
    occupation_group = case_when(
      soc2 == "11" ~ "Management", soc2 == "13" ~ "Business and Financial Operations", soc2 == "15" ~ "Computer and Mathematical",
      soc2 == "17" ~ "Architecture and Engineering", soc2 == "19" ~ "Life, Physical, and Social Science", soc2 == "21" ~ "Community and Social Service",
      soc2 == "23" ~ "Legal", soc2 == "25" ~ "Educational Instruction and Library", soc2 == "27" ~ "Arts, Design, Entertainment, Sports",
      soc2 == "29" ~ "Healthcare Practitioners", soc2 == "31" ~ "Healthcare Support", soc2 == "33" ~ "Protective Service",
      soc2 == "35" ~ "Food Preparation and Serving", soc2 == "37" ~ "Building and Grounds Cleaning", soc2 == "39" ~ "Personal Care and Service",
      soc2 == "41" ~ "Sales and Related", soc2 == "43" ~ "Office and Administrative Support", soc2 == "45" ~ "Farming, Fishing, and Forestry",
      soc2 == "47" ~ "Construction and Extraction", soc2 == "49" ~ "Installation, Maintenance, Repair", soc2 == "51" ~ "Production",
      soc2 == "53" ~ "Transportation and Material Moving", TRUE ~ "Other"
    )
  ) %>%
  left_join(naics3_names, by = "naics3") %>%
  mutate(
    industry_name = ifelse(is.na(industry_name), paste("Industry", naics3), industry_name),
    PWGTP = as.numeric(PWGTP), WAGP = as.numeric(WAGP)
  )

philly_scored <- philly_workforce %>%
  dplyr::mutate(
    socp_num = stringr::str_extract(SOCP, "^[0-9]+"),
    p5 = substr(socp_num, 1, 5),
    p4 = substr(socp_num, 1, 4),
    p3 = substr(socp_num, 1, 3),
    p2 = substr(socp_num, 1, 2)
  ) %>%
  dplyr::left_join(
    soc_metrics %>% dplyr::rename_with(~ paste0(.x, "_d6"), dplyr::all_of(metric_cols)),
    by = c("socp_num" = "socp6")
  ) %>%
  dplyr::left_join(soc_p5, by = c("p5" = "prefix")) %>%
  dplyr::left_join(soc_p4, by = c("p4" = "prefix")) %>%
  dplyr::left_join(soc_p3, by = c("p3" = "prefix")) %>%
  dplyr::left_join(soc_p2, by = c("p2" = "prefix")) %>%
  dplyr::mutate(
    observed_exposure = dplyr::coalesce(observed_exposure_d6, observed_exposure_p5, observed_exposure_p4, observed_exposure_p3, observed_exposure_p2, 0),
    usage             = dplyr::coalesce(usage_d6, usage_p5, usage_p4, usage_p3, usage_p2, 0),
    aioe              = dplyr::coalesce(aioe_d6, aioe_p5, aioe_p4, aioe_p3, aioe_p2),
    eloundou_beta     = dplyr::coalesce(eloundou_beta_d6, eloundou_beta_p5, eloundou_beta_p4, eloundou_beta_p3, eloundou_beta_p2),
    ms_applicability  = dplyr::coalesce(ms_applicability_d6, ms_applicability_p5, ms_applicability_p4, ms_applicability_p3, ms_applicability_p2),
    exposure_match_level = dplyr::case_when(
      !is.na(observed_exposure_d6) ~ "1_detailed_6digit",
      !is.na(observed_exposure_p5) ~ "2_prefix5_mean",
      !is.na(observed_exposure_p4) ~ "3_prefix4_mean",
      !is.na(observed_exposure_p3) ~ "4_minor_group_mean",
      !is.na(observed_exposure_p2) ~ "5_major_group_mean",
      TRUE                          ~ "6_unmatched_zero"
    ),
    usage_match_6digit = !is.na(usage_d6)
  ) %>%
  dplyr::select(-dplyr::ends_with("_d6"), -dplyr::ends_with("_p5"), -dplyr::ends_with("_p4"),
                -dplyr::ends_with("_p3"), -dplyr::ends_with("_p2"), -p5, -p4, -p3, -p2)

print(paste("Scored", nrow(philly_scored), "person-records."))

philly_scored %>%
  dplyr::count(exposure_match_level, wt = PWGTP, name = "workers") %>%
  dplyr::mutate(pct = round(workers / sum(workers) * 100, 1)) %>%
  print()

print(paste0("Usage matched at 6-digit SOC: ",
             round(sum(philly_scored$PWGTP[philly_scored$usage_match_6digit]) / sum(philly_scored$PWGTP) * 100, 1), "% of workers"))

overall_usage <- weighted.mean(philly_scored$usage, philly_scored$PWGTP, na.rm = TRUE)

industry_summary <- philly_scored %>%
  group_by(naics3, industry_name) %>%
  summarise(
    total_workers  = sum(PWGTP, na.rm = TRUE),
    avg_wage       = weighted.mean(WAGP, PWGTP, na.rm = TRUE),
    exposure_index = weighted.mean(observed_exposure, PWGTP, na.rm = TRUE),
    adoption_ratio = weighted.mean(usage, PWGTP, na.rm = TRUE) / overall_usage,
    aioe_index     = weighted.mean(aioe, PWGTP, na.rm = TRUE),
    eloundou_index = weighted.mean(eloundou_beta, PWGTP, na.rm = TRUE),
    msappl_index   = weighted.mean(ms_applicability, PWGTP, na.rm = TRUE),
    pct_admin      = sum(PWGTP[occupation_group == "Office and Administrative Support"], na.rm = TRUE) / sum(PWGTP, na.rm = TRUE) * 100,
    pct_tech       = sum(PWGTP[occupation_group == "Computer and Mathematical"], na.rm = TRUE) / sum(PWGTP, na.rm = TRUE) * 100,
    .groups = "drop"
  )

robustness_cor <- industry_summary %>%
  select(AEI_exposure = exposure_index, AIOE = aioe_index,
         Eloundou_beta = eloundou_index, MS_applicability = msappl_index) %>%
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  round(2)
print("=== ROBUSTNESS: industry rank correlations across published exposure metrics ===")
print(robustness_cor)

mid_wage     <- median(industry_summary$avg_wage, na.rm = TRUE)
mid_exposure <- median(industry_summary$exposure_index, na.rm = TRUE)
max_exposure <- max(industry_summary$exposure_index, na.rm = TRUE)

zone_levels <- c("High Exposure · High Wage", "High Exposure · Low Wage",
                 "Low Exposure · Low Wage",  "Low Exposure · High Wage")
zones <- data.frame(
  x_start = c(0, mid_wage, 0, mid_wage),
  x_end   = c(mid_wage, Inf, mid_wage, Inf),
  y_start = c(mid_exposure, mid_exposure, 0, 0),
  y_end   = c(Inf, Inf, mid_exposure, mid_exposure),
  Fill    = c("#FFF3C8", "#AEE2FF", "#D9F9DF", "#B5BAFF"))

plot_labels <- industry_summary %>%
  mutate(
    label_flag = case_when(
      rank(desc(exposure_index)) <= 12 ~ TRUE,
      rank(desc(total_workers)) <= 5 ~ TRUE,
      industry_name %in% c("Postal Service", "Insurance Carriers & Related", "Credit Intermediation (Banks)") ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% filter(label_flag == TRUE)

plot_strategic <- ggplot() +
  geom_rect(data = zones, aes(xmin = x_start, xmax = x_end, ymin = y_start, ymax = y_end, fill = Fill),
            alpha = 0.6,
            inherit.aes = FALSE) +
  scale_fill_identity() +
  annotate("text", x = mid_wage * 0.5, y = max_exposure * 1.02, label = "HIGH EXPOSURE · LOW WAGE", color = "#E5CB90", fontface = "bold", size = 5, alpha = 0.8) +
  annotate("text", x = mid_wage * 1.5, y = max_exposure * 1.02, label = "HIGH EXPOSURE · HIGH WAGE", color = "#458393", fontface = "bold", size = 5, alpha = 0.8) +
  annotate("text", x = mid_wage * 0.5, y = mid_exposure * 0.15, label = "LOW EXPOSURE · LOW WAGE", color = "#34A99D", fontface = "bold", size = 5, alpha = 0.8) +
  annotate("text", x = mid_wage * 1.5, y = mid_exposure * 0.15, label = "LOW EXPOSURE · HIGH WAGE", color = "#9FA1FF", fontface = "bold", size = 5, alpha = 0.8) +
  geom_point(data = industry_summary, aes(x = avg_wage, y = exposure_index, size = total_workers), color = "grey30", fill = "grey30", alpha = 0.6, shape = 21) +
  geom_point(data = plot_labels, aes(x = avg_wage, y = exposure_index, size = total_workers), color = "black", fill = "#34A99D", shape = 21, stroke = 1) +
  geom_label_repel(data = plot_labels, aes(x = avg_wage, y = exposure_index, label = industry_name), size = 3.5, fontface = "bold", box.padding = 0.6, max.overlaps = Inf) +
  scale_size_continuous(range = c(2, 14), name = "Workers", labels = comma) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Observed AI Exposure by Industry, Philadelphia MSA",
    x = "Average annual wage",
    y = "Observed AI exposure (AEI)",
    caption  = "ACS 2024 PUMS · Anthropic Economic Index, 2026"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_line(color = "white"), plot.title = element_text(face = "bold", size = 18), legend.position = "bottom")

plot_strategic
ggsave("philly_exposure_quadrant_FINAL.png", plot_strategic, bg = "transparent", width = 12, height = 10)
write.csv(industry_summary, "industry_summary.csv")

final_export_table <- industry_summary %>%
  mutate(
    zone = case_when(
      avg_wage <  mid_wage & exposure_index >= mid_exposure ~ zone_levels[2],
      avg_wage >= mid_wage & exposure_index >= mid_exposure ~ zone_levels[1],
      avg_wage <  mid_wage & exposure_index <  mid_exposure ~ zone_levels[3],
      TRUE                                                  ~ zone_levels[4]
    ),
    avg_wage       = round(avg_wage, 0),
    exposure_index = round(exposure_index, 3),
    adoption_ratio = round(adoption_ratio, 2),
    pct_admin      = round(pct_admin, 1),
    pct_tech       = round(pct_tech, 1)
  ) %>%
  select(
    NAICS_Code            = naics3,
    Industry_Name         = industry_name,
    Zone                  = zone,
    AEI_Observed_Exposure = exposure_index,
    Workforce_Size        = total_workers,
    Average_Annual_Wage   = avg_wage,
    Adoption_Ratio        = adoption_ratio,
    Pct_Admin_Roles       = pct_admin,
    Pct_Tech_Roles        = pct_tech
  ) %>%
  arrange(desc(AEI_Observed_Exposure))

write_csv(final_export_table, "philly_msa_ai_exposure_FINAL.csv")

zone_colors <- c("High Exposure · High Wage" = "#458393",
                 "High Exposure · Low Wage"  = "#E5CB90",
                 "Low Exposure · Low Wage"   = "#34A99D",
                 "Low Exposure · High Wage"  = "#9FA1FF")

plot_data_workforce <- final_export_table %>%
  arrange(desc(Workforce_Size)) %>%
  head(30) %>%
  mutate(Zone = factor(Zone, levels = zone_levels))

ggplot(plot_data_workforce, aes(x = reorder(Industry_Name, Workforce_Size), y = Workforce_Size, fill = Zone)) +
  geom_col(width = 0.75, alpha = 0.95) +
  geom_text(aes(label = format(Workforce_Size, big.mark = ",", scientific = FALSE)), hjust = -0.1, size = 3, color = "gray30", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = zone_colors, name = "Zone") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Top 30 Industries by Employment, Philadelphia MSA",
    x = NULL, y = "Workers",
    caption  = "ACS 2024 PUMS · Anthropic Economic Index, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left", legend.title = element_text(face = "bold", size = 10), plot.title = element_text(face = "bold", size = 16), panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold", color = "black"))

ggsave("philly_top30_workforce_FINAL.png", width = 12, height = 14, bg = "transparent")

gender_exposure <- philly_scored %>%
  group_by(gender) %>%
  summarise(total_workers = sum(PWGTP), avg_exposure = weighted.mean(observed_exposure, PWGTP, na.rm = TRUE), .groups = "drop")
print("=== GENDER EXPOSURE ===")
print(gender_exposure)

industry_gender_gap <- philly_scored %>%
  group_by(industry_name, gender) %>%
  summarise(avg_exposure = weighted.mean(observed_exposure, PWGTP, na.rm = TRUE), gender_workers = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  group_by(industry_name) %>% mutate(total_industry_workers = sum(gender_workers)) %>% ungroup() %>%
  filter(total_industry_workers > 2000) %>%
  select(industry_name, gender, avg_exposure) %>%
  pivot_wider(names_from = gender, values_from = avg_exposure) %>%
  filter(!is.na(Female) & !is.na(Male)) %>%
  arrange(desc(Female)) %>%
  head(50)

ggplot(industry_gender_gap, aes(y = reorder(industry_name, Female))) +
  geom_dumbbell(aes(x = Male, xend = Female), size = 1.5, color = "#e5e7eb", colour_x = "#34A99D", colour_xend = "#9FA1FF", size_x = 3.5, size_xend = 3.5) +
  geom_text(aes(x = Male, label = "M"), color = "#34A99D", vjust = -1.5, size = 3, fontface = "bold") +
  geom_text(aes(x = Female, label = "F"), color = "#9FA1FF", vjust = -1.5, size = 3, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1)), labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Observed AI Exposure by Gender and Industry, Philadelphia MSA",
    x = "Observed AI exposure (AEI)", y = NULL,
    caption  = "ACS 2024 PUMS · Anthropic Economic Index, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16), panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold", color = "black"))

ggsave("philly_gender_gap_FINAL.png", width = 12, height = 18, bg = "transparent")

ai_leading <- final_export_table %>%
  filter(Workforce_Size >= 5000) %>%
  arrange(desc(Adoption_Ratio)) %>%
  head(30) %>%
  mutate(
    naics2 = substr(NAICS_Code, 1, 2),
    Sector_Category = case_when(
      naics2 %in% c("31","32","33","3M") ~ "Manufacturing",
      naics2 == "51"                      ~ "Information & Media",
      naics2 == "54"                      ~ "Professional & Tech Services",
      naics2 == "61"                      ~ "Educational Services",
      naics2 == "62"                      ~ "Health Care",
      naics2 == "92"                      ~ "Public Administration",
      naics2 == "52"                      ~ "Finance & Insurance",
      naics2 == "55"                      ~ "Management of Companies",
      naics2 %in% c("48","49")           ~ "Transportation & Warehousing",
      naics2 == "71"                      ~ "Arts & Entertainment",
      naics2 == "22"                      ~ "Utilities",
      naics2 %in% c("11","21","81","82") ~ "Agriculture, Mining & Other Services",
      TRUE                                ~ "Other"
    )
  )

sector_colors <- setNames(
  colorRampPalette(c("#E5CB90", "#FFF3C8", "#D9F9DF", "#34A99D", "#458393", "#AEE2FF", "#B5BAFF", "#9FA1FF"))(13),
  c("Manufacturing", "Information & Media", "Professional & Tech Services",
    "Educational Services", "Health Care", "Public Administration",
    "Finance & Insurance", "Management of Companies",
    "Transportation & Warehousing", "Arts & Entertainment", "Utilities",
    "Agriculture, Mining & Other Services", "Other"))

ggplot(ai_leading,
       aes(x = reorder(Industry_Name, Adoption_Ratio),
           y = Adoption_Ratio, fill = Sector_Category)) +
  geom_col(alpha = 0.9, width = 0.75) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_text(aes(label = round(Adoption_Ratio, 2)),
            hjust = -0.15,
            size = 3, fontface = "bold", color = "gray30") +
  scale_fill_manual(values = sector_colors, name = "Sector") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Top 30 Industries by AI Usage Intensity, Philadelphia MSA",
    x = NULL,
    y = "Usage intensity ratio (industry / workforce average)",
    caption  = "ACS 2024 PUMS · Anthropic Economic Index, 2026 · TBL crosswalk, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    legend.title     = element_text(face = "bold", size = 10),
    plot.title       = element_text(face = "bold", size = 16),
    plot.caption     = element_text(size = 10, color = "gray60"),
    panel.grid.major.y = element_blank(),
    axis.text.y      = element_text(face = "bold", color = "black")
  )

ggsave("philly_adoption_ratio_FINAL.png", width = 12, height = 12, bg = "transparent")
write.csv(ai_leading, "ai_leading.csv", row.names = FALSE)

occ_group_usage <- philly_scored %>%
  group_by(occupation_group) %>%
  summarise(usage_ratio = weighted.mean(usage, PWGTP, na.rm = TRUE) / overall_usage,
            total_workers = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  filter(occupation_group != "Other")

ggplot(occ_group_usage, aes(x = reorder(occupation_group, usage_ratio),
                            y = usage_ratio,
                            fill = usage_ratio)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_text(aes(label = round(usage_ratio, 2)),
            hjust = -0.15, size = 3.2, fontface = "bold", color = "gray30") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_gradient(low = "#AEE2FF", high = "#458393") +
  labs(
    title    = "AI Usage by Occupation Group, Philadelphia MSA",
    x = NULL,
    y = "Usage intensity ratio",
    caption  = "ACS 2024 PUMS · Anthropic Economic Index, 2026 · TBL crosswalk, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title       = element_text(face = "bold", size = 16),
    plot.caption    = element_text(size = 10, color = "gray60"),
    panel.grid.major.y = element_blank(),
    axis.text.y      = element_text(face = "bold", color = "black")
  )

ggsave("chart_A_usage_by_occupation.png", width = 12, height = 8, bg = "transparent")

puma_exposure <- philly_scored %>%
  group_by(STATE_CODE, PUMA) %>%
  summarise(avg_exposure = weighted.mean(observed_exposure, PWGTP, na.rm = TRUE), total_workers = sum(PWGTP, na.rm = TRUE), .groups = "drop")

map_data_exposure <- puma_shape_all %>%
  inner_join(puma_exposure, by = c("STATEFP20" = "STATE_CODE", "PUMACE20" = "PUMA"))

ggplot() +
  geom_sf(data = map_data_exposure, aes(fill = avg_exposure), color = "white", size = 0.05) +
  geom_sf(data = msa_shape, fill = NA, color = "black", size = 0.4, alpha = 0.6) +
  geom_sf_text(data = msa_shape, aes(label = NAME), size = 3.5, fontface = "bold", color = "black", fun.geometry = sf::st_centroid, check_overlap = TRUE) +
  scale_fill_gradientn(colours = c("#FFF3C8", "#AEE2FF", "#34A99D", "#458393"), name = "Exposure", labels = percent_format(accuracy = 1)) +
  labs(
    title    = "AI Exposure by Neighborhood, Philadelphia MSA",
    caption  = "ACS 2024 PUMS · Anthropic Economic Index, 2026"
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16, color = "#0f172a"),
        plot.caption    = element_text(size = 10, color = "gray60"))

ggsave("philly_puma_exposure_map_FINAL.png", width = 12, height = 10, bg = "transparent")

puma_usage <- philly_scored %>%
  group_by(STATE_CODE, PUMA) %>%
  summarise(
    usage_ratio = weighted.mean(usage, PWGTP, na.rm = TRUE) / overall_usage,
    total_workers = sum(PWGTP, na.rm = TRUE),
    .groups = "drop"
  )

map_data_usage <- puma_shape_all %>%
  inner_join(puma_usage, by = c("STATEFP20" = "STATE_CODE", "PUMACE20" = "PUMA"))

ai_usage_map <- ggplot() +
  geom_sf(data = map_data_usage, aes(fill = usage_ratio), color = "white", size = 0.05) +
  geom_sf(data = msa_shape, fill = NA, color = "black", size = 0.3, alpha = 0.5) +
  geom_sf_text(data = msa_shape, aes(label = NAME),
               size = 3.5, fontface = "bold", color = "black",
               fun.geometry = sf::st_centroid,
               check_overlap = TRUE) +
  scale_fill_gradientn(
    colours = c("#D9F9DF", "#AEE2FF", "#9FA1FF", "#458393"),
    name = "Usage vs. metro avg"
  ) +
  labs(
    title = "AI Usage by Neighborhood, Philadelphia MSA",
    caption = "ACS 2024 PUMS · Anthropic Economic Index, 2026 · TBL crosswalk, 2026"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 16, color = "#0f172a"),
    plot.caption    = element_text(size = 10, color = "gray60")
  )

ai_usage_map
ggsave("philly_ai_usage_map.png", ai_usage_map, width = 12, height = 10, bg = "transparent")

exposure_tiers <- philly_scored %>%
  mutate(
    tier = case_when(
      observed_exposure == 0 ~ "Below usage threshold",
      observed_exposure < 0.10 ~ "Under 10%",
      observed_exposure < 0.25 ~ "10–25%",
      observed_exposure < 0.50 ~ "25–50%",
      TRUE ~ "50%+"
    ),
    tier = factor(tier, levels = c("Below usage threshold", "Under 10%", "10–25%", "25–50%", "50%+"))
  ) %>%
  group_by(tier) %>%
  summarise(workers = sum(PWGTP), .groups = "drop") %>%
  mutate(pct = workers / sum(workers) * 100, label = paste0(format(workers, big.mark = ","), "\n(", round(pct, 1), "%)"))

tier_colors <- c("Below usage threshold" = "#D9F9DF", "Under 10%" = "#FFF3C8", "10–25%" = "#E5CB90", "25–50%" = "#9FA1FF", "50%+" = "#458393")

ggplot(exposure_tiers, aes(x = tier, y = workers, fill = tier)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = label), size = 3.5, fontface = "bold") +
  scale_fill_manual(values = tier_colors) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Workers by Observed AI Exposure, Philadelphia MSA",
    x = NULL, y = "Workers",
    caption  = "ACS 2024 PUMS · Anthropic Economic Index, 2026"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16), panel.grid.major.x = element_blank(), axis.text.x = element_text(face = "bold", size = 10))

ggsave("chart_F_worker_exposure_tiers.png", width = 12, height = 3, bg = "transparent")

collapsed_titles <- tibble::tribble(
  ~socp_num, ~occ_title_fix,
  "414010", "Sales Representatives, Wholesale & Manufacturing",
  "151230", "Computer Support Specialists",
  "15124",  "Database & Network Administrators & Architects")

occupation_landscape <- philly_scored %>%
  dplyr::group_by(socp_num, occupation_group) %>%
  dplyr::summarise(
    occ_title         = dplyr::first(occ_title[!is.na(occ_title)], default = NA_character_),
    total_workers     = sum(PWGTP, na.rm = TRUE),
    avg_wage          = weighted.mean(WAGP, PWGTP, na.rm = TRUE),
    pct_female        = sum(PWGTP[gender == "Female"], na.rm = TRUE) / sum(PWGTP, na.rm = TRUE) * 100,
    observed_exposure = weighted.mean(observed_exposure, PWGTP, na.rm = TRUE),
    usage_ratio       = weighted.mean(usage, PWGTP, na.rm = TRUE) / overall_usage,
    match_level       = dplyr::first(exposure_match_level),
    .groups = "drop"
  ) %>%
  dplyr::left_join(collapsed_titles, by = "socp_num") %>%
  dplyr::mutate(occ_title = dplyr::coalesce(occ_title, occ_title_fix, paste("SOCP", socp_num, "(collapsed code)"))) %>%
  dplyr::select(-occ_title_fix) %>%
  dplyr::arrange(dplyr::desc(observed_exposure))

readr::write_csv(occupation_landscape, "occupation_landscape_v5.csv")
occ_top <- occupation_landscape %>%
  dplyr::filter(total_workers >= 2000) %>%
  dplyr::slice_max(observed_exposure, n = 25)

ggplot(occ_top, aes(x = reorder(occ_title, observed_exposure), y = observed_exposure)) +
  geom_col(fill = "#458393", alpha = 0.9, width = 0.7) +
  geom_text(aes(label = paste0(format(round(total_workers), big.mark = ","), " workers")),
            hjust = -0.05, size = 3, color = "gray30") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22)), labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Top 25 Occupations by Observed AI Exposure, Philadelphia MSA",
    x = NULL, y = "Observed AI exposure (AEI)",
    caption  = "ACS 2024 PUMS · Anthropic Economic Index, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        panel.grid.major.y = element_blank())

ggsave("chart_G_top25_occupations_exposure.png", width = 12, height = 10, bg = "transparent")

occ_admin <- occupation_landscape %>%
  dplyr::filter(occupation_group == "Office and Administrative Support",
                total_workers >= 1000,
                match_level == "1_detailed_6digit")

ggplot(occ_admin, aes(x = observed_exposure, y = total_workers)) +
  geom_point(aes(size = total_workers), color = "#458393", alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = occ_title), size = 3, max.overlaps = 14, color = "gray25") +
  scale_y_log10(labels = comma) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "AI Exposure in Office & Admin (SOC 43) Occupations, Philadelphia MSA",
    x = "Observed AI exposure",
    y = "Workers (log)",
    caption  = "ACS 2024 PUMS · Anthropic Economic Index, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 15))

ggsave("chart_H_admin_within_group.png", width = 12, height = 10, bg = "transparent")

print(paste("v6 occupation landscape:", nrow(occupation_landscape), "detailed occupations."))

stopifnot(exists("industry_summary"), exists("mid_wage"), exists("mid_exposure"),
          exists("philly_msa_counties"))
dir.create("qcew_cache", showWarnings = FALSE)

fetch_qcew <- function(area, year, qtr) {
  f <- file.path("qcew_cache", paste0(area, "_", year, "q", qtr, ".csv"))
  if (!file.exists(f)) {
    url <- sprintf("https://data.bls.gov/cew/data/api/%d/%d/area/%s.csv", year, qtr, area)
    ok <- tryCatch({ download.file(url, f, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) { if (file.exists(f)) file.remove(f); return(NULL) }
  }
  tryCatch(readr::read_csv(f, col_types = readr::cols(.default = "c"), show_col_types = FALSE),
           error = function(e) NULL)
}

qcew_raw <- tidyr::expand_grid(area = philly_msa_counties, year = 2022:2025, qtr = 1:4) %>%
  purrr::pmap_dfr(function(area, year, qtr) fetch_qcew(area, year, qtr))

qcew_raw %>%
  dplyr::distinct(area_fips, year, qtr) %>%
  dplyr::count(year, qtr, name = "counties") %>%
  print(n = 20)

qcew_keys <- industry_summary$naics3 %>%
  unique() %>%
  setdiff("55") %>%
  purrr::keep(~ grepl("^[0-9]+$", .x))

qcew_ind <- qcew_raw %>%
  dplyr::filter(industry_code %in% qcew_keys, own_code %in% c("1", "2", "3", "5")) %>%
  dplyr::mutate(
    dplyr::across(c(month1_emplvl, month2_emplvl, month3_emplvl),
                  ~ dplyr::if_else(disclosure_code %in% "N", NA_real_, as.numeric(.x))),
    emp_q = rowMeans(cbind(month1_emplvl, month2_emplvl, month3_emplvl), na.rm = TRUE),
    year  = as.integer(year),
    qtr   = as.integer(qtr)
  ) %>%
  dplyr::group_by(naics3 = industry_code, year, qtr) %>%
  dplyr::summarise(emp = sum(emp_q, na.rm = TRUE), .groups = "drop")

zone_map <- industry_summary %>%
  dplyr::mutate(zone = dplyr::case_when(
    avg_wage >= mid_wage & exposure_index >= mid_exposure ~ zone_levels[1],
    avg_wage <  mid_wage & exposure_index >= mid_exposure ~ zone_levels[2],
    avg_wage <  mid_wage & exposure_index <  mid_exposure ~ zone_levels[3],
    TRUE                                                  ~ zone_levels[4]
  )) %>%
  dplyr::select(naics3, industry_name, zone)

zone_traj <- qcew_ind %>%
  dplyr::inner_join(zone_map, by = "naics3") %>%
  dplyr::group_by(zone, year, qtr) %>%
  dplyr::summarise(emp = sum(emp, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(zone, year, qtr) %>%
  dplyr::mutate(t = year + (qtr - 1) / 4,
                zone = factor(zone, levels = zone_levels))

readr::write_csv(zone_traj, "zone_employment_trajectory_qcew.csv")

ggplot(zone_traj, aes(x = t, y = emp, color = zone)) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_point(size = 1.6) +
  scale_color_manual(values = zone_colors) +
  scale_x_continuous(breaks = 2022:2026) +
  scale_y_continuous(labels = comma) +
  labs(title    = "Employment by Exposure-Wage Zone, Philadelphia MSA",
       x = NULL, y = "Employment", color = NULL,
       caption  = "BLS QCEW") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

ggsave("chart_I_zone_employment_qcew.png", width = 12, height = 9, bg = "transparent")

qtr_cov <- qcew_ind %>% dplyr::count(year, qtr)
end_year <- max(qtr_cov$year[qtr_cov$n == max(qtr_cov$n)])
end_qtr  <- max(qtr_cov$qtr[qtr_cov$year == end_year & qtr_cov$n == max(qtr_cov$n)])

ind_growth <- qcew_ind %>%
  dplyr::filter((year == 2022 & qtr == 1) | (year == end_year & qtr == end_qtr)) %>%
  dplyr::mutate(period = dplyr::if_else(year == 2022, "emp_start", "emp_end")) %>%
  dplyr::select(naics3, period, emp) %>%
  tidyr::pivot_wider(names_from = period, values_from = emp) %>%
  dplyr::filter(!is.na(emp_start), !is.na(emp_end), emp_start > 0) %>%
  dplyr::mutate(growth_pct = (emp_end / emp_start - 1) * 100) %>%
  dplyr::inner_join(industry_summary %>%
                      dplyr::select(naics3, industry_name, exposure_index, total_workers),
                    by = "naics3") %>%
  dplyr::filter(is.finite(growth_pct))

cor_test <- stats::cor.test(ind_growth$exposure_index, ind_growth$growth_pct,
                            method = "spearman", exact = FALSE)
print(cor_test)

ggplot(ind_growth, aes(x = exposure_index, y = growth_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_point(aes(size = total_workers), color = "#458393", alpha = 0.6) +
  ggrepel::geom_text_repel(
    data = ind_growth %>%
      dplyr::filter(rank(-exposure_index) <= 8 | rank(-abs(growth_pct)) <= 5),
    aes(label = industry_name), size = 3, color = "gray25", max.overlaps = 12) +
  scale_size_continuous(guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(title    = "Observed AI Exposure vs. Employment Change, Philadelphia MSA",
       subtitle = sprintf("Spearman rho = %.2f (p = %.2f) · 2022 Q1 to %d Q%d", cor_test$estimate, cor_test$p.value, end_year, end_qtr),
       x = "Observed AI exposure (AEI)", y = "Employment change since 2022 (%)",
       caption  = "BLS QCEW · ACS 2024 PUMS · Anthropic Economic Index, 2026") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "gray50"))

ggsave("chart_J_exposure_vs_growth.png", width = 12, height = 14, bg = "transparent")

print(paste("Part 12:", nrow(zone_traj), "zone-quarters;", nrow(ind_growth), "industries."))

#msa comparison
msa_defs <- list(
  "Philadelphia" = list(
    states = c("PA", "NJ", "DE", "MD"),
    fips   = philly_msa_counties
  ),
  "New York" = list(
    states = c("NY", "NJ"),
    fips = c("36005", "36027", "36047", "36059", "36061", "36071", "36079",
             "36081", "36085", "36087", "36103", "36119",
             "34003", "34013", "34017", "34019", "34023", "34025", "34027",
             "34029", "34031", "34035", "34037", "34039")
  ),
  "Chicago" = list(
    states = c("IL", "IN", "WI"),
    fips = c("17031", "17037", "17043", "17063", "17089", "17093", "17097",
             "17111", "17197", "18073", "18089", "18111", "18127", "55059")
  ),
  "Los Angeles" = list(
    states = "CA",
    fips = c("06037", "06059")
  ),
  "Miami" = list(
    states = "FL",
    fips = c("12086", "12011", "12099")
  ),
  "Boston" = list(
    states = c("MA", "NH"),
    fips = c("25009", "25017", "25021", "25023", "25025", "33015", "33017")
  ),
  "Washington DC" = list(
    states = c("DC", "MD", "VA", "WV"),
    fips = c("11001", "24009", "24017", "24021", "24031", "24033",
             "51013", "51043", "51047", "51059", "51061", "51107", "51153",
             "51157", "51177", "51179", "51187",
             "51510", "51600", "51610", "51630", "51683", "51685", "54037")
  ),
  "San Francisco" = list(
    states = "CA",
    fips = c("06001", "06013", "06041", "06075", "06081")
  ),
  "Seattle" = list(
    states = "WA",
    fips = c("53033", "53053", "53061")
  ),
  "Dallas" = list(
    states = "TX",
    fips = c("48085", "48113", "48121", "48139", "48231", "48251",
             "48257", "48367", "48397", "48439", "48497")
  ))

get_pums_cached <- function(st) {
  f <- paste0("pums_cache_", st, "_2024.rds")
  if (file.exists(f)) return(readRDS(f))
  d <- get_pums(variables = pums_vars, state = st, survey = "acs1",
                year = 2024, rep_weights = NULL)
  saveRDS(d, f)
  d
}

score_msa <- function(msa_name, def) {
  shp <- map_df(def$states, ~counties(state = .x, cb = TRUE, year = 2024)) %>%
    mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%
    filter(FIPS %in% def$fips) %>%
    st_transform(3857)
  
  puma_ids <- map_df(def$states, ~pumas(state = .x, cb = TRUE, year = 2020)) %>%
    st_transform(3857) %>%
    st_centroid() %>%
    st_filter(shp) %>%
    st_drop_geometry() %>%
    transmute(valid_geo_id = paste0(STATEFP20, PUMACE20))
  
  scored <- map_df(def$states, get_pums_cached) %>%
    mutate(STATE_CODE = if ("ST" %in% names(.)) ST else if ("state" %in% names(.)) state else STATE) %>%
    filter(ESR %in% c("1", "2"), !is.na(SOCP)) %>%
    mutate(geo_id = paste0(STATE_CODE, PUMA)) %>%
    inner_join(puma_ids, by = c("geo_id" = "valid_geo_id")) %>%
    mutate(
      gender = if_else(SEX == "2", "Female", "Male"),
      soc2   = substr(SOCP, 1, 2),
      naics3 = substr(NAICSP, 1, 3),
      socp_num = stringr::str_extract(SOCP, "^[0-9]+"),
      p5 = substr(socp_num, 1, 5), p4 = substr(socp_num, 1, 4),
      p3 = substr(socp_num, 1, 3), p2 = substr(socp_num, 1, 2)
    ) %>%
    left_join(soc_metrics %>%
                dplyr::select(socp6, observed_exposure) %>%
                dplyr::rename(exp_d6 = observed_exposure),
              by = c("socp_num" = "socp6")) %>%
    left_join(soc_p5 %>% dplyr::select(prefix, exp_p5 = observed_exposure_p5), by = c("p5" = "prefix")) %>%
    left_join(soc_p4 %>% dplyr::select(prefix, exp_p4 = observed_exposure_p4), by = c("p4" = "prefix")) %>%
    left_join(soc_p3 %>% dplyr::select(prefix, exp_p3 = observed_exposure_p3), by = c("p3" = "prefix")) %>%
    left_join(soc_p2 %>% dplyr::select(prefix, exp_p2 = observed_exposure_p2), by = c("p2" = "prefix")) %>%
    mutate(
      PWGTP = as.numeric(PWGTP),
      observed_exposure = dplyr::coalesce(exp_d6, exp_p5, exp_p4, exp_p3, exp_p2, 0)
    )
  
  summary_out <- scored %>%
    summarise(
      msa             = msa_name,
      workers         = sum(PWGTP, na.rm = TRUE),
      exposure        = weighted.mean(observed_exposure, PWGTP, na.rm = TRUE),
      pct_high        = sum(PWGTP[observed_exposure >= 0.25], na.rm = TRUE) / sum(PWGTP, na.rm = TRUE) * 100,
      pct_admin       = sum(PWGTP[soc2 == "43"], na.rm = TRUE) / sum(PWGTP, na.rm = TRUE) * 100,
      exposure_female = weighted.mean(observed_exposure[gender == "Female"], PWGTP[gender == "Female"], na.rm = TRUE),
      exposure_male   = weighted.mean(observed_exposure[gender == "Male"], PWGTP[gender == "Male"], na.rm = TRUE)
    ) %>%
    mutate(gender_gap_pct = (exposure_female / exposure_male - 1) * 100)
  
  industry_out <- scored %>%
    group_by(naics3) %>%
    summarise(
      exposure = weighted.mean(observed_exposure, PWGTP, na.rm = TRUE),
      workers  = sum(PWGTP, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(workers >= 10000) %>%
    left_join(naics3_names, by = "naics3") %>%
    mutate(industry_name = ifelse(is.na(industry_name), paste("Industry", naics3), industry_name),
           msa = msa_name)
  
  rm(scored); gc()
  print(paste(msa_name, "done"))
  list(summary = summary_out, industries = industry_out)
}

msa_results    <- purrr::imap(msa_defs, ~score_msa(.y, .x))
msa_compare    <- purrr::map_dfr(msa_results, "summary")
msa_industries <- purrr::map_dfr(msa_results, "industries")
readr::write_csv(msa_compare, "msa_comparison.csv")
readr::write_csv(msa_industries, "msa_industries_exposure.csv")
print(msa_compare)

msa_compare <- msa_compare %>%
  mutate(high_workers = workers * pct_high / 100)
msa_order <- msa_compare %>% arrange(pct_high) %>% pull(msa)
ggplot(msa_compare, aes(y = factor(msa, levels = msa_order))) +
  geom_col(aes(x = workers, fill = "All workers"), width = 0.65, alpha = 0.95) +
  geom_col(aes(x = high_workers, fill = "Workers in occupations with 25%+ task exposure"), width = 0.65, alpha = 0.95) +
  geom_text(aes(x = high_workers / 2, label = sprintf("%.1fM", high_workers / 1e6)),
            size = 3.1, color = "white", fontface = "bold") +
  geom_text(aes(x = (high_workers + workers) / 2, label = sprintf("%.1fM", workers / 1e6)),
            size = 3.1, color = "gray30", fontface = "bold") +
  geom_text(aes(x = workers, label = sprintf("%.0f%%", pct_high)),
            hjust = -0.25, size = 3.4, color = "#458393", fontface = "bold") +
  scale_fill_manual(values = c("All workers" = "#AEE2FF",
                               "Workers in occupations with 25%+ task exposure" = "#458393"),
                    name = NULL) +
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M"),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title   = "Workers in High-Exposure Occupations by Metro",
       x = "Workers", y = NULL,
       caption = "ACS 2024 PUMS · Anthropic Economic Index, 2026") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        legend.position = "top",
        legend.justification = "left",
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(face = "bold", color = "black"))

ggsave("chart_K_msa_workers_exposed.png", width = 12, height = 7, bg = "transparent")

msa_order_gap <- msa_compare %>% arrange(exposure) %>% pull(msa)

ggplot(msa_compare, aes(y = factor(msa, levels = msa_order_gap))) +
  geom_dumbbell(aes(x = exposure_male, xend = exposure_female),
                size = 1.5, color = "#e5e7eb",
                colour_x = "#34A99D", colour_xend = "#9FA1FF",
                size_x = 4, size_xend = 4) +
  geom_text(aes(x = exposure_male, label = "M"), color = "#34A99D", vjust = -1.3, size = 3, fontface = "bold") +
  geom_text(aes(x = exposure_female, label = "F"), color = "#9FA1FF", vjust = -1.3, size = 3, fontface = "bold") +
  geom_text(aes(x = exposure_male, label = sprintf("%.1f%%", exposure_male * 100)),
            color = "#34A99D", vjust = 2.1, size = 2.9, fontface = "bold") +
  geom_text(aes(x = exposure_female, label = sprintf("%.1f%%", exposure_female * 100)),
            color = "#9FA1FF", vjust = 2.1, size = 2.9, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.08)), labels = percent_format(accuracy = 1)) +
  labs(title   = "Observed AI Exposure by Gender and Metro",
       x = "Observed AI exposure (AEI)", y = NULL,
       caption = "ACS 2024 PUMS · Anthropic Economic Index, 2026") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(face = "bold", color = "black"))

ggsave("chart_L_msa_gender_gap.png", width = 12, height = 7, bg = "transparent")



msa_ranked <- msa_industries %>%
  filter(naics3 %in% industry_name) %>%
  group_by(msa) %>%
  mutate(rank = rank(-exposure, ties.method = "first")) %>%
  ungroup()

bump_series <- msa_ranked %>%
  group_by(naics3, industry_name) %>%
  summarise(mean_rank = mean(rank), .groups = "drop") %>%
  slice_min(mean_rank, n = 10) %>%
  pull(naics3)

msa_levels <- msa_compare %>% arrange(desc(exposure)) %>% pull(msa)

bump_data <- msa_ranked %>%
  filter(naics3 %in% bump_series) %>%
  mutate(msa = factor(msa, levels = msa_levels))

bump_meta <- bump_data %>%
  distinct(naics3, industry_name) %>%
  mutate(
    naics2 = substr(naics3, 1, 2),
    family = case_when(
      naics2 == "52"               ~ "finance",
      naics2 %in% c("44", "45")   ~ "retail",
      naics2 == "42"               ~ "wholesale",
      naics2 %in% c("51", "54")   ~ "prof_info",
      naics2 %in% c("53", "55")   ~ "real_estate_mgmt",
      TRUE                         ~ "other"
    )
  )

family_palettes <- list(
  finance          = c("#34A99D", "#8FD6CC"),
  retail           = c("#E5CB90", "#9FA1FF", "#6C6FD4", "#C9A55C", "#B5BAFF"),
  wholesale        = c("#AEE2FF", "#5FA8D3"),
  prof_info        = c("#458393", "#6FA5B3", "#2C5560"),
  real_estate_mgmt = c("#274E57", "#3E7580"),
  other            = c("#1F6B60", "#C9A55C")
)

bump_meta <- bump_meta %>%
  group_by(family) %>%
  arrange(industry_name, .by_group = TRUE) %>%
  mutate(color = family_palettes[[dplyr::first(family)]][dplyr::row_number()]) %>%
  ungroup()

bump_colors <- setNames(bump_meta$color, bump_meta$industry_name)

label_left  <- bump_data %>% filter(msa == msa_levels[1])
label_right <- bump_data %>% filter(msa == msa_levels[length(msa_levels)])

ggplot(bump_data, aes(x = msa, y = rank, group = industry_name, color = industry_name)) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_point(size = 7) +
  geom_text(aes(label = rank), color = "white", size = 2.8, fontface = "bold") +
  geom_text(data = label_left, aes(label = industry_name),
            x = 0.8, hjust = 1, size = 3.1, fontface = "bold") +
  geom_text(data = label_right, aes(label = industry_name),
            x = length(msa_levels) + 0.2, hjust = 0, size = 3.1, fontface = "bold") +
  scale_color_manual(values = bump_colors, guide = "none") +
  scale_y_reverse() +
  scale_x_discrete(expand = expansion(add = c(4.5, 4.5)), position = "top",
                   guide = guide_axis(n.dodge = 2)) +
  labs(title   = "Industry Ranking by Observed AI Exposure Across MSAs",
       x = NULL, y = NULL,
       caption = "ACS 2024 PUMS · Anthropic Economic Index, 2026 · industries with 10,000+ workers in all 10 metros") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        axis.text.x = element_text(face = "bold", color = "black", size = 9),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("chart_M_msa_industry_bump.png", width = 12, height = 6, bg = "transparent")
