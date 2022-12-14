library(colorspace)
library(scales)
library(scico)
library(ggthemes)
library(cowplot)
# 1+ hour outages ------------------------------------------------ 
# Gen outage decile colors ------------------------------------------------

outage90_rounded_map <- read_csv(here("Data", "Outputs", "bivar_mapping", "po1_map_dta.csv"))

outage90_rounded_map$decile_v2 <- factor(outage90_rounded_map$decile_v2, levels = c("0",
                                                                                    "[  1, 49)",
                                                                                    "[ 49, 76)",
                                                                                    "[ 76,107)",
                                                                                    "[107,414]"))

outage90_rounded_map_bivar <- outage90_rounded_map %>% 
  mutate(group = paste0(ntile, "-", n_years_available))

outage_decile_labels <- outage90_rounded_map_bivar %>% 
  ungroup() %>% 
  select(ntile, decile_v2) %>% 
  distinct() %>% 
  rename(outages = ntile)

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons

show_col(viridis_pal(direction = 1, option = "inferno")(5))
scico_palette_show(palettes = c("broc", "cork", "vik",
                                "lisbon", "tofino", "berlin",
                                "batlow", "roma"))
scico_palette <- scico(5, palette = 'batlow')
show_col(scico_palette)


# color_list_3yrs <- (c("#000004FF", "#420A68FF", "#932667FF", "#DD513AFF", "#FCA50AFF", "#FCFFA4FF"))
color_list_3yrs <- c(scico_palette[1:4], "#FCFFA4FF")
color_list_2yrs <- lighten(color_list_3yrs, amount = 0.5)
color_list_1yrs <- lighten(color_list_3yrs, amount = 0.75)
show_col(color_list_3yrs)
show_col(color_list_2yrs)
show_col(color_list_1yrs)

bivariate_color_scale <- tibble(
  "0-1" = color_list_1yrs[5], # 1 year data
  "1-1" = color_list_1yrs[4],
  "2-1" = color_list_1yrs[3],
  "3-1" = color_list_1yrs[2],
  "4-1" = color_list_1yrs[1],
  "0-2" = color_list_2yrs[5], # 2 years data
  "1-2" = color_list_2yrs[4],
  "2-2" = color_list_2yrs[3],
  "3-2" = color_list_2yrs[2],
  "4-2" = color_list_2yrs[1],
  "0-3" = color_list_3yrs[5], # 3 years data
  "1-3" = color_list_3yrs[4],
  "2-3" = color_list_3yrs[3],
  "3-3" = color_list_3yrs[2],
  "4-3" = color_list_3yrs[1],
) %>%
  gather("group", "fill")



# Join color codes to data ------------------------------------------------
po1_bivar <- outage90_rounded_map_bivar %>% 
  left_join(., bivariate_color_scale, by = "group")


# Draw legend -------------------------------------------------------------
# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("outages", "n_years"), sep = "-") %>%
  mutate(outages = as.integer(outages),
         n_years = as.integer(n_years))

bivariate_color_scale <- bivariate_color_scale %>% 
  arrange(desc(n_years), desc(outages)) %>% 
  left_join(., outage_decile_labels, by = ("outages"))

po1_legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = n_years,
      y = decile_v2,
      fill = fill)
  ) +
  scale_fill_identity() +
  theme_map() +
  # make font small enough
  labs(x ="Years of Data",
       y = "Outages") +
  theme(
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()
po1_legend

ggsave(path = here::here("Visuals"),
       filename = "po1_alpha_legend.jpg", 
       dpi=300,
       height=4, width=7, units="in")


# (2) 8+ hour outages ------------------------------------------------ 
# Gen outage decile colors ------------------------------------------------

outage90_rounded_map <- read_csv(here("Data", "Outputs", "bivar_mapping", "po8_map_dta.csv"))

unique(outage90_rounded_map$decile_v2)
outage90_rounded_map$decile_v2 <- factor(outage90_rounded_map$decile_v2, levels = c("0",
                                                                                    "[1, 3)",
                                                                                    "3",
                                                                                    "[4, 7)",
                                                                                    "[7,35]"))

outage90_rounded_map_bivar <- outage90_rounded_map %>% 
  mutate(group = paste0(ntile, "-", n_years_available))

outage_decile_labels <- outage90_rounded_map_bivar %>% 
  ungroup() %>% 
  select(ntile, decile_v2) %>% 
  distinct() %>% 
  rename(outages = ntile)

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons

bivariate_color_scale <- tibble(
  "0-1" = color_list_1yrs[5], # 1 year data
  "1-1" = color_list_1yrs[4],
  "2-1" = color_list_1yrs[3],
  "3-1" = color_list_1yrs[2],
  "4-1" = color_list_1yrs[1],
  "0-2" = color_list_2yrs[5], # 2 years data
  "1-2" = color_list_2yrs[4],
  "2-2" = color_list_2yrs[3],
  "3-2" = color_list_2yrs[2],
  "4-2" = color_list_2yrs[1],
  "0-3" = color_list_3yrs[5], # 3 years data
  "1-3" = color_list_3yrs[4],
  "2-3" = color_list_3yrs[3],
  "3-3" = color_list_3yrs[2],
  "4-3" = color_list_3yrs[1],
) %>%
  gather("group", "fill")




# Join color codes to data ------------------------------------------------
po8_bivar <- outage90_rounded_map_bivar %>% 
  left_join(., bivariate_color_scale, by = "group")


# Draw legend -------------------------------------------------------------
# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("outages", "n_years"), sep = "-") %>%
  mutate(outages = as.integer(outages),
         n_years = as.integer(n_years))

bivariate_color_scale <- bivariate_color_scale %>% 
  arrange(desc(n_years), desc(outages)) %>% 
  left_join(., outage_decile_labels, by = ("outages"))

po8_legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = n_years,
      y = decile_v2,
      fill = fill)
  ) +
  scale_fill_identity() +
  theme_map() +
  # make font small enough
  labs(x ="Years of Data",
       y = "Outages") +
  theme(
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()
po8_legend

ggsave(path = here::here("Visuals"),
       filename = "po1_alpha_legend.jpg", 
       dpi=300,
       height=4, width=7, units="in")

# (3) Cust out non std ------------------------------------------------------------
# Gen outage decile colors ------------------------------------------------
outage90_rounded_map <- read_csv(here::here("Data", "Outputs", "bivar_mapping", "custoutnonstd_map_dta.csv")) 

# outage90_rounded_map$decile_v2 <- factor(outage90_rounded_map$decile, levels = c("0",
#                                                                                          "[     0.5,    3113)",
#                                                                                          "[  3113.3,   39786)",
#                                                                                          "[ 39786.0,  163190)",
#                                                                                          "[163190.3,10045051]"))
outage90_rounded_map$decile_v2 <- factor(outage90_rounded_map$decile, levels = c("0",
                                                                                         "[     1,    3169)",
                                                                                         "[  3169,   39825)",
                                                                                         "[ 39825,  163281)",
                                                                                         "[163281,10045051]"))
outage90_rounded_map_bivar <- outage90_rounded_map %>% 
  mutate(group = paste0(ntile, "-", n_years_available))

outage_decile_labels <- outage90_rounded_map_bivar %>% 
  ungroup() %>% 
  select(ntile, decile_v2) %>% 
  distinct() %>% 
  rename(outages = ntile)

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons

bivariate_color_scale <- tibble(
  "0-1" = color_list_1yrs[5], # 1 year data
  "1-1" = color_list_1yrs[4],
  "2-1" = color_list_1yrs[3],
  "3-1" = color_list_1yrs[2],
  "4-1" = color_list_1yrs[1],
  "0-2" = color_list_2yrs[5], # 2 years data
  "1-2" = color_list_2yrs[4],
  "2-2" = color_list_2yrs[3],
  "3-2" = color_list_2yrs[2],
  "4-2" = color_list_2yrs[1],
  "0-3" = color_list_3yrs[5], # 3 years data
  "1-3" = color_list_3yrs[4],
  "2-3" = color_list_3yrs[3],
  "3-3" = color_list_3yrs[2],
  "4-3" = color_list_3yrs[1],
) %>%
  gather("group", "fill")



# Join color codes to data ------------------------------------------------
custout_nonstd_bivar <- outage90_rounded_map_bivar %>% 
  left_join(., bivariate_color_scale, by = "group")


# Draw legend -------------------------------------------------------------
# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("outages", "n_years"), sep = "-") %>%
  mutate(outages = as.integer(outages),
         n_years = as.integer(n_years))

bivariate_color_scale <- bivariate_color_scale %>% 
  arrange(desc(n_years), desc(outages)) %>% 
  left_join(., outage_decile_labels, by = ("outages"))

custout_nonstd_legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = n_years,
      y = decile_v2,
      fill = fill)
  ) +
  scale_fill_identity() +
  theme_map() +
  # make font small enough
  labs(x ="Years of Data",
       y = "Customers Out") +
  theme(
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()
custout_nonstd_legend

ggsave(path = here::here("Visuals"),
       filename = "custout_nonstd_alpha_legend.jpg", 
       dpi=300,
       height=4, width=7, units="in")



# (4) Cust out std ------------------------------------------------------------
# Gen outage decile colors ------------------------------------------------
outage90_rounded_map <- read_csv(here::here("Data", "Outputs", "bivar_mapping", "custoutstd_map_dta.csv")) 

# outage90_rounded_map$decile_v2 <- factor(outage90_rounded_map$decile, levels = c("0",
#                                                                                    "[0.01, 1)",
#                                                                                    "[   1, 5)",
#                                                                                    "[   5, 20)",
#                                                                                    "[20,251.36]"))

outage90_rounded_map$decile_v2 <- factor(outage90_rounded_map$decile, levels = c("0",
                                                                                   "1",
                                                                                   "(   1, 5)",
                                                                                   "[   5, 20)",
                                                                                   "[20,251]"))

outage90_rounded_map_bivar <- outage90_rounded_map %>% 
  mutate(group = paste0(ntile, "-", n_years_available))

outage_decile_labels <- outage90_rounded_map_bivar %>% 
  ungroup() %>% 
  select(ntile, decile_v2) %>% 
  distinct() %>% 
  rename(outages = ntile)

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons

bivariate_color_scale <- tibble(
  "0-1" = color_list_1yrs[5], # 1 year data
  "1-1" = color_list_1yrs[4],
  "2-1" = color_list_1yrs[3],
  "3-1" = color_list_1yrs[2],
  "4-1" = color_list_1yrs[1],
  "0-2" = color_list_2yrs[5], # 2 years data
  "1-2" = color_list_2yrs[4],
  "2-2" = color_list_2yrs[3],
  "3-2" = color_list_2yrs[2],
  "4-2" = color_list_2yrs[1],
  "0-3" = color_list_3yrs[5], # 3 years data
  "1-3" = color_list_3yrs[4],
  "2-3" = color_list_3yrs[3],
  "3-3" = color_list_3yrs[2],
  "4-3" = color_list_3yrs[1],
) %>%
  gather("group", "fill")



# Join color codes to data ------------------------------------------------
custout_std_bivar <- outage90_rounded_map_bivar %>% 
  left_join(., bivariate_color_scale, by = "group")


# Draw legend -------------------------------------------------------------
# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("outages", "n_years"), sep = "-") %>%
  mutate(outages = as.integer(outages),
         n_years = as.integer(n_years))

bivariate_color_scale <- bivariate_color_scale %>% 
  arrange(desc(n_years), desc(outages)) %>% 
  left_join(., outage_decile_labels, by = ("outages"))

custout_std_legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = n_years,
      y = decile_v2,
      fill = fill)
  ) +
  scale_fill_identity() +
  theme_map() +
  # make font small enough
  labs(x ="Years of Data",
       y = "Standardized Customers Out") +
  theme(
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()
custout_std_legend

ggsave(path = here::here("Visuals"),
       filename = "custout_std_alpha_legend.jpg", 
       dpi=300,
       height=4, width=7, units="in")

