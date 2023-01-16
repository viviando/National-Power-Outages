
# 1+ hour outages ------------------------------------------------ 
bivar_tmp <- po1_bivar %>% 
  select(fips, group, fill) %>% 
  mutate(group_num = as.numeric(gsub("-", "", group))) 

bivar_map_dta <- bivar_tmp %>% 
  select(fips, group_num) %>% 
  mutate(group_num = as.factor(group_num))

coloring <- bivar_tmp %>% 
  ungroup() %>% 
  select(group_num, fill) %>% 
  distinct() %>% 
  mutate(group_num = as.factor(group_num))

coloring_named_vector <- c("43" = "#001959", 
                           "32" = "#81ACB1",
                           "31" = "#AED9DE",
                           "33" = "#225B60",
                           "23" = "#808133",
                           "21" = "#E0E19E",
                           "42" = "#7E84AE",
                           "11" = "#FFE6DE",
                           "13" = "#F49E71",
                           "41" = "#B9BEE9",
                           "12" = "#FFCEBB",
                           "22" = "#BFC07C",
                           "3" = "#FCFFA4FF",
                           "2" = "#FEFFD1FF",
                           "1" = "#FEFFE9FF")

po1_map <- plot_usmap(regions = "counties",
                          data = bivar_map_dta,
                          values = "group_num",
                          color = "black",
                          size=0.2) +
  theme_minimal() +
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # labs(title = "County Yearly Average of 1+ Hour Outage Events") +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none") +
  # color municipalities according to their decile-year combination +
  ggplot2::scale_fill_manual(values = coloring_named_vector, 
                             na.value = "white") 

ggdraw() +
  draw_plot(po1_map, 0, 0, 1, 1) +
  draw_plot(po1_legend, -0.05, 0.075, 0.35, 0.35) +
  annotate("text",  x=Inf, y = Inf, label = "b", vjust=1.5, hjust=25, size = 12)


ggsave(path = here::here("Visuals"),
       filename = "alpha_po1.jpg", 
       dpi=300,
       height=4, width=7, units="in")

# 8+ hour outages ------------------------------------------------ 
bivar_tmp <- po8_bivar %>% 
  select(fips, group, fill) %>% 
  mutate(group_num = as.numeric(gsub("-", "", group)))

bivar_map_dta <- bivar_tmp %>% 
  select(fips, group_num) %>% 
  mutate(group_num = as.factor(group_num))

coloring <- bivar_tmp %>% 
  ungroup() %>% 
  select(group_num, fill) %>% 
  distinct()

po8_map <- plot_usmap(regions = "counties",
           data = bivar_map_dta,
           values = "group_num",
           color = "black",
           size=0.2) +
  theme_minimal() +
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # labs(title = "County Yearly Average of 8+ Hour Outage Events") +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none") +
  # color municipalities according to their decile-year combination +
  ggplot2::scale_fill_manual(values = coloring_named_vector,
                             na.value = "white") 

ggdraw() +
  draw_plot(po8_map, 0, 0, 1, 1) +
  draw_plot(po8_legend, -0.05, 0.075, 0.35, 0.35) +
  annotate("text",  x=Inf, y = Inf, label = "a", vjust=1.5, hjust=25, size = 12)

ggsave(path = here::here("Visuals"),
       filename = "alpha_po8.jpg", 
       dpi=300,
       height=4, width=7, units="in")

# Cust out non std hour outages ------------------------------------------------ 
#below is how we create the var
# customer_time_nonstd <- outage_event_expanded %>% 
#   select(clean_state_name, clean_county_name, fips, year, downscaled_county_estimate, customers_out_total) %>%
#   dplyr::group_by(year, fips) %>% 
#   mutate(sum_customer_time = sum(customers_out_total, na.rm = TRUE)) %>% 
#   filter(row_number() == 1) %>% 
#   dplyr::group_by(fips) %>% 
#   mutate(avg_customers_out = mean(sum_customer_time, na.rm = TRUE)) %>% 
#   filter(row_number() == 1) %>% 
#   select(-sum_customer_time)

bivar_tmp <- custout_nonstd_bivar %>% 
  select(fips, group, fill) %>% 
  mutate(group_num = as.numeric(gsub("-", "", group)))

bivar_map_dta <- bivar_tmp %>% 
  select(fips, group_num) %>% 
  mutate(group_num = as.factor(group_num))

coloring <- bivar_tmp %>% 
  ungroup() %>% 
  select(group_num, fill) %>% 
  distinct()

custout_nonstd_map <- plot_usmap(regions = "counties",
           data = bivar_map_dta,
           values = "group_num",
           color = "black",
           size=0.2) +
  theme_minimal() +
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # labs(title = "Averaged Yearly Total Customer Hours") +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none") +
  # color municipalities according to their decile-year combination +
  ggplot2::scale_fill_manual(values = coloring_named_vector,
                             na.value = "white") 

ggdraw() +
  draw_plot(custout_nonstd_map, 0, 0, 1, 1) +
  draw_plot(custout_nonstd_legend, -0.05, 0.075, 0.35, 0.35) #+
  # annotate("text",  x=Inf, y = Inf, label = "a", vjust=1.5, hjust=27, size = 12)

ggsave(path = here::here("Visuals"),
       filename = "alpha_custout_nonstd.jpg", 
       dpi=300,
       height=4, width=8, units="in")

# Cust out std hour outages ------------------------------------------------ 

#below is how we calculate std outage hours
# total yearly customer hours per customer averaged across the study period
# maybe title should be "Averaged Yearly Customer Hours Per Customer"
# customer_time_std <- outage_event_expanded %>%
#   group_by(fips, year) %>% 
#   mutate(sum_customer_time = sum(customers_out_total, na.rm = TRUE),
#          customer_time_std = sum_customer_time/downscaled_county_estimate) %>% 
#   filter(row_number() == 1) %>% 
#   group_by(fips) %>%
#   mutate(avg_customer_time_std = mean(customer_time_std)) %>%
#   select(clean_state_name, clean_county_name, fips, avg_customer_time_std) %>%
#   filter(row_number() == 1)

bivar_tmp <- custout_std_bivar %>% 
  select(fips, group, fill) %>% 
  mutate(group_num = as.numeric(gsub("-", "", group)))

bivar_map_dta <- bivar_tmp %>% 
  select(fips, group_num) %>% 
  mutate(group_num = as.factor(group_num))

coloring <- bivar_tmp %>% 
  ungroup() %>% 
  select(group_num, fill) %>% 
  distinct()

custout_std_map <- plot_usmap(regions = "counties",
           data = bivar_map_dta,
           values = "group_num",
           color = "black",
           size=0.2) +
  theme_minimal() +
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # labs(title = "Averaged Yearly Customer Hours Per Customer") +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none") +
  # color municipalities according to their decile-year combination +
  ggplot2::scale_fill_manual(values = coloring_named_vector,
                             na.value = "white") 

ggdraw() +
  draw_plot(custout_std_map, 0, 0, 1, 1) +
  draw_plot(custout_std_legend, -0.05, 0.075, 0.35, 0.35) #+
  # annotate("text",  x=Inf, y = Inf, label = "b", vjust=1.5, hjust=25, size = 12)

ggsave(path = here::here("Visuals"),
       filename = "alpha_custout_std.jpg", 
       dpi=300,
       height=4, width=7, units="in")

