
# title: "HEMS Benefit Data analysis"
# author: "Lars Eide Næss"
# contact: "lars.eide.ness@stolav.no"
# revision date: 2026-03-23

# Preparation ----
## Setup ----

# Clearing the workspace and setting local settings -
rm(list = ls())
setwd("C:/YOUR_DIRECTORY")

# Loading in libraries -
library(tidyverse)
library(janitor)
library(writexl)
library(patchwork)
library(ggtext)
library(marginaleffects)
library(splines)
library(grid)
library(cowplot)
library(magick)

## Data import ----
load(file = "./Data/hemsqi_data.RData")
study_data <- hemsqi %>% filter(included == 1)

## Analysis configuration ---- 

### Colors ----
benefit_colors <- c(
  "Logistical benefit" = "#91beff",
  "Medical benefit" = "#ffe087",
  "No benefit" = "#B5B5B5"
  )

logistic_colors <- c(
  "Time benefit" = "#ffb3ba",
  "Inaccessible" = "#baffc9"
  )

medical_colors <- c(
  "HEMS procedures" = "#ffb3ba",
  "Other procedures" = "#bae1ff",
  "Defer treatment" = "#ffffba",
  "Difficult situation" = "#baffc9"
  )

### Regression variables ----
reg_vars_def  <- "ns(age,4) + year + season + weekpart + shift + gender + doc"
reg_vars_nmi  <- "ns(age,4) + year + season + weekpart + shift + gender + doc + nmi"

### Analysis functions ----
benefit_figA <- function(data, scenario_label, filter_expr) {
  data %>%
    filter({{ filter_expr }}) %>%
    summarise(
      log = sum(qi_logistical_benefit == "Yes", na.rm = TRUE),
      med = sum(qi_medical_benefit == "Yes", na.rm = TRUE),
      dbl = sum(qi_logistical_benefit == "Yes" & qi_medical_benefit == "Yes", na.rm = TRUE),
      no  = sum(
        (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)) &
          (qi_medical_benefit    != "Yes" | is.na(qi_medical_benefit)) &
          (qi_logistical_benefit == "No" | qi_medical_benefit == "No") &
          !(is.na(qi_logistical_benefit) & is.na(qi_medical_benefit)),
        na.rm = TRUE
      )
    ) %>%
    {
      log <- .$log
      med <- .$med
      dbl <- .$dbl
      no  <- .$no
      total <- log + med + no - dbl
      dbl_pct <- round(dbl / total * 100)
      
      tibble(
        scenario = scenario_label,
        category = c("Logistical benefit", "Medical benefit", "No benefit"),
        xmin = c(0, log - dbl, log + med - dbl),
        xmax = c(log, log + med - dbl, log + med - dbl + no),
        ymin = c(0.29, 0.31, 0.30),
        ymax = c(0.59, 0.61, 0.60),
        alpha = 0.4,
        y = NA,
        dbl_pct = dbl_pct
      )
    }
}


benefit_figB <- function(data, scenario_label, filter_expr) {
  data %>%
    filter({{ filter_expr }}) %>%
    summarise(
      log = sum(qi_logistical_benefit == "Yes", na.rm = TRUE),
      med = sum(qi_medical_benefit == "Yes", na.rm = TRUE),
      dbl = sum(qi_logistical_benefit == "Yes" & qi_medical_benefit == "Yes", na.rm = TRUE),
      no  = sum(
        (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)) &
          (qi_medical_benefit    != "Yes" | is.na(qi_medical_benefit)) &
          (qi_logistical_benefit == "No" | qi_medical_benefit == "No") &
          !(is.na(qi_logistical_benefit) & is.na(qi_medical_benefit)),
        na.rm = TRUE
      )
    ) %>%
    {
      log <- .$log
      med <- .$med
      dbl <- .$dbl
      no  <- .$no
      total <- log + med + no - dbl
      dbl_pct <- round(dbl / total * 100)
      
      tibble(
        scenario = scenario_label,
        category = c("Logistical benefit", "Medical benefit", "No benefit"),
        xmin = c(0, log - dbl, log + med - dbl),
        xmax = c(log, log + med - dbl, log + med - dbl + no),
        ymin = c(0.3, 0.4, 0.35),
        ymax = c(0.6, 0.65, 0.62),
        alpha = 0.4,
        y = NA,
        dbl_pct = dbl_pct
      )
    }
}


# Results ----

## Figures ----

### Figure 1: Map ----

# Trondheim HEMS coordinates
trh <- c(10.374228, 63.368063)

# Inset map
inset_europe <- ggplot(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
                         dplyr::filter(region_un == "Europe")) +
  geom_sf(fill = "grey90", color = "white") +
  annotate("rect", xmin = 6, xmax = 14, ymin = 61, ymax = 66,
           fill = NA, color = "red", linewidth = 0.3) +
  coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE) +
  theme_void() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

#Base data
map_data <- study_data %>%
  filter(hm_vessel != "Other")

hlp <- scales::comma(sum(map_data$hm_vessel == "Helicopter"))
rrc <- scales::comma(sum(map_data$hm_vessel == "RRC"))

#Population data
popdata <- read.csv2(
  file          = "./Resources/2024_SSB_Areal og befolkning per kommune (11342).csv",
  sep           = ";",
  quote         = "\"",
  dec           = ",",
  fileEncoding  = "UTF-8",
  check.names   = FALSE,
  stringsAsFactors = FALSE
) %>%
  separate(region, into = c("kommune_nr", "kommune_navn"),
           sep = " ", extra = "merge")

# Shared base map data (filtered municipalities)
base_map <- csmaps::nor_municip_map_b2024_default_dt %>%
  filter(
    str_detect(location_code, "^municip_nor50")  |
      str_detect(location_code, "^municip_nor15")  |
      str_detect(location_code, "^municip_nor342") |
      str_detect(location_code, "^municip_nor343") |
      str_detect(location_code, "^municip_nor182") |
      str_detect(location_code, "^municip_nor181")
  ) %>%
  left_join(csdata::nor_locations_names(border = 2024), by = "location_code") %>%
  mutate(municipality_nr = str_extract(location_code, "\\d{4}"))

# Shared HEMS count data (joined separately per vessel)
hems_counts <- map_data %>%
  mutate(vessel = case_when(
    hm_vessel == "Helicopter" ~ "Helicopter",
    hm_vessel == "RRC"        ~ "Rapid Response Car"
  )) %>%
  group_by(emcc_municipality_nr, vessel) %>%
  summarise(hems_count = n(), .groups = "drop")

# Shared theme for all three maps
map_theme <- theme_minimal() +
  theme(
    axis.title.x    = element_blank(),
    axis.title.y    = element_blank(),
    legend.position = "bottom",
    plot.margin     = margin(0, 4, 0, 4),
    plot.title      = element_text(size = 10, hjust = 0.5,
                                   margin = margin(2, 0, 4, 0))
  )

# Shared coord
map_coord <- coord_map(projection = "conic", parameters = c(trh[2]), expand = FALSE)

# Shared x/y scales
map_scale_x <- scale_x_continuous(
  breaks = function(x) {
    spc <- ceiling((x[2] - x[1]) / 6)
    seq(floor(x[1]) + spc, ceiling(x[2]) - spc, by = spc)
  },
  labels = function(x) paste0(round(x), "\u00b0E")
)
map_scale_y <- scale_y_continuous(
  labels = function(y) paste0(round(y), "\u00b0N")
)

# Shared HEMS fill scale (used to extract legend later)
hems_fill_scale <- scico::scale_fill_scico(
  palette  = "navia",
  labels   = scales::label_number(accuracy = 10),
  direction = -1,
  na.value = "grey80",
  limits   = c(0, 450),
  guide    = guide_colorbar(
    barheight      = 0.5,
    barwidth       = 20,
    ticks          = FALSE,
    direction      = "horizontal",
    title.position = "top",
    title.hjust    = 0.5
  )
)

# --- a) Helicopter map ---
fig_hlp <- base_map %>%
  left_join(
    hems_counts %>% filter(vessel == "Helicopter"),
    by = c("municipality_nr" = "emcc_municipality_nr")
  ) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = hems_count)) +
  geom_polygon() +
  geom_path(color = "black", linewidth = 0.1) +
  map_scale_x +
  map_scale_y +
  labs(
    title = paste0("a) Helicopter missions (n = ", hlp, ")"),
    fill  = "Completed HEMS dispatches per municipality 2022-2024"
  ) +
  hems_fill_scale +
  geom_point(aes(x = trh[1], y = trh[2]),
             shape = 21, color = "white", fill = "red",
             size = 2.0, stroke = 1.0, show.legend = FALSE) +
  geom_label(
    data = data.frame(x = 5.5, y = 61.0, label = "    Trondheim HEMS"),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE, size = 2, color = "black", fill = "white",
    linewidth = 0.3, hjust = 0, label.padding = unit(0.3, "lines")
  ) +
  geom_point(
    data = data.frame(x = 5.7, y = 61.0),
    aes(x = x, y = y),
    inherit.aes = FALSE, shape = 21,
    color = "white", fill = "red", size = 2.0, stroke = 1.0
  ) +
  map_coord +
  map_theme

# --- b) Rapid Response Car map ---
fig_rrc <- base_map %>%
  left_join(
    hems_counts %>% filter(vessel == "Rapid Response Car"),
    by = c("municipality_nr" = "emcc_municipality_nr")
  ) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = hems_count)) +
  geom_polygon() +
  geom_path(color = "black", linewidth = 0.1) +
  map_scale_x +
  map_scale_y +
  labs(
    title = paste0("b) Rapid response car missions (n = ", rrc, ")"),
    fill  = "Completed HEMS dispatches per municipality 2022-2024"
  ) +
  hems_fill_scale +
  geom_point(aes(x = trh[1], y = trh[2]),
             shape = 21, color = "white", fill = "red",
             size = 2.0, stroke = 1.0, show.legend = FALSE) +
  map_coord +
  map_theme

# --- c) Population density map ---
fig_pop <- base_map %>%
  left_join(
    popdata %>%
      select(kommune_nr,
             pop_density = `2024 Innbyggere per km² landareal`),
    by = c("municipality_nr" = "kommune_nr")
  ) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = pop_density)) +
  geom_polygon() +
  geom_path(color = "black", linewidth = 0.1) +
  map_scale_x +
  map_scale_y +
  labs(
    title = "c) Population density",
    fill  = "Population per km²"
  ) +
  scico::scale_fill_scico(
    palette   = "lajolla",
    labels    = scales::label_number(accuracy = 10),
    direction = -1,
    na.value  = "grey80",
    limits    = c(0, 450),
    guide     = guide_colorbar(
      barheight      = 0.45,
      barwidth       = 14,
      ticks          = FALSE,
      direction      = "horizontal",
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  geom_point(aes(x = trh[1], y = trh[2]),
             shape = 21, color = "white", fill = "red",
             size = 2.0, stroke = 1.0, show.legend = FALSE) +
  map_coord +
  map_theme

# --- Assembly ---

fig_hlp_noleg <- fig_hlp + theme(legend.position = "none")
fig_rrc_noleg <- fig_rrc + theme(legend.position = "none")
fig_pop_noleg <- fig_pop + theme(legend.position = "none")

aligned <- cowplot::align_plots(
  fig_hlp_noleg, fig_rrc_noleg, fig_pop_noleg,
  align = "hv", axis = "tblr"
)

aligned[[1]] <- cowplot::ggdraw(aligned[[1]]) +
  cowplot::draw_plot(
    inset_europe,
    x = 0.15, y = 0.6, width = 0.28, height = 0.20
  )

top_row <- cowplot::plot_grid(
  aligned[[1]], aligned[[2]], aligned[[3]],
  nrow = 1, rel_widths = c(1, 1, 1)
)

legend_hems <- cowplot::get_legend(
  fig_hlp + theme(legend.position = "bottom", legend.box = "horizontal")
)
legend_pop <- cowplot::get_legend(
  fig_pop + theme(legend.position = "bottom", legend.box = "horizontal")
)

bottom_row <- cowplot::plot_grid(
  legend_hems, legend_pop,
  nrow = 1, rel_widths = c(2, 1)
)

figure1 <- cowplot::ggdraw() +
  cowplot::draw_plot(top_row,    x = 0, y = 0.11, width = 1, height = 0.89) +
  cowplot::draw_plot(bottom_row, x = 0, y = 0,    width = 1, height = 0.11)

ggsave(
  filename = "./Figures/Figure1 - catchment area.png",
  plot     = figure1,
  width    = 10,
  height = 5.4,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Magick-trimming remove excess white space
magick::image_read("./Figures/Figure1 - catchment area.png") %>%
  magick::image_trim() %>%
  magick::image_border("white", "30x30") %>%
  magick::image_write("./Figures/Figure1 - catchment area.png")


#Cleanup
rm(aligned, base_map, bottom_row, fig_hlp, fig_hlp_noleg, fig_pop, fig_pop_noleg, fig_rrc, fig_rrc_noleg,
   hems_counts, hems_fill_scale, legend_hems, legend_pop, map_coord, map_scale_x, map_scale_y, map_data,
   map_theme, popdata, top_row, trh, inset_europe, hlp, rrc)


### Figure 2: Flowchart ----

# Counting cases -
fig2_data <- c(
  "HEMS requests"       = nrow(hemsqi),
  "Rejected requests"   = hemsqi %>% filter(hm_dispatch == "Rejected") %>% nrow(),
  "HEMS dispatches"     = hemsqi %>% filter(hm_dispatch != "Rejected") %>% nrow(),
  "Aborted dispatches"  = hemsqi %>% filter(hm_dispatch == "Aborted") %>% nrow(),
  "No patient contact"  = hemsqi %>% filter(hm_dispatch == "Completed" & eligible == 0) %>% nrow(),
  "Eligible missions"   = hemsqi %>% filter(eligible == 1) %>%  nrow(),
  "QI data"             = hemsqi %>% filter(eligible ==1 & included == 1) %>% nrow(),
  "Missing QI data"     = hemsqi %>% filter(eligible ==1 & included != 1) %>% nrow()
)

# Ploting flowchart -
fig2 <- DiagrammeR::grViz(glue::glue("
digraph hems_flow {{
  graph [layout = dot, rankdir = TB]

  node [shape = box, fontname = 'sans', fontsize = 12, style = filled, color = black]

  hems_requests [label = 'HEMS requests\\n n = {fig2_data['HEMS requests']}', fillcolor = '#E5E5E5']
  rejected       [label = 'Rejected requests\\n n = {fig2_data['Rejected requests']}', fillcolor = '#F4A6A6']
  hems_dispatch  [label = 'HEMS dispatches\\n n = {fig2_data['HEMS dispatches']}', fillcolor = '#E5E5E5']
  aborted        [label = 'Aborted dispatches\\n n = {fig2_data['Aborted dispatches']}', fillcolor = '#F4A6A6']
  no_patient     [label = 'No patient contact\\n n = {fig2_data['No patient contact']}', fillcolor = '#F4A6A6']
  eligible       [label = 'Eligible missions\\n n = {fig2_data['Eligible missions']}', fillcolor = '#91BEFF']
  qi_data        [label = 'QI data\\n n = {fig2_data['QI data']}', fillcolor = '#DFF0D8']
  missing_qi     [label = 'Missing QI data\\n n = {fig2_data['Missing QI data']}', fillcolor = '#F4A6A6']

  hems_requests -> rejected
  hems_requests -> hems_dispatch
  hems_dispatch -> aborted
  hems_dispatch -> no_patient
  hems_dispatch -> eligible
  eligible -> qi_data [style = dashed]
  eligible -> missing_qi [style = dashed]
}}
"))



figure2 <- DiagrammeRsvg::export_svg(fig2)
rsvg::rsvg_png(
  charToRaw(figure2),
  file = "./Figures/Raw/Figure2 - flowchart_sketch.png",
  width = 3000,
  height = 2000
  )

#Cleanup
rm(fig2_data, fig2)


### Figure 3: Association with benefit I ----

fig3a_data <- benefit_figA(study_data, "Overall benefit", TRUE) %>%
  mutate(
    category = factor(
      category,
      levels = c(
        "Logistical benefit",
        "Medical benefit",
        "No benefit"
      )
    )
  ) %>%
  group_by(scenario) %>%
  mutate(
    total = max(xmax),
    xmin_pct = xmin / total * 100,
    xmax_pct = xmax / total * 100
  ) %>%
  ungroup() %>%
  mutate(
    y = 0.5,
    label_pos = (xmin_pct + xmax_pct) / 2,
    label_text = paste0(round(xmax_pct - xmin_pct, 0), "%"),
    y_label = y + (ymin + ymax) / 2
  )



fig3a <- ggplot(fig3a_data) +
  geom_rect(aes(xmin = xmin_pct, xmax = xmax_pct, ymin = y + ymin, ymax = y + ymax, fill = category, alpha = alpha),
            color = "black") +
  geom_text(aes(x = label_pos, y = y_label, label = label_text),
            size = 3, color = "black") +
  geom_textbox(
    data = data.frame(
      x = 52, y = 0.90,
      label = glue::glue("Both <b>logistical</b> and <b>medical</b> benefit <br> {fig3a_data$dbl_pct[1]}%")
    ),
    aes(x = x, y = y, label = label),
    box.colour = "black",
    fill = "white",
    width = unit(3, "cm"),
    halign = 0.5,
    valign = 0.5,
    hjust = 0,
    vjust = 1,
    size = 3
  ) +
  geom_curve(
    data = data.frame(x = 52, y = 0.88, xend = 40, yend = 0.92),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 20),
    curvature = -0.3,
    color = "black"
  )+
  annotate("text", x = -Inf, y = Inf, label = "a)", 
           hjust = -0.2, vjust = 1.2, fontface = "plain", size = 4)+
  scale_fill_manual(values = adjustcolor(benefit_colors, alpha.f = 0.7)) +
  scale_alpha_identity() +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  scale_y_continuous(breaks = c(1.5), labels = c("Overall benefit")) +
  labs(
    title = "Overall",
    x = "Proportion (%)",
    y = NULL,
    fill = "Benefit",
    caption = paste0("Overall benefit (n = ", unique(fig3a_data$total), ")")
  )+
  theme_void() +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    plot.caption = element_text(size = 14, face = "bold",hjust = 0.1, vjust = -1),
    legend.box.margin = margin(t = 50),
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.margin = margin(t = 50, r = 10, b = 40, l = 10)
  )


fig3b_data <- bind_rows(
  benefit_figB(study_data, "Primary mission", hm_type_cat == "Primary"),
  benefit_figB(study_data, "Secondary mission", hm_type_cat == "Secondary"),
  benefit_figB(study_data, "Helicopter", hm_vessel == "Helicopter"),
  benefit_figB(study_data, "Rapid response car", hm_vessel == "RRC")
  ) %>%
  mutate(
    category = factor(
      category,
      levels = c(
        "Logistical benefit",
        "Medical benefit",
        "No benefit"
      )
    )
  ) %>%
  group_by(scenario) %>%
  mutate(
    total = max(xmax),
    xmin_pct = xmin / total * 100,
    xmax_pct = xmax / total * 100
  ) %>%
  ungroup() %>%
  mutate(
    y = as.numeric(factor(scenario, levels = rev(unique(scenario))))
  ) %>%
  mutate(
    bar_height = ymax - ymin,
    label_pos = (xmin_pct + xmax_pct) / 2,
    label_text = paste0(round(xmax_pct - xmin_pct, 0), "%"),
    y_label = ifelse(bar_height < 0.1, y + ymax + 0.05, y + ymin + bar_height / 2)
  )



fig3b <- ggplot(fig3b_data %>% filter(!(scenario == "Rapid response car" & category == "Logistical benefit"))) +
  geom_rect(aes(xmin = xmin_pct, xmax = xmax_pct, ymin = y + ymin, ymax = y + ymax, fill = category, alpha = alpha),
            color = "black") +
  geom_hline(yintercept = 3.0, linetype = "dashed", color = "black") +
  geom_text(aes(x = label_pos, y = y_label, label = label_text),
            size = 2, color = "black") +
  geom_textbox(
    data = data.frame(
      x = 54, y = 4.25,
      label = glue::glue("Both <b>logistical</b> and <b>medical</b> benefit {fig3b_data$dbl_pct[1]}%")
    ),
    aes(x = x, y = y, label = label),
    box.colour = "black",
    fill = "white",
    width = unit(4.05, "cm"),
    height = unit(0.5, "cm"),
    halign = 0.5,
    valign = 0.5,
    hjust = 0,
    vjust = 1,
    size = 2
  ) +
  geom_curve(
    data = data.frame(x = 54, y = 4.15, xend = 40, yend = 4.55),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 20),
    curvature = -0.3,
    color = "black"
  )+
  geom_textbox(
    data = data.frame(
      x = 54, y = 3.25,
      label = glue::glue("Both <b>logistical</b> and <b>medical</b> benefit {fig3b_data$dbl_pct[5]}%")
    ),
    aes(x = x, y = y, label = label),
    box.colour = "black",
    fill = "white",
    width = unit(4.05, "cm"),
    height = unit(0.5, "cm"),
    halign = 0.5,
    valign = 0.5,
    hjust = 0,
    vjust = 1,
    size = 2
  ) +
  geom_curve(
    data = data.frame(x = 54, y = 3.15, xend = 40, yend = 3.55),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 20),
    curvature = -0.3,
    color = "black"
  )+
  geom_textbox(
    data = data.frame(
      x = 54, y = 2.2,
      label = glue::glue("Both <b>logistical</b> and <b>medical</b> benefit {fig3b_data$dbl_pct[9]}%")
    ),
    aes(x = x, y = y, label = label),
    box.colour = "black",
    fill = "white",
    width = unit(4.05, "cm"),
    height = unit(0.5, "cm"),
    halign = 0.5,
    valign = 0.5,
    hjust = 0,
    vjust = 1,
    size = 2
  ) +
  geom_curve(
    data = data.frame(x = 54, y = 2.15, xend = 49, yend = 2.55),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 20),
    curvature = -0.3,
    color = "black"
  )+
  scale_fill_manual(values = adjustcolor(benefit_colors, alpha.f = 0.7)) +
  scale_alpha_identity() +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100))+
  scale_y_continuous(
    breaks = unique(fig3b_data$y) + 0.5,
    labels = unique(fig3b_data$scenario)
  ) +
  geom_text(
    data = fig3b_data %>% distinct(scenario, y, total),
    aes(x = 0, y = y + 0.8, label = paste0(scenario, " (n = ", total, ")")),
    hjust = 0,
    size = 4,
    fontface = "bold"
  ) +
  annotate("text", x = -Inf, y = Inf, label = "b)", 
           hjust = -0.2, vjust = 1.2, fontface = "plain", size = 4)+
  labs(
    title = "Assessment of benefit",
    x = "Proportion (%)",
    y = NULL,
    fill = "Benefit"
  ) +
  theme_void() +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )


figure3 <- fig3a|fig3b + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.justification = "center")

ggsave(
  filename = "./Figures/Figure3 - benefit association I.png",
  plot = figure3,
  width = 10,
  units = "in",
  dpi = 300,
  bg = "white"
  )

#Cleanup
rm(fig3a_data, fig3b_data, fig3a, fig3b)



### Figure 4: Association with benefit II ----

fig4a_data <- bind_rows(
  benefit_figB(study_data, "NACA 1-3", hp_naca <= 3),
  benefit_figB(study_data, "NACA 4", hp_naca == 4),
  benefit_figB(study_data, "NACA 5", hp_naca == 5),
  benefit_figB(study_data, "NACA 6", hp_naca == 6),
  benefit_figB(study_data, "NACA 7", hp_naca == 7)
) %>%
  mutate(
    category = factor(
      category,
      levels = c(
        "Logistical benefit",
        "Medical benefit",
        "No benefit"
      )
    )
  ) %>%
  group_by(scenario) %>%
  mutate(
    total = max(xmax),
    xmin_pct = xmin / total * 100,
    xmax_pct = xmax / total * 100
  ) %>%
  ungroup() %>%
  mutate(
    y = as.numeric(factor(scenario, levels = rev(unique(scenario))))
  ) %>%
  mutate(
    bar_height = ymax - ymin,
    label_pos = (xmin_pct + xmax_pct) / 2,
    label_text = paste0(round(xmax_pct - xmin_pct, 0), "%"),
    y_label = ifelse(bar_height < 0.1, y + ymax + 0.05, y + ymin + bar_height / 2)
  )



fig4a <- ggplot(fig4a_data) +
  geom_rect(aes(xmin = xmin_pct, xmax = xmax_pct, ymin = y + ymin, ymax = y + ymax, fill = category, alpha = alpha),
            color = "black") +
  geom_text(aes(x = label_pos, y = y_label, label = label_text),
            size = 2, color = "black") +
  scale_fill_manual(values = adjustcolor(benefit_colors, alpha.f = 0.7)) +
  scale_alpha_identity() +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100))+
  scale_y_continuous(
    breaks = unique(fig4a_data$y) + 0.5,
    labels = unique(fig4a_data$scenario)
  ) +
  geom_text(
    data = fig4a_data %>% distinct(scenario, y, total),
    aes(x = 0, y = y + 0.9, label = paste0(scenario, " (n = ", total, ")")),
    hjust = 0,
    size = 3,
    fontface = "bold"
  ) +
  annotate("text", x = -Inf, y = Inf, label = "a)", 
           hjust = -0.2, vjust = 1.2, fontface = "plain", size = 4)+
  labs(
    title = "Assessment of benefit",
    x = "Proportion (%)",
    y = NULL,
    fill = "Benefit"
  ) +
  theme_void() +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )


fig4b_data <- bind_rows(
  benefit_figB(study_data, "0 advanced medical procedures", hp_proc_sum == 0),
  benefit_figB(study_data, "1 advanced medical procedure", hp_proc_sum == 1),
  benefit_figB(study_data, "2 advanced medical procedures", hp_proc_sum == 2),
  benefit_figB(study_data, "3-4 advanced medical procedures", hp_proc_sum == 3 | hp_proc_sum == 4),
  benefit_figB(study_data, "5+ advanced medical procedures", hp_proc_sum >= 5)
) %>%
  mutate(
    category = factor(
      category,
      levels = c(
        "Logistical benefit",
        "Medical benefit",
        "No benefit"
      )
    )
  ) %>%
  group_by(scenario) %>%
  mutate(
    total = max(xmax),
    xmin_pct = xmin / total * 100,
    xmax_pct = xmax / total * 100
  ) %>%
  ungroup() %>%
  mutate(
    y = as.numeric(factor(scenario, levels = rev(unique(scenario))))
  ) %>%
  mutate(
    bar_height = ymax - ymin,
    label_pos = (xmin_pct + xmax_pct) / 2,
    label_text = paste0(round(xmax_pct - xmin_pct, 0), "%"),
    y_label = ifelse(bar_height < 0.1, y + ymax + 0.05, y + ymin + bar_height / 2)
  )



fig4b <- ggplot(fig4b_data) +
  geom_rect(aes(xmin = xmin_pct, xmax = xmax_pct, ymin = y + ymin, ymax = y + ymax, fill = category, alpha = alpha),
            color = "black") +
  geom_text(aes(x = label_pos, y = y_label, label = label_text),
            size = 2, color = "black") +
  scale_fill_manual(values = adjustcolor(benefit_colors, alpha.f = 0.7)) +
  scale_alpha_identity() +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100))+
  scale_y_continuous(
    breaks = unique(fig4b_data$y) + 0.5,
    labels = unique(fig4b_data$scenario)
  ) +
  geom_text(
    data = fig4b_data %>% distinct(scenario, y, total),
    aes(x = 0, y = y + 0.9, label = paste0(scenario, " (n = ", total, ")")),
    hjust = 0,
    size = 3,
    fontface ="bold"
  ) +
  annotate("text", x = -Inf, y = Inf, label = "b)", 
           hjust = -0.2, vjust = 1.2, fontface = "plain", size = 4)+
  labs(
    title = "Assessment of benefit",
    x = "Proportion (%)",
    y = NULL,
    fill = "Benefit"
  ) +
  theme_void() +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )


figure4 <- fig4a/fig4b
#figure4 <- ((fig4a|fig4b)/fig4c)

ggsave(
  filename = "./Figures/Figure4 - benefit association II.png",
  plot = figure4,
  width = 10,
  units = "in",
  dpi = 300,
  bg = "white"
  )

#Cleanup
rm(fig4a_data, fig4a, fig4b_data, fig4b)

### Figure 5: Regression: Benefit prediction - Descriptives ----

# Regression data for prediction of benefit
regdata_pred <- study_data %>%
  filter(!is.na(qi_logistical_benefit) | !is.na(qi_medical_benefit)) %>%
  transmute(
    
    qi_log = ifelse(qi_logistical_benefit == "Yes", 1, 0) %>% replace_na(0),
    
    qi_med = ifelse(qi_medical_benefit == "Yes", 1, 0) %>% replace_na(0),
    
    qi_no = ifelse(
      (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)) &
        (qi_medical_benefit    != "Yes" | is.na(qi_medical_benefit)) &
        (qi_logistical_benefit == "No" | qi_medical_benefit == "No"),
      1, 0
    ),
    
    year = factor(alarm_year),
    
    season = factor(alarm_season, levels = c("Winter (Dec-Feb)", "Spring (Mar-May)", "Summer (Jun-Aug)", "Autumn (Sep-Nov)"), ordered = FALSE),
    
    weekpart = factor(alarm_weekpart, levels = rev(c("Weekday", "Weekend")), ordered = FALSE) %>% 
      fct_rev(),
    
    shift = factor(alarm_shift, levels = rev(c("0000-0600", "0600-1200", "1200-1800", "1800-2400")), ordered = FALSE) %>% 
      fct_rev(),
    
    
    gender = factor(
      hp_sex,
      levels = c("Female", "Male"),
      ordered = FALSE
      ),
    
    age = hp_age,
    
    nmi = emcc_nmi_grp_agg %>%
      factor() %>%
      fct_recode(
        "Unresponsive, NOT breathing normally*" = "Unresponsive, NOT breathing normally",
        "Unresponsive, breathing normally*" = "Unresponsive, breathing normally",
        "Choking/airway obstruction*" = "Choking/airway obstruction",
        "Major disaster*" = "Major disaster"
      ) %>%
      fct_infreq()%>%
      fct_rev(),
    
    
    mtype = factor(
      hm_type_cat,
      levels = c("Primary", "Secondary"),
      ordered = FALSE
    ),
    
    vessel = factor(
      hm_vessel,
      levels = c("Helicopter", "RRC"),
      ordered = FALSE
    ),
    
    
    doc = hm_doc_label %>%
      fct_recode(
        "Physician a" = "doc_a",
        "Physician b" = "doc_b",
        "Physician c" = "doc_c",
        "Physician d" = "doc_d",
        "Physician e" = "doc_e",
        "Physician f" = "doc_f",
        "Physician g" = "doc_g",
        "Physician h" = "doc_h"
      ) 
  )




#Regression models
reg_log_age <- glm(as.formula(paste("qi_log ~",
                                    reg_vars_def)),
                   data = regdata_pred,
                   family = "binomial")

reg_med_age <- glm(as.formula(paste("qi_med ~",
                                    reg_vars_def)),
                   data = regdata_pred,
                   family = "binomial")

reg_no_age  <- glm(as.formula(paste("qi_no  ~",
                                    reg_vars_def)),
                   data = regdata_pred,
                   family = "binomial")

#Predictions
pred_log_age <- avg_predictions(reg_log_age,
                                newdata = datagrid(
                                  age = seq(0, 100, by = 2),
                                  model = reg_log_age),
                                type = "link",
                                transform = plogis,
                                by = "age")

pred_med_age <- avg_predictions(reg_med_age,
                                newdata = datagrid(
                                  age = seq(0, 100, by = 2),
                                  model = reg_med_age),
                                type = "link",
                                transform = plogis,
                                by = "age")

pred_no_age  <- avg_predictions(reg_no_age,
                                 newdata = datagrid(
                                   age = seq(0, 100, by = 2),
                                   model = reg_no_age),
                                type = "link",
                                transform = plogis,
                                by = "age")

#Combining prediction data
preds_age <- bind_rows(
  pred_log_age %>% mutate(type = "log"),
  pred_med_age %>% mutate(type = "med"),
  pred_no_age %>% mutate(type= "no")
  ) %>%
  select(type, age, estimate, conf.low, conf.high) %>%
  mutate(type = factor(type, levels = c(
    "log", "med", "no"
  ))) %>% 
  pivot_wider(names_from = type, values_from = c(estimate, conf.low, conf.high))


# Plotting age predictions
plot_log_age <- ggplot(pred_log_age,aes(x=age,y=estimate))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),
              alpha=.3,
              fill = benefit_colors["Logistical benefit"]
              )+
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Logistical benefit",
    x = "Patient age (years)",
    y = "Predicted probability"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

plot_med_age <- ggplot(pred_med_age,aes(x=age,y=estimate))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),
              alpha=.3,
              fill = benefit_colors["Medical benefit"]
  )+
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Medical benefit",
    x = "Patient age (years)",
    y = "Predicted probability"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


plot_no_age <- ggplot(pred_no_age,aes(x=age,y=estimate))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),
              alpha=.3,
              fill = benefit_colors["No benefit"]
  )+
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "No benefit",
    x = "Patient age (years)",
    y = "Predicted probability",
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0),
    axis.title.y = element_blank()
  )


# Descriptives prediction
reg_desc_log <- glm(as.formula(paste("qi_log ~", reg_vars_def)), data = regdata_pred, family = "binomial")
reg_desc_med <- glm(as.formula(paste("qi_med ~", reg_vars_def)), data = regdata_pred, family = "binomial")
reg_desc_no  <- glm(as.formula(paste("qi_no  ~", reg_vars_def)), data = regdata_pred, family = "binomial")
reg_desc_models <- list("Logistical benefit" = reg_desc_log, "Medical benefit" = reg_desc_med, "No benefit" = reg_desc_no)

#### Regresssion variables (descriptives) ----
pred_vars_desc <- c("year", "season", "weekpart", "shift", "gender")

pred_desc <- map_dfr(names(reg_desc_models), function(name) {
  model <- reg_desc_models[[name]]
  map_dfr(pred_vars_desc, function(v) {
    avg_predictions(model,
                    type = "link",
                    transform = plogis,
                    by = v
                    ) %>%
      rename(var = !!sym(v)) %>%
      mutate(Var = tools::toTitleCase(v), outcome = name)
  })
})

preds_desc <- pred_desc %>%
  select(var, outcome, estimate, conf.low, conf.high) %>%
  mutate(outcome = factor(outcome, levels = c("Logistical benefit", "Medical benefit", "No benefit"))) %>% 
  pivot_wider(names_from = outcome, values_from = c(estimate, conf.low, conf.high))

# Plot descriptives
plot_desc <- ggplot(pred_desc, aes(x = estimate, y = var)) +
  geom_point(aes(color = fct_rev(outcome))) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = fct_rev(outcome)),
                orientation = "y",
                width = 0.2,
                alpha = 1.0) +
  scale_color_manual(values = benefit_colors) +
  scale_x_continuous(
    limits = c(NA, NA),
    breaks = seq(0, 1, by = 0.1)
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Predicted Probabilities of benefit assessment",
    x = "Predicted probability",
    y = "Predictor"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_text(),
    axis.title.y = element_blank()
  ) +
  geom_hline(yintercept = c(2.5, 6.5, 8.5, 12.5), linetype = "dashed", color = "grey70")



figure5 <- plot_grid(
  ggdraw() + 
    draw_label("Predicted probability", angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
  plot_grid(
    plot_grid(
      plot_log_age, plot_med_age, plot_no_age,
      ncol = 1,
      align = "v",
      axis = "lr"
    ),
    plot_desc,
    ncol = 2,
    rel_widths = c(1, 1.1),
    
    labels = c("a)", "b)"),
    label_size = 12,
    label_fontface = "plain",
    label_x = c(-0.05, 0.05),
    label_y = c(0.99, 0.99),
    hjust   = c(0, 0),
    vjust   = c(1, 1)
    
    ),
  NULL,
  ncol = 2,
  rel_heights = c(1, 0.02),
  rel_widths = c(0.05, 1)
)

ggsave(
  filename = "./Figures/Figure5 - benefit prediction I.png",
  plot = figure5,
  width = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)


#Cleanup
rm(reg_log_age, reg_med_age, reg_no_age)
rm(pred_log_age, pred_med_age, pred_no_age)
rm(reg_desc_models)
rm(pred_desc, reg_desc_log, reg_desc_med, reg_desc_no)
rm(plot_log_age, plot_med_age, plot_no_age, plot_desc)
rm(pred_vars_desc)


### Figure 6: Regression: Benefit prediction - NMI ----

#### Regression variables (nmi) ----

reg_nmi_log <- glm(as.formula(paste("qi_log ~", reg_vars_nmi)), data = regdata_pred, family = "binomial")
reg_nmi_med <- glm(as.formula(paste("qi_med ~", reg_vars_nmi)), data = regdata_pred, family = "binomial")
reg_nmi_no  <- glm(as.formula(paste("qi_no  ~", reg_vars_nmi)), data = regdata_pred, family = "binomial")
reg_nmi_models <- list("Logistical benefit" = reg_nmi_log, "Medical benefit" = reg_nmi_med, "No benefit" = reg_nmi_no)



pred_nmi <- map_dfr(names(reg_nmi_models), function(name) {
  avg_predictions(reg_nmi_models[[name]],
                  type = "link",
                  transform = plogis,
                  by = "nmi") %>%
    mutate(outcome = name,
           var = nmi)
})


preds_nmi <- pred_nmi %>% 
  select(var, outcome, estimate, conf.low, conf.high) %>%
  mutate(outcome = factor(outcome, levels = c("Logistical benefit", "Medical benefit", "No benefit"))) %>% 
  pivot_wider(names_from = outcome, values_from = c(estimate, conf.low, conf.high))


figure6 <- ggdraw() +
  draw_plot(
plot_grid(
    ggplot(pred_nmi, aes(x = estimate, y = var)) +
      geom_point(aes(color = outcome)) +
      geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = outcome),
                    orientation = "y",
                    width = 0.2,
                    alpha = 1.0) +
      facet_wrap(~ outcome, scales = "fixed") +
      scale_color_manual(values = benefit_colors) +
      labs(title = "Predicted Probabilities of benefit",
           y = "NMI Category",
           x = "Predicted probability"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(),
        panel.spacing = unit(1.5, "lines")
      ),
    NULL,
    ncol = 1,
    rel_heights = c(1, 0.02)
    ),
  x = 0, y = 0, width = 1, height = 1)

ggsave(
  filename = "./Figures/Figure6 - benefit prediction II.png",
  plot = figure6,
  width = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)


#Cleanup
rm(reg_nmi_models)
rm(pred_nmi)
rm(reg_nmi_log)
rm(reg_nmi_med)
rm(reg_nmi_no)


### Figure 7: Regression: Outcomes predicted by benefit ----

# Regression data for outcome
regdata_out <- study_data %>%
  
  filter(!is.na(qi_logistical_benefit) | !is.na(qi_medical_benefit)) %>%
  
  transmute(
    
    dth_data =   case_when(!is.na(pat_pas_first) & hp_naca != 7  ~ 1,
                      TRUE ~ 0),
    
    hos_data =   case_when(!is.na(he_in) ~ 1,
                      TRUE ~ 0),
    
    
    qi_log = ifelse(qi_logistical_benefit == "Yes", 1, 0) %>% replace_na(0),
    
    qi_med = ifelse(qi_medical_benefit == "Yes", 1, 0) %>% replace_na(0),
    
    qi_no = ifelse(qi_log == 0 & qi_med == 0, 1, 0),
  
    year = factor(alarm_year),
    
    season = factor(alarm_season, levels = c("Winter (Dec-Feb)", "Spring (Mar-May)", "Summer (Jun-Aug)", "Autumn (Sep-Nov)"), ordered = FALSE),
    
    weekpart = factor(alarm_weekpart, levels = rev(c("Weekday", "Weekend")), ordered = FALSE) %>% 
      fct_rev(),
    
    shift = factor(alarm_shift, levels = rev(c("0000-0600", "0600-1200", "1200-1800", "1800-2400")), ordered = FALSE) %>% 
      fct_rev(),
    
    
    gender = factor(
      hp_sex,
      levels = c("Female", "Male"),
      ordered = FALSE
    ) %>% 
      fct_rev(),
    
    age = hp_age,
    
    doc = factor(hm_doc_label),

    d30 = factor(
      pat_d30,
      levels = c(0, 1),
      ordered = FALSE
    ),
    
    los = hs_days,
    
    drg = hs_drg_points,
    
    nmi = emcc_nmi_grp_agg %>%
      factor() %>%
      fct_recode(
        "Unresponsive, NOT breathing normally*" = "Unresponsive, NOT breathing normally",
        "Unresponsive, breathing normally*" = "Unresponsive, breathing normally",
        "Choking/airway obstruction*" = "Choking/airway obstruction",
        "Major disaster*" = "Major disaster"
      ) %>%
      fct_infreq()%>%
      fct_rev()

  )



# Regression models
reg_out_d30 <- glm(as.formula(paste("d30 ~ qi_log*qi_med +",reg_vars_def)),
                   data = regdata_out %>% filter(dth_data == 1),
                   family = "binomial")

reg_out_los <- lm(as.formula(paste("los ~ qi_log*qi_med +", reg_vars_def)),
                  data = regdata_out %>% filter(hos_data == 1))

reg_out_drg <- lm(as.formula(paste("drg ~ qi_log*qi_med +", reg_vars_def)),
                  data = regdata_out %>% filter(hos_data == 1))


#Prediction thirty days mortality (glm)
pred_out_d30 <- bind_rows(
  avg_predictions(
    reg_out_d30,
    variables = list(qi_log = 1),
    type = "link",
    transform = plogis
    ) %>% mutate(outcome = "Logistical benefit"),
  avg_predictions(
    reg_out_d30,
    variables = list(qi_med = 1),
   type = "link",
   transform = plogis
    ) %>% mutate(outcome = "Medical benefit"),
  avg_predictions(
    reg_out_d30,
    variables = list(qi_log = 0, qi_med = 0),
    type = "link",
    transform = plogis
    ) %>% mutate(outcome = "No benefit")
) %>%
  select(outcome, estimate, conf.low, conf.high) %>%
  mutate(outcome = factor(outcome, levels = c(
    "No benefit", "Medical benefit", "Logistical benefit"
  )))


## Length of stay (lm)
pred_out_los <- bind_rows(
  avg_predictions(
    reg_out_los,
    variables = list(qi_log = 1),
    type = "response"
  ) %>% mutate(outcome = "Logistical benefit"),
  avg_predictions(
    reg_out_los,
    variables = list(qi_med = 1),
    type = "response"
  ) %>% mutate(outcome = "Medical benefit"),
  avg_predictions(
    reg_out_los,
    variables = list(qi_log = 0, qi_med = 0),
    type = "response"
  ) %>% mutate(outcome = "No benefit")
) %>%
  select(outcome, estimate, conf.low, conf.high) %>%
  mutate(outcome = factor(outcome, levels = c(
    "No benefit", "Medical benefit", "Logistical benefit"
  )))


## DRG (lm)
pred_out_drg <- bind_rows(
  avg_predictions(
    reg_out_drg,
    variables = list(qi_log = 1),
    type = "response"
  ) %>% mutate(outcome = "Logistical benefit"),
  avg_predictions(
    reg_out_drg,
    variables = list(qi_med = 1),
    type = "response"
  ) %>% mutate(outcome = "Medical benefit"),
  avg_predictions(
    reg_out_drg,
    variables = list(qi_log = 0, qi_med = 0),
    type = "response"
  ) %>% mutate(outcome = "No benefit")
) %>%
  select(outcome, estimate, conf.low, conf.high) %>%
  mutate(outcome = factor(outcome, levels = c(
    "No benefit", "Medical benefit", "Logistical benefit"
  )))


#Plotting

# Ploting thirty days mortality
fig_out_d30 <- ggplot(pred_out_d30, aes(y = outcome, x = estimate, color = outcome)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                 orientation = "y",
                 width = 0.2,
                 alpha = 1.0) +
  scale_color_manual(values = benefit_colors) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = "Predicted probability",
    y = "Modelltype",
    color = "Modelltype"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    legend.position = "none"
  ) +
  ggtitle("Thirty-day mortality")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))



# Ploting length of stay
fig_out_los <- ggplot(pred_out_los, aes(y = outcome, x = estimate, color = outcome)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                orientation = "y",
                width = 0.2,
                alpha = 1.0) +
  scale_color_manual(values = benefit_colors) +
  scale_x_continuous(
    limits = c(NA, NA),
    breaks = seq(0, max(pred_out_los$conf.high, na.rm = TRUE), by = 2)
  )+
  labs(
    x = "Days",
    y = "Benefit",
    color = "Benefit"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    legend.position = "none"
  )+
  ggtitle("Predicted length of stay")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))


# Ploting DRG-points
fig_out_drg <- ggplot(pred_out_drg, aes(y = outcome, x = estimate, color = outcome)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                orientation = "y",
                width = 0.2,
                alpha = 1.0) +
  scale_color_manual(values = benefit_colors) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = "DRG points",
    y = "Benefit",
    color = "Benefit"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    legend.position = "none"
  ) +
  ggtitle("Predicted hospital cost")+
  theme(plot.title = element_text(hjust = 0.5, size = 12))



main_plots_out <- fig_out_d30 + plot_spacer() + fig_out_los + plot_spacer() + fig_out_drg +
  plot_layout(ncol = 5, widths = c(1, 0.1, 1, 0.1, 1)) &
  theme(plot.background = element_blank())

labels_plot_out <- ggplot(
  transform(
    data.frame(outcome = factor(
      c("No benefit", "*Medical benefit", "*Logistical benefit"),
      levels = c("No benefit", "*Medical benefit", "*Logistical benefit")
    )),
    y = c(0.26, 0.54, 0.82)
  ),
  aes(x = 1, y = y, label = outcome)
) +
  geom_text(hjust = 0.5, vjust = 0.5, size = 3) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 5, r = -20, b = 5, l = 10))

figure7 <- labels_plot_out + main_plots_out +
  plot_layout(ncol = 2, widths = c(0.15, 1))


ggsave(
  filename = "./Figures/Figure7 - outcome prediction.png",
  plot = figure7,
  width = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

#Cleanup
rm(reg_vars_def, reg_vars_nmi)
rm(regdata_pred, regdata_out)
rm(reg_out_d30, reg_out_los, reg_out_drg)
rm(fig_out_d30, fig_out_los, fig_out_drg)
rm(labels_plot_out)
rm(main_plots_out)


## Tables ----

### Table 1: Trondheim HEMS at a glance ----
# Overall
t1_overall <- study_data %>%
  group_by(hm_vessel) %>%
  mutate(
    hm_vessel = factor(hm_vessel, levels = c(
      "Helicopter",
      "RRC",
      "Other"))
  ) %>% 
  summarise(n = n(), .groups = "drop") %>%
  adorn_totals("row") %>% 
  pivot_wider(names_from = hm_vessel,
              values_from = n,
              values_fill = list(n = 0)) %>% 
  mutate(category = as.character("Overall"))

# Vessel
t1_mtype <- study_data %>% 
  group_by(hm_type_cat, hm_vessel) %>%
  mutate(
    hm_type_cat = ifelse(hm_type_cat %in% c("SAR", "Other"), "SAR / Other", hm_type_cat),
    hm_type_cat = factor(hm_type_cat, levels = c("Primary", "Secondary", "SAR / Other")),
    hm_vessel = factor(hm_vessel, levels = c(
      "Helicopter",
      "RRC",
      "Other"))
  ) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = hm_vessel,
              values_from = n,
              values_fill = list(n = 0)) %>% 
  adorn_totals("col") %>% 
  mutate(category = as.character(hm_type_cat)) %>% 
  select(-hm_type_cat)

#Transport
t1_transport <- study_data %>%
  group_by(hp_transport, hm_vessel) %>%
  mutate(
    hm_vessel = factor(hm_vessel, levels = c(
      "Helicopter",
      "RRC",
      "Other"))
  ) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = hm_vessel,
              values_from = n,
              values_fill = list(n = 0)) %>% 
  adorn_totals("col") %>% 
  mutate(category = as.character(hp_transport),
         category = fct_relevel(category,
                                "Transported by HEMS",
                                "Transported by GEMS w/HEMS-physician",
                                "No HEMS-assisted transport")
         ) %>% 
  arrange(category) %>%
  select(-hp_transport)

#Sex
t1_sex <- study_data %>%
  group_by(hp_sex, hm_vessel) %>%
  mutate(
    hm_vessel = factor(hm_vessel, levels = c(
      "Helicopter",
      "RRC",
      "Other"))
  ) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = hm_vessel,
              values_from = n,
              values_fill = list(n = 0)) %>% 
  adorn_totals("col") %>% 
  mutate(category = as.character(hp_sex)) %>% 
  arrange(category) %>%
  select(-hp_sex)

#Age group
t1_age <- study_data %>% 
  group_by(hp_age_gr, hm_vessel) %>%
  mutate(
    hm_vessel = factor(hm_vessel, levels = c(
      "Helicopter",
      "RRC",
      "Other"))
  ) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = hm_vessel,
              values_from = n,
              values_fill = list(n = 0)) %>% 
  adorn_totals("col") %>% 
  mutate(category = as.character(hp_age_gr)) %>% 
  select(-hp_age_gr)

#NACA
t1_naca <- study_data %>% 
  group_by(hp_naca_gr, hm_vessel) %>%
  mutate(
    hm_vessel = factor(hm_vessel, levels = c(
      "Helicopter",
      "RRC",
      "Other"))
  ) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = hm_vessel,
    values_from = n,
    values_fill = list(n = 0)
  )%>% 
  adorn_totals("col") %>% 
  mutate(category = as.character(hp_naca_gr)) %>% 
  select(-hp_naca_gr)

#HEMS_outcome
t1_out <- study_data %>% 
  group_by(hp_outcome, hm_vessel) %>%
  mutate(
    hm_vessel = factor(hm_vessel, levels = c(
      "Helicopter",
      "RRC",
      "Other"))
  ) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = hm_vessel,
    values_from = n,
    values_fill = list(n = 0)
  )%>% 
  adorn_totals("col") %>% 
  mutate(category = as.character(hp_outcome)) %>% 
  select(-hp_outcome)


#30d mortality
t1_d30 <- study_data %>% 
  group_by(pat_d30, hm_vessel) %>%
  mutate(
    hm_vessel = factor(hm_vessel, levels = c(
      "Helicopter",
      "RRC",
      "Other"))
  ) %>% 
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = hm_vessel, values_from = n, values_fill = list(n = 0)) %>% 
  adorn_totals("col") %>%
  mutate(
    category = case_when(
      pat_d30 == 1 ~ "Dead within 30 days",
      pat_d30 == 0 ~ "Alive within 30 days"
    ),
    category = fct_relevel(category, "Dead within 30 days", "Alive within 30 days")
  ) %>%
  arrange(category) %>%
  select(-pat_d30)

#T1 combined
t1 <- bind_rows(
  t1_overall,
  t1_mtype,
  t1_sex,
  t1_age,
  t1_naca,
  t1_out,
  t1_d30
) %>% 
  select(category, everything())

write_xlsx(t1, "./Tables/Raw tables/t1_raw.xlsx")
rm(t1_overall, t1_mtype, t1_transport, t1_sex, t1_age, t1_naca, t1_out, t1_d30)    #Cleanup


# Appendix ----


## Appendix Figures ----

### sFigure 2: Detalied benefit descriptions ----

#Logistical benefit
benefit_log <- ComplexUpset::upset(
  
  study_data %>%
    filter(qi_logistical_benefit == "Yes") %>% 
    transmute(
      `Time benefit` = qi_time_gain != "No",
      Inaccessible   = qi_inaccessible == "Yes"
    ) %>%
    mutate(across(everything(), ~replace_na(.x, FALSE))),
  
  intersect        = c("Time benefit", "Inaccessible"),
  
  set_sizes  = ComplexUpset::upset_set_size() +
    labs(y= NULL) +
    geom_text(
      aes(label = after_stat(count), y = after_stat(count)),
      stat  = "count",
      vjust = -0.25,
      size  = 3
    ),
  
  matrix = ComplexUpset::intersection_matrix(
    geom = geom_point(shape = "circle filled", size = 3)
  ),
  
  queries = list(
    ComplexUpset::upset_query(
      set   = "Time benefit",
      fill  = logistic_colors[["Time benefit"]],
      color = logistic_colors[["Time benefit"]]
    ),
    ComplexUpset::upset_query(
      set   = "Inaccessible",
      fill  = logistic_colors[["Inaccessible"]],
      color = logistic_colors[["Inaccessible"]]
    )
  ),
  
  base_annotations = setNames(
    list(
      ComplexUpset::intersection_size()
    ),
    paste0("Logistical benefit n = ", scales::comma(nrow(study_data %>% filter(qi_logistical_benefit == "Yes"))))
  )
) +
  
  theme_minimal(base_size = 12) +
  labs(
    title = "Overlapping categories for logistical benefit",
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )



#Medical benefit
benefit_med <- ComplexUpset::upset(
  study_data %>%
    filter(qi_medical_benefit == "Yes") %>%
    transmute(
      `HEMS procedures`      = qi_la_procedures       == "Yes",
      `Other procedures`     = qi_other_procedures    == "Yes",
      `Defer treatment`      = qi_defer_treatment     == "Yes",
      `Difficult situation`  = qi_difficult_situation == "Yes"
    ) %>%
    mutate(across(everything(), ~replace_na(.x, FALSE))),
  
  intersect  = c("HEMS procedures", "Other procedures", "Defer treatment", "Difficult situation"),
  
  set_sizes  = ComplexUpset::upset_set_size() +
    labs(y = NULL) +
    geom_text(
      aes(label = after_stat(count), y = after_stat(count)),
      stat  = "count",
      vjust = -0.25,
      size  = 3
    ),
  
  matrix = ComplexUpset::intersection_matrix(
    geom = geom_point(shape = "circle filled", size = 3)
  ),
  
  queries = list(
    ComplexUpset::upset_query(set = "HEMS procedures",     fill = medical_colors[["HEMS procedures"]],     color = medical_colors[["HEMS procedures"]]),
    ComplexUpset::upset_query(set = "Other procedures",    fill = medical_colors[["Other procedures"]],    color = medical_colors[["Other procedures"]]),
    ComplexUpset::upset_query(set = "Defer treatment",     fill = medical_colors[["Defer treatment"]],     color = medical_colors[["Defer treatment"]]),
    ComplexUpset::upset_query(set = "Difficult situation", fill = medical_colors[["Difficult situation"]], color = medical_colors[["Difficult situation"]])
  ),
  
  base_annotations = setNames(
    list(
      ComplexUpset::intersection_size()
    ),
    paste0("Medical benefit n = ", scales::comma(nrow(study_data %>% filter(qi_medical_benefit == "Yes"))))
  )
) +
  
  theme_minimal(base_size = 12) +
  labs(
    title = "Overlapping categories for medical benefit",
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin  = unit(c(5.5, 5.5, 10, 5.5), "pt")
  )


#Combined plot
sFigure2 <- plot_grid(
  benefit_log, benefit_med,
  ncol = 1, align = "v",
  labels = c("a)", "b)"),
  label_size = 12, label_fontface = "bold",
  label_x = 0.02, label_y = 0.9,
  hjust = 0, vjust = 1
)

ggsave(
  filename = "./Figures/sFigure2 - benefit details.png",
  plot = sFigure2,
  width = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)


rm(benefit_log, benefit_med)



## Appendix Tables ----

# Overall table (for building benefittables)
aT0_overall <- study_data %>% 
  summarise(n_log = sum(qi_logistical_benefit == "Yes", na.rm = TRUE),
            n_med = sum(qi_medical_benefit == "Yes", na.rm = TRUE),
            n_int = sum(qi_logistical_benefit == "Yes" & qi_medical_benefit == "Yes", na.rm = TRUE),
            n_no = sum(
              (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)) &
                (qi_medical_benefit    != "Yes" | is.na(qi_medical_benefit)) &
                (qi_logistical_benefit == "No" | qi_medical_benefit == "No") &
                !(is.na(qi_logistical_benefit) & is.na(qi_medical_benefit)),
              na.rm = TRUE
            ),
            total = n()) %>% 
  mutate(category = as.character("Overall"))


### aTable 1: NACA scores ----
aT1 <- data.frame(
  Score = 0:7,
  Patient_state = c(
    "No injury or disease",
    "Injuries/diseases without any need for acute physician’s care",
    "Injuries/diseases requiring examination and therapy by a physician, but hospital admission is not indicated",
    "Injuries/diseases without acute threat to life but requiring hospital admission",
    "Injuries/diseases that can possibly lead to deterioration of vital signs",
    "Injuries/diseases with acute threat to life",
    "Injuries/diseases transported after successful resuscitation of vital signs",
    "Lethal injuries or diseases (with or without resuscitation attempts)"
  ),
  stringsAsFactors = FALSE
)

write_xlsx(aT1, "./Tables/Raw tables/@t1_raw.xlsx")



### aTable 2: Benefit associations ----
aT2a <- study_data %>%
  group_by(hm_type_cat) %>% 
  mutate(hm_type_cat = case_when(hm_type_cat == "SAR" ~ "SAR / Other",
                                 hm_type_cat == "Other" ~ "SAR / Other",
                                 TRUE ~ hm_type_cat)) %>% 
                   summarise(n_log = sum(qi_logistical_benefit == "Yes", na.rm = TRUE),
                             n_med = sum(qi_medical_benefit == "Yes", na.rm = TRUE),
                             n_int = sum(qi_logistical_benefit == "Yes" & qi_medical_benefit == "Yes", na.rm = TRUE),
                             n_no = sum(
                               (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)) &
                                 (qi_medical_benefit    != "Yes" | is.na(qi_medical_benefit)) &
                                 (qi_logistical_benefit == "No" | qi_medical_benefit == "No") &
                                 !(is.na(qi_logistical_benefit) & is.na(qi_medical_benefit)),
                               na.rm = TRUE
                             ),
                             total = n()) %>% 
                   mutate(category = as.character(hm_type_cat)) %>% 
                   select(-hm_type_cat) %>% 
                   arrange(is.na(category), desc(total))


aT2b <- study_data %>%
  group_by(hm_vessel) %>% 
  summarise(n_log = sum(qi_logistical_benefit == "Yes", na.rm = TRUE),
            n_med = sum(qi_medical_benefit == "Yes", na.rm = TRUE),
            n_int = sum(qi_logistical_benefit == "Yes" & qi_medical_benefit == "Yes", na.rm = TRUE),
            n_no = sum(
              (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)) &
                (qi_medical_benefit    != "Yes" | is.na(qi_medical_benefit)) &
                (qi_logistical_benefit == "No" | qi_medical_benefit == "No") &
                !(is.na(qi_logistical_benefit) & is.na(qi_medical_benefit)),
              na.rm = TRUE
            ),
            total = n()) %>% 
  mutate(category = as.character(hm_vessel)) %>% 
  select(-hm_vessel) %>% 
  arrange(is.na(category), desc(total))


aT2c <- study_data %>%
  group_by(hp_naca_gr) %>% 
  summarise(n_log = sum(qi_logistical_benefit == "Yes", na.rm = TRUE),
            n_med = sum(qi_medical_benefit == "Yes", na.rm = TRUE),
            n_int = sum(qi_logistical_benefit == "Yes" & qi_medical_benefit == "Yes", na.rm = TRUE),
            n_no = sum(
              (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)) &
                (qi_medical_benefit    != "Yes" | is.na(qi_medical_benefit)) &
                (qi_logistical_benefit == "No" | qi_medical_benefit == "No") &
                !(is.na(qi_logistical_benefit) & is.na(qi_medical_benefit)),
              na.rm = TRUE
            ),
            total = n()) %>% 
  mutate(category = as.character(hp_naca_gr)) %>% 
  select(-hp_naca_gr)

aT2d <- study_data %>%
  mutate(proc = case_when(hp_proc_sum == 0 ~ "0 proc",
                           hp_proc_sum == 1 ~ "1 proc",
                           hp_proc_sum == 2 ~ "2 proc",
                           hp_proc_sum == 3 | hp_proc_sum == 4 ~ "3-4 proc",
                           hp_proc_sum >= 5 ~ "5+ proc"
                           )
         ) %>% 
  group_by(proc) %>% 
  summarise(n_log = sum(qi_logistical_benefit == "Yes", na.rm = TRUE),
            n_med = sum(qi_medical_benefit == "Yes", na.rm = TRUE),
            n_int = sum(qi_logistical_benefit == "Yes" & qi_medical_benefit == "Yes", na.rm = TRUE),
            n_no = sum(
              (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)) &
                (qi_medical_benefit    != "Yes" | is.na(qi_medical_benefit)) &
                (qi_logistical_benefit == "No" | qi_medical_benefit == "No") &
                !(is.na(qi_logistical_benefit) & is.na(qi_medical_benefit)),
              na.rm = TRUE
            ),
            total = n()) %>% 
  mutate(category = as.character(proc)) %>% 
  select(-proc)

aT2 <- bind_rows(aT0_overall, aT2a, aT2b, aT2c, aT2d) %>% 
  select(category, everything())
write_xlsx(aT2, "./Tables/Raw tables/@t2_raw.xlsx")
rm(aT2a, aT2b, aT2c, aT2d)

### aTable 3: HEMS diagnosis ----
aT3 <- bind_rows(aT0_overall,
                 study_data %>%
                   group_by(hp_dia_3) %>% 
                   summarise(n_log = sum(qi_logistical_benefit == "Yes", na.rm = TRUE),
                             n_med = sum(qi_medical_benefit == "Yes", na.rm = TRUE),
                             n_int = sum(qi_logistical_benefit == "Yes" & qi_medical_benefit == "Yes", na.rm = TRUE),
                             n_no = sum(
                               (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)) &
                                 (qi_medical_benefit    != "Yes" | is.na(qi_medical_benefit)) &
                                 (qi_logistical_benefit == "No" | qi_medical_benefit == "No") &
                                 !(is.na(qi_logistical_benefit) & is.na(qi_medical_benefit)),
                               na.rm = TRUE
                             ),
                             total = n()) %>% 
                   mutate(category = as.character(hp_dia_3)) %>% 
                   select(-hp_dia_3) %>% 
                   arrange(is.na(category), desc(total))) %>% 
  select(category, everything())

write_xlsx(aT3, "./Tables/Raw tables/@t3_raw.xlsx")


### aTable 4: Prediction estimates age ----
aT4 <- preds_age %>%
  left_join((study_data %>% count(age = hp_age)),
            by = "age") %>%
  select(age, n, everything())

write_xlsx(aT4, "./Tables/Raw tables/@t4_raw.xlsx")
rm(preds_age)   #Cleanup


### aTable 5: Prediction estimates descriptives ----
aT5 <- preds_desc %>%
  left_join(
    (bind_rows(
    study_data %>% count(var = alarm_year,  name = "n") %>% mutate(var = as.character(var)),
    study_data %>% count(var = alarm_season, name = "n"),
    study_data %>% count(var = alarm_weekpart, name = "n"),
    study_data %>% count(var = alarm_shift, name = "n"),
    study_data %>% count(var = hp_sex, name = "n")
    ) %>%
      select(var, n)),
    by = "var") %>%
  select(var, n, everything())

write_xlsx(aT5, "./Tables/Raw tables/@t5_raw.xlsx")
rm(preds_desc)    #Cleanup


### aTable 6: Prediction estimates NMI ----
aT6 <- preds_nmi %>% 
  left_join(study_data %>%
              count(var = emcc_nmi_grp_agg %>%
                      factor() %>%
                      fct_recode(
                        "Unresponsive, NOT breathing normally*" = "Unresponsive, NOT breathing normally",
                        "Unresponsive, breathing normally*" = "Unresponsive, breathing normally",
                        "Choking/airway obstruction*" = "Choking/airway obstruction",
                        "Major disaster*" = "Major disaster"
                      )
                    ),
            by = "var") %>%
  select(var, n, everything()) %>% 
  arrange(desc(row_number()))


write_xlsx(aT6, "./Tables/Raw tables/@t6_raw.xlsx")
rm(preds_nmi)   #Cleanup


### aTable 7: Prediction estimates outcome ----

#30-day mortality
aT7a <- study_data %>%
  filter(!is.na(qi_logistical_benefit) | !is.na(qi_medical_benefit)) %>%
  filter(!is.na(pat_pas_first) & hp_naca != 7) %>%
  mutate(
    qi_log = ifelse(qi_logistical_benefit == "Yes", 1, 0) %>% replace_na(0),
    qi_med = ifelse(qi_medical_benefit == "Yes", 1, 0) %>% replace_na(0),
    qi_no  = ifelse(qi_log == 0 & qi_med == 0, 1, 0)
  ) %>%
  reframe(
    outcome = c("Logistical benefit", "Medical benefit", "No benefit"),
    total   = n(),
    cases   = c(sum(qi_log),
                sum(qi_med),
                sum(qi_no)),
    d30_rate = c(sum(qi_log == 1 & pat_d30 == 1)/sum(qi_log),
                 sum(qi_med == 1 & pat_d30 == 1)/sum(qi_med),
                 sum(qi_no  == 1 & pat_d30 == 1)/sum(qi_no))
  ) %>% 
  left_join(pred_out_d30, by = "outcome")
  
write_xlsx(aT7a, "./Tables/Raw tables/@t7a_raw.xlsx")


#Hospital days
aT7b <- study_data %>%
  filter(!is.na(qi_logistical_benefit) | !is.na(qi_medical_benefit)) %>%
  filter(!is.na(he_in)) %>%
  mutate(
    qi_log = ifelse(qi_logistical_benefit == "Yes", 1, 0) %>% replace_na(0),
    qi_med = ifelse(qi_medical_benefit == "Yes", 1, 0) %>% replace_na(0),
    qi_no  = ifelse(qi_log == 0 & qi_med == 0, 1, 0)
  ) %>%
  reframe(
    outcome = c("Logistical benefit", "Medical benefit", "No benefit"),
    total   = n(),
    cases   = c(sum(qi_log),
                sum(qi_med),
                sum(qi_no)),
    mean_los     = c(mean(hs_days[qi_log == 1], na.rm = TRUE),
                mean(hs_days[qi_med == 1], na.rm = TRUE),
                mean(hs_days[qi_no  == 1], na.rm = TRUE))
  ) %>% 
  left_join(pred_out_los, by = "outcome")

write_xlsx(aT7b, "./Tables/Raw tables/@t7b_raw.xlsx")


#DRG Points
aT7c <- study_data %>%
  filter(!is.na(qi_logistical_benefit) | !is.na(qi_medical_benefit)) %>%
  filter(!is.na(he_in)) %>%
  mutate(
    qi_log = ifelse(qi_logistical_benefit == "Yes", 1, 0) %>% replace_na(0),
    qi_med = ifelse(qi_medical_benefit == "Yes", 1, 0) %>% replace_na(0),
    qi_no  = ifelse(qi_log == 0 & qi_med == 0, 1, 0)
  ) %>%
  reframe(
    outcome = c("Logistical benefit", "Medical benefit", "No benefit"),
    total   = n(),
    cases   = c(sum(qi_log),
                sum(qi_med),
                sum(qi_no)),
    mean_drg  = c(mean(hs_drg_points[qi_log == 1], na.rm = TRUE),
                  mean(hs_drg_points[qi_med == 1], na.rm = TRUE),
                  mean(hs_drg_points[qi_no  == 1], na.rm = TRUE))
  ) %>% 
  left_join(pred_out_drg, by = "outcome")

write_xlsx(aT7c, "./Tables/Raw tables/@t7c_raw.xlsx")
rm(pred_out_d30, pred_out_los, pred_out_drg)


# Extra ----

## Benefit distribution ----

benefit_dist <- study_data %>% 
  summarise(
    n         = n(),
    log_all   = sum(qi_logistical_benefit == "Yes", na.rm=TRUE),
    med_all   = sum(qi_medical_benefit == "Yes", na.rm=TRUE),
    dbl       = sum(qi_logistical_benefit == "Yes" & qi_medical_benefit == "Yes", na.rm=TRUE),
    log_exc   = sum(qi_logistical_benefit == "Yes" & (qi_medical_benefit != "Yes" | is.na(qi_medical_benefit)), na.rm=TRUE),
    med_exc   = sum(qi_medical_benefit == "Yes" & (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit)), na.rm=TRUE),
    no        = sum((qi_logistical_benefit == "No" | is.na(qi_logistical_benefit)) & (qi_medical_benefit == "No" | is.na(qi_medical_benefit)), na.rm=TRUE),
  )

write_xlsx(age_dist, "./Tables/Raw tables/benefit_dist.xlsx")

medical_dist <- study_data %>%
  filter(qi_medical_benefit == "Yes" & (qi_logistical_benefit != "Yes" | is.na(qi_logistical_benefit))) %>%
  group_by(hm_vessel) %>%
  summarise(n = n())


## Age distribution ----

age_dist <- study_data %>% 
  summarise(
    n      = sum(!is.na(hp_age)),
    q25    = round(quantile(hp_age, 0.25, na.rm = TRUE)),
    median = round(median(hp_age, na.rm = TRUE)),
    q75    = round(quantile(hp_age, 0.75, na.rm = TRUE))
  )

write_xlsx(age_dist, "./Tables/Raw tables/age_dist.xlsx")


## Benefit assessment per physician ----

#Numeric summary
physician_num <- study_data %>% 
  group_by(hm_doc_label) %>%
  summarise(n = n(),
            log = sum(qi_logistical_benefit == "Yes", na.rm = TRUE),
            med = sum(qi_medical_benefit == "Yes", na.rm = TRUE),
            no = sum((qi_logistical_benefit == "No" | is.na(qi_logistical_benefit)) & (qi_medical_benefit == "No" | is.na(qi_medical_benefit)))
            )

write_xlsx(physician_num, "./Tables/Raw tables/physician_summary.xlsx")

#Median, Q1 and Q3 per benefit category
physician_summary <- study_data %>%
  group_by(hm_doc_label) %>%
  summarise(
    n   = n(),
    log = 100 * mean(qi_logistical_benefit == "Yes", na.rm = TRUE),
    med = 100 * mean(qi_medical_benefit == "Yes",     na.rm = TRUE),
    no  = 100 * mean(
      (qi_logistical_benefit == "No"  | is.na(qi_logistical_benefit)) &
        (qi_medical_benefit     == "No" | is.na(qi_medical_benefit)),
      na.rm = TRUE
    )
  ) %>%
  summarise(
    across(
      c(log, med, no),
      list(
        median = ~round(median(.x, na.rm = TRUE)),
        q1     = ~round(quantile(.x, 0.25, na.rm = TRUE)),
        q3     = ~round(quantile(.x, 0.75, na.rm = TRUE))
      ),
      .names = "{.col}_{.fn}"
    )
  )

write_xlsx(physician_summary, "./Tables/Raw tables/physician_variation.xlsx")

  
# Cleanup ----
rm(aT0_overall)
rm(benefit_colors, logistic_colors, medical_colors)
rm(benefit_figA, benefit_figB)


