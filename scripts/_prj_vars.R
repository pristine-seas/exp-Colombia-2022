#ps_data_path <- "/Volumes/ps-science/datasets/"
ps_data_path <- "~/marine.data.science@ngs.org - Google Drive/My Drive/Pristine Seas/SCIENCE/datasets/"

#ps_exp_path <- "/Volumes/Colombia - 2022/"

ps_exp_path <- "~/marine.data.science@ngs.org - Google Drive/My Drive/Pristine Seas/SCIENCE/expeditions/COL-2022/"

default_font_family <- "Arial"

default_font_color <- '#22211d'

default_background_color <- "#f5f5f2"

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {

  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}


cabo_stations <- c("COL_2022_birds_48","COL_2022_birds_49", "COL_2022_birds_50", "COL_2022_birds_58", "COL_2022_birds_59",
                   "COL_2022_dscm_24", "COL_2022_dscm_26",
                   "COL_2022_edna_077", "COL_2022_edna_080","COL_2022_edna_081", "COL_2022_edna_082",
                   "COL_2022_meso_01", "COL_2022_meso_02",
                   "COL_2022_pcam_23", "COL_2022_pcam_25",
                   "COL_2022_sub_15", "COL_2022_sub_16",
                   "COL_2022_uvs_03","COL_2022_uvs_09", "COL_2022_uvs_10", "COL_2022_uvs_14", "COL_2022_uvs_15", "COL_2022_uvs_16",
                   "COL_2022_ysi_08","COL_2022_ysi_13","COL_2022_ysi_14","COL_2022_ysi_18","COL_2022_ysi_19","COL_2022_ysi_20")

utria_stations <- c("COL_2022_birds_45", "COL_2022_birds_46", "COL_2022_birds_63", "COL_2022_birds_64", "COL_2022_birds_65", "COL_2022_birds_66",
                    "COL_2022_birds_67", "COL_2022_birds_68", "COL_2022_birds_69",
                    "COL_2022_dscm_29", "COL_2022_dscm_30","COL_2022_dscm_31", "COL_2022_dscm_32",
                    "COL_2022_edna_075", "COL_2022_edna_076", "COL_2022_edna_084", "COL_2022_edna_085",
                    "COL_2022_pcam_27",
                    "COL_2022_sub_07", "COL_2022_sub_18",
                    "COL_2022_uvs_06","COL_2022_uvs_07", "COL_2022_uvs_08", "COL_2022_uvs_18", "COL_2022_uvs_19", "COL_2022_uvs_20",
                    "COL_2022_ysi_11","COL_2022_ysi_12","COL_2022_ysi_22","COL_2022_ysi_23")
