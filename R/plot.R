library(ggplot2)
library(ggforce)
library(ggrepel)
library(waffle)
library(tidyverse)

REAL_ESTATE_CATEGORIES <- c(
  "REAL ESTATE (DEVELOPER)",
  "REAL ESTATE (INVESTMENT)",
  "REAL ESTATE (LANDLORD)",
  "REAL ESTATE (NON-PROFIT)",
  "REAL ESTATE (OTHER)",
  "LANDLORD LAW",
  "TENANT LAW",
  "BUILDING TRADES"
)

ADVOCATE_REAL_ESTATE_CATEGORIES <- c(
  "REAL ESTATE (NON-PROFIT)",
  "TENANT LAW"
)

PROFIT_REAL_ESTATE_CATEGORIES <- setdiff(REAL_ESTATE_CATEGORIES, ADVOCATE_REAL_ESTATE_CATEGORIES)

summary_df <- industry_totals_per_filer_df %>%
  group_by(filer_name) %>%
  mutate(
    total_pct = 100 * total_donations / sum(total_donations),
    industry_name = factor(industry_name, levels = industry_taxonomy_df$name)
  ) %>%
  complete(industry_name, fill = list(total_pct = 0, total_donations = 0)) %>%
  ungroup() %>%
  transmute(filer_name = factor(filer_name), industry_name, total_pct, total_donations) %>%
  mutate(
    filer_name = recode_factor(
      filer_name,
      `DEAN PRESTON FOR SUPERVISOR 2019` = "Dean Preston",
      `VOTE VALLIE BROWN FOR SUPERVISOR 2019` = "Vallie Brown"
    ),
    nice_industry_name = industry_name %>% str_to_title() %>% str_replace_all("Real Estate \\((.*)\\)", "\\1"),
    real_estate = factor(ifelse(industry_name %in% REAL_ESTATE_CATEGORIES, "Real Estate", "Other")),
    profit_real_estate = factor(ifelse(industry_name %in% PROFIT_REAL_ESTATE_CATEGORIES, "Real Estate (non-advocacy)", "Other"))
  )

update_geom_defaults("text", list(family = "Georgia", size = 1.5))

plot_df <-
  summary_df %>%
  filter(industry_name %in% REAL_ESTATE_CATEGORIES)

code_real_estate <- function(cat) {
  if (cat %in% ADVOCATE_REAL_ESTATE_CATEGORIES) {
    "#8de4d3"
  } else {
    "#c9245d"
  }
}

plot_df %>%
  ggplot(aes(x = filer_name, y = total_pct, group = industry_name)) +
  geom_line(aes(color = sapply(industry_name, code_real_estate)), size = .1) +
  # Category names
  geom_text_repel(aes(label = paste(nice_industry_name, "|", scales::percent(total_pct / 100, accuracy = .1)), color = sapply(industry_name, code_real_estate)),
    data = filter(plot_df, filer_name == "Dean Preston"),
    hjust = 1,
    size = 2.5,
    segment.size = 0.1,
    force = 3,
    direction = "y",
    nudge_x = -.2
  ) +
  geom_text_repel(aes(label = paste(scales::percent(total_pct / 100, accuracy = .1), "|", nice_industry_name), color = sapply(industry_name, code_real_estate)),
    data = filter(plot_df, filer_name == "Vallie Brown"),
    hjust = 0,
    segment.size = 0.1,
    size = 2.5,
    direction = "y",
    nudge_x = +.15
  ) +
  # Data values
  # geom_text(aes(label = scales::percent(total_pct/100)),
  #           data = filter(plot_df, filer_name == "Dean Preston"),
  #           hjust = 1,
  #           nudge_x = -.05) +
  # geom_text(aes(label = scales::percent(total_pct/100)),
  #           data = filter(plot_df, filer_name == "Vallie Brown"),
  #           hjust = 0,
  #           nudge_x = +.05) +
  scale_x_discrete(position = "top") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#fffff8")) +
  theme(legend.position = "none") +
  theme(panel.border = element_blank()) +
  # Remove just about everything from the y axis
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top = element_text(size = 10)) +
  scale_color_identity()

summary_df %>%
  group_by(profit_real_estate, filer_name) %>%
  summarize(total = sum(total_donations)) %>%
  ungroup()
