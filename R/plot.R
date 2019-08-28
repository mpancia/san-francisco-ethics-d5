library(ggplot2)
library(tidyverse)
library(glue)
library(magrittr)
library(extrafont)

extrafont::font_import("data/static/et-book/", prompt = FALSE)

TODAY <- lubridate::today()

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

INDUSTRIES <- industry_taxonomy_df$name

NON_PROFIT_INDUSTRIES <- c(
  "NON-PROFIT (ADVOCACY)",
  "NON-PROFIT (COMMUNITY)",
  "NON-PROFIT (LAW)",
  "NON-PROFIT (OTHER)",
  "REAL ESTATE (NON-PROFIT)",
  "POLITICS",
  "GOVERNMENT"
)

PROFIT_INDUSTRIES <- setdiff(INDUSTRIES, NON_PROFIT_INDUSTRIES)

PROFIT_REAL_ESTATE_CATEGORIES <- setdiff(REAL_ESTATE_CATEGORIES, ADVOCATE_REAL_ESTATE_CATEGORIES)

summary_industry_df <- industry_totals_per_filer_df %>%
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
    profit_real_estate = factor(ifelse(industry_name %in% PROFIT_REAL_ESTATE_CATEGORIES, "Real Estate", "Other"))
  )

summary_rank_file_df <- industry_totals_per_filer_df %>%
  filter(occupation_class != "RETIRED") %>%
  filter(!industry_name %in% NON_PROFIT_INDUSTRIES) %>%
  mutate(filer_name = factor(filer_name), occupation_class = factor(occupation_class)) %>%
  mutate(
    filer_name = recode_factor(
      filer_name,
      `DEAN PRESTON FOR SUPERVISOR 2019` = "Dean Preston",
      `VOTE VALLIE BROWN FOR SUPERVISOR 2019` = "Vallie Brown"
    ),
    occupation_class = fct_recode(occupation_class,
      "RANK & FILE / INDEPENDENT" = "INDEPENDENT CONTRACTOR /CONSULTANT/SELF-EMPLOYED",
      "RANK & FILE / INDEPENDENT" = "RANK AND FILE"
    )
  ) %>%
  mutate(occupation_class = fct_relevel(
    occupation_class,
    "OWNER/FOUNDER",
    "EXECUTIVE/UPPER-MANAGEMENT",
    "MIDDLE MANAGEMENT",
    "RANK & FILE / INDEPENDENT",
  )) %>%
  mutate(occupation_class = fct_relabel(occupation_class, str_to_title)) %>%
  group_by(filer_name, occupation_class) %>%
  summarize(total = sum(total_donations)) %>%
  ungroup() %>%
  group_by(filer_name) %>%
  mutate(total_frac = total / sum(total)) %>%
  ungroup() %>%
  transmute(filer_name, occupation_class, total_frac, total)

# plot_df <-
#   summary_df %>%
#   filter(industry_name %in% REAL_ESTATE_CATEGORIES)
#
# code_real_estate <- function(cat) {
#   if (cat %in% ADVOCATE_REAL_ESTATE_CATEGORIES) {
#     "#8de4d3"
#   } else {
#     "#c9245d"
#   }
# }
#
# plot_df %>%
#   ggplot(aes(x = filer_name, y = total_pct, group = industry_name)) +
#   geom_line(aes(color = sapply(industry_name, code_real_estate)), size = .1) +
#   # Category names
#   geom_text_repel(aes(label = paste(nice_industry_name, "|", scales::percent(total_pct / 100, accuracy = .1)), color = sapply(industry_name, code_real_estate)),
#     data = filter(plot_df, filer_name == "Dean Preston"),
#     hjust = 1,
#     size = 2.5,
#     segment.size = 0.1,
#     force = 3,
#     direction = "y",
#     nudge_x = -.2
#   ) +
#   geom_text_repel(aes(label = paste(scales::percent(total_pct / 100, accuracy = .1), "|", nice_industry_name), color = sapply(industry_name, code_real_estate)),
#     data = filter(plot_df, filer_name == "Vallie Brown"),
#     hjust = 0,
#     segment.size = 0.1,
#     size = 2.5,
#     direction = "y",
#     nudge_x = +.15
#   ) +
#   # Data values
#   # geom_text(aes(label = scales::percent(total_pct/100)),
#   #           data = filter(plot_df, filer_name == "Dean Preston"),
#   #           hjust = 1,
#   #           nudge_x = -.05) +
#   # geom_text(aes(label = scales::percent(total_pct/100)),
#   #           data = filter(plot_df, filer_name == "Vallie Brown"),
#   #           hjust = 0,
#   #           nudge_x = +.05) +
#   scale_x_discrete(position = "top") +
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "#fffff8")) +
#   theme(legend.position = "none") +
#   theme(panel.border = element_blank()) +
#   # Remove just about everything from the y axis
#   theme(axis.title.y = element_blank()) +
#   theme(axis.text.y = element_blank()) +
#   theme(panel.grid.major.y = element_blank()) +
#   theme(panel.grid.minor.y = element_blank()) +
#   # Remove a few things from the x axis and increase font size
#   theme(axis.title.x = element_blank()) +
#   theme(panel.grid.major.x = element_blank()) +
#   theme(axis.text.x.top = element_text(size = 10)) +
#   scale_color_identity()
#
p_title <- "Vallie Brown has strong real estate industry support"
p_subtitle <- "Total donations from for-profit real estate interests for the SF D5 Supervisor race"
p_caption <- glue("Includes donations from construction, landlords, investors, lawyers, and others.", "Source: Ethics filings as of {TODAY}. See also: https://bit.ly/2Zb9nEq", .sep = "\n")
p_annotation_text <- paste("Brown takes more than 10x as much money from for-profit real estate interests than Preston.", "This is true in both absolute dollar amounts and as a percentage of their total donations.", sep = "\n")
theme_d5 <- theme(
  text = element_text(family = "ETBembo", color = "#111111"),
  panel.background = element_rect(fill = "#fffff8"),
  plot.background = element_rect(fill = "#fffff8"),
  plot.title.position = "plot",
  legend.position = "top",
  legend.text = element_text(face = "plain", size = 11),
  legend.margin = margin(),
  title = element_text(face = "bold", size = 18),
  plot.subtitle = element_text(face = "plain", size = 14),
  # plot.margin = unit(c(0,0,1,0), "cm"),
  plot.caption = element_text(face = "plain", size = 10, vjust = -1.5),
  axis.text.y = element_text(face = "bold", size = 16, color = c("#31AAED", "#DF238A")),
  axis.text.x.bottom = element_text(face = "plain", size = 12, vjust = 1),
  axis.text.x.top = element_text(face = "plain", size = 12, vjust = -1),
  axis.ticks.length.x = unit(0, "cm"),
  axis.ticks.length.y = unit(0, "cm"),
  panel.grid.major.y = element_blank()
)

real_estate_df <- summary_industry_df %>%
  group_by(profit_real_estate, filer_name) %>%
  summarize(total = sum(total_donations)) %>%
  ungroup() %>%
  group_by(filer_name) %>%
  mutate(total_frac = total / sum(total)) %>%
  ungroup() %>%
  rename(label_column = profit_real_estate) %>%
  filter(label_column != "Other")
df <- real_estate_df
real_estate_plot <-
  df %>%
  ggplot(aes(x = filer_name, y = total_frac)) +
  geom_bar(stat = "identity", width = .95, fill = "#a4201d", color = "#a4201d") +
  scale_y_continuous(breaks = NULL) +
  annotate("text",
    x = 1,
    y = .045,
    size = 10 * (5 / 14),
    label = p_annotation_text,
    color = "#111111",
    hjust = 0
  ) +
  annotate("text",
    x = 1,
    y = min(df$total_frac) - .01,
    size = 18 * (5 / 14),
    label = scales::dollar(min(df$total), scale = .001, suffix = "K", accuracy = 1),
    color = "#fffff8"
  ) +
  annotate("text",
    x = 1,
    size = 18 * (5 / 14),
    y = min(df$total_frac) + .01,
    label = scales::percent(min(df$total_frac), accuracy = 1),
    color = "#111111"
  ) +
  annotate("text",
    x = 2,
    size = 18 * (5 / 14),
    y = max(df$total_frac) - .01,
    label = scales::dollar(max(df$total), scale = .001, suffix = "K", accuracy = 1),
    color = "#fffff8"
  ) +
  annotate("text",
    x = 2,
    y = max(df$total_frac) + .01,
    size = 18 * (5 / 14),
    label = scales::percent(max(df$total_frac), accuracy = 1),
    color = "#111111"
  ) +
  theme_d5 +
  theme(plot.title.position = "plot") +
  labs(
    x = NULL, y = NULL,
    fill = "",
    caption = p_caption,
    title = p_title,
    subtitle = p_subtitle
  ) +
  coord_flip()

ggsave("data/output/real_estate_plot.png", plot = real_estate_plot, width = 10, height = 3)

# ---------------------
library(grid)
p_title <- "Dean Preston has stronger support from rank and file workers"
p_subtitle <- "Total donations from non-bosses in the SF D5 Supervisor race"
p_caption <- glue("Bosses are middle/upper management, executives, and owners/founders of companies.", "Source: Ethics filings as of {TODAY}. See also: https://bit.ly/2Zb9nEq", .sep = "\n")
p_annotation_text <- paste("Preston gets ~ 20% more of his donations from rank & file/independent contractors.",
  "Brown gets ~ 35% of her donations from bosses.",
  sep = "\n"
)
annotation_grob <- textGrob(p_annotation_text, gp = gpar(fontsize = 10, fontface = "bold", col = "#111111"), just = "left")
bosses_df <- summary_rank_file_df %>%
  group_by(occupation_class, filer_name) %>%
  rename(label_column = occupation_class) %>%
  filter(label_column == "Rank & File / Independent")
df <- bosses_df
bosses_plot <-
  df %>%
  ggplot(aes(x = filer_name, y = total_frac)) +
  geom_bar(stat = "identity", width = .95, fill = "#2b83ba", color = "#2b83ba") +
  scale_y_continuous(breaks = NULL) +
  annotation_custom(annotation_grob, xmin = -.1, xmax = -.1, ymin = -.2, ymax = -.2) +
  annotate("text",
    x = 1,
    y = max(df$total_frac) - .05,
    size = 18 * (5 / 14),
    label = scales::dollar(max(df$total), scale = .001, suffix = "K", accuracy = 1),
    color = "#fffff8"
  ) +
  annotate("text",
    x = 1,
    size = 18 * (5 / 14),
    y = max(df$total_frac) + .05,
    label = scales::percent(max(df$total_frac), accuracy = 1),
    color = "#111111"
  ) +
  annotate("text",
    x = 2,
    size = 18 * (5 / 14),
    y = min(df$total_frac) - .05,
    label = scales::dollar(min(df$total), scale = .001, suffix = "K", accuracy = 1),
    color = "#fffff8"
  ) +
  annotate("text",
    x = 2,
    y = min(df$total_frac) + .05,
    size = 18 * (5 / 14),
    label = scales::percent(min(df$total_frac), accuracy = 1),
    color = "#111111"
  ) +
  theme_d5 +
  theme(
    plot.title.position = "plot",
    plot.caption = element_text(face = "plain", size = 10, vjust = -10),
    plot.margin = unit(c(1, 1, 3, 1), "lines")
  ) +
  labs(
    x = NULL, y = NULL,
    fill = "",
    caption = p_caption,
    title = p_title,
    subtitle = p_subtitle
  ) +
  coord_flip(clip = "off")

ggsave("data/output/bosses_plot.png", plot = bosses_plot, width = 10, height = 3)
