## functions for ANES analysis
library(tidyverse) # to do tidyverse things

# converts anything not 1 to 0
one_zero <- function(x){
  ifelse(x == 1, 1, 0)
}

# reverses 5-point scale
rev_one_five <- function(x){
  case_when(
    x == 1 ~ 5,
    x == 2 ~ 4,
    x == 3 ~ 3,
    x == 4 ~ 2,
    x == 5 ~ 1,
    TRUE ~ 0)
}

# reverses 4-point scale
rev_one_four <- function(x){
  case_when(
    x == 1 ~ 4,
    x == 2 ~ 3,
    x == 3 ~ 2,
    x == 4 ~ 1,
    TRUE ~ 0)
}

#####
# functions for trust analysis
# turn index components to factors for charts & tables
always_never_f <- function(x) {
  factor(
    case_when(
      x == 1 ~ "Never", x == 2 ~ "Some of the time",
      x == 3 ~ "About half the time", x == 4 ~ "Most of the time",
      x == 5 ~ "Always"),
    levels = c("Always", "Most of the time", "About half the time",
      "Some of the time", "Never")
  )
}


always_never_4 <- function(x) {
  factor(
    case_when(
      x == 1 ~ "Do not trust at all",
      x == 2 ~ "Do not trust very much",
      x == 3 ~ "Trust somewhat",
      x == 4 ~ "Trust a lot"),
    levels = c("Trust a lot", "Trust somewhat",
      "Do not trust very much", "Do not trust at all"))
}

# create df with grouped by trust item and vote pref
vote_by_item <- function(x) {
  anes_2024_weighted_trust %>%
    filter(!is.na({{ x }})) %>%
    filter(!is.na(voted_president_2024)) %>%
    group_by({{ x }}) %>%
    srvyr::survey_count(voted_president_2024, vartype = c("se", "cv")) %>%
    mutate(pct = n / sum(n)) %>%
    select({{ x }}, voted_president_2024, pct, everything()) %>%
    ungroup()
}

# for two items
vote_by_item2 <- function(x, y) {
  anes_2024_weighted_trust %>%
    filter(!is.na({{ x }})) %>%
    filter(!is.na({{ y }})) %>%
    group_by({{ x }}) %>%
    srvyr::survey_count({{ y }}, vartype = c("se", "cv")) %>%
    mutate(pct = n / sum(n)) %>%
    select({{ x }}, {{ y }}, pct, everything())
}


# plot function
plot_vote_item <- function(df, item) {

  vlines_df <- data.frame(xintercept = seq(-100, 100, 20))

  df %>%
    mutate(pct = ifelse(voted_president_2024 == "Kamala Harris", pct *-1, pct)) %>%
    mutate(pct2 = round(pct * 100, 0)) %>%
    filter(!voted_president_2024 == "Other") %>%
    mutate({{ item }} := fct_reorder({{ item }}, desc({{ item }}))) %>%
    {. ->> tmp} %>%
    ggplot() +
    geom_col(aes(x = -50, y ={{ item }}), width = 0.75, fill = "#e0e0e0") +
    geom_col(aes(x = 50, y ={{ item }}), width = 0.75, fill = "#e0e0e0") +
    geom_col(aes(x = pct2, y ={{ item }}, fill = voted_president_2024,
      color = voted_president_2024), width = 0.75) +
    scale_x_continuous(expand = c(0, 0),
      labels = function(x) abs(x), breaks = seq(-100, 100, 20)) +
    geom_vline(data = vlines_df, aes(xintercept = xintercept), color = "#FFFFFF", size = 0.1, alpha = 0.5) +
    coord_cartesian(clip = "off") +
    scale_color_manual(values = c("white", "white")) +
    geom_text(data = subset(tmp, voted_president_2024 == "Kamala Harris"),
      aes(x = pct2, y =!!enquo(item), label = paste0(abs(pct2), "%")),
      size = 5, color = "white", position = position_dodge(width = .4), hjust = -0.09) +
    geom_text(data = subset(tmp, voted_president_2024 == "Donald Trump"),
      aes(x = pct2, y =!!enquo(item), label = paste0(abs(pct2), "%")),
      size = 5, color = "white", position = position_dodge(width = .4), hjust = 1.1) +
    geom_text(data = subset(tmp, voted_president_2024 == "Kamala Harris"),
      aes(x = -100, y = !!enquo(item),
        label = paste0("Coefficient of variation = ", percent(round(n_cv, 3)))),
      size = 3, color = "#3b3b3b", hjust = -.1, vjust = 3.8, fontface = "italic") +
    geom_text(data = subset(tmp, voted_president_2024 == "Donald Trump"),
      aes(x = 70, y = !!enquo(item),
        label = paste0("Coefficient of variation = ", percent(round(n_cv, 3)))),
      size = 3, color = "#3b3b3b", hjust = -.1, vjust = 3.8, fontface = "italic") +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
      plot.title = element_markdown(size = 16, lineheight = 1.25),
      plot.subtitle = element_markdown(size = 12, lineheight = 1.25),
      plot.caption = element_markdown(),
      legend.position = "bottom", legend.justification = "center",
      legend.title = element_text(size = 8),
      axis.text.y = element_text(size = 10)) +
    guides(color = "none",
      fill = guide_legend(nrow = 1, reverse=T,
      title.position = "left", title = "Pct voting for..."))

}

