## analysis of questions related to trust and voting in 2024 election

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(gregeRs) # custom functions
library(srvyr) # survey analysis functions
library(DataExplorer) # EDA tools
library(ggtext) # enhancements for text in ggplot
library(scales)
library(ggrepel)

# some custom functions  - Now in gregeRs package
## source("~/Data/r/basic functions.R")
# functions for project
source("project_functions.R")

# create ordered factors of index variables
anes_2024_trust <- readRDS("data/anes_2024.rds") %>%
  mutate(ht_ind_v241229_f = always_never_f(ht_ind_v241229)) %>%
  mutate(ht_ind_v241230_f = always_never_f(ht_ind_v241230)) %>%
  mutate(ht_ind_v241233_f = always_never_f(ht_ind_v241233)) %>%
  mutate(ht_ind_v241234_f = always_never_f(ht_ind_v241234)) %>%
  mutate(ht_ind_v241315_f = always_never_f(ht_ind_v241315)) %>%
  mutate(ht_ind_v241335_f = always_never_f(ht_ind_v241335)) %>%
  mutate(ht_ind_v242417_f = always_never_4(ht_ind_v242417)) %>%
  mutate(ht_ind_v242418_f = always_never_4(ht_ind_v242418)) %>%
  mutate(ht_ind_v242419_f = always_never_4(ht_ind_v242419)) %>%
  mutate(ht_ind_v242420_f = always_never_4(ht_ind_v242420)) %>%
  mutate(ht_ind_v242421_f = always_never_4(ht_ind_v242421)) %>%
  mutate(ht_ind_v242422_f = always_never_4(ht_ind_v242422)) %>%
  mutate(ht_ind_v242423_f = always_never_4(ht_ind_v242423))

glimpse(anes_2024_trust)

# create survey object
anes_2024_weighted_trust <- anes_2024_trust %>%
  srvyr::as_survey_design(weights = Weight2, strata = Stratum, ids = VarUnit, nest=TRUE)

# test creation of dfs for each trust item
vote_by_item(ht_ind_v241229_f)
vote_by_item2(ht_ind_v241229_f, voted_president_2024)

## analysis! create dfs, make plots

# index_high_trust = ht_ind_v241229 + ht_ind_v241230 +
#   ht_ind_v241233 + ht_ind_v241234 + ht_ind_v241315 + ht_ind_v241335

## gt tables of pcts of response for each variable
anes_2024_weighted_trust %>%
  filter(!is.na(ht_ind_v241229_f)) %>%
  filter(!voted_president_2024 == "Other") %>%
  group_by(voted_president_2024) %>%
  survey_count(ht_ind_v241229_f, vartype = c("se", "cv")) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# make dfs
trust_pre_govtdc_1 <- item_by_vote(ht_ind_v241229_f)
trust_pre_courts_1 <- item_by_vote(ht_ind_v241230_f)
trust_pre_electionoff_1 <- item_by_vote(ht_ind_v241315_f)
trust_pre_newsmedia_1 <- item_by_vote(ht_ind_v241335_f)
trust_people_1 <- item_by_vote(ht_ind_v241234_f)

plot_trust_pre_govtdc_1 <-
  plot_item_vote(trust_pre_govtdc_1, ht_ind_v241229_f)

plot_trust_pre_govtdc_1 +
  labs(
    title = "Kamala Harris voters were more likely than Donald Trump voters to
    have higher levels of trust in the federal government.",
    subtitle = "*Item V241229; PRE: How often trust government in Washington to do what is right. <br>
    Percents are proportion responding to item ratings by candidate voted for.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_pre_govtdc_1.jpg", width = 15, height = 8,
  units = "in", dpi = 300)

plot_trust_pre_courts_1 <-
  plot_item_vote(trust_pre_courts_1, ht_ind_v241230_f)

plot_trust_pre_courts_1 +
  labs(
    title = "People with moderately favorable views on the courts were only slightly more likely to vote
     for <span style = 'color: #00BFC4;'>Kamala Harris</span> than
    for <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V241230; PRE: How often trust the court system to do what is right. <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or  <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_pre_courts_1.jpg", width = 15, height = 8,
  units = "in", dpi = 300)


 # make dfs
trust_pre_govtdc <- vote_by_item(ht_ind_v241229_f)
trust_pre_courts <- vote_by_item(ht_ind_v241230_f)
trust_pre_electionoff <- vote_by_item(ht_ind_v241315_f)
trust_pre_newsmedia <- vote_by_item(ht_ind_v241335_f)
trust_people <- vote_by_item(ht_ind_v241234_f)

 # make plots
plot_trust_pre_govtdc <-
  plot_vote_item(trust_pre_govtdc, ht_ind_v241229_f)

plot_trust_pre_govtdc +
  labs(
    title = "People with higher levels of trust in the federal government were much more likely to vote for
    <span style = 'color: #00BFC4;'> Kamala Harris</span> than
    for <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V241229; PRE: How often trust government in Washington to do what is right. <br>
    Percents are share of respondents chosing level of trust who voted for
    <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or  <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_pre_govtdc.jpg", width = 15, height = 8,
  units = "in", dpi = 300)

plot_trust_pre_courts <-
  plot_vote_item(trust_pre_courts, ht_ind_v241230_f)

plot_trust_pre_courts +
  labs(
    title = "People with moderately favorable views on the courts were only slightly more likely to vote
     for <span style = 'color: #00BFC4;'>Kamala Harris</span> than
    for <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V241230; PRE: How often trust the court system to do what is right. <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or  <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_pre_courts.jpg", width = 15, height = 8,
  units = "in", dpi = 300)

plot_trust_pre_electionoff <-
  plot_vote_item(trust_pre_electionoff, ht_ind_v241315_f)

plot_trust_pre_electionoff +
  labs(
    title = "People with higher levels of trust in election officials were far more likely
    to have voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span> than
    for  <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V241315; PRE: How much do you trust officials who oversee elections
where you live. <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or  <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_pre_electionoff.jpg", width = 15, height = 8,
  units = "in", dpi = 300)

plot_trust_pre_newsmedia <-
  plot_vote_item(trust_pre_newsmedia, ht_ind_v241335_f)

plot_trust_pre_newsmedia +
  labs(
    title = "People who trusted the mews media half or more of the time were far more likely
    to have voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span> than
    for <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V241335; PRE: trust and confidence do you have in the news media
    report the news fully, accurately, & fairly?. <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_pre_newsmedia.jpg", width = 15, height = 8,
  units = "in", dpi = 300)

plot_trust_pre_people <-
  plot_vote_item(trust_people, ht_ind_v241234_f)

plot_trust_pre_people +
  labs(
    title = "People who trust others most of the time were more likely
    to have voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span> than
    for <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V241234; PRE: Generally speaking, how often can you trust other people?. <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_pre_people.jpg", width = 15, height = 8,
  units = "in", dpi = 300)




# high index trust post

# Trust congress
trust_post_congress <- vote_by_item(ht_ind_v242417_f)

plot_trust_post_congress <-
  plot_vote_item(trust_post_congress, ht_ind_v242417_f)

plot_trust_post_congress +
  labs(
    title = "As the level of trust in Congress declines, the share of votes
    for <span style = 'color: #00BFC4;'>Kamala Harris</span> increases.",
    subtitle = "*Item V242417; POST: How about Congress - do you trust them a lot, (etc)? <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_post_congress.jpg", width = 15, height = 8,
  units = "in", dpi = 300)


# Trust government
trust_post_govt <- vote_by_item(ht_ind_v242418_f)

plot_trust_post_govt <-
  plot_vote_item(trust_post_govt, ht_ind_v242418_f)

plot_trust_post_govt +
  labs(
    title = "People reporting moderate to low levels of trust in government were more likely to
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>, <br> while people with the most
    negative views were more likely to vote for <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V242418; POST: How about government - do you trust them a lot, (etc)? <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_post_govt.jpg", width = 15, height = 8,
  units = "in", dpi = 300)


# Trust judiciary
trust_post_judic <- vote_by_item(ht_ind_v242419_f)

plot_trust_post_judic <-
  plot_vote_item(trust_post_judic, ht_ind_v242419_f)

plot_trust_post_judic +
  labs(
    title = "People with more negative views of the judiciary were more likely to vote
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>.",
    subtitle = "*Item V242419; POST: How about the judiciary - do you trust them a lot, (etc)?<br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_post_judic.jpg", width = 15, height = 8,
  units = "in", dpi = 300)


# trust scientists
trust_post_sci <- vote_by_item(ht_ind_v242420_f)

plot_trust_post_sci <-
  plot_vote_item(trust_post_sci, ht_ind_v242420_f)

plot_trust_post_sci +
  labs(
    title = "People at the extremes of levels of trust in scientists supported their candidate by a
    5:1 margin.",
    subtitle = "*Item V242420; POST: How about scientists - do you trust them a lot, (etc)?<br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_post_sci.jpg", width = 15, height = 8,
  units = "in", dpi = 300)


# trust pol parties
trust_post_polpart <- vote_by_item(ht_ind_v242421_f)

plot_trust_post_polpart <-
  plot_vote_item(trust_post_polpart, ht_ind_v242421_f)

plot_trust_post_polpart +
  labs(
    title = "People with the highest levels of trust in political parties were equally as likely to
    vote for <span style = 'color: #00BFC4;'>Kamala Harris</span> as they were
    for <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V242421; POST: How about the political parties - do you trust them a lot, (etc)? <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_post_polpart.jpg", width = 15, height = 8,
  units = "in", dpi = 300)


# trust trad media
trust_post_tradmed <- vote_by_item(ht_ind_v242422_f)

plot_trust_post_tradmed <-
  plot_vote_item(trust_post_tradmed, ht_ind_v242422_f)

plot_trust_post_tradmed +
  labs(
    title = "People with moderate-to-high levels of trust in traditional media were at least three
    times as likely to vote
    for <span style = 'color: #00BFC4;'>Kamala Harris</span> <br>as they were
    for <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V242422; POST: How about traditional media - do you trust them a lot, (etc)? <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_post_tradmed.jpg", width = 15, height = 8,
  units = "in", dpi = 300)


# trust social media
trust_post_socmed <- vote_by_item(ht_ind_v242423_f)

plot_trust_post_socmed <-
  plot_vote_item(trust_post_socmed, ht_ind_v242423_f)

plot_trust_post_socmed +
  labs(
    title = "People with moderate-to-high levels of trust in traditional media were at least three
    times as likely to vote
    for <span style = 'color: #00BFC4;'>Kamala Harris</span> <br>as they were
    for <span style = 'color: #F8766D;'>Donald Trump</span>.",
    subtitle = "*Item V242422; POST: How about traditional media - do you trust them a lot, (etc)? <br>
    Percents are share of respondents chosing level of trust who voted
    for <span style = 'color: #00BFC4;'>Kamala Harris</span>
    or <span style = 'color: #F8766D;'>Donald Trump</span>.
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*")

ggsave("images/plot_trust_post_tradmed.jpg", width = 15, height = 8,
  units = "in", dpi = 300)



## regression to predict vote.





## base code
# grouped by trust q
anes_2024_weighted2 %>%
  filter(!is.na(ht_ind_v241229_f)) %>%
  filter(!is.na(voted_president_2024)) %>%
  group_by(ht_ind_v241229_f) %>%
  srvyr::survey_count(voted_president_2024, vartype = c("se", "cv")) %>%
  mutate(pct = n / sum(n)) %>%
  select(ht_ind_v241229_f, voted_president_2024, pct, everything())


## plot template
## for faint highlight lines in chart
vlines_df <- data.frame(xintercept = seq(-100, 100, 20))

glimpse(trust_pre_govtdc)

trust_pre_govtdc %>%
  mutate(pct = ifelse(voted_president_2024 == "Kamala Harris", pct *-1, pct)) %>%
  mutate(pct2 = round(pct * 100, 0)) %>%
  filter(!voted_president_2024 == "Other") %>%
  mutate(ht_ind_v241229_f= fct_reorder(ht_ind_v241229_f, desc(ht_ind_v241229_f))) %>%
  {. ->> tmp} %>%
  ggplot() +
  geom_col(aes(x = -50, y = ht_ind_v241229_f), width = 0.75, fill = "#e0e0e0") +
  geom_col(aes(x = 50, y = ht_ind_v241229_f), width = 0.75, fill = "#e0e0e0") +
  geom_col(aes(x = pct2, y = ht_ind_v241229_f, fill = voted_president_2024,
    color = voted_president_2024), width = 0.75) +
  scale_x_continuous(expand = c(0, 0),
    labels = function(x) abs(x), breaks = seq(-100, 100, 20)) +
  geom_vline(data = vlines_df, aes(xintercept = xintercept), color = "#FFFFFF", size = 0.1, alpha = 0.5) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("white", "white")) +
 # scale_fill_manual(values = c("Kamala Harris" = "#00BFC4", "Donald Trump" = "#F8766D")) +
  geom_text(data = subset(tmp, voted_president_2024 == "Kamala Harris"),
    aes(x = pct2, y = ht_ind_v241229_f, label = paste0(abs(pct2), "%")),
    size = 5, color = "white", position = position_dodge(width = .4), hjust = -0.09) +
  geom_text(data = subset(tmp, voted_president_2024 == "Donald Trump"),
    aes(x = pct2, y = ht_ind_v241229_f, label = paste0(abs(pct2), "%")),
    size = 5, color = "white", position = position_dodge(width = .4), hjust = 1.1) +
  geom_text(data = subset(tmp, voted_president_2024 == "Kamala Harris"),
    aes(x = -100, y = ht_ind_v241229_f,
    label = paste0("Coefficient of variation = ", percent(round(n_cv, 3)))),
    size = 4, color = "#3b3b3b", hjust = -.1, vjust = 4.2, fontface = "italic") +
  geom_text(data = subset(tmp, voted_president_2024 == "Donald Trump"),
    aes(x = 68, y = ht_ind_v241229_f,
      label = paste0("Coefficient of variation = ", percent(round(n_cv, 3)))),
    size = 4, color = "#3b3b3b", hjust = -.1, vjust = 4.2, fontface = "italic") +
  labs(x = "", y = "",
    title = "Kamala Harris voters were more likely than Trump voters to trust the federal government",
    subtitle = "*Item V241229; PRE: How often trust government in Washington to do what is right.
    Percents are share of respondents chosing level of trust who voted for Harris or Trump. <br>
    Excludes those who voted for other candidates.*",
    caption = "*Data from American National Election Survey 2024 time series*") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
    plot.title = element_markdown(size = 16),
    plot.subtitle = element_markdown(size = 12),
    plot.caption = element_markdown(),
    legend.position = "bottom", legend.justification = "center",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.y = element_text(size = 10)) +
  guides(
    color = "none",
    fill = guide_legend(nrow = 1, reverse=T,
    title.position = "left", title = "Pct voting for..."))

trust_pre_govtdc_1 %>%
  filter(!voted_president_2024 == "Other") %>%
  {. ->> tmp} %>%
  ggplot(aes(x = pct, y = voted_president_2024, fill = ht_ind_v241229_f)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  geom_text(data = subset(tmp, pct > .05),
    aes(label = scales::percent(round(pct , 2))),
    position = position_stack(vjust = 0.5),
    color= "grey40", vjust = 0.5, size = 8) +
  scale_x_continuous(
    expand = c(0,0),
    breaks = c(0, 1),
    labels = c("0", "100%")) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
    plot.title = element_markdown(size = 16),
    plot.subtitle = element_markdown(size = 12),
    plot.caption = element_markdown(),
    legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
    legend.key.width = unit(4, 'cm'), legend.margin=margin(-10, 0, 0, 0),
    legend.text = element_text(size = 12), legend.title = element_text(size = 16),
    # legend.position = "bottom", legend.justification = "left",
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 8),
    axis.text.y = element_text(size = 10)) +
  guides(fill = guide_legend(label.position = "bottom",
    reverse = TRUE, direction = "horizontal",
    nrow = 1, title = "", title.position = "left"))

plot_item_vote(trust_pre_govtdc_1, ht_ind_v241229_f)



