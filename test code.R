# replicate analysis steps from:
# https://tidy-survey-r.github.io/tidy-survey-short-course/Presentation/Slides-day-1.html#22

anes_2020 <- readRDS("~/Data/r/anes_data/data/anes_2020.rds") %>%
  mutate(Weight=Weight/sum(Weight)*231592693)

glimpse(anes_2020)

anes_2020 %>%
  count(VotedPres2020_selection)

anes_2020w <- anes_2020 %>%
  as_survey_design(weights = Weight,
    strata = Stratum,
    ids = VarUnit,
    nest = TRUE)

anes_2020w %>%
  survey_count(VotedPres2020_selection)

femgd <- anes_2020w %>%
  filter(Gender=="Female", Education=="Graduate") %>%
  survey_count(name="n")
#Option 2:
femgd <- anes_2020w %>%
  filter(Gender=="Female", Education=="Graduate") %>%
  summarize(
    N=survey_total(),  .groups="drop"
  )
