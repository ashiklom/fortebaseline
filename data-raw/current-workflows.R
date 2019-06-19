library(dplyr)
library(readr)
library(forcats)

stopifnot(
  requireNamespace("here", quietly = TRUE),
  requireNamespace("RColorBrewer", quietly = TRUE),
  requireNamespace("usethis", quietly = TRUE)
)

current_workflows <- here::here("data-raw", "current-workflows.csv") %>%
  read_csv(col_types = "ddlll") %>%
  mutate(
    crown = factor(crown_model) %>%
      fct_recode("closed" = "FALSE", "finite" = "TRUE"),
    rtm = factor(multiple_scatter) %>%
      fct_recode("two-stream" = "FALSE", "multiple-scatter" = "TRUE"),
    traits = factor(trait_plasticity) %>%
      fct_recode("static" = "FALSE", "plastic" = "TRUE"),
    model = interaction(crown, rtm, traits, sep = " "),
    model_split = fct_relabel(model, ~gsub(" ", "\n", .))
  ) %>%
  arrange(model) %>%
  mutate(color = RColorBrewer::brewer.pal(n(), "Paired")) %>%
  select(workflow_id, short_id, crown:model_split,
         crown_model, multiple_scatter, trait_plasticity,
         color)

usethis::use_data(current_workflows, overwrite = TRUE)

if (FALSE) {

  # Preview color scheme
  library(ggplot2)
  ggplot(current_workflows) +
    aes(x = model_split, y = 1, fill = color) +
    geom_col() +
    scale_fill_identity()

  ggplot(current_workflows) +
    aes(x = crown, y = 1, fill = color) +
    geom_col() +
    facet_grid(vars(rtm), vars(traits)) +
    scale_fill_identity()

}
