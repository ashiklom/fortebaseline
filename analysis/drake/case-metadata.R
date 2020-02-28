plan <- bind_plans(plan, drake_plan(
  params = read_csv(file_in(!!ensemble_params_file),
                    col_types = c(name = "c", .default = "d")),
  model_ids = c("CTS", "CTP", "CMS", "CMP",
                 "FTS", "FTP", "FMS", "FMP"),
  cases = expand_grid(model_id = model_ids, params) %>%
    mutate(
      crown = fct_recode(substring(model_id, 1, 1),
                         "closed" = "C", "finite" = "F") %>%
        fct_relevel("closed", "finite"),
      rtm = fct_recode(substring(model_id, 2, 2),
                       "multi-scatter" = "M", "two-stream" = "T") %>%
        fct_relevel("two-stream", "multi-scatter"),
      traits = fct_recode(substring(model_id, 3, 3),
                          "static" = "S", "plastic" = "P") %>%
        fct_relevel("static", "plastic"),
      model = interaction(crown, rtm, traits, sep = " "),
      model_id = factor(model_id, model_ids),
      case = sprintf("%03d%s", param_id, model_id)
    ) %>%
    select(case, everything()) %>%
    as_tibble(),
  models = cases %>%
    distinct(model_id, model, crown, rtm, traits) %>%
    arrange(model) %>%
    mutate(color = RColorBrewer::brewer.pal(n(), "Paired")),
  model_colors = tibble::deframe(models[, c("model", "color")])
))
