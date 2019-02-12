library(tidyverse)
library(fst)

workflow_id <- 99000000066
outdir <- file.path("analysis", "data", "model_output", workflow_id)
runs <- list.files(outdir)

pft_dict <- tribble(
  ~pft, ~num,
  "Early hardwood", 9,
  "Mid hardwood", 10,
  "Late hardwood", 11,
  "Pine", 6
) %>%
  mutate(pft = fct_inorder(pft))

read_run_configs <- function(run_id) {
  import::from("magrittr", "%>%", .into = "")
  import::from("pecanapi", "output_url", .into = "")
  import::from("XML", "xmlToList", "xmlParse", .into = "")
  import::from("tidyr", "gather", .into = "")
  import::from("dplyr", "bind_rows", "mutate_all", "mutate",
               "left_join", "select", .into = "")

  config_file <- output_url(
    workflow_id,
    file.path("run", run_id, "config.xml")
  )
  raw_xml <- xmlToList(xmlParse(config_file))

  bind_rows(raw_xml) %>%
    mutate_all(as.numeric) %>%
    mutate(run_id = !!run_id) %>%
    gather(variable, value, -num, -run_id) %>%
    left_join(pft_dict, by = "num") %>%
    select(run_id, pft, variable, value)
}

pft_configs <- map_dfr(runs, read_run_configs)

## pft_configs %>%
##   group_by(variable, pft) %>%
##   filter(sd(value) > 0) %>%
##   ungroup(variable, pft) %>%
##   ggplot() +
##   aes(x = pft, y = value) +
##   geom_violin() +
##   geom_point(alpha = 0.5) +
##   facet_wrap(~variable, scales = "free_y")

fst::write_fst(pft_configs, "analysis/data/derived-data/ed-params.fst")
