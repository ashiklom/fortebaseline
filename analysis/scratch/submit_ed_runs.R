library(pecanapi)
import::from(magrittr, "%>%")
source(file.path("analysis", "scratch", "helpers.R"))

model_id <- 99000000006                 # ED develop
site_id <- 1000000033                   # UMBS disturbance

workflow <- insert_new_workflow(con, site_id, model_id,
                                start_date = "1902-01-01",
                                end_date = "2003-12-30")
workflow_id <- workflow[["id"]]

pft_list <- list(
  list(name = "umbs.early_hardwood", ed2_pft_number = 9),
  list(name = "umbs.mid_hardwood", ed2_pft_number = 10),
  list(name = "umbs.late_hardwood", ed2_pft_number = 11),
  list(name = "umbs.northern_pine", ed2_pft_number = 6)
)

settings <- list() %>%
  add_workflow(workflow) %>%
  add_database() %>%
  add_pft_list(pft_list) %>%
  add_rabbitmq(model_queue = "ED2_develop") %>%
  modifyList(list(
    meta.analysis = list(iter = 3000, random.effects = FALSE),
    run = list(inputs = list(met = list(source = "CRUNCEP", output = "ED2", method = "ncss"))),
    ensemble = list(size = 50, variable = "NPP")
  )) %>%
  modifyList(list(
    run = list(inputs = list(
      lu = list(id = 294),
      soil = list(id = 297),
      thsum = list(id = 295),
      veg = list(id = 296)
    )),
    model = list(
      phenol.scheme = 0,
      edin = "ED2IN.rgit",
      prerun = "ulimit -s unlimited",
      barebones_ed2in = "true",
      ed2in_tags = list(
        IOOUTPUT = 0,
        PLANT_HYDRO_SCHEME = 0,
        ISTOMATA_SCHEME = 0,
        ISTRUCT_GROWTH_SCHEME = 0,
        TRAIT_PLASTICITY_SCHEME = 0,
        INCLUDE_THESE_PFT = "6,9,10,11"
      )
    )
  ))

submit_workflow(settings)
watch_workflow(workflow_id)

while (TRUE) {
  readLines(run_url(workflow_id, "logfile.txt")) %>% tail(2) %>% writeLines()
  writeLines("---------------")
  Sys.sleep(2)
}

if (FALSE) {
  output <- workflow_output(workflow_id)
  writeLines(output)
  readLines(run_url(workflow_id, "ED")) %>% writeLines()
  readLines(output_url(workflow_id, "pecan.xml")) %>% writeLines()

  run_id <- list_runs(con, workflow_id)[["id"]]
  ed2in <- output_url(workflow_id, file.path("run", run_id, "ED2IN")) %>%
    readLines()
  writeLines(ed2in)

  result_nc <- ncdf4::nc_open(run_dap(workflow_id, "2004.nc"))
  gpp <- ncdf4::ncvar_get(result_nc, "GPP")
  time <- ncdf4::ncvar_get(result_nc, "time")
  ncdf4::nc_close(result_nc)

  plot(time, gpp, type = "l")
}
