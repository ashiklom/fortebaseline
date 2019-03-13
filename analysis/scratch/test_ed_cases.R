library(pecanapi)
import::from(magrittr, "%>%")
source(file.path("analysis", "scratch", "helpers.R"))

## model_id <- 99000000006                 # ED develop
model_id <- 99000000001                 # ED develop
site_id <- 1000000033                   # UMBS disturbance

machine_id <- 99000000001

## DBI::dbSendStatement(con, paste0(
##   "INSERT INTO models (model_name, revision, modeltype_id) ",
##   "VALUES ('ED2-experimental', 'experimental', 1)"
## ))

## DBI::dbSendStatement(con, glue::glue(
##   "INSERT INTO dbfiles (container_type, container_id, file_name, file_path, machine_id) ",
##   "VALUES ('Model', {model_id}, 'ed2.develop', '/usr/local/bin', {machine_id})"
## ))

## dplyr::tbl(con, "dbfiles") %>% dplyr::glimpse()
## dplyr::tbl(con, "machines") %>%
##   dplyr::filter(hostname %like% "%docker%") %>%
##   dplyr::pull(id)

workflow <- insert_new_workflow(con, site_id, model_id,
                                start_date = "1901-06-01",
                                end_date = "1901-08-31")
workflow_id <- workflow[["id"]]

pft_list <- list(
  list(name = "temperate.Early_Hardwood", ed2_pft_number = 9),
  list(name = "temperate.North_Mid_Hardwood", ed2_pft_number = 10),
  list(name = "temperate.Late_Hardwood", ed2_pft_number = 11)
)

settings <- list() %>%
  add_workflow(workflow) %>%
  add_database() %>%
  add_pft_list(pft_list) %>%
  add_rabbitmq(model_queue = "ED2_develop") %>%
  modifyList(list(
    meta.analysis = list(iter = 3000, random.effects = FALSE),
    run = list(inputs = list(met = list(source = "CRUNCEP", output = "ED2", method = "ncss"))),
    ensemble = list(size = 1, variable = "NPP")
  )) %>%
  modifyList(list(
    run = list(inputs = list(
      lu = list(id = 294),
      soil = list(id = 297),
      thsum = list(id = 295),
      veg = list(id = 296)
    )),
    model = list(
      exact.dates = "true",
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
        ICANRAD = 2,
        CROWN_MOD = 0,
        N_PLANT_LIM = 0,
        N_DECOMP_LIM = 1,
        INCLUDE_THESE_PFT = "9,10,11"
      )
    )
  ))

submit_workflow(settings)
follow_workflow(workflow_id, start_at = Inf)

if (FALSE) {
  rout <- output_url(workflow_id, "workflow.Rout") %>% readLines()
  ## writeLines(rout[])
  ## writeLines(tail(rout, 200))
  ## tail(rout, 40) %>% writeLines()

  # Follow model log file
  logfile <- run_url(workflow_id, "logfile.txt")
  tail_file(logfile)

  read_output_var <- function(workflow_id, variable) {
    ncfile <- run_dap(workflow_id, "1980.nc")
    nc <- ncdf4::nc_open(ncfile)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    ncdf4::ncvar_get(nc, variable)
  }

  albedo_2s <- read_output_var(99000000035, "Albedo")
  albedo_ms <- read_output_var(99000000034, "Albedo")
  albedo_2s_nc <- read_output_var(99000000036, "Albedo")
  matplot(cbind(albedo_2s, albedo_ms, albedo_2s_nc), type = "l")
  plot(albedo_2s - albedo_2s_nc, type = "l")

  outfile <- run_dap(workflow_id, "1980.nc")
  nc <- ncdf4::nc_open(outfile)
  gpp <- ncdf4::ncvar_get(nc, "GPP")
  npp <- ncdf4::ncvar_get(nc, "NPP")
  albedo <- ncdf4::ncvar_get(nc, "Albedo")
  ## names(nc$var)
  plot(albedo, type = 'l')

}

## while (TRUE) {
if (FALSE) {
  ## runid <- 99000000117
  ## message("As of: ", Sys.time())
  readLines(run_url(workflow_id, "logfile.txt", runid)) %>% tail(2) %>% writeLines()
  writeLines("---------------")
  ## Sys.sleep(2)
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
