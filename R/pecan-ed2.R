# Functions imported from PEcAn.ED2

##-------------------------------------------------------------------------------------------------#
##' convert parameters from PEcAn database default units to ED defaults
##'
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @return matrix or dataframe with values transformed
##' @author Shawn Serbin, David LeBauer, Carl Davidson, Ryan Kelly
convert.samples.ED <- function(trait.samples) {
  DEFAULT.LEAF.C <- 0.48
  DEFAULT.MAINTENANCE.RESPIRATION <- 1 / 2
  ## convert SLA from m2 / kg leaf to m2 / kg C

  # IF: trait.samples not being a list throws an error later in the write.config.xml.ED2
  trait.samples <- as.list(trait.samples)

  if ("SLA" %in% names(trait.samples)) {
    sla <- as.numeric(trait.samples[["SLA"]])
    trait.samples[["SLA"]] <- sla/DEFAULT.LEAF.C
  }

  # for model version compatibility (q and fineroot2leaf are the same)
  if ("fineroot2leaf" %in% names(trait.samples)) {
    trait.samples[["q"]] <- as.numeric(trait.samples[["fineroot2leaf"]])
  }

  ## convert leaf width / 1000
  if ("leaf_width" %in% names(trait.samples)) {
    lw <- as.numeric(trait.samples[["leaf_width"]])
    trait.samples[["leaf_width"]] <- lw / 1000
  }

  if ("root_respiration_rate" %in% names(trait.samples)) {
    rrr1 <- as.numeric(trait.samples[["root_respiration_rate"]])
    rrr2 <- rrr1 * DEFAULT.MAINTENANCE.RESPIRATION
    trait.samples[["root_respiration_rate"]] <- arrhenius.scaling(rrr2, old.temp = 25, new.temp = 15)
    # model version compatibility (rrr and rrf are the same)
    trait.samples[["root_respiration_factor"]] <- trait.samples[["root_respiration_rate"]]
  }

  if ("Vcmax" %in% names(trait.samples)) {
    vcmax <- as.numeric(trait.samples[["Vcmax"]])
    trait.samples[["Vcmax"]] <- arrhenius.scaling(vcmax, old.temp = 25, new.temp = 15)
    # write as Vm0 for version compatibility (Vm0 = Vcmax @ 15C)
    trait.samples[["Vm0"]] <- trait.samples[["Vcmax"]]

    ## Convert leaf_respiration_rate_m2 to dark_resp_factor; requires Vcmax
    if ("leaf_respiration_rate_m2" %in% names(trait.samples)) {
      leaf_resp <- as.numeric(trait.samples[["leaf_respiration_rate_m2"]])

      ## First scale variables to 15 degC
      trait.samples[["leaf_respiration_rate_m2"]] <-
        arrhenius.scaling(leaf_resp, old.temp = 25, new.temp = 15)
      # convert leaf_respiration_rate_m2 to Rd0 (variable used in ED2)
      trait.samples[["Rd0"]] <- trait.samples[["leaf_respiration_rate_m2"]]

      ## Calculate dark_resp_factor -- Will be depreciated when moving from older versions of ED2
      trait.samples[["dark_respiration_factor"]] <-
        trait.samples[["leaf_respiration_rate_m2"]] / trait.samples[["Vcmax"]]


    }  ## End dark_respiration_factor loop
  }  ## End Vcmax
  # for debugging conversions save(trait.samples, file = file.path(settings$outdir,
  # 'trait.samples.Rdata'))

  # return converted samples
  return(trait.samples)
}
# ==================================================================================================#


#' Write ED2 config.xml file
#'
#' Refactored by Alexey Shiklomanov to allow use in PEcAn RTM module.
#' @param settings PEcAn settings file. Settings required for this script are: model$revision, model$config.header, constants
#' @param trait.values List of trait values with which to replace defaults
#' @param defaults List of defaults to process. Default = settings$constants
#' @return R XML object containing full ED2 XML file
#' @author David LeBauer, Shawn Serbin, Carl Davidson, Alexey Shiklomanov
write.config.xml.ED2 <- function(settings, trait.values, defaults = settings$constants) {

  edhistory <- read.csv2(system.file("history.rgit.csv", package = "fortebaseline"))
  edtraits <- names(edhistory)

  ## Get ED2 specific model settings and put into output config xml file
  xml <- listToXml(settings$model$config.header, "config")

  ## Process the names in defaults. Runs only if names(defaults) are null or have at least one
  ## instance of name attribute 'pft'. Otherwise, AS assumes that names in defaults are already set
  ## to the corresponding PFT names.
  currentnames <- names(defaults)
  if (is.null(currentnames) | "pft" %in% currentnames) {
    newnames <- sapply(defaults, "[[", "name")
    newnames.notnull <- which(!sapply(newnames, is.null))
    names(defaults)[newnames.notnull] <- newnames[newnames.notnull]
  }

  for (i in seq_along(trait.values)) {
    group <- names(trait.values)[i]
    if (group == "env") {

      ## set defaults from config.header

    } else {
      # Make this agnostic to the way PFT names are defined in `trait.values` -- either directly as
      # list names or as object 'name' within each sublist is fine
      if (group == "pft") {
        pft <- trait.values[[i]]$name
      } else {
        pft <- group
      }

      # This is the ED PFT number (1-17; see comments in ED2IN file
      # for PFT definitions). It is used to set any ED parameters that
      # are not set explicitly from PEcAn.
      pft.number <- settings[["pfts"]][[i]][["ed2_pft_number"]]

      if (is.null(pft.number)) stop("Unknown PFT number")

      if (grepl("soil", pft)) {
        stop("Soil data not available")
        data(soil, package = "PEcAn.ED2")
        vals <- as.list(soil)
        names(vals) <- colnames(soil)

        converted.trait.values <- convert.samples.ED(trait.values[[i]])
        vals <- modifyList(vals, converted.trait.values)

        decompositon.xml <- PEcAn.settings::listToXml(vals, "decomposition")
        xml <- XML::append.xmlNode(xml, decompositon.xml)

      } else if (length(pft.number) == 0) {
        stop(glue::glue(
          "Unable to set PFT number automatically. ",
          "PFT `{pft}` was not matched in `pftmapping`. ",
          "Either set the PFT number explicitly via the ",
          "`<ed2_pft_number>` XML tag (recommended), ",
          "or add the PFT to `pftmapping.csv` file in ",
          "`models/ed/data/pftmapping.csv`."
        ))

      } else {
        ## Get default trait values from ED history
        vals <- as.list(edhistory[edhistory$num == pft.number, ])

        ## Convert trait values to ED units
        converted.trait.values <- convert.samples.ED(trait.values[[i]])

        ## Selectively replace defaults with trait values
        vals <- modifyList(vals, converted.trait.values)

        ## Convert settings constants to ED units
        converted.defaults <- convert.samples.ED(defaults[[pft]]$constants)

        ## Selectively replace defaults and trait values with constants from settings
        if (!is.null(converted.defaults)){
          vals <- modifyList(vals, converted.defaults)
        }

        ## Make sure that include_pft is set to 1
        vals$include_pft = 1

        pft.xml <- listToXml(vals, "pft")
        xml <- XML::append.xmlNode(xml, pft.xml)
      }

    }
  }
  return(xml)
} # write.config.xml.ED2

#' Convert List to XML
#'
#' Can convert list or other object to an xml object using xmlNode
#' @param item object to be converted.
#'   Despite the function name, need not actually be a list
#' @param tag xml tag
#' @return xmlNode
#' @author David LeBauer, Carl Davidson, Rob Kooper
listToXml <- function(item, tag) {

  # just a textnode, or empty node with attributes
  if (typeof(item) != "list") {
    if (length(item) > 1) {
      xml <- XML::xmlNode(tag)
      for (name in names(item)) {
        XML::xmlAttrs(xml)[[name]] <- item[[name]]
      }
      return(xml)
    } else {
      return(XML::xmlNode(tag, item))
    }
  }

  # create the node
  if (identical(names(item), c("text", ".attrs"))) {
    # special case a node with text and attributes
    xml <- XML::xmlNode(tag, item[["text"]])
  } else {
    # node with child nodes
    xml <- XML::xmlNode(tag)
    for (i in seq_along(item)) {
      if (is.null(names(item)) || names(item)[i] != ".attrs") {
        xml <- XML::append.xmlNode(xml, listToXml(item[[i]], names(item)[i]))
      }
    }
  }

  # add attributes to node
  attrs <- item[[".attrs"]]
  for (name in names(attrs)) {
    XML::xmlAttrs(xml)[[name]] <- attrs[[name]]
  }
  return(xml)
} # listToXml.default
