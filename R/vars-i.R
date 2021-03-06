# cohort (NCOHORTS_GLOBAL)
cohort_vars <- function() {
  c(
    "BALIVE",
    "BA_CO",
    "BDEAD",
    "BLEAF",
    "BROOT",
    "BSAPWOODA",
    "BSAPWOODB",
    "BSTORAGE",
    "CROWN_AREA_CO",
    "DBH",
    "FMEAN_A_CLOSED_CO",
    "FMEAN_A_CO2_CO",
    "FMEAN_A_LIGHT_CO",
    "FMEAN_A_NET_CO",
    "FMEAN_A_OPEN_CO",
    "FMEAN_A_RUBP_CO",
    "FMEAN_FSN_CO",
    "FMEAN_FSW_CO",
    "FMEAN_FS_OPEN_CO",
    "FMEAN_GPP_CO",
    "FMEAN_INTERCEPTED_AL_CO",
    "FMEAN_INTERCEPTED_AW_CO",
    "FMEAN_LEAF_ENERGY_CO",
    "FMEAN_LEAF_FLIQ_CO",
    "FMEAN_LEAF_GBW_CO",
    "FMEAN_LEAF_GROWTH_RESP_CO",
    "FMEAN_LEAF_GSW_CO",
    "FMEAN_LEAF_HCAP_CO",
    "FMEAN_LEAF_PSI_CO",
    "FMEAN_LEAF_RESP_CO",
    "FMEAN_LEAF_STORAGE_RESP_CO",
    "FMEAN_LEAF_TEMP_CO",
    "FMEAN_LEAF_VPDEF_CO",
    "FMEAN_LEAF_WATER_CO",
    "FMEAN_LEAF_WATER_INT_CO",
    "FMEAN_LIGHT_LEVEL_BEAM_CO",
    "FMEAN_LIGHT_LEVEL_CO",
    "FMEAN_LIGHT_LEVEL_DIFF_CO",
    "FMEAN_NPP_CO",
    "FMEAN_PAR_LEVEL_BEAM",
    "FMEAN_PAR_LEVEL_DIFFD",
    "FMEAN_PAR_LEVEL_DIFFU",
    "FMEAN_PAR_L_BEAM_CO",
    "FMEAN_PAR_L_CO",
    "FMEAN_PAR_L_DIFF_CO",
    "FMEAN_PLRESP_CO",
    "FMEAN_PSI_CLOSED_CO",
    "FMEAN_PSI_OPEN_CO",
    "FMEAN_RLONG_L_CO",
    "FMEAN_RLONG_W_CO",
    "FMEAN_ROOT_GROWTH_RESP_CO",
    "FMEAN_ROOT_RESP_CO",
    "FMEAN_ROOT_STORAGE_RESP_CO",
    "FMEAN_RSHORT_L_CO",
    "FMEAN_RSHORT_W_CO",
    "FMEAN_SAPA_GROWTH_RESP_CO",
    "FMEAN_SAPA_STORAGE_RESP_CO",
    "FMEAN_SAPB_GROWTH_RESP_CO",
    "FMEAN_SAPB_STORAGE_RESP_CO",
    "FMEAN_SENSIBLE_LC_CO",
    "FMEAN_SENSIBLE_WC_CO",
    "FMEAN_TRANSP_CO",
    "FMEAN_VAPOR_LC_CO",
    "FMEAN_VAPOR_WC_CO",
    "FMEAN_WATER_SUPPLY_CO",
    "FMEAN_WFLUX_GW_CO",
    "FMEAN_WFLUX_WL_CO",
    "FMEAN_WOOD_ENERGY_CO",
    "FMEAN_WOOD_FLIQ_CO",
    "FMEAN_WOOD_GBW_CO",
    "FMEAN_WOOD_HCAP_CO",
    "FMEAN_WOOD_PSI_CO",
    "FMEAN_WOOD_TEMP_CO",
    "FMEAN_WOOD_WATER_CO",
    "FMEAN_WOOD_WATER_INT_CO",
    "FMEAN_WSHED_LG_CO",
    "FMEAN_WSHED_WG_CO",
    "HITE",
    "LAI_CO",
    "LEAF_GROWTH_RESP",
    "LEAF_STORAGE_RESP",
    "NPLANT",
    "PFT",
    "ROOT_GROWTH_RESP",
    "ROOT_STORAGE_RESP",
    "SAPA_GROWTH_RESP",
    "SAPA_STORAGE_RESP",
    "SAPB_GROWTH_RESP",
    "SAPB_STORAGE_RESP",
    "WAI_CO"
  )
}

# soil (length(SLZ)) x site (1)
soil_vars <- function() {
  c(
    "FMEAN_SENSIBLE_GG_PY",
    "FMEAN_SMOIST_GG_PY",
    "FMEAN_SOIL_ENERGY_PY",
    "FMEAN_SOIL_FLIQ_PY",
    "FMEAN_SOIL_MSTPOT_PY",
    "FMEAN_SOIL_TEMP_PY",
    "FMEAN_SOIL_WATER_PY",
    "FMEAN_TRANSLOSS_PY",
    "NTEXT_SOIL"
  )
}

# site (1)
scalar_vars <- function() {
  c(
    "AGE",
    "AREA",
    "AVG_MONTHLY_WATERDEF",
    "BASEFLOW",
    "CCWD_FLUX",
    "CDEAD_GROW",
    "CLEAF_GROW",
    "CLEAF_LITTER_FLUX",
    "CROOT_GROW",
    "CROOT_LITTER_FLUX",
    "CSTORE_GROW",
    "CWD_C_PY",
    "CWD_N_PY",
    "DIST_TYPE",
    "FAST_SOIL_C",
    "FAST_SOIL_C_PY",
    "FAST_SOIL_N",
    "FAST_SOIL_N_PY",
    "FF_NHGT",
    "FMEAN_ALBEDO_NIR_PY",
    "FMEAN_ALBEDO_PAR_PY",
    "FMEAN_ALBEDO_PY",
    "FMEAN_ATM_CO2_PY",
    "FMEAN_ATM_PAR_DIFF_PY",
    "FMEAN_ATM_PAR_PY",
    "FMEAN_ATM_PRSS_PY",
    "FMEAN_ATM_RHOS_PY",
    "FMEAN_ATM_RLONG_PY",
    "FMEAN_ATM_RSHORT_DIFF_PY",
    "FMEAN_ATM_RSHORT_PY",
    "FMEAN_ATM_SHV_PY",
    "FMEAN_ATM_TEMP_PY",
    "FMEAN_ATM_THEIV_PY",
    "FMEAN_ATM_THETA_PY",
    "FMEAN_ATM_VELS_PY",
    "FMEAN_ATM_VPDEF_PY",
    "FMEAN_AVAILABLE_WATER_PY",
    "FMEAN_A_CLOSED_PY",
    "FMEAN_A_CO2_PY",
    "FMEAN_A_LIGHT_PY",
    "FMEAN_A_NET_PY",
    "FMEAN_A_OPEN_PY",
    "FMEAN_A_RUBP_PY",
    "FMEAN_BDEAD_PY",
    "FMEAN_CAN_CO2_PY",
    "FMEAN_CAN_GGND_PY",
    "FMEAN_CAN_PRSS_PY",
    "FMEAN_CAN_RHOS_PY",
    "FMEAN_CAN_SHV_PY",
    "FMEAN_CAN_TEMP_PY",
    "FMEAN_CAN_THEIV_PY",
    "FMEAN_CAN_THETA_PY",
    "FMEAN_CAN_VPDEF_PY",
    "FMEAN_CARBON_AC_PY",
    "FMEAN_CARBON_ST_PY",
    "FMEAN_CSTAR_PY",
    "FMEAN_CWD_RH_PY",
    "FMEAN_DPCPG_PY",
    "FMEAN_DRAINAGE_PY",
    "FMEAN_FSN_PY",
    "FMEAN_FSW_PY",
    "FMEAN_FS_OPEN_PY",
    "FMEAN_GND_SHV_PY",
    "FMEAN_GND_TEMP_PY",
    "FMEAN_GPP_PY",
    "FMEAN_INTERCEPTED_AL_PY",
    "FMEAN_INTERCEPTED_AW_PY",
    "FMEAN_LAI_PY",
    "FMEAN_LEAF_ENERGY_PY",
    "FMEAN_LEAF_FLIQ_PY",
    "FMEAN_LEAF_GBW_PY",
    "FMEAN_LEAF_GROWTH_RESP_PY",
    "FMEAN_LEAF_GSW_PY",
    "FMEAN_LEAF_HCAP_PY",
    "FMEAN_LEAF_RESP_PY",
    "FMEAN_LEAF_STORAGE_RESP_PY",
    "FMEAN_LEAF_TEMP_PY",
    "FMEAN_LEAF_VPDEF_PY",
    "FMEAN_LEAF_WATER_PY",
    "FMEAN_NEP_PY",
    "FMEAN_NIRUP_PY",
    "FMEAN_NPP_PY",
    "FMEAN_PARUP_PY",
    "FMEAN_PAR_GND_PY",
    "FMEAN_PAR_L_BEAM_PY",
    "FMEAN_PAR_L_DIFF_PY",
    "FMEAN_PAR_L_PY",
    "FMEAN_PCPG_PY",
    "FMEAN_PLRESP_PY",
    "FMEAN_PSI_CLOSED_PY",
    "FMEAN_PSI_OPEN_PY",
    "FMEAN_QDRAINAGE_PY",
    "FMEAN_QPCPG_PY",
    "FMEAN_QRUNOFF_PY",
    "FMEAN_QSTAR_PY",
    "FMEAN_QTHROUGHFALL_PY",
    "FMEAN_RH_PY",
    "FMEAN_RK4STEP_PY",
    "FMEAN_RLONGUP_PY",
    "FMEAN_RLONG_ALBEDO_PY",
    "FMEAN_RLONG_GND_PY",
    "FMEAN_RLONG_L_PY",
    "FMEAN_RLONG_W_PY",
    "FMEAN_RNET_PY",
    "FMEAN_ROOT_GROWTH_RESP_PY",
    "FMEAN_ROOT_RESP_PY",
    "FMEAN_ROOT_STORAGE_RESP_PY",
    "FMEAN_RSHORTUP_PY",
    "FMEAN_RSHORT_GND_PY",
    "FMEAN_RSHORT_L_PY",
    "FMEAN_RSHORT_W_PY",
    "FMEAN_RUNOFF_PY",
    "FMEAN_SAPA_GROWTH_RESP_PY",
    "FMEAN_SAPA_STORAGE_RESP_PY",
    "FMEAN_SAPB_GROWTH_RESP_PY",
    "FMEAN_SAPB_STORAGE_RESP_PY",
    "FMEAN_SENSIBLE_AC_PY",
    "FMEAN_SENSIBLE_GC_PY",
    "FMEAN_SENSIBLE_LC_PY",
    "FMEAN_SENSIBLE_WC_PY",
    "FMEAN_SFCW_DEPTH_PY",
    "FMEAN_SFCW_ENERGY_PY",
    "FMEAN_SFCW_FLIQ_PY",
    "FMEAN_SFCW_MASS_PY",
    "FMEAN_SFCW_TEMP_PY",
    "FMEAN_SKIN_TEMP_PY",
    "FMEAN_SOIL_WETNESS_PY",
    "FMEAN_THROUGHFALL_PY",
    "FMEAN_TRANSP_PY",
    "FMEAN_TSTAR_PY",
    "FMEAN_USTAR_PY",
    "FMEAN_VAPOR_AC_PY",
    "FMEAN_VAPOR_GC_PY",
    "FMEAN_VAPOR_LC_PY",
    "FMEAN_VAPOR_WC_PY",
    "FMEAN_WATER_SUPPLY_PY",
    "FMEAN_WOOD_ENERGY_PY",
    "FMEAN_WOOD_FLIQ_PY",
    "FMEAN_WOOD_GBW_PY",
    "FMEAN_WOOD_HCAP_PY",
    "FMEAN_WOOD_TEMP_PY",
    "FMEAN_WOOD_WATER_PY",
    "FMEAN_WSHED_LG_PY",
    "FMEAN_WSHED_WG_PY",
    "ISOILFLG",
    "LATITUDE",
    "LONGITUDE",
    "LSL",
    "MINERALIZED_SOIL_N",
    "MINERAL_SOIL_N_PY",
    "NBIOMASS_UPTAKE",
    "NCOHORTS_GLOBAL",
    "NCOL_SOIL",
    "NCWD_FLUX",
    "NDCYCLE",
    "NDEAD_GROW",
    "NGROSS_MIN",
    "NLEAF_GROW",
    "NLEAF_LITTER_FLUX",
    "NNET_MIN",
    "NPATCHES_GLOBAL",
    "NPOLYGONS_GLOBAL",
    "NROOT_GROW",
    "NROOT_LITTER_FLUX",
    "NSITES_GLOBAL",
    "NSTORE_GROW",
    "NZG",
    "NZS",
    "PACO_ID",
    "PACO_N",
    "PATCH_COUNT",
    "PYSI_ID",
    "PYSI_N",
    "QRUNOFF",
    "RUNOFF",
    "SIPA_ID",
    "SIPA_N",
    "SITENUM",
    "SLOW_SOIL_C",
    "SLOW_SOIL_C_PY",
    "SLXCLAY",
    "SLXSAND",
    "STRUCTURAL_SOIL_C",
    "STRUCTURAL_SOIL_L",
    "STRUCT_SOIL_C_PY",
    "STRUCT_SOIL_L_PY",
    "SWLIQ",
    "TOTAL_AGB",
    "TOTAL_AGB_GROWTH",
    "TOTAL_AGB_MORT",
    "TOTAL_AGB_RECRUIT",
    "TOTAL_BASAL_AREA",
    "TOTAL_BASAL_AREA_GROWTH",
    "TOTAL_BASAL_AREA_MORT",
    "TOTAL_BASAL_AREA_RECRUIT",
    "XATM",
    "YATM",
    "ZBAR"
  )
}

# pft(17) x dbh_class(11) x site(1)
py_vars <- function() {
  c(
    "AGB_PY",
    "BALIVE_N_PY",
    "BALIVE_PY",
    "BASAL_AREA_PY",
    "BDEAD_N_PY",
    "BDEAD_PY",
    "BLEAF_N_PY",
    "BLEAF_PY",
    "BROOT_N_PY",
    "BROOT_PY",
    "BSAPWOODA_N_PY",
    "BSAPWOODA_PY",
    "BSAPWOODB_N_PY",
    "BSAPWOODB_PY",
    "BSEEDS_N_PY",
    "BSEEDS_PY",
    "BSTORAGE_N_PY",
    "BSTORAGE_PY",
    "LAI_PY",
    "LEAF_DROP_PY",
    "LEAF_MAINTENANCE_PY",
    "NPLANT_PY",
    "ROOT_MAINTENANCE_PY",
    "WAI_PY"
  )
}
