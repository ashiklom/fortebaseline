# Monthly variables, by analogy to `cohort_vars`
pft_vars_m <- function() {
  c(
    "AGB_PY",
    "BALIVE_N_PY",
    "BALIVE_PY",
    "BASAL_AREA_PY",
    "BDEAD_N_PY",
    "BDEAD_PY",
    "BSAPWOODA_N_PY",
    "BSAPWOODA_PY",
    "BSAPWOODB_N_PY",
    "BSAPWOODB_PY",
    "BSEEDS_N_PY",
    "BSEEDS_PY",
    "MMEAN_BLEAF_N_PY",
    "MMEAN_BLEAF_PY",
    "MMEAN_BROOT_N_PY",
    "MMEAN_BROOT_PY",
    "MMEAN_BSTORAGE_N_PY",
    "MMEAN_BSTORAGE_PY",
    "MMEAN_LAI_PY",
    "MMEAN_LEAF_DROP_PY",
    "MMEAN_LEAF_MAINTENANCE_PY",
    "MMEAN_ROOT_MAINTENANCE_PY",
    "NPLANT_PY",
    "WAI_PY"
  )
}

cb_vars_m <- function() {
  c("CB", "CB_LIGHTMAX", "CB_MLMAX", "CB_MOISTMAX")
}

soil_vars_m <- function() {
  c(
    "DMEAN_SENSIBLE_GG_PY",
    "DMEAN_SMOIST_GG_PY",
    "DMEAN_SOIL_ENERGY_PY",
    "DMEAN_SOIL_FLIQ_PY",
    "DMEAN_SOIL_MSTPOT_PY",
    "DMEAN_SOIL_TEMP_PY",
    "DMEAN_SOIL_WATER_PY",
    "DMEAN_TRANSLOSS_PY",
    "MMEAN_SENSIBLE_GG_PY",
    "MMEAN_SMOIST_GG_PY",
    "MMEAN_SOIL_ENERGY_PY",
    "MMEAN_SOIL_FLIQ_PY",
    "MMEAN_SOIL_MSTPOT_PY",
    "MMEAN_SOIL_TEMP_PY",
    "MMEAN_SOIL_WATER_PY",
    "MMEAN_TRANSLOSS_PY",
    "NTEXT_SOIL"
  )
}

cohort_vars_m <- function() {
  c(
    "AGB_CO",
    "BALIVE",
    "BA_CO",
    "BDEAD",
    "BLEAF",
    "BROOT",
    "BSAPWOODA",
    "BSAPWOODB",
    "BSEEDS_CO",
    "BSTORAGE",
    "CBR_BAR",
    "CENSUS_STATUS",
    "CROWN_AREA_CO",
    "DAGB_DT",
    "DBA_DT",
    "DBH",
    "DDBH_DT",
    "DLNAGB_DT",
    "DLNBA_DT",
    "DLNDBH_DT",
    "ELONGF",
    "HITE",
    "KRDEPTH",
    "LAI_CO",
    "MMEAN_A_CLOSED_CO",
    "MMEAN_A_CO2_CO",
    "MMEAN_A_LIGHT_CO",
    "MMEAN_A_NET_CO",
    "MMEAN_A_OPEN_CO",
    "MMEAN_A_RUBP_CO",
    "MMEAN_BLEAF_CO",
    "MMEAN_BROOT_CO",
    "MMEAN_BSTORAGE_CO",
    "MMEAN_CB_CO",
    "MMEAN_DMAX_LEAF_PSI_CO",
    "MMEAN_DMAX_WOOD_PSI_CO",
    "MMEAN_DMIN_LEAF_PSI_CO",
    "MMEAN_DMIN_WOOD_PSI_CO",
    "MMEAN_FSN_CO",
    "MMEAN_FSW_CO",
    "MMEAN_FS_OPEN_CO",
    "MMEAN_GPP_CO",
    "MMEAN_INTERCEPTED_AL_CO",
    "MMEAN_INTERCEPTED_AW_CO",
    "MMEAN_LAI_CO",
    "MMEAN_LEAF_DROP_CO",
    "MMEAN_LEAF_ENERGY_CO",
    "MMEAN_LEAF_FLIQ_CO",
    "MMEAN_LEAF_GBW_CO",
    "MMEAN_LEAF_GROWTH_RESP_CO",
    "MMEAN_LEAF_GSW_CO",
    "MMEAN_LEAF_HCAP_CO",
    "MMEAN_LEAF_MAINTENANCE_CO",
    "MMEAN_LEAF_RESP_CO",
    "MMEAN_LEAF_STORAGE_RESP_CO",
    "MMEAN_LEAF_TEMP_CO",
    "MMEAN_LEAF_VPDEF_CO",
    "MMEAN_LEAF_WATER_CO",
    "MMEAN_LEAF_WATER_INT_CO",
    "MMEAN_LIGHT_LEVEL_BEAM_CO",
    "MMEAN_LIGHT_LEVEL_CO",
    "MMEAN_LIGHT_LEVEL_DIFF_CO",
    "MMEAN_NPPCROOT_CO",
    "MMEAN_NPPDAILY_CO",
    "MMEAN_NPPFROOT_CO",
    "MMEAN_NPPLEAF_CO",
    "MMEAN_NPPSAPWOOD_CO",
    "MMEAN_NPPSEEDS_CO",
    "MMEAN_NPPWOOD_CO",
    "MMEAN_NPP_CO",
    "MMEAN_PAR_LEVEL_BEAM_CO",
    "MMEAN_PAR_LEVEL_DIFFD_CO",
    "MMEAN_PAR_LEVEL_DIFFU_CO",
    "MMEAN_PAR_L_BEAM_CO",
    "MMEAN_PAR_L_CO",
    "MMEAN_PAR_L_DIFF_CO",
    "MMEAN_PLRESP_CO",
    "MMEAN_PSI_CLOSED_CO",
    "MMEAN_PSI_OPEN_CO",
    "MMEAN_RLONG_L_CO",
    "MMEAN_RLONG_W_CO",
    "MMEAN_ROOT_GROWTH_RESP_CO",
    "MMEAN_ROOT_MAINTENANCE_CO",
    "MMEAN_ROOT_RESP_CO",
    "MMEAN_ROOT_STORAGE_RESP_CO",
    "MMEAN_RSHORT_L_CO",
    "MMEAN_RSHORT_W_CO",
    "MMEAN_SAPA_GROWTH_RESP_CO",
    "MMEAN_SAPA_STORAGE_RESP_CO",
    "MMEAN_SAPB_GROWTH_RESP_CO",
    "MMEAN_SAPB_STORAGE_RESP_CO",
    "MMEAN_SENSIBLE_LC_CO",
    "MMEAN_SENSIBLE_WC_CO",
    "MMEAN_TRANSP_CO",
    "MMEAN_VAPOR_LC_CO",
    "MMEAN_VAPOR_WC_CO",
    "MMEAN_WATER_SUPPLY_CO",
    "MMEAN_WFLUX_GW_CO",
    "MMEAN_WFLUX_WL_CO",
    "MMEAN_WOOD_ENERGY_CO",
    "MMEAN_WOOD_FLIQ_CO",
    "MMEAN_WOOD_GBW_CO",
    "MMEAN_WOOD_HCAP_CO",
    "MMEAN_WOOD_TEMP_CO",
    "MMEAN_WOOD_WATER_CO",
    "MMEAN_WOOD_WATER_INT_CO",
    "MMEAN_WSHED_LG_CO",
    "MMEAN_WSHED_WG_CO",
    "MMSQU_GPP_CO",
    "MMSQU_NPP_CO",
    "MMSQU_PLRESP_CO",
    "MMSQU_SENSIBLE_LC_CO",
    "MMSQU_SENSIBLE_WC_CO",
    "MMSQU_TRANSP_CO",
    "MMSQU_VAPOR_LC_CO",
    "MMSQU_VAPOR_WC_CO",
    "NPLANT",
    "PAW_AVG",
    "PFT",
    "RECRUIT_DBH",
    "WAI_CO"
  ) 
}

scalar_vars_m <- function() {
  c(
    "AGE",
    "AREA",
    "AREA_SI",
    "AVG_MONTHLY_WATERDEF",
    "DIST_TYPE",
    "FF_NHGT",
    "IGNITION_RATE",
    "ISOILFLG",
    "LATITUDE",
    "LONGITUDE",
    "LSL",
    "MMEAN_AF_DECOMP_PY",
    "MMEAN_ALBEDO_NIR_PY",
    "MMEAN_ALBEDO_PAR_PY",
    "MMEAN_ALBEDO_PY",
    "MMEAN_ATM_CO2_PY",
    "MMEAN_ATM_PAR_DIFF_PY",
    "MMEAN_ATM_PAR_PY",
    "MMEAN_ATM_PRSS_PY",
    "MMEAN_ATM_RHOS_PY",
    "MMEAN_ATM_RLONG_PY",
    "MMEAN_ATM_RSHORT_DIFF_PY",
    "MMEAN_ATM_RSHORT_PY",
    "MMEAN_ATM_SHV_PY",
    "MMEAN_ATM_TEMP_PY",
    "MMEAN_ATM_THEIV_PY",
    "MMEAN_ATM_THETA_PY",
    "MMEAN_ATM_VELS_PY",
    "MMEAN_ATM_VPDEF_PY",
    "MMEAN_AVAILABLE_WATER_PY",
    "MMEAN_A_CLOSED_PY",
    "MMEAN_A_CO2_PY",
    "MMEAN_A_DECOMP_PY",
    "MMEAN_A_LIGHT_PY",
    "MMEAN_A_NET_PY",
    "MMEAN_A_OPEN_PY",
    "MMEAN_A_RUBP_PY",
    "MMEAN_CAN_CO2_PY",
    "MMEAN_CAN_GGND_PY",
    "MMEAN_CAN_PRSS_PY",
    "MMEAN_CAN_RHOS_PY",
    "MMEAN_CAN_SHV_PY",
    "MMEAN_CAN_TEMP_PY",
    "MMEAN_CAN_THEIV_PY",
    "MMEAN_CAN_THETA_PY",
    "MMEAN_CAN_VPDEF_PY",
    "MMEAN_CARBON_AC_PY",
    "MMEAN_CARBON_ST_PY",
    "MMEAN_CO2_RESIDUAL_PY",
    "MMEAN_CSTAR_PY",
    "MMEAN_CWD_C_PY",
    "MMEAN_CWD_N_PY",
    "MMEAN_CWD_RH_PY",
    "MMEAN_DPCPG_PY",
    "MMEAN_DRAINAGE_PY",
    "MMEAN_ENERGY_RESIDUAL_PY",
    "MMEAN_FAST_SOIL_C_PY",
    "MMEAN_FAST_SOIL_N_PY",
    "MMEAN_FSN_PY",
    "MMEAN_FSW_PY",
    "MMEAN_FS_OPEN_PY",
    "MMEAN_GND_SHV_PY",
    "MMEAN_GND_TEMP_PY",
    "MMEAN_GPP_PY",
    "MMEAN_INTERCEPTED_AL_PY",
    "MMEAN_INTERCEPTED_AW_PY",
    "MMEAN_LEAF_ENERGY_PY",
    "MMEAN_LEAF_FLIQ_PY",
    "MMEAN_LEAF_GBW_PY",
    "MMEAN_LEAF_GROWTH_RESP_PY",
    "MMEAN_LEAF_GSW_PY",
    "MMEAN_LEAF_HCAP_PY",
    "MMEAN_LEAF_RESP_PY",
    "MMEAN_LEAF_STORAGE_RESP_PY",
    "MMEAN_LEAF_TEMP_PY",
    "MMEAN_LEAF_VPDEF_PY",
    "MMEAN_LEAF_WATER_PY",
    "MMEAN_MINERAL_SOIL_N_PY",
    "MMEAN_NEP_PY",
    "MMEAN_NIRUP_PY",
    "MMEAN_NPPCROOT_PY",
    "MMEAN_NPPDAILY_PY",
    "MMEAN_NPPFROOT_PY",
    "MMEAN_NPPLEAF_PY",
    "MMEAN_NPPSAPWOOD_PY",
    "MMEAN_NPPSEEDS_PY",
    "MMEAN_NPPWOOD_PY",
    "MMEAN_NPP_PY",
    "MMEAN_PARUP_PY",
    "MMEAN_PAR_GND_PY",
    "MMEAN_PAR_L_BEAM_PY",
    "MMEAN_PAR_L_DIFF_PY",
    "MMEAN_PAR_L_PY",
    "MMEAN_PCPG_PY",
    "MMEAN_PLRESP_PY",
    "MMEAN_PSI_CLOSED_PY",
    "MMEAN_PSI_OPEN_PY",
    "MMEAN_QDRAINAGE_PY",
    "MMEAN_QPCPG_PY",
    "MMEAN_QRUNOFF_PY",
    "MMEAN_QSTAR_PY",
    "MMEAN_QTHROUGHFALL_PY",
    "MMEAN_RH_PY",
    "MMEAN_RK4STEP_PY",
    "MMEAN_RLONGUP_PY",
    "MMEAN_RLONG_ALBEDO_PY",
    "MMEAN_RLONG_GND_PY",
    "MMEAN_RLONG_L_PY",
    "MMEAN_RLONG_W_PY",
    "MMEAN_RNET_PY",
    "MMEAN_ROOT_GROWTH_RESP_PY",
    "MMEAN_ROOT_RESP_PY",
    "MMEAN_ROOT_STORAGE_RESP_PY",
    "MMEAN_RSHORTUP_PY",
    "MMEAN_RSHORT_GND_PY",
    "MMEAN_RSHORT_L_PY",
    "MMEAN_RSHORT_W_PY",
    "MMEAN_RUNOFF_PY",
    "MMEAN_SAPA_GROWTH_RESP_PY",
    "MMEAN_SAPA_STORAGE_RESP_PY",
    "MMEAN_SAPB_GROWTH_RESP_PY",
    "MMEAN_SAPB_STORAGE_RESP_PY",
    "MMEAN_SENSIBLE_AC_PY",
    "MMEAN_SENSIBLE_GC_PY",
    "MMEAN_SENSIBLE_LC_PY",
    "MMEAN_SENSIBLE_WC_PY",
    "MMEAN_SFCW_DEPTH_PY",
    "MMEAN_SFCW_ENERGY_PY",
    "MMEAN_SFCW_FLIQ_PY",
    "MMEAN_SFCW_MASS_PY",
    "MMEAN_SFCW_TEMP_PY",
    "MMEAN_SLOW_SOIL_C_PY",
    "MMEAN_STRUCT_SOIL_C_PY",
    "MMEAN_STRUCT_SOIL_L_PY",
    "MMEAN_THROUGHFALL_PY",
    "MMEAN_TRANSP_PY",
    "MMEAN_TSTAR_PY",
    "MMEAN_USTAR_PY",
    "MMEAN_VAPOR_AC_PY",
    "MMEAN_VAPOR_GC_PY",
    "MMEAN_VAPOR_LC_PY",
    "MMEAN_VAPOR_WC_PY",
    "MMEAN_WATER_RESIDUAL_PY",
    "MMEAN_WATER_SUPPLY_PY",
    "MMEAN_WOOD_ENERGY_PY",
    "MMEAN_WOOD_FLIQ_PY",
    "MMEAN_WOOD_GBW_PY",
    "MMEAN_WOOD_HCAP_PY",
    "MMEAN_WOOD_TEMP_PY",
    "MMEAN_WOOD_WATER_PY",
    "MMEAN_WSHED_LG_PY",
    "MMEAN_WSHED_WG_PY",
    "MMSQU_ALBEDO_PY",
    "MMSQU_CARBON_AC_PY",
    "MMSQU_CARBON_ST_PY",
    "MMSQU_CWD_RH_PY",
    "MMSQU_GPP_PY",
    "MMSQU_NEP_PY",
    "MMSQU_NIRUP_PY",
    "MMSQU_NPP_PY",
    "MMSQU_PARUP_PY",
    "MMSQU_PLRESP_PY",
    "MMSQU_RH_PY",
    "MMSQU_RLONGUP_PY",
    "MMSQU_RNET_PY",
    "MMSQU_RSHORTUP_PY",
    "MMSQU_SENSIBLE_AC_PY",
    "MMSQU_SENSIBLE_GC_PY",
    "MMSQU_SENSIBLE_LC_PY",
    "MMSQU_SENSIBLE_WC_PY",
    "MMSQU_TRANSP_PY",
    "MMSQU_USTAR_PY",
    "MMSQU_VAPOR_AC_PY",
    "MMSQU_VAPOR_GC_PY",
    "MMSQU_VAPOR_LC_PY",
    "MMSQU_VAPOR_WC_PY",
    "NCOHORTS_GLOBAL",
    "NCOL_SOIL",
    "NDCYCLE",
    "NPATCHES_GLOBAL",
    "NPOLYGONS_GLOBAL",
    "NSITES_GLOBAL",
    "NZG",
    "NZS",
    "PACO_ID",
    "PACO_N",
    "PATCH_COUNT",
    "PYSI_ID",
    "PYSI_N",
    "SIPA_ID",
    "SIPA_N",
    "SLXCLAY",
    "SLXSAND",
    "XATM",
    "YATM"
  )
}
