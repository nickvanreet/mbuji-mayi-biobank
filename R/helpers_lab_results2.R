# =====================================================================
# HELPERS: Load & merge PCR, ELISA PE, ELISA VSG, iELISA, and controls
# =====================================================================

parse_pcr_results <- function(dir) {
  files <- list.files(dir, "\\.xlsx?$", full.names = TRUE)
  # add your parse logic here
  # Expect: LabID, Cq_177T, Cq_18S2, Cq_RNAseP, PCR_call
}

parse_elisa_pe <- function(dir) {
  # parse using your ELISA PE reader
  # Expect: LabID, file, D_OD, PP_percent, pos_flag
}

parse_elisa_vsg <- function(dir) {
  # parse with VSG logic
}

parse_ielisa <- function(dir) {
  # parse % inhibition sheets, merged cells logic
}

merge_lab_with_biobank <- function(biobank, lab) {
  biobank %>%
    left_join(lab$pcr,      by = "lab_id") %>%
    left_join(lab$elisa_pe, by = "lab_id") %>%
    left_join(lab$elisa_vsg,by = "lab_id") %>%
    left_join(lab$ielisa,   by = "lab_id")
}

compute_flags <- function(df, pcr_cq_max, elisa_pp_cut, ielisa_inh_cut) {
  df %>%
    mutate(
      PCR_pos = Cq_177T <= pcr_cq_max | Cq_18S2 <= pcr_cq_max,
      ELISA_PE_pos  = PP_percent_PE  >= elisa_pp_cut,
      ELISA_VSG_pos = PP_percent_VSG >= elisa_pp_cut,
      iELISA_pos    = inh_percent    >= ielisa_inh_cut,
      Concordant    = PCR_pos | ELISA_PE_pos | ELISA_VSG_pos | iELISA_pos
    )
}
