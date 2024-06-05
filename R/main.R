#' @export
backCalculateRace <- function(df) {

  df <- df %>%
    mutate(height = height/100,
           binary_sex     = ifelse(sex_at_birth %in% c("Male","male"), 1, 2),
           dif_White      = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 1, param = "FEV1")),
           dif_Black      = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 2, param = "FEV1")),
           dif_NEAsian    = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 3, param = "FEV1")),
           dif_SEAsian    = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 4, param = "FEV1")),
           dif_Other      = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 5, param = "FEV1")),
           difIII_White   = abs(fev1_predicted-pred_NHANES3(age_pft, height, gender = binary_sex, ethnicity = 1, param = "FEV1")),
           difIII_Black   = abs(fev1_predicted-pred_NHANES3(age_pft, height, gender = binary_sex, ethnicity = 2, param = "FEV1")),
           difIII_MexicanWhite= abs(fev1_predicted-pred_NHANES3(age_pft, height, gender = binary_sex, ethnicity = 3, param = "FEV1")),
           ) %>%
    rowwise %>%
    mutate(race_spiro = names(.)[(dim(df)[2]+2):(dim(df)[2]+9)][which.min(c_across(c(dif_White, dif_Black, dif_NEAsian, dif_SEAsian, dif_Other)))]) %>%
    ungroup

  return(df)

}
