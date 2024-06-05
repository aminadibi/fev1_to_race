#' @export
backCalculateRace <- function(df) {

  df <- df %>%
    mutate(height = height/100,
           binary_sex = ifelse(sex_at_birth %in% c("Male","male"), 1, 2),
           dif_White = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 1, param = "FEV1")),
           dif_Black = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 2, param = "FEV1")),
           dif_NEAsian = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 3, param = "FEV1")),
           dif_SEAsian = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 4, param = "FEV1")),
           dif_Other = abs(fev1_predicted-pred_GLI(age_pft, height, gender = binary_sex, ethnicity = 5, param = "FEV1"))) %>%
    rowwise %>%
    mutate(MIN_COL = names(.)[(dim(df)[2]+2):(dim(df)[2]+6)][which.min(c_across(c(dif_White, dif_Black, dif_NEAsian, dif_SEAsian, dif_Other)))]) %>%
    ungroup

  return(df)

}


#' @export
backCalculateRace2 <- function(predicted_fev1, sex, age, height) {
  height <- height/100
  binary_sex <- ifelse(sex %in% c("Male","male"), 1, 2)

  race_names <- c("White", "Black", "Asian", "Asian", "Mixed/Other",
                  "White NHANES3", "Black NHANES3", "White NHANES3")

  find_closest_race <- function(pred_fev1, age, height, bin_sex) {
    ethnicities <- 1:5
    race_diffs <- sapply(ethnicities, function(eth) {
      abs(pred_fev1 - pred_GLI(age, height, gender = bin_sex, ethnicity = eth, param = "FEV1"))
    })
    race_diffs <- append(race_diffs, sapply(ethnicities, function(eth) {
      abs(pred_fev1 - pred_NHANES3(age, height, gender = bin_sex, ethnicity = eth, param = "FEV1"))
    }))
    min_index <- which.min(race_diffs)
    return(race_names[min_index])
  }

  closest_races <- mapply(find_closest_race, predicted_fev1, age, height, binary_sex)

  return(closest_races)
}

#' @export
backCalculateRace3 <- function(predicted_fev1, sex, age, height, tolerance=1e-1){
  height <- height/100
  binary_sex <- case_when(sex %in% c("Male","male") ~ 1,
                          sex %in% c("Female", "female") ~2)
  res <- case_when(
  (abs(predicted_fev1-pred_GLI(age, height, gender=binary_sex, ethnicity=1, param="FEV1"))<=tolerance) ~ "White",
  (abs(predicted_fev1-pred_GLI(age, height, gender=binary_sex, ethnicity=2, param="FEV1"))<=tolerance) ~ "Black",
  (abs(predicted_fev1-pred_GLI(age, height, gender=binary_sex, ethnicity=3, param="FEV1"))<=tolerance) ~ "Asian",
  (abs(predicted_fev1-pred_GLI(age, height, gender=binary_sex, ethnicity=4, param="FEV1"))<=tolerance) ~ "Asian",
  (abs(predicted_fev1-pred_GLI(age, height, gender=binary_sex, ethnicity=5, param="FEV1"))<=tolerance) ~ "Mixed/Other",
  (abs(predicted_fev1-pred_NHANES3(age, height, gender=binary_sex, ethnicity=1, param="FEV1"))<=tolerance) ~ "White",
  (abs(predicted_fev1-pred_NHANES3(age, height, gender=binary_sex, ethnicity=2, param="FEV1"))<=tolerance) ~ "Black",
  (abs(predicted_fev1-pred_NHANES3(age, height, gender=binary_sex, ethnicity=3, param="FEV1"))<=tolerance) ~ "White",
  .default = NA)

  return(res)
}



#' @export
backCalculateRace4 <- function(predicted_fev1, sex, age, height){
  height <- height/100
  binary_sex <- case_when(sex %in% c("Male","male") ~ 1,
                          sex %in% c("Female", "female") ~2)
  lookup_dict <- tibble(
    names = c("NHANES3_White",
              "NHANES3_Black",
              "NHANES3_Mexican_American",
              "GLI2012_White",
              "GLI2012_Black",
              "GLI2012_NE_ASIAN",
              "GLI2012_SE_Asian",
              "GLI2012_Other_Mixed",
              "GLI_gl_2022"),
    value =
      c(pred_NHANES3(age, height, gender=binary_sex, ethnicity=1, param="FEV1"),
        pred_NHANES3(age, height, gender=binary_sex, ethnicity=2, param="FEV1"),
        pred_NHANES3(age, height, gender=binary_sex, ethnicity=3, param="FEV1"),
        pred_GLI(age, height, gender=binary_sex, ethnicity=1, param="FEV1"),
        pred_GLI(age, height, gender=binary_sex, ethnicity=2, param="FEV1"),
        pred_GLI(age, height, gender=binary_sex, ethnicity=3, param="FEV1"),
        pred_GLI(age, height, gender=binary_sex, ethnicity=4, param="FEV1"),
        pred_GLI(age, height, gender=binary_sex, ethnicity=5, param="FEV1"),
        pred_GLIgl(age, height, gender=binary_sex, param="FEV1"))) %>%
    mutate(match=(abs(predicted_fev1-value)<=1e-2)) %>%
    filter(match==TRUE) %>%
    select(names)

  if (nrow(lookup_dict)==0) {
    return(99)
  }

  if (length(lookup_dict)>1) {
    warning("multiple matches found")
  }
  return(lookup_dict$names)
}
