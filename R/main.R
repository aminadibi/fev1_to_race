#' @export
backCalculateRace <- function(fev1, percent_predicted_fev1, sex, age, height){
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
    mutate(match=(abs(percent_predicted_fev1-value)<=1e-2)) %>%
    filter(match==TRUE) %>%
    select(names)

  if (length(lookup_dict)==0) {
    return(NA)
  }

  if (length(lookup_dict)>1) {
    warning("multiple matches found")
  }
  return(lookup_dict$names)
}
