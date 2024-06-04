#' @export
backCalculateRace <- function(fev1, percent_predicted_fev1, sex, age, height){
  height <- height/100

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
      c(pctpred_NHANES3(age, height, ethnicity=1, FEV1=fev1),
        pctpred_NHANES3(age, height, ethnicity=2, FEV1=fev1),
        pctpred_NHANES3(age, height, ethnicity=3, FEV1=fev1),
        pctpred_GLI(age, height, ethnicity=1, FEV1=fev1),
        pctpred_GLI(age, height, ethnicity=2, FEV1=fev1),
        pctpred_GLI(age, height, ethnicity=3, FEV1=fev1),
        pctpred_GLI(age, height, ethnicity=4, FEV1=fev1),
        pctpred_GLI(age, height, ethnicity=5, FEV1=fev1),
        pctpred_GLIgl(age, height,  FEV1=fev1))) %>%
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
