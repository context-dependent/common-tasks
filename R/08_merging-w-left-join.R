# MERGING -----------------------------------------------------------------

# merge the icm files - ORIGINAL
# [ref] ICM & SPRS--Import[1] TM.R : line 354

icm_all <-
  merge(
    x = icm_person_details,
    y = icm_client_assessment,
    by.x = "ICASE_NO",
    by.y = "ICASE_NO",
    all = TRUE
  )
icm_all <-
  merge(
    x = icm_all,
    y = icm_outcomes,
    by.x = "ICASE_NO",
    by.y = "ICASE_NO",
    all = TRUE
  )
icm_all <-
  merge(
    x = icm_all,
    y = icm_followup,
    by.x = "ICASE_NO",
    by.y = "ICASE_NO",
    all = TRUE
  )
icm_all <-
  merge(
    x = icm_all,
    y = icm_plan_contract,
    by.x = "ICASE_NO",
    by.y = "ICASE_NO",
    all = TRUE
  )

# Recommendation
#   1: chain left joins together. This stops you from needing to make 3 
#      extraneous copies of icm_all, lightening the load on your memory. 
#      as a bonys, it involves less typing. 

icm_all <- 
  
  icm_person_details %>% 
  
  left_join(icm_client_assessment, by = "ICASE_NO") %>% 
  left_join(icm_outcomes, by = "ICASE_NO") %>% 
  left_join(icm_followup, by = "ICASE_NO") %>% 
  left_join(icm_plan_contract, by = "ICASE_NO") 

