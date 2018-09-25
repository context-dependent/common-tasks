
# 1. SEPARATING AND LUMPING CHARACTER COLUMNS -----------------------------

## funding source of service
sprs_services <- sprs_services %>%  
  mutate(
    fundserv = case_when(
      PRIM_PGM_DESC %in% c(
        "Apprenticeship - LMDA",
        "Employment Assistance Services - LMDA",
        "Employment Assistance Services - LMDA",
        "Employment Partnerships - LMDA",
        "Labour Market Partnerships - LMDA",
        "Research And Innovation - LMDA",
        "Self Employment Assistance - LMDA",
        "Skills Development - LMDA",
        "Wage Subsidy - LMDA"
      )
      ~ "LMDA",
      PRIM_PGM_DESC %in% c(
        "Apprenticeship - CJF",
        "Direct Employment Services - CJF",
        "Employment Partnerships - CJF",
        "Labour Market Partnerships - CJF",
        "Self Employment Assistance - CJF",
        "Skills Development - CJF",
        "Strategic Training Transition Fund - CJF",
        "Wage Subsidy - CJF"
      )
      ~ "CJF",
      PRIM_PGM_DESC %in% c(
        "Community Partnerships Projects - ETS",
        "Community-Based Employability Projects/Special Needs - ETS",
        "Direct Employment Services - ETS",
        "Employment Partnerships - ETS",
        "Floodway Training Initiative - ETS",
        "Hydro 25 - ETS",
        "Hydro 75 - ETS",
        "Individualized Labour Market Training - ETS",
        "National Child Benefit - ETS",
        "Neighbourhoods Alive - ETS",
        "New Careers North - ETS",
        "Older Workers Program - ETS",
        "Skills Development - ETS",
        "Taking Charge - ETS",
        "Wage Subsidy - ETS",
        "Youth NOW - ETS"
      )
      ~ "ETS",
      PRIM_PGM_DESC %in% c(
        "Apprenticeship - APP_PROV",
        "-",
        "Regular Provincial Funding - IWD"
      )
      ~ "Other"
    )
  )

## program of service
sprs_services <- sprs_services %>%
  mutate(
    progserv = case_when(
      PRIM_PGM_DESC %in% c("Employment Assistance Services - LMDA") ~ "EAS",
      
      PRIM_PGM_DESC %in% c(
        "Labour Market Partnerships - CJF",
        "Labour Market Partnerships - LMDA"
      )
      ~ "Lab. market partnership",
      
      PRIM_PGM_DESC %in% c(
        "Apprenticeship - APP_PROV",
        "Apprenticeship - LMDA",
        "Apprenticeship - CJF"
      )
      ~ "Apprentice",
      
      PRIM_PGM_DESC %in% c(
        "Skills Development - CJF",
        "Skills Development - ETS",
        "Skills Development - LMDA"
      )
      ~ "Skills development",
      
      PRIM_PGM_DESC %in% c(
        "Employment Partnerships - CJF",
        "Employment Partnerships - ETS",
        "Employment Partnerships - LMDA"
      )
      ~ "Employ. partnership",
      
      PRIM_PGM_DESC %in% c(
        "Direct Employment Services - CJF",
        "Direct Employment Services - ETS"
      )
      ~ "Direct employ. serv.",
      
      PRIM_PGM_DESC %in% c(
        "Community Partnerships Projects - ETS",
        "Research And Innovation - LMDA",
        "Self Employment Assistance - CJF",
        "Self Employment Assistance - LMDA",
        "Strategic Training Transition Fund - CJF",
        "Self Employment Assistance - CJF",
        "Self Employment Assistance - LMDA",
        "Wage Subsidy - CJF",
        "Wage Subsidy - ETS",
        "Wage Subsidy - LMDA",
        "Community Partnerships Projects - ETS",
        "Community-Based Employability Projects/Special Needs - ETS",
        "Floodway Training Initiative - ETS",
        "Hydro 25 - ETS",
        "Hydro 75 - ETS",
        "Hydro 25 - ETS",
        "Individualized Labour Market Training - ETS",
        "National Child Benefit - ETS",
        "Neighbourhoods Alive - ETS",
        "New Careers North - ETS",
        "Older Workers Program - ETS",
        "Strategic Training Transition Fund - CJF",
        "Taking Charge - ETS",
        "Youth NOW - ETS",
        "-",
        "Regular Provincial Funding - IWD"
      )
      ~ "Other"
    )
  )

# Recommendation: 
# 
#   1: use separate to split the PRIM_PGM_DESC into progserv and fundserv
#   2: use fct_lump to generate "Other" categories. 

# WARNING: this will not produce exactly the same results, but it is quicker to 
# type and more extensible than the above. 

sprs_services <- 
  
  sprs_services %>% 
  
  # NEW FUNCTION: separate splits a string column by a delimiter, in this case
  # the " - ", into an arbitrary number of columns, in this case 2, 
  # with names given by the into argument, in this case, fundserv and progserv
  separate(
    PRIM_PGM_DESC,
    into = c("fundserv", "progserv"), 
    sep = " - "
  ) %>% 
  
  mutate(
    # NEW FUNCTION: fct_lump preserves the n most common categories in a 
    # character or factor variable, converting all other observations to 
    # "Other" 
    progserv = fct_lump(progserv, n = 6), 
    fundserv = fct_lump(fundserv, n = 3)
  )


# USING str_detect() TO ABBREVIATE AND EXTED case_when --------------------

# ORIGINAL CODE
# [ref] ICM & SPRS--Import[1] TM.R : line 1041
# employed at follow-up

df <- df %>%
  mutate(
    emp_fu = case_when(
      EMP_STTS_DESC %in% c(
        "Not Employed",
        "Not Employed - Unemployed",
        "Unemployed - Looking for Work",
        "Unemployed - Not Looking for Work",
        "Not Employed - Not in the Labour Force",
        "Unemployed - Not Looking for Work",
        "Unemployed - In Further Education / Training"
      ) ~ "Not employed at follow-up",
      EMP_STTS_DESC %in% c("Employed", "Self-Employed") ~ "Employed at follow-up",
      EMP_STTS_DESC_LEAVING %in% c(
        "Not Employed",
        "Not Employed - Unemployed",
        "Unemployed - Looking for Work",
        "Unemployed - Not Looking for Work",
        "Not Employed - Not in the Labour Force",
        "Unemployed - Not Looking for Work",
        "Unemployed - In Further Education / Training"
      ) ~ "Not employed at follow-up",
      EMP_STTS_DESC_LEAVING %in% c("Employed", "Self-Employed") ~ "Employed at follow-up"
    )
  )

# Recommendation: 
# 
#   1: instead of making an exhaustive list of categories you want to roll up, 
#      detect the patterns in the text that define them 

df <- df %>% 
  mutate(
    emp_fu = case_when(
      # str_detect test whether each element in a vector matches a pattern 
      # in this case the vector is EMP_STTS_DESC, and the pattern is 
      # Unemployed|Not Employed ("Unemployed" or "Not Employed") 
      str_detect(EMP_STTS_DESC, "Unemployed|Not Employed") ~ 
        "Not employed at follow-up",
      str_detect(EMP_STTS_DESC, "^Employed|Self-Employed") ~ 
        "Employed at follow-up", 
      str_detect(EMP_STS_DESC_LEAVING, "Unemployed|Not Employed") ~
        "Not employed at follow-up", 
      TRUE ~ "Follow-up employment missing"
    )
  )