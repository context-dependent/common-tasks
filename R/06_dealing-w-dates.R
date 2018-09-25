
# 1. DETECTING AND TYPING DATES WITH LUBRIDATE ----------------------------

# ORIGINAL code 
# [ref] ICM & SPRS--Import[1] TM.R : line 179

sprs_services$dybegserv <- as.Date(
  sprs_services$BKD_SERV_START_DT,
  format = '%m/%d/%Y', 
  origin = "1960-01-01"
)

sprs_services$dyendserv <- as.Date(
  sprs_services$BKD_SERV_END_DT,
  format = '%m/%d/%Y', 
  origin = "1960-01-01"
)

sprs_services$dydurserv <- difftime(
  sprs_services$dyendserv, 
  sprs_services$dybegserv, 
  units = "days"
) + 1

# Recommendations: 
#   1:use lubridate helper function (mdy), and don't specify origin, because
#     you shouldn't need it 
#   2:use arithmetic operator to calculate time difference in days. 

sprs_services <- 
  
  sprs_services %>% 
  
  mutate(
    dybegserv = mdy(BKD_SERV_START_DT), # I don't see a need to set the origin
    dyendserv = mdy(BKD_SERV_END_DT), 
    dydurserv = as.numeric(dyendserv - dybegserv + 1)
  )



# 2. CONVERTING TO CALENDAR MONTH -----------------------------------------

# ORIGINAL CODE
# [ref] ICM & SPRS--Import[1] TM.R : line 962
df <- df %>%
  mutate(
    calmonth = case_when(
      mobegcase %in% c(636, 648, 660, 672, 684, 696) ~ "Jan",
      mobegcase %in% c(637, 649, 661, 673, 685, 697) ~ "Feb",
      mobegcase %in% c(638, 650, 662, 674, 686, 698) ~ "Mar",
      mobegcase %in% c(639, 651, 663, 675, 687, 699) ~ "Apr",
      mobegcase %in% c(640, 652, 664, 676, 688, 700) ~ "May",
      mobegcase %in% c(641, 653, 665, 677, 689, 701) ~ "Jun",
      mobegcase %in% c(642, 654, 666, 678, 690, 702) ~ "Jul",
      mobegcase %in% c(643, 655, 667, 679, 691, 703) ~ "Aug",
      mobegcase %in% c(644, 656, 668, 680, 692, 704) ~ "Sep",
      mobegcase %in% c(645, 657, 669, 681, 693, 705) ~ "Oct",
      mobegcase %in% c(646, 658, 670, 682, 694, 706) ~ "Nov",
      mobegcase %in% c(647, 659, 671, 683, 695, 707) ~ "Dec"
    )
  )

# Recommendations: 
#   1: use lubridate function month on the actual date and set label = TRUE

df <- df %>% mutate(calmonth = month(dybegcase, label = TRUE))
