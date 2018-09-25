
# LOAD PACKAGES ROBUSTLY --------------------------------------------------

if(!require(pacman)) install.packages("pacman")
if(!require(statar)) install.packages("statar")

#install.packages("stringr")
library(stringr)

pacman::p_load(
  "tidyverse",
  "lubridate",
  "haven",
  "RcppRoll"
)


# READ SPSS FORMATTED CASE EXTRACTS ---------------------------------------

n_max <- 1000

case_data_00a_99n02 <-

  haven::read_sav(
    "../../../EAS Service Delivery Network/raw_data/SAMIN/CASE files/casea99n02v1.sav"
  ) %>%

  select(
    casenum,
    currmthy,
    currmthm,
    currmthd,
    caseffy,
    caseffm,
    caseffd,
    casecat,
    acasecat,
    postcode,
    tnb,
    casbenp
  ) %>%

  # TRIM FOR TESTING
  group_by(
    currmthy,
    currmthm
  ) %>%

  filter(
    row_number() <= n_max
  ) %>%

  ungroup() %>%

  mutate_all(
    as.character
  )


# READ FIXED WIDTH CASE FILES ---------------------------------------------

case_paths_dat <-

  list.files(
    "../raw_data/SAMIN/CASE files/",
    full.names = TRUE,
    pattern = "dat$"
  )

case_cols <-

  tribble(
    ~ col_names, ~ begin, ~ end,
    "casenum"  , 1      , 7    ,
    "currmthy" , 8      , 11   ,
    "currmthm" , 12     , 13   ,
    "currmthd" , 14     , 15   ,
    "caseffy"  , 16     , 19   ,
    "caseffm"  , 20     , 21   ,
    "caseffd"  , 22     , 23   ,
    "casecat"  , 36     , 39   ,
    "acasecat" , 40     , 43   ,
    "postcode" , 45     , 50   ,
    "tnb" 		 , 97     , 105  ,
    "casbenp"  , 174    , 182
  ) %>%
  mutate(
    begin = begin - 1
  )


## n_max rows of each fixed width file will be read.
## set to Inf to read all rows
#n_max <- 1000
n_max <- Inf


case_data_00b_03n17 <- case_paths_dat %>%

  map_df(
    ~ read_fwf(
      .x,
      col_positions = case_cols,
      n_max = n_max,
      col_types = cols(.default = "c")
    )
  )


# CONCATENATE SPSS AND FWF DATA -------------------------------------------

case_data_01_02n17 <-

  ## Concatenate the data frames
  bind_rows(
    case_data_00a_99n02,
    case_data_00b_03n17
  ) %>%

  ## Change type of casenum and tnb variables
  mutate_at(
    vars(
      casenum,
      tnb
    ),
    funs(
      as.numeric
    )
  ) %>%

  filter(
    !is.na(casenum)
  )


# CLEAN DATES AND CREATE STATA INTEGER MONTHS -----------------------------

case_data_02 <-

  case_data_01_02n17 %>%

  # generate
  mutate(

    currmthd = ifelse(
      currmthd == "00",
      "01",
      currmthd
    ),

    daystartcase = ymd(
      str_c(
        caseffy, "-",
        caseffm, "-",
        caseffd
      )
    ),

    currmth = ymd(
      str_c(
        currmthy, "-",
        currmthm, "-",
        currmthd
      )
    ),

    monthstartcase = as.integer(statar::as.monthly(daystartcase)),
    currmthm       = as.integer(statar::as.monthly(currmth)),

    category = case_when(
      casecat == "GA" ~ "General assist.",
      casecat %in% c("FA", "MA") ~ "Single parent",
      casecat == "DIS" ~ "Disability",
      TRUE ~ "Other"
    )
  )


# FORMAT NEEDS AND BENEFITS AMOUNTS ---------------------------------------

case_data_03 <-

  case_data_02 %>%

  rename(
    needs = tnb
  ) %>%

  mutate(
    needs = as.numeric(needs),
    benefits = as.numeric(casbenp) / 100,
    benefits = ifelse(is.na(benefits), 0, benefits)
  ) %>%

  group_by( #bysort
    currmth,
    casenum
  ) %>%

  mutate( # generate N = _N
    N = n()
  ) %>%

  ungroup() %>%

  mutate(
    benefits = benefits / N,
    eia = benefits > 0 & !(acasecat %in% "HEX")
  ) %>%

  select(
    casenum,
    eia,
    currmth,
    currmthm
  )


# MAKE A FUNCTION TO CROSS JOIN IDs AND MONTHs ----------------------------



pad_month <- function(.tbl, id, month_col) {

  id <- enquo(id)
  col <- enquo(month_col)

  expand.grid(
    unique(.tbl[[quo_name(id)]]),
    unique(.tbl[[quo_name(col)]])
  ) %>%

    set_names(
      c(
        quo_name(id),
        quo_name(col)
      )
    ) %>%

    left_join(
      .tbl
    )
}


# GET LIST OF INVOLVEMENT FILES -------------------------------------------



inv_paths <- list.files(
  "Z:/EAS Service Delivery Network/raw_data/SAMIN/INVOLVEMENT files",
  # use full.names = TRUE to return full paths to the files
  full.names = TRUE
)



# READ ALL INVOLVEMENT FILES INTO ONE DATA FRAME --------------------------


inv_data_00 <-

  # I'm using a limited subset of the files here for testing
  # replace inv_paths[1:3] in the following line with inv_paths to use all
  inv_paths %>%

    # map_* functions work like for loops. They do the same thing to each
    # item in the list or vector you're passing in.
    #
    # the value of *, in this case df, specifies the return format of the
    # map.
    map_df(
      # Tilde is a short-hand for an anonymous function
      # .x represents the list item. In this case, the path to a
      # given involvment file
      ~ read_sav( # infix
          .x
        ) %>%

        filter( # keep if
          relat %in% c("AP", "CL", "SP")
        ) %>% 
        
        mutate(
          
          currmth = ymd(
            str_c(
              currmthy, "-",
              currmthm, "-",
              currmthd + 1
            )
          ),
          currmthm = as.integer(statar::as.monthly(currmth))
        ) %>% 
        
        select(
          sin, 
          casenum, 
          currmth, 
          currmthm
        ) 
     ) 


# CREATE MONTHLY EIA INDEX FOR EACH SIN -----------------------------------


case_sin <-

  inv_data_00 %>%

    # get index variables from inv data
    select(
      sin,
      casenum,
      currmthm
    ) %>%

    # join with case data, keep only matches
    inner_join(
      case_data_03
    ) %>%

    # create a row for each possible combination of sin and month
    pad_month(
      sin,
      currmthm
    ) %>%

    # for each sin, filter out sin / month combinations that aer before
    # the client's start, or after the client's finish
    group_by(sin) %>%
    arrange(currmthm) %>%
    filter(
      row_number() > min(which(eia))
    ) %>%

    # fill empty values of eia with FALSE
    mutate(
      eia = eia %in% TRUE
    ) %>%

    ungroup()

# CREATE ROLLING SUMS -----------------------------------------------------

case_data_05 <-

  case_sin %>%

    group_by(
      sin
    ) %>%

    arrange(currmthm) %>%

    # generate cumulative total and bracketed historical sums for each
    # person.
    mutate(
      montheiatotal = cumsum(eia),
      eiaprev120 = RcppRoll::roll_sum(
        eia,
        120L,
        align = "right",
        fill = NA,
        na.rm = TRUE
      ) %>%

      coalesce(as.numeric(montheiatotal))
    ) %>%

    ungroup()


