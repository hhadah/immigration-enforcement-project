# This a script to 
# merge the state level
# immigration laws with
# Current Population Survey (CPS)
# county level

# Date: May 7th, 2024

### Open state laws

activation_dates <- read_dta(file.path(raw,"activation_dates_makayla_revisedforbalance.dta")) 
activation_dates |> 
  glimpse()

### Open CPS data
### of 17 year olds
### living with their
### parents

CPS <- fread(CPS_path)
CPS <- as.data.frame(CPS)
# Assume df is your dataframe
duplicate_names <- names(CPS)[duplicated(names(CPS))]

# Create a logical vector where only the first occurrence of each column name is TRUE
non_duplicate_index <- !duplicated(names(CPS)) | !(names(CPS) %in% duplicate_names)

# Subset the dataframe to keep only non-duplicated columns
CPS <- CPS[, non_duplicate_index]

CPS <- CPS |> 
  filter(age < 18)


# merge wit CPS data
CPS_dates <- left_join(CPS,
                     activation_dates,
                     na_matches = "never",
                     by = c("county" = "countyfips"
                     )) |> 
  mutate(Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0))
CPS_dates |> 
  glimpse()
CPS_dates <- CPS_dates |>
  # filter(Type != "Fourth Generation+ Hispanic") |> 
  filter(Type != "") |> 
  mutate(ftotval_mom = ifelse(ftotval_mom <= 1, 1, ftotval_mom),
         lnftotval_mom = log(ftotval_mom),
         Age = age,
         Age_sq = age^2,
         Age_cube = age^3,
         Age_quad = age^4,
         HH = ifelse(Hispanic_Dad == 1 & Hispanic_Mom == 1, 1, 0),
         HW = ifelse(Hispanic_Dad == 1 & Hispanic_Mom == 0, 1, 0),
         WH = ifelse(Hispanic_Dad == 0 & Hispanic_Mom == 1, 1, 0),
         WW = ifelse(Hispanic_Dad == 0 & Hispanic_Mom == 0, 1, 0),
         HH_0bj = ifelse((SpanishSpeakingPOB_Father == 1 & SpanishSpeakingPOB_Mother == 1), 1, 0),
         HW_0bj = ifelse((SpanishSpeakingPOB_Father == 1 & SpanishSpeakingPOB_Mother == 0), 1, 0),
         WH_0bj = ifelse((SpanishSpeakingPOB_Father == 0 & SpanishSpeakingPOB_Mother == 1), 1, 0),
         WW_0bj = ifelse((SpanishSpeakingPOB_Father == 0 & SpanishSpeakingPOB_Mother == 0), 1, 0),
         ParentType = case_when(HH == 1 ~ "Hispanic-Hispanic",
                                HW == 1 ~ "Hispanic-White",
                                WH == 1 ~ "White-Hispanic",
                                WW == 1 ~ "White-White"),
         ParentType = as.factor(ParentType),
         ParentType2 = case_when(HH_0bj == 1 ~ "Hispanic-Hispanic",
                                 HW_0bj == 1 ~ "Hispanic-White",
                                 WH_0bj == 1 ~ "White-Hispanic",
                                 WW_0bj == 1 ~ "White-White"),
         ParentType2 = as.factor(ParentType2),
         
         HH_0bj_3 = ifelse((SpanishSpeakingPOB_PatGrandMother == 1 | SpanishSpeakingPOB_PatGrandFather == 1) 
                           & (SpanishSpeakingPOB_MatGrandMother == 1 | SpanishSpeakingPOB_MatGrandFather == 1), 1, 0),
         HW_0bj_3 = ifelse((SpanishSpeakingPOB_PatGrandMother == 1 | SpanishSpeakingPOB_PatGrandFather == 1) 
                           & (SpanishSpeakingPOB_MatGrandMother == 0 & SpanishSpeakingPOB_MatGrandFather == 0), 1, 0),
         WH_0bj_3 = ifelse((SpanishSpeakingPOB_PatGrandMother == 0 & SpanishSpeakingPOB_PatGrandFather == 0) 
                           & (SpanishSpeakingPOB_MatGrandMother == 1 | SpanishSpeakingPOB_MatGrandFather == 1), 1, 0),
         WW_0bj_3 = ifelse((SpanishSpeakingPOB_PatGrandMother == 0 & SpanishSpeakingPOB_PatGrandFather == 0) 
                           & (SpanishSpeakingPOB_MatGrandMother == 0 & SpanishSpeakingPOB_MatGrandFather == 0), 1, 0),
         ParentType3 = case_when(HH == 1 ~ "Hispanic-Hispanic",
                                 HW == 1 ~ "Hispanic-White",
                                 WH == 1 ~ "White-Hispanic",
                                 WW == 1 ~ "White-White"),
         ParentType3 = as.factor(ParentType3),
         
         Grandparent_Type = case_when((SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'WWWW',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 1) ~ "WWWH",
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'WWHW',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'WWHH',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'WHWW',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'WHWH',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'WHHW',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'WHHH',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'HWWW',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'HWWH',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'HWHW',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'HWHH',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'HHWW',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'HHWH',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'HHHW',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'HHHH'
         ),
         Grandparent_Type = as.factor(Grandparent_Type),
         weight = case_when(!is.na(hwtfinl) ~ hwtfinl,
                            !is.na(asecfwt) ~ asecfwt,
                            !is.na(asecwt04) ~ asecwt04))
# Open fraction Hispanic data

CPS_frac <- read_csv(CPS_hispanic_mean_county)
CPS_frac <- as.data.frame(CPS_frac)

CPS_dates <- left_join(CPS_dates,
                     CPS_frac,
                     na_matches = "never",
                     by = c("county", "year"#, 
                            #"month"
                     )) |> 
  rename(frac_hispanic = MeanHispanic)

### Create first.treat variable
CPS_dates <- CPS_dates |>
  mutate(first_treat = case_when(!is.na(activation_year) ~ activation_year,
                                 TRUE ~ 0))

# Create the year_month variable as a numerical value YYYYMM
CPS_dates$year_month <- CPS_dates$year * 100 + CPS_dates$month

# To see the structure of the new variable
head(CPS_dates$year_month)

# Create the year_month_treat variable as a numerical value YYYYMM
CPS_dates$year_month_treat <- CPS_dates$activation_year * 100 + CPS_dates$activation_month

# To see the structure of the new variable
head(CPS_dates$year_month_treat)

### Create first_treat variable
CPS_dates <- CPS_dates %>%
  mutate(first_treat_year_month = case_when(
    !is.na(year_month_treat) ~ year_month_treat,
    TRUE ~ 0  # Set NA of type double for the TRUE case
  ))

# To see the structure of the new variable
table(CPS_dates$first_treat_year_month)

# Create poor health 
# log school and SNAP
# and poverty line variables
CPS_dates <- CPS_dates |>
  mutate(poor_health  = case_when(health == 5 ~ 1,
                                 TRUE ~ 0),
        ln_schl_lunch = case_when(schllunch > 0 ~ log(schllunch),
                                  TRUE ~ 0),
        school_lunch  = case_when(schllunch > 0 ~ 1,
                                TRUE ~ 0),
        snap_status   = case_when(foodstamp > 0 ~ 1,
                               TRUE ~ 0),
        ln_snap       = case_when(foodstamp > 0 ~ log(foodstamp),
                            TRUE ~ 0),
        poverty_line  = case_when(offpov == 1 ~ 1,
                                 TRUE ~ 0),
        food_insecure = case_when(fsstatus == 2 ~ 1,
                                  fsstatus == 3 ~ 1,
                                  fsstatus == 1 ~ 0),
        food_insecure_child = case_when(fsstatusc == 2 ~ 1,
                                  fsstatusc == 3 ~ 1,
                                  fsstatusc == 1 ~ 0)
                                 )
# save
write_csv(CPS_dates, file.path(big_data_dir,"CPS_dates_county.csv"))



