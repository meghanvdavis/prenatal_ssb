#-----------------------------------------------------------------------
# Initial data exploration, cleaning, & identification of cohort
#-----------------------------------------------------------------------

# Load libraries and data -------------------

library(tidyverse)
library(mosaic)
library(descr)

ssb <- read.csv('Hirko08022024_Dataset 2.csv')

# Explore exposure data ---------------------

exp_vars <- select(ssb, 
                   SUGARSODA,
                   SUGARSODA_ECHO,
                   FRUITJUICE,
                   FRUITJUICE_ECHO,
                   SUGARFRUIT_DRINK, 
                   SUGARFRUIT_DRINK_ECHO,
                   SWEETDRINK_ECHO,
                   SUGAR_HONEY_ECHO,
                   HIGHCAFFEIN_SODA,
                   HIGHCAFFEIN_SODA_ECHO)

lapply(exp_vars, freq, plot = FALSE)

# Explore outcome data ----------------------

# cbq ---------------------------------------
cbq_vars <- select(ssb, starts_with("rcbqvsf"))

lapply(cbq_vars, freq, plot = FALSE)

# create score vars
ssb <- ssb %>% 
  mutate_at(c(43:78), as.numeric)    
  # "_6" & missing converted to NA by coercion

# reverse score items 
# 13, 19, 20, 22, 26, 29, 31, 34
ssb <- ssb %>% 
  mutate_at(
    c("rcbqvsf_13",
      "rcbqvsf_19",
      "rcbqvsf_20",
      "rcbqvsf_22",
      "rcbqvsf_26",
      "rcbqvsf_29",
      "rcbqvsf_31",
      "rcbqvsf_34"), 
    funs(recode(.,  
                `1`=7, 
                `2`=6,
                `3`=5,
                `4`=4,
                `5`=3,
                `6`=2,
                `7`=1, 
                .default = NaN)))

# cbq_total
cols_to_avg <- c(43:78)

ssb$cbq_total <- apply(ssb[cols_to_avg], 1, function(x) {
  if (sum(is.na(x)) > 7) {          
    # 8 or more NAs is an invalid score
    return(NA)
  } else {
    return(mean(x, na.rm = TRUE))
  }
})

# cbq - negative affect
# (items: 2, 5, 8, 11, 14, 17, 20r, 23, 26r, 29r, 32, 35)
cols_to_avg <- c(44, 47, 50, 53, 56, 59, 62, 65, 68, 71, 74, 77)

ssb$cbq_na <- apply(ssb[cols_to_avg], 1, function(x) {
  if (sum(is.na(x)) > 2) {          
    # 3 or more NAs is an invalid score
    return(NA)
  } else {
    return(mean(x, na.rm = TRUE))
  }
})

# cbq - surgency 
# (items: 1, 4, 7, 10, 13r, 16, 19r, 22r, 25, 28, 31r, 34r)
cols_to_avg <- c(43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76)

ssb$cbq_surg <- apply(ssb[cols_to_avg], 1, function(x) {
  if (sum(is.na(x)) > 2) {        
    # 3 or more NAs is an invalid score
    return(NA)
  } else {
    return(mean(x, na.rm = TRUE))
  }
})

# cbq - effort control
# (items: 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)
cols_to_avg <- c(45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78)

ssb$cbq_ec <- apply(ssb[cols_to_avg], 1, function(x) {
  if (sum(is.na(x)) > 2) {          
    # 3 or more NAs is an invalid score
    return(NA)
  } else {
    return(mean(x, na.rm = TRUE))
  }
})

# cbq summary stats
cbq_total_vars <- select(ssb, 
                         cbq_total,
                         cbq_na,
                         cbq_surg,
                         cbq_ec)

lapply(cbq_total_vars, favstats)

        

# asq ---------------------------------------

asq_num_vars <- select(ssb,
                       asq_36_z_cnum,
                       asq_36_z_pnum,
                       asq_36_z_snum)

lapply(asq_num_vars, freq, plot = FALSE)

asq_vars <- select(ssb,
                   asq_36_total_communication,
                   asq_36_total_problemsolving,
                   asq_36_total_personal_social)

lapply(asq_vars, favstats)


# construct final cohort --------------------
# complete cases of some exposure AND outcome data

# create dummy variables for exposure and outcome data
ssb$exposure <- 0
ssb$outcome <- 0

# populate dummy variables

ssb$exposure[ssb$SUGARSODA != "NA"] <- 1
ssb$exposure[ssb$SUGARSODA_ECHO != "NA"] <- 1
ssb$exposure[ssb$FRUITJUICE != "NA"] <- 1
ssb$exposure[ssb$FRUITJUICE_ECHO != "NA"] <- 1
ssb$exposure[ssb$SUGARFRUIT_DRINK != "NA"] <- 1
ssb$exposure[ssb$SWEETDRINK_ECHO != "NA"] <- 1
ssb$exposure[ssb$SUGAR_HONEY_ECHO != "NA"] <- 1
ssb$exposure[ssb$HIGHCAFFEIN_SODA != "NA"] <- 1
# check
freq(ssb$exposure, plot = FALSE)

ssb$outcome[ssb$cbq_total != "NaN"] <- 1
ssb$outcome[ssb$cbq_na != "NaN"] <- 1
ssb$outcome[ssb$cbq_surg != "NaN"] <- 1
ssb$outcome[ssb$cbq_ec != "NaN"] <- 1
ssb$outcome[ssb$asq_36_total_communication != "NaN"] <- 1
ssb$outcome[ssb$asq_36_total_problemsolving != "NaN"] <- 1
ssb$outcome[ssb$asq_36_total_personal_social != "NaN"] <- 1
# check
freq(ssb$outcome, plot = FALSE) 

# subset data & write dataset
ssb_cohort <- subset(ssb,           
                     exposure == 1 & outcome == 1,
                     select = c(ChildID,
                                MomID,
                                maternal_age_birth,
                                Mother_Race,
                                Mother_Ethnicity,
                                HEIGHT_FEET,
                                HEIGHT_INCH,
                                PREWT_LBS,
                                MARIT_STATUS,
                                mmra_b3c,
                                MTHR_BRST_STOPAGE_ECHO,
                                Child_Race,
                                Child_Ethnicity,
                                dem_c_2,
                                cbmra_c2,
                                cbmra_c7a_grams,
                                cbmra_c7b_kg,
                                cbmra_c7c_lbs,
                                cbmra_c7c_oz,
                                GA_Birth,
                                EDUC_LVL,
                                final_income,
                                ALCOH_DRINKPREG,
                                HOOKAH_PREG,
                                ECIGARETTE_PREG,
                                CIGARETTE_SMOKING,
                                AMPHE_PREG,
                                STEROID_PREG,
                                TRANQUI_PREG,
                                MARIJ_PREG,
                                COCAINE_PREG,
                                METHAM_PREG,
                                OPIOID_PREG,
                                NOPRESCOPIOID_PREG,
                                AMPHE_PREG2,
                                STEROID_PREG2,
                                MARIJ_PREG2,
                                COCAINE_PREG2,
                                METHAM_PREG2,
                                OPIOID_PREG2,
                                OPIOIDTREAT_PREG2,
                                NOPRESCOPIOID_PREG2,
                                SUGARSODA,
                                SUGARSODA_ECHO,
                                FRUITJUICE,
                                FRUITJUICE_ECHO,
                                SUGARFRUIT_DRINK,
                                SWEETDRINK_ECHO,
                                SUGAR_HONEY_ECHO,
                                HIGHCAFFEIN_SODA,
                                cbq_total,
                                cbq_na,
                                cbq_surg,
                                cbq_ec,
                                asq_36_total_communication,
                                asq_36_total_problemsolving,
                                asq_36_total_personal_social))
# check
length(ssb_cohort$ChildID)

write.csv(ssb_cohort,
          "ssb_cohort.csv") 

