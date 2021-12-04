library(utils)
library(tidyverse)
library(dplyr)
library(plyr)
library(glue)


# subset and rename Fed survey data
tidyFed <- function(df) {
    
    
    df <- df %>% select(
        ppgender, ppage, ppagecat, ppagect4,
        ppstaten, ppreg9, ppreg4, ppmsacat,
        ppmarit, pphhsize, pphhhead, ppt01, ppt1317, ppt612, ppt18ov, 
        pprent, pphouse, ind1, ppcm0160, ppwork,             
        union100, pph10001, ppcm1301, ppcm0062,
        ppethm, pphispan, ppeducat, ED0, iclevel, race_5cat, ppracem, educ_4cat, 
        CH2, CH3, ppfs1482, CFPB_score,
        ppfs0596, inc_4cat_50k, ppincimp, atleast_okay, pay_casheqv, skip_medical 
    ) 
    
    df <- df %>%
        dplyr::rename(
            gender = ppgender,
            age = ppage, 
            age_7cat = ppagecat, 
            age_4cat = ppagect4,
            state = ppstaten, 
            region_9base = ppreg9, 
            region_4base = ppreg4, 
            metro = ppmsacat,
            marital_status = ppmarit, 
            household_size = pphhsize, 
            household_head = pphhhead, 
            hh_child_0.1 = ppt01, 
            hh_child_13.17 = ppt1317, 
            hh_child_6.12 = ppt612, 
            hh_child_18p = ppt18ov, 
            house_ownership = pprent, 
            house_type = pphouse, 
            emp_industry = ind1, 
            emp_occupation = ppcm0160, 
            emp_status = ppwork,             
            emp_union = union100, 
            health = pph10001, 
            emp_type = ppcm1301, 
            emp_num_jobs = ppcm0062,
            race = ppethm, 
            race_hisp = pphispan, 
            edu_cat = ppeducat, 
            edu = ED0, 
            edu_level = iclevel, 
            race_5cat = race_5cat, 
            race_census = ppracem, 
            edu_4cat = educ_4cat, 
            edu_mother = CH2, 
            edu_father = CH3, 
            credit_guess = ppfs1482, 
            cfpb_score = CFPB_score,
            econ_saving = ppfs0596, 
            econ_inc_4cat = inc_4cat_50k, 
            econ_hh_income = ppincimp, 
            econ_fin_ok = atleast_okay, 
            econ_pay_exp400 = pay_casheqv, 
            econ_skip_med = skip_medical 
        )        
    
    
  
    df <- df %>%
        dplyr::mutate(
            gender = as.factor(gender),
            age_7cat = as.factor(age_7cat), 
            age_4cat = as.factor(age_4cat),
            state = as.factor(state), 
            region_9base = as.factor(region_9base), 
            region_4base = as.factor(region_4base), 
            metro = as.factor(metro),
            marital_status = as.factor(marital_status), 
            household_head = as.factor(household_head), 
            house_ownership = as.factor(house_ownership), 
            house_type = as.factor(house_type), 
            emp_industry = as.factor(emp_industry), 
            emp_occupation = as.factor(emp_occupation), 
            emp_status = as.factor(emp_status),             
            emp_union = as.factor(emp_union), 
            health = as.factor(health), 
            emp_type = as.factor(emp_type), 
            emp_num_jobs = as.factor(emp_num_jobs),
            race = as.factor(race), 
            race_hisp = as.factor(race_hisp), 
            edu_cat = as.factor(edu_cat), 
            edu = as.factor(edu), 
            edu_level = as.factor(edu_level), 
            race_5cat = as.factor(race_5cat), 
            race_census = as.factor(race_census), 
            edu_4cat = as.factor(edu_4cat), 
            edu_mother = as.factor(edu_mother), 
            edu_father = as.factor(edu_father), 
            credit_guess = as.factor(credit_guess), 
            econ_saving = as.factor(econ_saving), 
            econ_inc_4cat = as.factor(econ_inc_4cat), 
            econ_hh_income = as.factor(econ_hh_income), 
            econ_fin_ok = as.factor(econ_fin_ok), 
            econ_pay_exp400 = as.factor(econ_pay_exp400), 
            econ_skip_med = as.factor(econ_skip_med) 
        )        
    
    return(df)
}


# create factors for CFPB data
factorCFPB <- function(df) {
    
    df$sample = revalue(factor(df$sample), c(
        `1` = "General population",
        `2` = "Age 62+ oversample",
        `3` = "Race/ethnicity and poverty oversample"
    ))
    df$fpl = revalue(factor(df$fpl), c(
        `1` = "<100% FPL",
        `2` = "100%-199% FPL",
        `3` = "200%+ FPL"
    ))
    df$LIVINGARRANGEMENT = revalue(factor(df$LIVINGARRANGEMENT), c(
        `-1` = "Refused",
        `1` = "only adult in the household",
        `2` = "my spouse/partner/significant other",
        `3` = "in my parents' home",
        `4` = "with other family, friends etc",
        `5` = "Some other arrangement"
    ))
    df$EARNERS = revalue(factor(df$EARNERS), c(
        `-1` = "Refused",
        `1` = "One",
        `2` = "Two",
        `3` = "More than two"
    ))
    df$SAVINGSRANGES = revalue(factor(df$SAVINGSRANGES), c(
        `-1` = "Refused",
        `1` = "0",
        `2` = "$1-99",
        `3` = "$100-999",
        `4` = "$1,000-4,999",
        `5` = "$5,000-19,999",
        `6` = "$20,000-74,999",
        `7` = "$75,000 or more",
        `98` = "I don't know",
        `99` = "Prefer not to say"
    ))
    df$HOUSING = revalue(factor(df$HOUSING), c(
        `-1` = "Refused",
        `1` = "I own my home",
        `2` = "I rent",
        `3` = "I do not currently own or rent"
    ))
    df$VALUERANGES = revalue(factor(df$VALUERANGES), c(
        `-2` = "Question not asked",
        `-1` = "Refused",
        `1` = "Less than $150,000",
        `2` = "$150,000-249,999",
        `3` = "$250,000-399,999",
        `4` = "$400,000 or more",
        `98` = "I don't know",
        `99` = "Prefer not to say"
    ))
    df$MORTGAGE = revalue(factor(df$MORTGAGE), c(
        `-2` = "Question not asked",
        `-1` = "Refused",
        `1` = "Less than $50,000",
        `2` = "$50,000-199,999",
        `3` = "$200,000 or more",
        `98` = "I don't know",
        `99` = "Prefer not to say"
    ))
    df$SAVINGSRANGES = revalue(factor(df$SAVINGSRANGES), c(
        `-1` = "Refused",
        `1` = "0",
        `2` = "$1-99",
        `3` = "$100-999",
        `4` = "$1,000-4,999",
        `5` = "$5,000-19,999",
        `6` = "$20,000-74,999",
        `7` = "$75,000 or more",
        `98` = "I don't know",
        `99` = "Prefer not to say"
    ))
    
    df$agecat = revalue(factor(df$agecat), c(
        `1` = "18-24",
        `2` = "25-34",
        `3` = "35-44",
        `4` = "45-54",
        `5` = "55-61",
        `6` = "62-69",
        `7` = "70-74",
        `8` = "75+"
    ))
    df$generation = revalue(factor(df$generation), c(
        `1` = "Pre-Boomer",
        `2` = "Boomer",
        `3` = "Gen X",
        `4` = "Millennial"
    ))
    df$PPEDUC = revalue(factor(df$PPEDUC), c(
        `1` = "Less than high school",
        `2` = "High school degree/GED",
        `3` = "Some college/Associate",
        `4` = "Bachelor's degree",
        `5` = "Graduate/professional degree"
    ))
    df$PPETHM = revalue(factor(df$PPETHM), c(
        `1` = "White, Non-Hispanic",
        `2` = "Black, Non-Hispanic",
        `3` = "Other, Non-Hispanic",
        `4` = "Hispanic"
    ))
    df$PPGENDER = revalue(factor(df$PPGENDER), c(
        `1` = "Male",
        `2` = "Female"
    ))
    df$PPHHSIZE = revalue(factor(df$PPHHSIZE), c(
        `1` = "1",
        `2` = "2",
        `3` = "3",
        `4` = "4",
        `5` = "5+"
    ))
    df$PPINCIMP = revalue(factor(df$PPINCIMP), c(
        `1` = "Less than $20,000",
        `2` = "$20,000 to $29,999",
        `3` = "$30,000 to $39,999",
        `4` = "$40,000 to $49,999",
        `5` = "$50,000 to $59,999",
        `6` = "$60,000 to $74,999",
        `7` = "$75,000 to $99,999",
        `8` = "$100,000 to $149,999",
        `9` = "$150,000 or more"
    ))
    df$PPMARIT = revalue(factor(df$PPMARIT), c(
        `1` = "Married",
        `2` = "Widowed",
        `3` = "Divorced/Separated",
        `4` = "Never married",
        `5` = "Living with partner"
    ))
    df$PPMSACAT = revalue(factor(df$PPMSACAT), c(
        `0` = "Non-Metro",
        `1` = "Metro"
    ))
    df$PPREG4 = revalue(factor(df$PPREG4), c(
        `1` = "Northeast",
        `2` = "Midwest",
        `3` = "South",
        `4` = "West"
    ))
    df$PPREG9 = revalue(factor(df$PPREG9), c(
        `1` = "New England",
        `2` = "Mid-Atlantic",
        `3` = "East-North Central",
        `4` = "West-North Central",
        `5` = "South Atlantic",
        `6` = "East-South Central",
        `7` = "West-South Central",
        `8` = "Mountain",
        `9` = "Pacific"
    ))
    
    return(df)
    
}


# subset and rename CFPB data
tidyCFPB <- function(df) {
    
    df <- df %>%
        select(sample, fpl, 
               FWBscore,FSscore,LMscore,KHscore,
               LIVINGARRANGEMENT,EARNERS, SAVINGSRANGES,
               HOUSING,VALUERANGES,MORTGAGE,
               agecat,generation,PPEDUC,PPETHM,PPGENDER,PPINCIMP,
               PPHHSIZE,PPMARIT,PPMSACAT,PPREG4,PPREG9
        )
    
    df <- factorCFPB(df)
    
    df <- df %>%
        dplyr::rename(
            sample_pop = sample, 
            poverty_stat = fpl, 
            cfpb_score = FWBscore,
            fs_skills_score = FSscore,
            fs_lm_score = LMscore,
            fs_kh_score = KHscore,
            hh_arrange = LIVINGARRANGEMENT,
            hh_earners = EARNERS, 
            econ_save_rate = SAVINGSRANGES,
            house_ownership = HOUSING,
            house_value = VALUERANGES,
            house_mortgage = MORTGAGE,
            age_8cat = agecat,
            age_generation = generation,
            edu_level = PPEDUC,
            race = PPETHM,
            gender = PPGENDER,
            econ_hh_income = PPINCIMP,
            hh_size = PPHHSIZE,
            marital_status = PPMARIT,
            metro = PPMSACAT,
            region_4base = PPREG4,
            region_9base = PPREG9
        )
    
    
    
    
    return(df)
    
}


# laod and format Fed survey data 
getRawFedFile <- function() {
    
    file_dir <- glue(getwd(),"/ProjectFinal/files/")
    fed_file_zip <- glue(file_dir,"fed.zip")
    fed_url <- "https://www.federalreserve.gov/consumerscommunities/files/SHED_public_use_data_2020_(CSV).zip"
    
    download.file(fed_url, destfile=fed_file_zip)
    unzip(fed_file_zip, exdir = file_dir)
    
    zip_meta <- unzip(fed_file_zip, list = TRUE)
    fed_file_name <- glue(file_dir,zip_meta$Name)
    
    fed_df <- read.csv(fed_file_name, encoding="UTF-8")
    
    fed_df <- purrr::modify_if(fed_df, is.character ,iconv ,"latin1", "UTF-8",sub='')
    fed_df <- purrr::modify_if(fed_df, is.character,str_trunc , 30)   
    
    return(fed_df)
    
}



# wrapper function to get Fed survey data
getFedFile <- function() {
    
    fed_df <- getRawFedFile()
    fed_df <- tidyFed(fed_df)
    
    return(fed_df)
    
}


# laod and format CFPB survey data 
getRawCFPBFile <- function() {
    
    cfpb_url <- "https://www.consumerfinance.gov/documents/5614/NFWBS_PUF_2016_data.csv"
    cfpb_df <- read.csv(cfpb_url, encoding="UTF-8")
    
    return(cfpb_df)
}



# wrapper function to get CFPB survey data
getCFPBFile <- function() {
    
    cfpb_df <- getRawCFPBFile()
    cfpb_df <- tidyCFPB(cfpb_df)
    
    return(cfpb_df)
}



normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

un_normalize <- function(norm, denorm) {
    return (norm * (max(denorm) - min(denorm)) + min(denorm))
}

