options(scipen=999)

# PACKAGE INSTALLING AND UPGRADING #
# install and load defined list of packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE,repos = c(
      CRAN = 'https://cran.rstudio.com',
      CRANextra = 'https://macos.rbind.io'
    )
    )
  sapply(pkg, require, character.only = TRUE)
}

list_of_required_pkg <- c(
  'haven',
  'tidyverse')

ipak(list_of_required_pkg)



#cntry Respondent's country of residence

#- welfare support
# gvslvol Standard of living for the old, governments' responsibility (0 Not governments' responsibility at all - 10 Entirely governments' responsibility)
# gvslvue Standard of living for the unemployed, governments' responsibility (0 Not governments' responsibility at all - 10 Entirely governments' responsibility)
# gvhlthc Health care for the sick, governments' responsibility (0 Not governments' responsibility at all - 10 Entirely governments' responsibility)
# gvcldcr Child care services for working parents, governments' responsibility (0 Not governments' responsibility at all - 10 Entirely governments' responsibility)
# gvjbevn Job for everyone, governments' responsibility (0 Not governments' responsibility at all - 10 Entirely governments' responsibility)
# gvpdlwk Paid leave from work to care for sick family, governments' responsibility (0 Not governments' responsibility at all - 10 Entirely governments' responsibility)

# -	Economic criticism
# sbstrec Social benefits/services place too great strain on economy (1 Agree strongly - 5 Disagree strongly)
# sbbsntx Social benefits/services cost businesses too much in taxes/charges (1 Agree strongly - 5 Disagree strongly)
# -	Social criticism: 
# sbprvpv Social benefits/services prevent widespread poverty (1 Agree strongly - 5 Disagree strongly)
# sbeqsoc Social benefits/services lead to a more equal society (1 Agree strongly - 5 Disagree strongly)
# sbcwkfm Social benefits/services make it easier to combine work and family (1 Agree strongly - 5 Disagree strongly)
# -	Moral criticism: 
# sblazy Social benefits/services make people lazy (1 Agree strongly - 5 Disagree strongly)
# sblwcoa Social benefits/services make people less willing care for one another (1 Agree strongly - 5 Disagree strongly)
# sblwlka Social benefits/services make people less willing look after themselves/family (1 Agree strongly - 5 Disagree strongly)

# Controls 
# agea Respondent's age
# eduyrs Years of full-time education completed
# gndr Gender (1 Male, 2 Feamle)
# hinctnta Household's total net income, all sources (Deciles of the actual household income range in the given country.)
# 
# egalitarianism 
# gincdif Government should reduce differences in income levels  (1 Agree strongly - 5 Disagree strongly)
# dfincac Large differences in income acceptable to reward talents and efforts  (1 Agree strongly - 5 Disagree strongly)
# smdfslv For fair society, differences in standard of living should be small  (1 Agree strongly - 5 Disagree strongly)

dataset_to_clean <- read_sav("ESS4e04_5.sav")

belgium <- dataset_to_clean %>% filter(cntry=="BE") %>% select(gvslvol,
                                                    gvslvue,
                                                    gvhlthc,
                                                    gvcldcr,
                                                    gvjbevn,
                                                    gvpdlwk,
                                                    sbstrec,
                                                    sbbsntx,
                                                    sbprvpv,
                                                    sbeqsoc,
                                                    sbcwkfm,
                                                    sblazy,
                                                    sblwcoa,
                                                    sblwlka,
                                                    agea,
                                                    eduyrs,
                                                    gndr,
                                                    hinctnta,
                                                    gincdif,
                                                    dfincac,
                                                    smdfslv)



write_sav(belgium, "ESS4_belgium.sav")

