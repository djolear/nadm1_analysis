library(tidyverse, lubridate)



FARMIN=1.2
FARMAX=2.5
SODMIN=1.1
SODMAX=2.0
RGMIN=1.8
RGMAX=4.3
SFATMIN=8
SFATMAX=16
ADDSUGMIN=6.5
ADDSUGMAX=26
SODMIN=1.1;
SODMAX=2.0;
RGMIN=1.8;
RGMAX=4.3;
SFATMIN=8;
SFATMAX=16;

hei <-
  asa_24_total %>% 
  mutate(subid = subid.x) %>% 
  filter(KCAL > 1000) %>% 
  mutate(
    VEGDEN = ( V_TOTAL + V_LEGUMES ) / (KCAL / 1000),
    HEI2015C1_TOTALVEG=5*(VEGDEN/1.1),
    HEI2015C1_TOTALVEG = ifelse(HEI2015C1_TOTALVEG > 5, 5, HEI2015C1_TOTALVEG),
    
    
    GRBNDEN = ( V_LEGUMES + V_DRKGR) / (KCAL / 1000),
    HEI2015C2_GREEN_AND_BEAN=5*(GRBNDEN/0.2),
    HEI2015C2_GREEN_AND_BEAN = ifelse(HEI2015C2_GREEN_AND_BEAN > 5, 5, HEI2015C2_GREEN_AND_BEAN),
    
        
    FRTDEN = ( F_TOTAL ) / (KCAL / 1000),
    HEI2015C3_TOTALFRUIT=5*(FRTDEN/0.8),
    HEI2015C3_TOTALFRUIT = ifelse(HEI2015C3_TOTALFRUIT > 5, 5, HEI2015C3_TOTALFRUIT),
    

    WHFRDEN = ( F_OTHER ) / (KCAL / 1000),
    HEI2015C4_WHOLEFRUIT=5*(WHFRDEN/0.4),
    HEI2015C4_WHOLEFRUIT = ifelse(HEI2015C4_WHOLEFRUIT > 5, 5, HEI2015C4_WHOLEFRUIT),
    
    
    WGRNDEN = ( G_WHOLE ) / (KCAL / 1000),
    HEI2015C5_WHOLEGRAIN=10*(WGRNDEN/1.5),
    HEI2015C5_WHOLEGRAIN = ifelse(HEI2015C5_WHOLEGRAIN > 10, 10, HEI2015C5_WHOLEGRAIN),
    

    DAIRYDEN = ( D_TOTAL ) / (KCAL / 1000),
    HEI2015C6_TOTALDAIRY=10*(DAIRYDEN/1.3),
    HEI2015C6_TOTALDAIRY = ifelse(HEI2015C6_TOTALDAIRY > 10, 10, HEI2015C6_TOTALDAIRY),
    
  
    # pfallprotleg,
    PROTDEN = ( PF_TOTAL ) / (KCAL / 1000),
    HEI2015C7_TOTPROT=5*(PROTDEN/2.5),
    HEI2015C7_TOTPROT = ifelse(HEI2015C7_TOTPROT > 5, 5, HEI2015C7_TOTPROT),
    
    
    # pfseaplantleg,
    SEAPLDEN = ( PF_SEAFD_HI + PF_SEAFD_LOW + PF_NUTSDS + PF_LEGUMES + PF_SOY) / (KCAL / 1000),
    HEI2015C8_SEAPLANT_PROT=5*(SEAPLDEN/0.8),
    HEI2015C8_SEAPLANT_PROT = ifelse(HEI2015C8_SEAPLANT_PROT > 5, 5, HEI2015C8_SEAPLANT_PROT),
    
    MONOPOLY = (MFAT + PFAT),
    FARATIO = (MFAT + PFAT) / SFAT,
    
    HEI2015C9_FATTYACID = 
      ifelse( SFAT == 0 & MONOPOLY == 0, 0,
              ifelse( SFAT == 0 & MONOPOLY > 0, 10, 
                      ifelse(FARATIO >= FARMAX, 10, 
                             ifelse(FARATIO <= FARMIN, 0, 10 * (FARATIO - FARMIN) / (FARMAX - FARMIN))
                      )
              )
      ),
                      

                      
      
    SODDEN  = SODI / (KCAL),
    HEI2015C10_SODIUM = 
      ifelse(SODDEN <= SODMIN, 10, 
             ifelse(SODDEN >= SODMAX, 0, (10 * (SODDEN - SODMIN) / (SODMAX - SODMIN)))
      ),
    
    RGDEN = G_REFINED / (KCAL / 1000),
    HEI2015C11_REFINEDGRAIN = 
      ifelse(RGDEN <= RGMIN, 10, 
             ifelse(RGDEN >= RGMAX, 0, (10 - ( 10* (RGDEN-RGMIN) / (RGMAX-RGMIN) )))
      ),

    
    SFAT_PERC = 100 * (SFAT * 9 / KCAL),
    HEI2015C12_SFAT = 
      ifelse(SFAT_PERC >= SFATMAX, 0, 10 - ( 10* (SFAT_PERC-SFATMIN) / (SFATMAX-SFATMIN))
    ),


    ADDSUG_PERC = 100 * (ADD_SUGARS * 16 / KCAL), 
    HEI2015C13_ADDSUG = 
      ifelse(ADDSUG_PERC <= ADDSUGMIN, 10, 
             ifelse(ADDSUG_PERC >= ADDSUGMAX, 0, 10 * (ADDSUG_PERC - ADDSUGMIN) / (ADDSUGMAX - ADDSUGMIN))
      ),
    
    hei = HEI2015C1_TOTALVEG + HEI2015C2_GREEN_AND_BEAN + HEI2015C3_TOTALFRUIT + HEI2015C4_WHOLEFRUIT + 
      HEI2015C5_WHOLEGRAIN + HEI2015C6_TOTALDAIRY + HEI2015C7_TOTPROT + HEI2015C8_SEAPLANT_PROT + 
      HEI2015C9_FATTYACID + HEI2015C10_SODIUM + HEI2015C11_REFINEDGRAIN + HEI2015C12_SFAT + HEI2015C13_ADDSUG,
    
    VEGDEN = ( V_TOTAL ) / (KCAL / 1000),
    VGRNDEN = ( V_DRKGR ) / (KCAL / 1000),
    FDEN = ( F_TOTAL ) / (KCAL / 1000),
    SUGDEN = ( ADD_SUGARS ) / (KCAL / 1000),
    GDEN = ( G_WHOLE ) / (KCAL / 1000),
    GREFDEN = ( G_REFINED ) / (KCAL / 1000),
    SFDEN = ( SOLID_FATS ) / (KCAL / 1000),
    CHEESEDEN = ( D_CHEESE ) / (KCAL / 1000),
    NUTDEN = ( PF_NUTSDS ) / (KCAL / 1000),
    he = ( scale(VEGDEN) + scale(FDEN) - scale(SUGDEN) + scale(GDEN) - scale(SFDEN) ) /5
             
  ) %>% 
  dplyr::select(
    subid ,
    #ReportingDate,
    health_lm_beta,
    health_glm_beta,
    ses_subj_all2,
    # hese,
    # effexp,
    # stai_trait,
    # pss,
    # cesd,
    # panas_neg,
    erq_reap,
    erq_10,
    hei,
    he,
    gender,
    # veg,
    # neg,
    # bmi,
    # tfeq_cr,
    # eth,
    # starts_with("gen_eating")
    KCAL,
    VEGDEN:he,
    #AmtUsual

  )  %>% 
  mutate(
    #wday = as.factor(lubridate::wday(lubridate::mdy(stringr::str_replace(stringr::str_replace(ReportingDate, "/", "-"), "/", "-"))))
  )

hei %>% 
  cor(., use = "pairwise.complete.obs") %>% 
  round(., 2)

hei <- 
  hei %>% 
  left_join(
    ssdm2_ddm
  )

lm1 <- lmer(hei ~ HealthWeight +  (1|subid), data = hei)
lm1 <- lmer(scale(he) ~ scale(HealthWeight) + scale(TasteWeight)  + (1|subid), data = hei)
lm1 <- lm(VEGDEN ~ erq_reap, data = hei)

lm1 <- lm(hei ~ HealthWeight , data = hei)

summary(lm1)

