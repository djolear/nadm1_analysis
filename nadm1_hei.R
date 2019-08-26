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

asa_24_total %>% 
  filter(KCAL > 0) %>% 
  filter(complete == "Y") %>% 
  filter(subid %!in% low_cals$subid) %>%   
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
                      

                      
      
    SODDEN  = SODI / (KCAL / 1000),
    HEI2015C10_SODIUM = 
      ifelse(SODDEN <= SODMIN, 10, 
             ifelse(SODDEN >= SODMAX, 0, 10 * (SODDEN - SODMIN) / (SODMAX - SODMIN))
      ),
    
    RGDEN = G_REFINED / (KCAL / 1000),
    
    
    SFAT_PERC = 100 * (SFAT * 9 / KCAL),
    

    ADDSUG_PERC = 100 * (ADD_SUGARS * 16 / KCAL), 
    HEI2015C13_ADDSUG = 
      ifelse(ADDSUG_PERC <= ADDSUGMIN, 10, 
             ifelse(ADDSUG_PERC >= ADDSUGMAX, 0, 10 * (ADDSUG_PERC - ADDSUGMIN) / (ADDSUGMAX - ADDSUGMIN))
      ),
    
    hei = HEI2015C1_TOTALVEG + HEI2015C2_GREEN_AND_BEAN + HEI2015C3_TOTALFRUIT + HEI2015C4_WHOLEFRUIT + HEI2015C5_WHOLEGRAIN + 
      HEI2015C6_TOTALDAIRY + HEI2015C7_TOTPROT + HEI2015C8_SEAPLANT_PROT + HEI2015C9_FATTYACID,
             
  ) %>% 
  select(
    health_lm_beta,
    health_glm_beta,
    ses_comp_all,
    ses_subj_all,
    ladd_comm,
    effexp,
    stai_trait,
    VEGDEN:HEI2015C13_ADDSUG,
    hei
  ) %>% 
  cor(., use = "pairwise.complete.obs") %>% 
  round(. , 2)
