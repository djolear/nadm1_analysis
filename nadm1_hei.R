library(tidyverse, lubridate)

asa_24_total %>% 
  filter(KCAL > 0) %>% 
  mutate(
    vtotalleg = ( V_TOTAL + V_LEGUMES ) / (KCAL / 1000),
    vdrkgrleg = ( V_LEGUMES + V_DRKGR) / (KCAL / 1000),
    f_total = ( F_TOTAL ) / (KCAL / 1000),
    fwholefrt = ( F_OTHER ) / (KCAL / 1000),
    sfat_perc = 100 * (SFAT * 9 / KCAL),
    add_sugars_per = 100 * (ADD_SUGARS * 16 / KCAL),
    
    hei = vtotalleg + vdrkgrleg
    
  ) 
