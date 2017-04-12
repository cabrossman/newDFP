DFP_joinGeo_PagePos <- function(DelByPagePos,DelByGeo){
  
  
  
  #This section does the data manipulation to join the page@position and geo information to estimate impressions by:
  #sales group (network, marine etc), Section(SR,DT, ADVSR, etc), Position, and Geography 
  print("Joining and Shaping Data")
  #Sum up delivery from geo to campaign level to make sure that GEO and PagePos campaigns have the same delivery. In some cases
  #like 'default' this is not the case. To get better visibility to campaings like this we assume that the distribution across 
  # geography is similar and apply a divisor to "discount" impressions which are too high. This forces the sum total to be equal
  DelByGeo_campSum <- DelByGeo %>% group_by(campaign) %>% summarise(Imp = sum(impByGeo))
  
  #Sum up delivery from geo to campaign level to get Divisor
  #steps: 1) filter by portal, sum impressions by campaign. 2) join with DelByGeo_campsum to join PagePos totals with Geo Totals
  # 3) take the ratio of Geo/PagePos totals to get divisor. most will be 1 and thus will be unaffected
  DelByPagePosYW <- DelByPagePos %>% filter(business == 'YW') %>% group_by(campaign,salesgroup) %>% summarise(Imp = sum(impByPagePos)) %>% left_join(DelByGeo_campSum, by = "campaign") %>% rename(websiteImp = Imp.x, GeoImp = Imp.y) %>% mutate(Divisor = GeoImp/websiteImp)
  DelByPagePosBT <- DelByPagePos %>% filter(business == 'BT') %>% group_by(campaign,salesgroup) %>% summarise(Imp = sum(impByPagePos)) %>% left_join(DelByGeo_campSum, by = "campaign") %>% rename(websiteImp = Imp.x, GeoImp = Imp.y) %>% mutate(Divisor = GeoImp/websiteImp)
  DelByPagePosBC <- DelByPagePos %>% filter(business == 'BC') %>% group_by(campaign,salesgroup) %>% summarise(Imp = sum(impByPagePos)) %>% left_join(DelByGeo_campSum, by = "campaign") %>% rename(websiteImp = Imp.x, GeoImp = Imp.y) %>% mutate(Divisor = GeoImp/websiteImp)
  
  #Bring In Divisor to Adj Impressions by Geo
  #need to sum DelByGeo due to the two 'unknown' categories which appear in delivery by Geo (at state)
  DelByGeo2 <- DelByGeo %>% select(-salesgroup) %>% group_by(Advertiser, campaign, Geo, NoGeoExist, Country, State, Area) %>% summarise(impByGeo = sum(impByGeo))
  DelByGeoYW <- DelByGeo2 %>% inner_join(DelByPagePosYW, by = "campaign") %>% mutate(adjImpByGeo = impByGeo/Divisor)
  DelByGeoBT <- DelByGeo2 %>% inner_join(DelByPagePosBT, by = "campaign") %>% mutate(adjImpByGeo = impByGeo/Divisor)
  DelByGeoBC <- DelByGeo2 %>% inner_join(DelByPagePosBC, by = "campaign") %>% mutate(adjImpByGeo = impByGeo/Divisor)
  
  
  #Get the Mult by Taking Pct of Total Impressions by Position and Section
  DelByPagePosYW_PosSection <- DelByPagePos %>% filter(business == 'YW') %>% group_by(campaign,Pos,section) %>% summarise(Imp = sum(impByPagePos)) %>% left_join(DelByPagePosYW, by = "campaign") %>% mutate(Mult = Imp/websiteImp)
  DelByPagePosBT_PosSection <- DelByPagePos %>% filter(business == 'BT') %>% group_by(campaign,Pos,section) %>% summarise(Imp = sum(impByPagePos)) %>% left_join(DelByPagePosBT, by = "campaign") %>% mutate(Mult = Imp/websiteImp)
  DelByPagePosBC_PosSection <- DelByPagePos %>% filter(business == 'BC') %>% group_by(campaign,Pos,section) %>% summarise(Imp = sum(impByPagePos)) %>% left_join(DelByPagePosBC, by = "campaign") %>% mutate(Mult = Imp/websiteImp)
  # rm(DelByPagePosYW, DelByPagePosBT, DelByPagePosBC, DelByGeo_campSum)
  
  
  
  #Estimate DelByPagePosSectionGeo
  DelByPagePosSectionGeoYW <- DelByGeoYW %>% inner_join(DelByPagePosYW_PosSection, by = "campaign") %>% mutate(EstImpByPosSecGeo = adjImpByGeo*Mult, business = 'YW')
  DelByPagePosSectionGeoBT <- DelByGeoBT %>% inner_join(DelByPagePosBT_PosSection, by = "campaign") %>% mutate(EstImpByPosSecGeo = adjImpByGeo*Mult, business = 'BT')
  DelByPagePosSectionGeoBC <- DelByGeoBC %>% inner_join(DelByPagePosBC_PosSection, by = "campaign") %>% mutate(EstImpByPosSecGeo = adjImpByGeo*Mult, business = 'BC')
  # rm(DelByGeoYW, DelByGeoBT, DelByGeoBC,DelByPagePosYW_PosSection,DelByPagePosBT_PosSection,DelByPagePosBC_PosSection)
  DelByPagePosSectionGeo <- rbind(DelByPagePosSectionGeoYW,DelByPagePosSectionGeoBT,DelByPagePosSectionGeoBC)
  # rm(DelByPagePosSectionGeoYW,DelByPagePosSectionGeoBT,DelByPagePosSectionGeoBC)
  
  return(DelByPagePosSectionGeo)
}