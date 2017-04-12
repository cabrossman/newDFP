rm(list = ls())
#get needed libraries
source(paste0(getwd(),'/lib/AuthFunctions.R'))
#download keys from the API
keyList <- pullKeysFromDB()
  
  
dfpAuth()


#get all line items to check
filter <- paste0("('38690-1_13645_BoatRack_BO-SR-LNR-728x90-ST-250_NC', '39755-1_13779_PrincessUSA-VClass_BO-SRDT-RT1-300x250',",
                 "'45889-1_14155_MultihullSolutions_BOALL-SRDT-RT2-300x250_AugThrJu', '51647-1_14497_BostonWhaler-Domestic_BO-SRDT-RT1-300x250_T4_July_',", 
                 "'52966-1_14571_BO-SR-LNR-728x90-WV_CANCEL', '57248-1_14810_TotalConcepts_BO-ROS-RT1-300x250', '57663-1_14830_SSL_BRAND_BOEUR-SRDT-RT1-300x250',", 
                 "'38518-1_13635_Pantaenius_BODE-SRDT-RT1-300x250_Power', '42620-1_13635_Pantaenius_BOES-SRDT-RT1-300x250')")


line_item_detail_Del <- API_exponential_backoff_retry(dfp_getLineItemsByStatement(list(filterStatement=
                                                                                         list(query=paste0("WHERE name in ",filter)))))

line_item_detail_Del$rval[[16]]$name
line_item_detail_Del$rval[[16]]$targeting$geoTargeting
line_item_detail_Del$rval[[16]]$targeting$inventoryTargeting
line_item_detail_Del$rval[[16]]$targeting$technologyTargeting
x <- line_item_detail_Del$rval[[16]]$targeting$customTargeting

#simpleTargeting
#'57248-1_14810_TotalConcepts_BO-ROS-RT1-300x250'
line_item_detail_Del$rval[[17]]$name
line_item_detail_Del$rval[[17]]$targeting$geoTargeting
line_item_detail_Del$rval[[17]]$targeting$inventoryTargeting
line_item_detail_Del$rval[[17]]$targeting$technologyTargeting
x <- line_item_detail_Del$rval[[17]]$targeting$customTargeting




criterias=list('589839'= c('209332942599')
               , '589479'= c('209336503239','209336420439','209336556519','209336595639','209336465799','209336392599','209336540439','209336565159')
               , '591399' = c('209335948599','209335950039')
               , '591639' = c('209335959159','209335959399','209335959639','209335957959','209335957719','209335958679','209335960599','209335958199','209335960839')
               , '590679' = c('209334320679','209334342759')
)

forecast <-   forecastImpressions(adUnitVec = c('262822239'),
                                  geoVec = c('2840'),
                                  criteriasListOfNamedVecs=criterias,
                                  keyList = keyList)


#test
criterias=list('589839'= c('209332942599')
               , '589479'= c('209336503239','209336420439','209336556519','209336595639','209336465799','209336392599','209336540439','209336565159')
               , '591399' = c('209335948599','209335950039')
               , '591639' = c('209335959159','209335959399','209335959639','209335957959','209335957719','209335958679','209335960599','209335958199','209335960839')
               , '590679' = c('209334320679','209334342759')
)

forecast <-   forecastImpressions(adUnitVec = c('262822239'),
                                  geoVec = c('2840'),
                                  criteriasListOfNamedVecs=criterias,
                                  keyList = keyList,
                                  boolStringChar='make-OR-class')


#test
criterias=list('589839'= c('209332942359')
               , '591639'= c('209335957719')
)

r <-   forecastImpressions(adUnitVec = c('262822239'),
                           geoVec = c('2840','2250'),
                           criteriasListOfNamedVecs=NULL, #criterias,
                           keyList = keyList,
                           boolStringChar='all AND')