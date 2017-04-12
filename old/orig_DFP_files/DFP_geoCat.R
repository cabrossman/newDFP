DFP_geoCat <- function(geo_data = NULL,geoInformation = NULL){
  
  
  
  if(is.null(geoInformation)){
    geoInformation <- DFP_getGeoInfo()
  }
  
  if(is.null(geo_data)){
    geo_data <- geoInformation
    x <- geo_data %>% select(Country = countrycode)
  } else {
    x <- left_join(geo_data,geoInformation, by = c("Dimension.REGION_CRITERIA_ID" = "id")) %>% select(Country = countrycode)
    geo_data
    
    for(i in 1:NCOL(geo_data)){
      if(names(geo_data)[i] == 'Dimension.REGION_NAME'){
        names(geo_data)[i] = 'name'
      }
    }
    
  }

  

  geo_data$Country <- ifelse(is.na(x$Country),'UNKNOWN',x$Country)
  
  #this is some sort of problem for keboola, so renamiong
  geo_data$name <- ifelse(grepl('North Ossetia',geo_data$name),'North Ossetia_Alania',geo_data$name)

  geo_data$Geo <- ifelse(geo_data$Country == 'UNKNOWN','UNKNOWN',paste0(geo_data$Country," -- ",geo_data$name))

  geo_data$NoGeoExist <- ifelse(geo_data$Country == 'UNKNOWN',1,0)
  
  
  CA_EAST <- c('CA -- Quebec', 'CA -- Prince Edward Island', 'CA -- Ontario', 'CA -- Nova Scotia', 'CA -- Newfoundland And Labrador', 'CA -- New Brunswick','CA -- Newfoundland and Labrador')
  CA_WEST <- c('CA -- Alberta', 'CA -- British Columbia', 'CA -- Manitoba', 'CA -- Northwest Territories', 'CA -- Nunavut', 'CA -- Saskatchewan', 'CA -- Yukon Territory')
  US_Flordia <- c('US -- Florida')
  US_Midwest <- c('US -- Wisconsin', 'US -- Ohio', 'US -- Minnesota', 'US -- Michigan', 'US -- Indiana', 'US -- Illinois','US -- South Dakota', 'US -- Oklahoma', 'US -- North Dakota', 'US -- Nebraska','US -- Missouri', 'US -- Kansas', 'US -- Iowa')
  US_GulfCoast <- c('US -- Texas', 'US -- Mississippi', 'US -- Louisiana', 'US -- Alabama')
  US_Mid_Atlantic <- c('US -- West Virginia', 'US -- Virginia', 'US -- Pennsylvania', 'US -- Maryland', 'US -- Delaware', 'US -- District Of Columbia','US -- District of Columbia')
  US_New_England <- c('US -- Vermont', 'US -- Rhode Island', 'US -- New Hampshire', 'US -- Massachusetts', 'US -- Maine', 'US -- Connecticut')
  US_NorthWest <- c('US -- Washington', 'US -- Oregon', 'US -- Idaho', 'US -- Alaska','US -- Wyoming', 'US -- Montana')
  US_Southeast <- c('US -- Tennessee', 'US -- South Carolina', 'US -- North Carolina', 'US -- Kentucky', 'US -- Georgia', 'US -- Arkansas')
  US_Southwest <- c('US -- Utah', 'US -- New Mexico', 'US -- Nevada', 'US -- Hawaii', 'US -- Colorado', 'US -- California', 'US -- Arizona')
  US_NY_NJ <- c('US -- New Jersey', 'US -- New York')

  

  
  Africa <- c('NG', 'DJ', 'CI', 'GH', 'SN', 'NA', 'TG', 'TN', 'DZ', 'SC', 'MU', 'KE', 'MA', 'BJ', 'MW', 'CD', 'ML', 'GM', 'MZ', 'TZ', 'AO', 'LY', 'GA', 'CM', 'UG', 'RE', 'BW', 'CV', 'TD', 'YT', 'SL', 'SD', 'ZW', 'GW', 'BI', 'GN', 'ZM', 'SZ', 'CG', 'RW', 'MR', 'CF', 'ET', 'SH', 'GQ', 'BF', 'SO', 'LS', 'NE', 'ST', 'ZA','ER','LR','KM')
  Europe <- c('DE', 'GR', 'CZ', 'MC', 'FR', 'IE', 'UA', 'BE', 'LV', 'PL', 'BG', 'NL', 'GI', 'ES', 'ME', 'RS', 'PT', 'HR', 'MT', 'AT', 'RO', 'SK', 'HU', 'CH', 'EE', 'LT', 'LU', 'BA', 'IM', 'BY', 'DK', 'AX', 'GG', 'AL', 'MD', 'LI', 'JE', 'SM', 'MK', 'IS', 'AD', 'VA','FO','GL','IT','SI','SJ','XK')
  FarEast <- c('JP', 'SG', 'IN', 'PK', 'CN', 'MY', 'TH', 'HK', 'ID', 'PH', 'VN', 'GE', 'TW', 'MO', 'BD', 'KZ', 'MN', 'BN', 'KH', 'BT', 'MM', 'IO', 'TM', 'TL', 'KG', 'LA', 'UZ', 'CX', 'TJ','KR')
  NorthAmericaInt <- c('MQ', 'PA', 'AG', 'BM', 'BS', 'VI', 'VC', 'PR', 'TT', 'VG', 'MX', 'DO', 'GD', 'KY', 'CR', 'DM', 'AW', 'GT', 'GP', 'BZ', 'SV', 'BB', 'KN', 'AI', 'JM', 'LC', 'MS', 'NI', 'CU', 'HT', 'TC', 'HN')
  Australiasia <- c('AU', 'NZ', 'NC', 'GU', 'PF', 'VU', 'FJ', 'PG', 'CK', 'MP', 'PW', 'TO', 'NU', 'SB', 'FM', 'KI', 'WF', 'TV', 'NR', 'WS', 'NF', 'AS','AF','AM','AZ')
  MiddleEast <- c('BH', 'CY', 'EG', 'IR', 'IQ', 'IL', 'JO', 'KW', 'LB', 'OM', 'QA', 'SA', 'SY', 'TR', 'AE', 'YE','PS')
  SouthAmerica <- c('EC', 'AR', 'VE', 'BR', 'CO', 'PE', 'UY', 'BO', 'CL', 'PY', 'SR', 'GY', 'GF', 'FK','BQ')
  RussiaScandinavia <- c('FI', 'NO', 'RU', 'SE')
  UK <- c('GB')
  
  #return4
  geo_data$Area <- ifelse(geo_data$Geo %in% CA_EAST,"CA_EAST",
                          ifelse(geo_data$Geo %in% CA_WEST, "CA_WEST",
                                 ifelse(geo_data$Geo %in% US_Flordia, "US_Florida",
                                        # ifelse(DelByGeo_df$Geo %in% US_GreatLakes,"US_GreatLakes",
                                        ifelse(geo_data$Geo %in% US_GulfCoast, "US_GulfCoast",
                                               ifelse(geo_data$Geo %in% US_Mid_Atlantic, "US_Mid_Atlantic",
                                                      ifelse(geo_data$Geo %in% US_Midwest,"US_MId_West",
                                                             ifelse(geo_data$Geo %in% US_New_England,"US_New_England",
                                                                    ifelse(geo_data$Geo %in% US_NorthWest, "US_NorthWest",
                                                                           ifelse(geo_data$Geo %in% US_Southeast, "US_Southeast",
                                                                                  ifelse(geo_data$Geo %in% US_Southwest, "US_Southwest",
                                                                                         ifelse(geo_data$Geo %in% US_NY_NJ, "US_NY_NJ",
                                                                                                   
                                                                                                   ifelse(geo_data$Country %in% Africa, "Africa",
                                                                                                          ifelse(geo_data$Country %in% Europe, "Europe",
                                                                                                                 ifelse(geo_data$Country %in% FarEast, "FarEast",
                                                                                                                        ifelse(geo_data$Country %in% NorthAmericaInt, "NorthAmericaInt",
                                                                                                                               ifelse(geo_data$Country %in% MiddleEast, "MiddleEast",
                                                                                                                                      ifelse(geo_data$Country %in% SouthAmerica, "SouthAmerica",
                                                                                                                                             ifelse(geo_data$Country %in% RussiaScandinavia, "RussiaScandinavia",
                                                                                                                                                    ifelse(geo_data$Country %in% Australiasia, "Australiasia",
                                                                                                                                                           ifelse(geo_data$Country %in% UK, "UK",
                                                                                                                                                                  ifelse(tolower(geo_data$Country) == 'unknown', "Other - unkown",
                                                                                                                                                                         "Other - international")))))))))))))))))))))
  
 
  return(geo_data)
}

