DFP_getAllMarineLineItems <- function(){
      options(warn=-1)
      print("this method gets all marine line items ever from DFP and returns a formatted data frame which is parsed from a list")
      
      dfpAuth()

      
      #get all line items
      line_item_detail_Del <- API_exponential_backoff_retry(dfp_getLineItemsByStatement(list(filterStatement=
                                                                        list(query="WHERE status='DELIVERING'"))))
      
      line_item_detail_notDel <- API_exponential_backoff_retry(dfp_getLineItemsByStatement(list(filterStatement=
                                                                 list(query="WHERE status<>'DELIVERING'"))))
      
      
      allDelivering <- NULL
      for(i in 3:length(line_item_detail_Del$rval)){
      
        if( 'orderId' %in% names(line_item_detail_Del$rval[[i]])) {orderId <- line_item_detail_Del$rval[[i]]$orderId} else { orderId <- NA}
        if( 'id' %in% names(line_item_detail_Del$rval[[i]])) {id <- line_item_detail_Del$rval[[i]]$id} else { id <- NA}
        if( 'name' %in% names(line_item_detail_Del$rval[[i]])) {name <- line_item_detail_Del$rval[[i]]$name} else { name <- NA}
        if( 'orderName' %in% names(line_item_detail_Del$rval[[i]])) {orderName <- line_item_detail_Del$rval[[i]]$orderName} else { orderName <- NA}
        if( 'startDateTime' %in% names(line_item_detail_Del$rval[[i]])) {
          startYear <- line_item_detail_Del$rval[[i]]$startDateTime$date$year
          startMonth <- line_item_detail_Del$rval[[i]]$startDateTime$date$month
          startDay <- line_item_detail_Del$rval[[i]]$startDateTime$date$day
        } else { 
          startYear <- NA
          startMonth <- NA
          startDay <- NA
        }
        if( 'endDateTime' %in% names(line_item_detail_Del$rval[[i]])) {
          endYear <- line_item_detail_Del$rval[[i]]$endDateTime$date$year
          endMonth <- line_item_detail_Del$rval[[i]]$endDateTime$date$month
          endDay <- line_item_detail_Del$rval[[i]]$endDateTime$date$day
        } else { 
          endYear <- NA
          endMonth <- NA
          endDay <- NA
        }
        if( 'autoExtensionDays' %in% names(line_item_detail_Del$rval[[i]])) {autoExtensionDays <- line_item_detail_Del$rval[[i]]$autoExtensionDays} else { autoExtensionDays <- NA}
        if( 'unlimitedEndDateTime' %in% names(line_item_detail_Del$rval[[i]])) {unlimitedEndDateTime <- line_item_detail_Del$rval[[i]]$unlimitedEndDateTime} else { unlimitedEndDateTime <- NA}
        if( 'creativeRotationType' %in% names(line_item_detail_Del$rval[[i]])) {creativeRotationType <- line_item_detail_Del$rval[[i]]$creativeRotationType} else { creativeRotationType <- NA}
        if( 'deliveryRateType' %in% names(line_item_detail_Del$rval[[i]])) {deliveryRateType <- line_item_detail_Del$rval[[i]]$deliveryRateType} else { deliveryRateType <- NA}
        if( 'roadblockingType' %in% names(line_item_detail_Del$rval[[i]])) {roadblockingType <- line_item_detail_Del$rval[[i]]$roadblockingType} else { roadblockingType <- NA}
        if( 'lineItemType' %in% names(line_item_detail_Del$rval[[i]])) {lineItemType <- line_item_detail_Del$rval[[i]]$lineItemType} else { lineItemType <- NA}
        if( 'priority' %in% names(line_item_detail_Del$rval[[i]])) {priority <- line_item_detail_Del$rval[[i]]$priority} else { priority <- NA}
        if( 'costPerUnit' %in% names(line_item_detail_Del$rval[[i]])) {
          costPerUnitCurrencyCode <- line_item_detail_Del$rval[[i]]$costPerUnit$currencyCode
          costPerUnitmicoAmount<- line_item_detail_Del$rval[[i]]$costPerUnit$microAmount
        } else { 
          costPerUnitCurrencyCode <- NA
          costPerUnitmicoAmount<- NA
        }
        if( 'costType' %in% names(line_item_detail_Del$rval[[i]])) {costType <- line_item_detail_Del$rval[[i]]$costType} else { costType <- NA}
        if( 'discountType' %in% names(line_item_detail_Del$rval[[i]])) {discountType <- line_item_detail_Del$rval[[i]]$discountType} else { discountType <- NA}
        if( 'discount' %in% names(line_item_detail_Del$rval[[i]])) {discount <- line_item_detail_Del$rval[[i]]$discount} else { discount <- NA}
        if( 'contractedUnitsBought' %in% names(line_item_detail_Del$rval[[i]])) {contractedUnitsBought <- line_item_detail_Del$rval[[i]]$contractedUnitsBought} else { contractedUnitsBought <- NA}
        if( 'creativePlaceholders' %in% names(line_item_detail_Del$rval[[i]])) {
          creativeWidth <- line_item_detail_Del$rval[[i]]$creativePlaceholders$size$width
          creativeHeight <- line_item_detail_Del$rval[[i]]$creativePlaceholders$size$height
        } else { 
          creativeWidth <- NA
          creativeHeight <- NA
        }
        if( 'targetPlatform' %in% names(line_item_detail_Del$rval[[i]])) {targetPlatform <- line_item_detail_Del$rval[[i]]$targetPlatform} else { targetPlatform <- NA}
        if( 'environmentType' %in% names(line_item_detail_Del$rval[[i]])) {environmentType <- line_item_detail_Del$rval[[i]]$environmentType} else { environmentType <- NA}
        if( 'companionDeliveryOption' %in% names(line_item_detail_Del$rval[[i]])) {companionDeliveryOption <- line_item_detail_Del$rval[[i]]$companionDeliveryOption} else { companionDeliveryOption <- NA}
        if( 'creativePersistenceType' %in% names(line_item_detail_Del$rval[[i]])) {creativePersistenceType <- line_item_detail_Del$rval[[i]]$creativePersistenceType} else { creativePersistenceType <- NA}
        if( 'allowOverbook' %in% names(line_item_detail_Del$rval[[i]])) {allowOverbook <- line_item_detail_Del$rval[[i]]$allowOverbook} else { allowOverbook <- NA}
        if( 'skipInventoryCheck' %in% names(line_item_detail_Del$rval[[i]])) {skipInventoryCheck <- line_item_detail_Del$rval[[i]]$skipInventoryCheck} else { skipInventoryCheck <- NA}
        if( 'skipCrossSellingRuleWarningChecks' %in% names(line_item_detail_Del$rval[[i]])) {skipCrossSellingRuleWarningChecks <- line_item_detail_Del$rval[[i]]$skipCrossSellingRuleWarningChecks} else { skipCrossSellingRuleWarningChecks <- NA}
        if( 'reserveAtCreation' %in% names(line_item_detail_Del$rval[[i]])) {reserveAtCreation <- line_item_detail_Del$rval[[i]]$reserveAtCreation} else { reserveAtCreation <- NA}
        if( 'stats' %in% names(line_item_detail_Del$rval[[i]])) {
          impressionsDelivered <- line_item_detail_Del$rval[[i]]$stats$impressionsDelivered
          clicksDelivered <- line_item_detail_Del$rval[[i]]$stats$clicksDelivered
          videoStartsDelivered <- line_item_detail_Del$rval[[i]]$stats$videoStartsDelivered
        } else { 
          impressionsDelivered <- NA
          clicksDelivered <- NA
          videoStartsDelivered <- NA
        }
        if( 'deliveryIndicator' %in% names(line_item_detail_Del$rval[[i]])) {
          deliveryExpectedPCT <- line_item_detail_Del$rval[[i]]$deliveryIndicator$expectedDeliveryPercentage
          deliveryActualPCT <- line_item_detail_Del$rval[[i]]$deliveryIndicator$actualDeliveryPercentage
        } else { 
          deliveryExpectedPCT <- NA
          deliveryActualPCT <- NA
        }
        if( 'status' %in% names(line_item_detail_Del$rval[[i]])) {status <- line_item_detail_Del$rval[[i]]$status} else { status <- NA}
        if( 'reservationStatus' %in% names(line_item_detail_Del$rval[[i]])) {reservationStatus <- line_item_detail_Del$rval[[i]]$reservationStatus} else { reservationStatus <- NA}
        if( 'isArchived' %in% names(line_item_detail_Del$rval[[i]])) {isArchived <- line_item_detail_Del$rval[[i]]$isArchived} else { isArchived <- NA}
        if( 'webPropertyCode' %in% names(line_item_detail_Del$rval[[i]])) {webPropertyCode <- line_item_detail_Del$rval[[i]]$webPropertyCode} else { webPropertyCode <- NA}
        if( 'disableSameAdvertiserCompetitiveExclusion' %in% names(line_item_detail_Del$rval[[i]])) {disableSameAdvertiserCompetitiveExclusion <- line_item_detail_Del$rval[[i]]$disableSameAdvertiserCompetitiveExclusion} else { disableSameAdvertiserCompetitiveExclusion <- NA}
        if( 'lastModifiedByApp' %in% names(line_item_detail_Del$rval[[i]])) {lastModifiedByApp <- line_item_detail_Del$rval[[i]]$lastModifiedByApp} else { lastModifiedByApp <- NA}
        if( 'lastModifiedDateTime' %in% names(line_item_detail_Del$rval[[i]])) {
          lastModifiedYear <- line_item_detail_Del$rval[[i]]$lastModifiedDateTime$date$year
          lastModifiedMonth <- line_item_detail_Del$rval[[i]]$lastModifiedDateTime$date$month
          lastModifiedDay <- line_item_detail_Del$rval[[i]]$lastModifiedDateTime$date$day
        } else { 
          lastModifiedYear <- NA
          lastModifiedMonth <- NA
          lastModifiedDay <- NA
        }
        if( 'creationDateTime' %in% names(line_item_detail_Del$rval[[i]])) {
          creationYear <- line_item_detail_Del$rval[[i]]$creationDateTime$date$year
          creationMonth <- line_item_detail_Del$rval[[i]]$creationDateTime$date$month
          creationDay <- line_item_detail_Del$rval[[i]]$creationDateTime$date$day
        } else { 
          creationYear <- NA
          creationMonth <- NA
          creationDay <- NA
        }
        if( 'isPrioritizedPreferredDealsEnabled' %in% names(line_item_detail_Del$rval[[i]])) {isPrioritizedPreferredDealsEnabled <- line_item_detail_Del$rval[[i]]$isPrioritizedPreferredDealsEnabled} else { isPrioritizedPreferredDealsEnabled <- NA}
        if( 'adExchangeAuctionOpeningPriority' %in% names(line_item_detail_Del$rval[[i]])) {adExchangeAuctionOpeningPriority <- line_item_detail_Del$rval[[i]]$adExchangeAuctionOpeningPriority} else { adExchangeAuctionOpeningPriority <- NA}
        if( 'isSetTopBoxEnabled' %in% names(line_item_detail_Del$rval[[i]])) {isSetTopBoxEnabled <- line_item_detail_Del$rval[[i]]$isSetTopBoxEnabled} else { isSetTopBoxEnabled <- NA}
        if( 'isMissingCreatives' %in% names(line_item_detail_Del$rval[[i]])) {isMissingCreatives <- line_item_detail_Del$rval[[i]]$isMissingCreatives} else { isMissingCreatives <- NA}
        if( 'primaryGoal' %in% names(line_item_detail_Del$rval[[i]])) {
          primaryGoalType <- line_item_detail_Del$rval[[i]]$primaryGoal$goalType
          primaryGoalUnitType <- line_item_detail_Del$rval[[i]]$primaryGoal$unitType
        } else { 
          primaryGoalType <- NA
          primaryGoalUnitType <- NA
            }
        
        temp <- c(
                  orderId = orderId,
                  id = id,
                  name = name,
                  orderName = orderName,
                  startYear = as.numeric(startYear),
                  startMonth = as.numeric(startMonth),
                  startDay = as.numeric(startDay),
                  endYear = as.numeric(endYear),
                  endMonth = as.numeric(endMonth),
                  endDay = as.numeric(endDay),
                  autoExtensionDays = autoExtensionDays,
                  unlimitedEndDateTime = unlimitedEndDateTime,
                  creativeRotationType = creativeRotationType,
                  deliveryRateType = deliveryRateType,
                  roadblockingType = roadblockingType,
                  lineItemType = lineItemType,
                  priority = as.numeric(priority),
                  costPerUnitCurrencyCode = as.numeric(costPerUnitCurrencyCode),
                  costPerUnitmicoAmount = costPerUnitmicoAmount,
                  costType = costType,
                  discountType = discountType,
                  discount = as.numeric(discount),
                  contractedUnitsBought = as.numeric(contractedUnitsBought),
                  creativeWidth = as.numeric(creativeWidth),
                  creativeHeight = as.numeric(creativeHeight),
                  targetPlatform = targetPlatform,
                  environmentType = environmentType,
                  companionDeliveryOption = companionDeliveryOption,
                  creativePersistenceType = creativePersistenceType,
                  allowOverbook = allowOverbook,
                  skipInventoryCheck = skipInventoryCheck,
                  skipCrossSellingRuleWarningChecks = skipCrossSellingRuleWarningChecks,
                  reserveAtCreation = reserveAtCreation,
                  impressionsDelivered = as.numeric(impressionsDelivered),
                  clicksDelivered = as.numeric(clicksDelivered),
                  videoStartsDelivered = as.numeric(videoStartsDelivered),
                  deliveryExpectedPCT = as.numeric(deliveryExpectedPCT),
                  deliveryActualPCT = as.numeric(deliveryActualPCT),
                  status = status,
                  reservationStatus = reservationStatus,
                  isArchived = isArchived,
                  webPropertyCode = webPropertyCode,
                  disableSameAdvertiserCompetitiveExclusion = disableSameAdvertiserCompetitiveExclusion,
                  lastModifiedByApp = lastModifiedByApp,
                  lastModifiedYear = as.numeric(lastModifiedYear),
                  lastModifiedMonth = as.numeric(lastModifiedMonth),
                  lastModifiedDay = as.numeric(lastModifiedDay),
                  creationYear = as.numeric(creationYear),
                  creationMonth = as.numeric(creationMonth),
                  creationDay = as.numeric(creationDay),
                  isPrioritizedPreferredDealsEnabled = isPrioritizedPreferredDealsEnabled,
                  adExchangeAuctionOpeningPriority = as.numeric(adExchangeAuctionOpeningPriority),
                  isSetTopBoxEnabled = isSetTopBoxEnabled,
                  isMissingCreatives = isMissingCreatives,
                  primaryGoalType = primaryGoalType,
                  primaryGoalUnitType = primaryGoalUnitType
      
        )
        
        allDelivering <- rbind(allDelivering,temp)
        row.names(allDelivering)[i-2] <- i-2
        
      }
      allDelivering <- as.data.frame(allDelivering)
      #not deli
      allNotDelivering <- NULL
      for(j in 3:length(line_item_detail_notDel$rval)){
        
        if( 'orderId' %in% names(line_item_detail_notDel$rval[[j]])) {orderId <- line_item_detail_notDel$rval[[j]]$orderId} else { orderId <- NA}
        if( 'id' %in% names(line_item_detail_notDel$rval[[j]])) {id <- line_item_detail_notDel$rval[[j]]$id} else { id <- NA}
        if( 'name' %in% names(line_item_detail_notDel$rval[[j]])) {name <- line_item_detail_notDel$rval[[j]]$name} else { name <- NA}
        if( 'orderName' %in% names(line_item_detail_notDel$rval[[j]])) {orderName <- line_item_detail_notDel$rval[[j]]$orderName} else { orderName <- NA}
        if( 'startDateTime' %in% names(line_item_detail_notDel$rval[[j]])) {
          startYear <- line_item_detail_notDel$rval[[j]]$startDateTime$date$year
          startMonth <- line_item_detail_notDel$rval[[j]]$startDateTime$date$month
          startDay <- line_item_detail_notDel$rval[[j]]$startDateTime$date$day
        } else { 
          startYear <- NA
          startMonth <- NA
          startDay <- NA
        }
        if( 'endDateTime' %in% names(line_item_detail_notDel$rval[[j]])) {
          endYear <- line_item_detail_notDel$rval[[j]]$endDateTime$date$year
          endMonth <- line_item_detail_notDel$rval[[j]]$endDateTime$date$month
          endDay <- line_item_detail_notDel$rval[[j]]$endDateTime$date$day
        } else { 
          endYear <- NA
          endMonth <- NA
          endDay <- NA
        }
        if( 'autoExtensionDays' %in% names(line_item_detail_notDel$rval[[j]])) {autoExtensionDays <- line_item_detail_notDel$rval[[j]]$autoExtensionDays} else { autoExtensionDays <- NA}
        if( 'unlimitedEndDateTime' %in% names(line_item_detail_notDel$rval[[j]])) {unlimitedEndDateTime <- line_item_detail_notDel$rval[[j]]$unlimitedEndDateTime} else { unlimitedEndDateTime <- NA}
        if( 'creativeRotationType' %in% names(line_item_detail_notDel$rval[[j]])) {creativeRotationType <- line_item_detail_notDel$rval[[j]]$creativeRotationType} else { creativeRotationType <- NA}
        if( 'deliveryRateType' %in% names(line_item_detail_notDel$rval[[j]])) {deliveryRateType <- line_item_detail_notDel$rval[[j]]$deliveryRateType} else { deliveryRateType <- NA}
        if( 'roadblockingType' %in% names(line_item_detail_notDel$rval[[j]])) {roadblockingType <- line_item_detail_notDel$rval[[j]]$roadblockingType} else { roadblockingType <- NA}
        if( 'lineItemType' %in% names(line_item_detail_notDel$rval[[j]])) {lineItemType <- line_item_detail_notDel$rval[[j]]$lineItemType} else { lineItemType <- NA}
        if( 'priority' %in% names(line_item_detail_notDel$rval[[j]])) {priority <- line_item_detail_notDel$rval[[j]]$priority} else { priority <- NA}
        if( 'costPerUnit' %in% names(line_item_detail_notDel$rval[[j]])) {
          costPerUnitCurrencyCode <- line_item_detail_notDel$rval[[j]]$costPerUnit$currencyCode
          costPerUnitmicoAmount<- line_item_detail_notDel$rval[[j]]$costPerUnit$microAmount
        } else { 
          costPerUnitCurrencyCode <- NA
          costPerUnitmicoAmount<- NA
        }
        if( 'costType' %in% names(line_item_detail_notDel$rval[[j]])) {costType <- line_item_detail_notDel$rval[[j]]$costType} else { costType <- NA}
        if( 'discountType' %in% names(line_item_detail_notDel$rval[[j]])) {discountType <- line_item_detail_notDel$rval[[j]]$discountType} else { discountType <- NA}
        if( 'discount' %in% names(line_item_detail_notDel$rval[[j]])) {discount <- line_item_detail_notDel$rval[[j]]$discount} else { discount <- NA}
        if( 'contractedUnitsBought' %in% names(line_item_detail_notDel$rval[[j]])) {contractedUnitsBought <- line_item_detail_notDel$rval[[j]]$contractedUnitsBought} else { contractedUnitsBought <- NA}
        if( 'creativePlaceholders' %in% names(line_item_detail_notDel$rval[[j]])) {
          creativeWidth <- line_item_detail_notDel$rval[[j]]$creativePlaceholders$size$width
          creativeHeight <- line_item_detail_notDel$rval[[j]]$creativePlaceholders$size$height
        } else { 
          creativeWidth <- NA
          creativeHeight <- NA
        }
        if( 'targetPlatform' %in% names(line_item_detail_notDel$rval[[j]])) {targetPlatform <- line_item_detail_notDel$rval[[j]]$targetPlatform} else { targetPlatform <- NA}
        if( 'environmentType' %in% names(line_item_detail_notDel$rval[[j]])) {environmentType <- line_item_detail_notDel$rval[[j]]$environmentType} else { environmentType <- NA}
        if( 'companionDeliveryOption' %in% names(line_item_detail_notDel$rval[[j]])) {companionDeliveryOption <- line_item_detail_notDel$rval[[j]]$companionDeliveryOption} else { companionDeliveryOption <- NA}
        if( 'creativePersistenceType' %in% names(line_item_detail_notDel$rval[[j]])) {creativePersistenceType <- line_item_detail_notDel$rval[[j]]$creativePersistenceType} else { creativePersistenceType <- NA}
        if( 'allowOverbook' %in% names(line_item_detail_notDel$rval[[j]])) {allowOverbook <- line_item_detail_notDel$rval[[j]]$allowOverbook} else { allowOverbook <- NA}
        if( 'skipInventoryCheck' %in% names(line_item_detail_notDel$rval[[j]])) {skipInventoryCheck <- line_item_detail_notDel$rval[[j]]$skipInventoryCheck} else { skipInventoryCheck <- NA}
        if( 'skipCrossSellingRuleWarningChecks' %in% names(line_item_detail_notDel$rval[[j]])) {skipCrossSellingRuleWarningChecks <- line_item_detail_notDel$rval[[j]]$skipCrossSellingRuleWarningChecks} else { skipCrossSellingRuleWarningChecks <- NA}
        if( 'reserveAtCreation' %in% names(line_item_detail_notDel$rval[[j]])) {reserveAtCreation <- line_item_detail_notDel$rval[[j]]$reserveAtCreation} else { reserveAtCreation <- NA}
        if( 'stats' %in% names(line_item_detail_notDel$rval[[j]])) {
          impressionsDelivered <- line_item_detail_notDel$rval[[j]]$stats$impressionsDelivered
          clicksDelivered <- line_item_detail_notDel$rval[[j]]$stats$clicksDelivered
          videoStartsDelivered <- line_item_detail_notDel$rval[[j]]$stats$videoStartsDelivered
        } else { 
          impressionsDelivered <- NA
          clicksDelivered <- NA
          videoStartsDelivered <- NA
        }
        if( 'deliveryIndicator' %in% names(line_item_detail_notDel$rval[[j]])) {
          deliveryExpectedPCT <- line_item_detail_notDel$rval[[j]]$deliveryIndicator$expectedDeliveryPercentage
          deliveryActualPCT <- line_item_detail_notDel$rval[[j]]$deliveryIndicator$actualDeliveryPercentage
        } else { 
          deliveryExpectedPCT <- NA
          deliveryActualPCT <- NA
        }
        if( 'status' %in% names(line_item_detail_notDel$rval[[j]])) {status <- line_item_detail_notDel$rval[[j]]$status} else { status <- NA}
        if( 'reservationStatus' %in% names(line_item_detail_notDel$rval[[j]])) {reservationStatus <- line_item_detail_notDel$rval[[j]]$reservationStatus} else { reservationStatus <- NA}
        if( 'isArchived' %in% names(line_item_detail_notDel$rval[[j]])) {isArchived <- line_item_detail_notDel$rval[[j]]$isArchived} else { isArchived <- NA}
        if( 'webPropertyCode' %in% names(line_item_detail_notDel$rval[[j]])) {webPropertyCode <- line_item_detail_notDel$rval[[j]]$webPropertyCode} else { webPropertyCode <- NA}
        if( 'disableSameAdvertiserCompetitiveExclusion' %in% names(line_item_detail_notDel$rval[[j]])) {disableSameAdvertiserCompetitiveExclusion <- line_item_detail_notDel$rval[[j]]$disableSameAdvertiserCompetitiveExclusion} else { disableSameAdvertiserCompetitiveExclusion <- NA}
        if( 'lastModifiedByApp' %in% names(line_item_detail_notDel$rval[[j]])) {lastModifiedByApp <- line_item_detail_notDel$rval[[j]]$lastModifiedByApp} else { lastModifiedByApp <- NA}
        if( 'lastModifiedDateTime' %in% names(line_item_detail_notDel$rval[[j]])) {
          lastModifiedYear <- line_item_detail_notDel$rval[[j]]$lastModifiedDateTime$date$year
          lastModifiedMonth <- line_item_detail_notDel$rval[[j]]$lastModifiedDateTime$date$month
          lastModifiedDay <- line_item_detail_notDel$rval[[j]]$lastModifiedDateTime$date$day
        } else { 
          lastModifiedYear <- NA
          lastModifiedMonth <- NA
          lastModifiedDay <- NA
        }
        if( 'creationDateTime' %in% names(line_item_detail_notDel$rval[[j]])) {
          creationYear <- line_item_detail_notDel$rval[[j]]$creationDateTime$date$year
          creationMonth <- line_item_detail_notDel$rval[[j]]$creationDateTime$date$month
          creationDay <- line_item_detail_notDel$rval[[j]]$creationDateTime$date$day
        } else { 
          creationYear <- NA
          creationMonth <- NA
          creationDay <- NA
        }
        if( 'isPrioritizedPreferredDealsEnabled' %in% names(line_item_detail_notDel$rval[[j]])) {isPrioritizedPreferredDealsEnabled <- line_item_detail_notDel$rval[[j]]$isPrioritizedPreferredDealsEnabled} else { isPrioritizedPreferredDealsEnabled <- NA}
        if( 'adExchangeAuctionOpeningPriority' %in% names(line_item_detail_notDel$rval[[j]])) {adExchangeAuctionOpeningPriority <- line_item_detail_notDel$rval[[j]]$adExchangeAuctionOpeningPriority} else { adExchangeAuctionOpeningPriority <- NA}
        if( 'isSetTopBoxEnabled' %in% names(line_item_detail_notDel$rval[[j]])) {isSetTopBoxEnabled <- line_item_detail_notDel$rval[[j]]$isSetTopBoxEnabled} else { isSetTopBoxEnabled <- NA}
        if( 'isMissingCreatives' %in% names(line_item_detail_notDel$rval[[j]])) {isMissingCreatives <- line_item_detail_notDel$rval[[j]]$isMissingCreatives} else { isMissingCreatives <- NA}
        if( 'primaryGoal' %in% names(line_item_detail_notDel$rval[[j]])) {
          primaryGoalType <- line_item_detail_notDel$rval[[j]]$primaryGoal$goalType
          primaryGoalUnitType <- line_item_detail_notDel$rval[[j]]$primaryGoal$unitType
        } else { 
          primaryGoalType <- NA
          primaryGoalUnitType <- NA
        }
        
        temp <- c(
          orderId = orderId,
          id = id,
          name = name,
          orderName = orderName,
          startYear = as.numeric(startYear),
          startMonth = as.numeric(startMonth),
          startDay = as.numeric(startDay),
          endYear = as.numeric(endYear),
          endMonth = as.numeric(endMonth),
          endDay = as.numeric(endDay),
          autoExtensionDays = autoExtensionDays,
          unlimitedEndDateTime = unlimitedEndDateTime,
          creativeRotationType = creativeRotationType,
          deliveryRateType = deliveryRateType,
          roadblockingType = roadblockingType,
          lineItemType = lineItemType,
          priority = as.numeric(priority),
          costPerUnitCurrencyCode = as.numeric(costPerUnitCurrencyCode),
          costPerUnitmicoAmount = costPerUnitmicoAmount,
          costType = costType,
          discountType = discountType,
          discount = as.numeric(discount),
          contractedUnitsBought = as.numeric(contractedUnitsBought),
          creativeWidth = as.numeric(creativeWidth),
          creativeHeight = as.numeric(creativeHeight),
          targetPlatform = targetPlatform,
          environmentType = environmentType,
          companionDeliveryOption = companionDeliveryOption,
          creativePersistenceType = creativePersistenceType,
          allowOverbook = allowOverbook,
          skipInventoryCheck = skipInventoryCheck,
          skipCrossSellingRuleWarningChecks = skipCrossSellingRuleWarningChecks,
          reserveAtCreation = reserveAtCreation,
          impressionsDelivered = as.numeric(impressionsDelivered),
          clicksDelivered = as.numeric(clicksDelivered),
          videoStartsDelivered = as.numeric(videoStartsDelivered),
          deliveryExpectedPCT = as.numeric(deliveryExpectedPCT),
          deliveryActualPCT = as.numeric(deliveryActualPCT),
          status = status,
          reservationStatus = reservationStatus,
          isArchived = isArchived,
          webPropertyCode = webPropertyCode,
          disableSameAdvertiserCompetitiveExclusion = disableSameAdvertiserCompetitiveExclusion,
          lastModifiedByApp = lastModifiedByApp,
          lastModifiedYear = as.numeric(lastModifiedYear),
          lastModifiedMonth = as.numeric(lastModifiedMonth),
          lastModifiedDay = as.numeric(lastModifiedDay),
          creationYear = as.numeric(creationYear),
          creationMonth = as.numeric(creationMonth),
          creationDay = as.numeric(creationDay),
          isPrioritizedPreferredDealsEnabled = isPrioritizedPreferredDealsEnabled,
          adExchangeAuctionOpeningPriority = as.numeric(adExchangeAuctionOpeningPriority),
          isSetTopBoxEnabled = isSetTopBoxEnabled,
          isMissingCreatives = isMissingCreatives,
          primaryGoalType = primaryGoalType,
          primaryGoalUnitType = primaryGoalUnitType
        )
        
        allNotDelivering <- rbind(allNotDelivering,temp)
        row.names(allNotDelivering)[j-2] <- j + i
        
      }
      
      allNotDelivering <- as.data.frame(allNotDelivering)
      allData <- rbind(allDelivering,allNotDelivering)
      allData$startYear  <- as.integer(allData$startYear )
      allData$startMonth  <- as.integer(allData$startMonth )
      allData$startDay  <- as.integer(allData$startDay )
      allData$endYear  <- as.integer(allData$endYear )
      allData$endMonth  <- as.integer(allData$endMonth )
      allData$endDay  <- as.integer(allData$endDay )
      allData$priority  <- as.integer(allData$priority )
      allData$costPerUnitCurrencyCode  <- as.numeric(allData$costPerUnitCurrencyCode )
      allData$discount  <- as.numeric(allData$discount )
      allData$contractedUnitsBought  <- as.numeric(allData$contractedUnitsBought )
      allData$creativeWidth  <- as.numeric(allData$creativeWidth )
      allData$creativeHeight  <- as.numeric(allData$creativeHeight )
      allData$impressionsDelivered  <- as.numeric(allData$impressionsDelivered )
      allData$clicksDelivered  <- as.numeric(allData$clicksDelivered )
      allData$videoStartsDelivered  <- as.numeric(allData$videoStartsDelivered )
      allData$deliveryExpectedPCT  <- as.numeric(allData$deliveryExpectedPCT )
      allData$deliveryActualPCT  <- as.numeric(allData$deliveryActualPCT )
      allData$lastModifiedYear  <- as.integer(allData$lastModifiedYear )
      allData$lastModifiedMonth  <- as.integer(allData$lastModifiedMonth )
      allData$lastModifiedDay  <- as.integer(allData$lastModifiedDay )
      allData$creationYear  <- as.integer(allData$creationYear )
      allData$creationMonth  <- as.integer(allData$creationMonth )
      allData$creationDay  <- as.integer(allData$creationDay )
      allData$adExchangeAuctionOpeningPriority  <- as.numeric(allData$adExchangeAuctionOpeningPriority )
      allData$startYMD <- as.integer(paste0(allData$startYear,sprintf("%02d",allData$startMonth),sprintf("%02d",allData$startDay)))
      allData$endYMD <- as.integer(paste0(allData$endYear,sprintf("%02d",allData$endMonth),sprintf("%02d",allData$endDay)))
      allData$lastModifiedYMD <- as.integer(paste0(allData$creationYear,sprintf("%02d",allData$creationMonth),sprintf("%02d",allData$creationDay)))
      allData$creationYMD <- as.integer(paste0(allData$creationYear,sprintf("%02d",allData$creationMonth),sprintf("%02d",allData$creationDay)))
      
      return(allData)
      
      options(warn=0)
}