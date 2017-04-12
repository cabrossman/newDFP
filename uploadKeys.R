rm(list = ls())
#get needed libraries
source(paste0(getwd(),'/lib/AuthFunctions.R'))

#download keys from the API
print('downloading keys...')
keyList <- DFP_getKeys()
#connect to redshift
print('connecting to Red shift...')
conn <- redShiftAuth(path)
#upload new data
print('uploading geoids')
dbWriteTable(conn, "geoids_ids", keyList$geoids_ids, overwrite=TRUE)

print('uploading ad units')
dbWriteTable(conn, "ad_unit_ids", keyList$ad_unit_ids, overwrite=TRUE)

print('uploading keyVals')
dbSendUpdate(conn, 'drop table keyVal_ids')
dbWriteTable(conn, "keyVal_ids", keyList$keyVal_ids, overwrite=TRUE)