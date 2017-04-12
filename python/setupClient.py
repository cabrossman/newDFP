path = 'C:/Users/christopher.brossman/Documents/Projects/work/newDFP/python/'
keyName = 'DFP service Account-ec3f20e59362.p12'
key_file_path = path + keyName
passwd = 'notasecret'
serviceAccountId = 'newdfp@dfp-service-account.iam.gserviceaccount.com'

application_name = 'brossmanPython'


from googleads import dfp
from googleads import oauth2


# Initialize the GoogleRefreshTokenClient using the credentials you received
# in the earlier steps.
oauth2_client = oauth2.GoogleServiceAccountClient(oauth2.GetAPIScope('dfp'), serviceAccountId, key_file_path)

# Initialize the DFP client.
dfp_client = dfp.DfpClient(
   oauth2_client, application_name)