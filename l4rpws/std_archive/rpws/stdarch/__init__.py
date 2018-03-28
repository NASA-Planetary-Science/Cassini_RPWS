
# These are reasonable default if you want to run out of the source directory,

#Locaion of PDS super volume
g_sSuperVol = '%s/pds'%os.getenv('RPWS_SUPERVOL')  

#Location of PDS volume
g_sTemplateVol = '%s/database/stdarch'%os.getenv('RPWS_DATA') 

from archutil import *
from cas_rewrite import *
import seq 
import trig



