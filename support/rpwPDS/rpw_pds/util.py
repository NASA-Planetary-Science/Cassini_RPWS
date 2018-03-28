"""Utilities for iowa PDS archiving"""

import sys
import os
import stat
import os.path
from os.path import join as pjoin
from copy import deepcopy
import textwrap
import fnmatch
import optparse

import logging
import time
import glob #mkVerFileName uses this
#import md5  #md5DirTree uses this
import hashlib

##############################################################################
# Cheap override of opt parse to add a "see the manual page" line where it
# will actually get read.
class ManRefParser(optparse.OptionParser):
	
	def __init__(self, usage=None, option_list=None, option_class=optparse.Option,
	             version=None, conflict_handler="error", description=None,
		          formatter=None, add_help_option=True, prog=None, man_ref=None):
	
		self.man_ref = man_ref
					  
		optparse.OptionParser.__init__(self, usage, option_list, option_class,
		                      version, conflict_handler, description,
		                      formatter, add_help_option, prog)
	
	def format_description(self, formatter):
		return self.get_description();
	
	
	def print_help(self, file=None):
		optparse.OptionParser.print_help(self, file)
		if file==None:
			file = sys.stderr
		
		if self.man_ref != None:
			file.write("\nSee the manual page %s for more information.\n\n"%self.man_ref)

##############################################################################

def listMerge(l1, l2):
	"""Given two lists, produce an output list with only the unique values"""
	
	lOut = []
	if type(l1) == type(""):
		lOut.append(l1)
	else:
		for item in l1:
			if item not in lOut:
				lOut.append(item)
	
	if type(l2) == type(""):
		if l2 not in l1:
			lOut.append(l2)
	else:
		for item in l2:
			if item not in lOut:
				lOut.append(item)

	return lOut
	
	
##############################################################################
def isLeapYear(nYear):
	"""
	Returns True if given year is leap year, returns False if not,
	Throws if Year is equal to or less than 0
	"""
	
	if (nYear % 4) != 0:
		return False
	elif (nYear % 400) == 0:
		return True
	elif (nYear % 100) == 0:
		return False
	else:
		return True

##############################################################################
# Algorithmically is probably faster for a single lookup, but a table is
# faster for many lookups.  -cwp

#NON LEAP YEAR DOY <--> DOM conversions
g_dConv = {
   1:( 1, 1),   2:( 1, 2),   3:( 1, 3),   4:( 1, 4),   5:( 1, 5),   6:( 1, 6),   7:( 1, 7),   8:( 1, 8),   9:( 1, 9),  10:( 1,10),  11:( 1,11),  12:( 1,12),  13:( 1,13),  14:( 1,14),  15:( 1,15),  16:( 1,16),  17:( 1,17),  18:( 1,18),  19:( 1,19),  20:( 1,20),  21:( 1,21),  22:( 1,22),  23:( 1,23),  24:( 1,24),  25:( 1,25),  26:( 1,26),  27:( 1,27),  28:( 1,28),  29:( 1,29),  30:( 1,30),  31:( 1,31),
  32:( 2, 1),  33:( 2, 2),  34:( 2, 3),  35:( 2, 4),  36:( 2, 5),  37:( 2, 6),  38:( 2, 7),  39:( 2, 8),  40:( 2, 9),  41:( 2,10),  42:( 2,11),  43:( 2,12),  44:( 2,13),  45:( 2,14),  46:( 2,15),  47:( 2,16),  48:( 2,17),  49:( 2,18),  50:( 2,19),  51:( 2,20),  52:( 2,21),  53:( 2,22),  54:( 2,23),  55:( 2,24),  56:( 2,25),  57:( 2,26),  58:( 2,27),  59:( 2,28),
  60:( 3, 1),  61:( 3, 2),  62:( 3, 3),  63:( 3, 4),  64:( 3, 5),  65:( 3, 6),  66:( 3, 7),  67:( 3, 8),  68:( 3, 9),  69:( 3,10),  70:( 3,11),  71:( 3,12),  72:( 3,13),  73:( 3,14),  74:( 3,15),  75:( 3,16),  76:( 3,17),  77:( 3,18),  78:( 3,19),  79:( 3,20),  80:( 3,21),  81:( 3,22),  82:( 3,23),  83:( 3,24),  84:( 3,25),  85:( 3,26),  86:( 3,27),  87:( 3,28),  88:( 3,29),  89:( 3,30),  90:( 3,31),
  91:( 4, 1),  92:( 4, 2),  93:( 4, 3),  94:( 4, 4),  95:( 4, 5),  96:( 4, 6),  97:( 4, 7),  98:( 4, 8),  99:( 4, 9), 100:( 4,10), 101:( 4,11), 102:( 4,12), 103:( 4,13), 104:( 4,14), 105:( 4,15), 106:( 4,16), 107:( 4,17), 108:( 4,18), 109:( 4,19), 110:( 4,20), 111:( 4,21), 112:( 4,22), 113:( 4,23), 114:( 4,24), 115:( 4,25), 116:( 4,26), 117:( 4,27), 118:( 4,28), 119:( 4,29), 120:( 4,30),
 121:( 5, 1), 122:( 5, 2), 123:( 5, 3), 124:( 5, 4), 125:( 5, 5), 126:( 5, 6), 127:( 5, 7), 128:( 5, 8), 129:( 5, 9), 130:( 5,10), 131:( 5,11), 132:( 5,12), 133:( 5,13), 134:( 5,14), 135:( 5,15), 136:( 5,16), 137:( 5,17), 138:( 5,18), 139:( 5,19), 140:( 5,20), 141:( 5,21), 142:( 5,22), 143:( 5,23), 144:( 5,24), 145:( 5,25), 146:( 5,26), 147:( 5,27), 148:( 5,28), 149:( 5,29), 150:( 5,30), 151:( 5,31),
 152:( 6, 1), 153:( 6, 2), 154:( 6, 3), 155:( 6, 4), 156:( 6, 5), 157:( 6, 6), 158:( 6, 7), 159:( 6, 8), 160:( 6, 9), 161:( 6,10), 162:( 6,11), 163:( 6,12), 164:( 6,13), 165:( 6,14), 166:( 6,15), 167:( 6,16), 168:( 6,17), 169:( 6,18), 170:( 6,19), 171:( 6,20), 172:( 6,21), 173:( 6,22), 174:( 6,23), 175:( 6,24), 176:( 6,25), 177:( 6,26), 178:( 6,27), 179:( 6,28), 180:( 6,29), 181:( 6,30),
 182:( 7, 1), 183:( 7, 2), 184:( 7, 3), 185:( 7, 4), 186:( 7, 5), 187:( 7, 6), 188:( 7, 7), 189:( 7, 8), 190:( 7, 9), 191:( 7,10), 192:( 7,11), 193:( 7,12), 194:( 7,13), 195:( 7,14), 196:( 7,15), 197:( 7,16), 198:( 7,17), 199:( 7,18), 200:( 7,19), 201:( 7,20), 202:( 7,21), 203:( 7,22), 204:( 7,23), 205:( 7,24), 206:( 7,25), 207:( 7,26), 208:( 7,27), 209:( 7,28), 210:( 7,29), 211:( 7,30), 212:( 7,31),
 213:( 8, 1), 214:( 8, 2), 215:( 8, 3), 216:( 8, 4), 217:( 8, 5), 218:( 8, 6), 219:( 8, 7), 220:( 8, 8), 221:( 8, 9), 222:( 8,10), 223:( 8,11), 224:( 8,12), 225:( 8,13), 226:( 8,14), 227:( 8,15), 228:( 8,16), 229:( 8,17), 230:( 8,18), 231:( 8,19), 232:( 8,20), 233:( 8,21), 234:( 8,22), 235:( 8,23), 236:( 8,24), 237:( 8,25), 238:( 8,26), 239:( 8,27), 240:( 8,28), 241:( 8,29), 242:( 8,30), 243:( 8,31),
 244:( 9, 1), 245:( 9, 2), 246:( 9, 3), 247:( 9, 4), 248:( 9, 5), 249:( 9, 6), 250:( 9, 7), 251:( 9, 8), 252:( 9, 9), 253:( 9,10), 254:( 9,11), 255:( 9,12), 256:( 9,13), 257:( 9,14), 258:( 9,15), 259:( 9,16), 260:( 9,17), 261:( 9,18), 262:( 9,19), 263:( 9,20), 264:( 9,21), 265:( 9,22), 266:( 9,23), 267:( 9,24), 268:( 9,25), 269:( 9,26), 270:( 9,27), 271:( 9,28), 272:( 9,29), 273:( 9,30),
 274:(10, 1), 275:(10, 2), 276:(10, 3), 277:(10, 4), 278:(10, 5), 279:(10, 6), 280:(10, 7), 281:(10, 8), 282:(10, 9), 283:(10,10), 284:(10,11), 285:(10,12), 286:(10,13), 287:(10,14), 288:(10,15), 289:(10,16), 290:(10,17), 291:(10,18), 292:(10,19), 293:(10,20), 294:(10,21), 295:(10,22), 296:(10,23), 297:(10,24), 298:(10,25), 299:(10,26), 300:(10,27), 301:(10,28), 302:(10,29), 303:(10,30), 304:(10,31),
 305:(11, 1), 306:(11, 2), 307:(11, 3), 308:(11, 4), 309:(11, 5), 310:(11, 6), 311:(11, 7), 312:(11, 8), 313:(11, 9), 314:(11,10), 315:(11,11), 316:(11,12), 317:(11,13), 318:(11,14), 319:(11,15), 320:(11,16), 321:(11,17), 322:(11,18), 323:(11,19), 324:(11,20), 325:(11,21), 326:(11,22), 327:(11,23), 328:(11,24), 329:(11,25), 330:(11,26), 331:(11,27), 332:(11,28), 333:(11,29), 334:(11,30),
 335:(12, 1), 336:(12, 2), 337:(12, 3), 338:(12, 4), 339:(12, 5), 340:(12, 6), 341:(12, 7), 342:(12, 8), 343:(12, 9), 344:(12,10), 345:(12,11), 346:(12,12), 347:(12,13), 348:(12,14), 349:(12,15), 350:(12,16), 351:(12,17), 352:(12,18), 353:(12,19), 354:(12,20), 355:(12,21), 356:(12,22), 357:(12,23), 358:(12,24), 359:(12,25), 360:(12,26), 361:(12,27), 362:(12,28), 363:(12,29), 364:(12,30), 365:(12,31),
 
 ( 1, 1):  1, ( 1, 2):  2, ( 1, 3):  3, ( 1, 4):  4, ( 1, 5):  5, ( 1, 6):  6, ( 1, 7):  7, ( 1, 8):  8, ( 1, 9):  9, ( 1,10): 10, ( 1,11): 11, ( 1,12): 12, ( 1,13): 13, ( 1,14): 14, ( 1,15): 15, ( 1,16): 16, ( 1,17): 17, ( 1,18): 18, ( 1,19): 19, ( 1,20): 20, ( 1,21): 21, ( 1,22): 22, ( 1,23): 23, ( 1,24): 24, ( 1,25): 25, ( 1,26): 26, ( 1,27): 27, ( 1,28): 28, ( 1,29): 29, ( 1,30): 30, ( 1,31): 31,
 ( 2, 1): 32, ( 2, 2): 33, ( 2, 3): 34, ( 2, 4): 35, ( 2, 5): 36, ( 2, 6): 37, ( 2, 7): 38, ( 2, 8): 39, ( 2, 9): 40, ( 2,10): 41, ( 2,11): 42, ( 2,12): 43, ( 2,13): 44, ( 2,14): 45, ( 2,15): 46, ( 2,16): 47, ( 2,17): 48, ( 2,18): 49, ( 2,19): 50, ( 2,20): 51, ( 2,21): 52, ( 2,22): 53, ( 2,23): 54, ( 2,24): 55, ( 2,25): 56, ( 2,26): 57, ( 2,27): 58, ( 2,28): 59,
 ( 3, 1): 60, ( 3, 2): 61, ( 3, 3): 62, ( 3, 4): 63, ( 3, 5): 64, ( 3, 6): 65, ( 3, 7): 66, ( 3, 8): 67, ( 3, 9): 68, ( 3,10): 69, ( 3,11): 70, ( 3,12): 71, ( 3,13): 72, ( 3,14): 73, ( 3,15): 74, ( 3,16): 75, ( 3,17): 76, ( 3,18): 77, ( 3,19): 78, ( 3,20): 79, ( 3,21): 80, ( 3,22): 81, ( 3,23): 82, ( 3,24): 83, ( 3,25): 84, ( 3,26): 85, ( 3,27): 86, ( 3,28): 87, ( 3,29): 88, ( 3,30): 89, ( 3,31): 90,
 ( 4, 1): 91, ( 4, 2): 92, ( 4, 3): 93, ( 4, 4): 94, ( 4, 5): 95, ( 4, 6): 96, ( 4, 7): 97, ( 4, 8): 98, ( 4, 9): 99, ( 4,10):100, ( 4,11):101, ( 4,12):102, ( 4,13):103, ( 4,14):104, ( 4,15):105, ( 4,16):106, ( 4,17):107, ( 4,18):108, ( 4,19):109, ( 4,20):110, ( 4,21):111, ( 4,22):112, ( 4,23):113, ( 4,24):114, ( 4,25):115, ( 4,26):116, ( 4,27):117, ( 4,28):118, ( 4,29):119, ( 4,30):120,
 ( 5, 1):121, ( 5, 2):122, ( 5, 3):123, ( 5, 4):124, ( 5, 5):125, ( 5, 6):126, ( 5, 7):127, ( 5, 8):128, ( 5, 9):129, ( 5,10):130, ( 5,11):131, ( 5,12):132, ( 5,13):133, ( 5,14):134, ( 5,15):135, ( 5,16):136, ( 5,17):137, ( 5,18):138, ( 5,19):139, ( 5,20):140, ( 5,21):141, ( 5,22):142, ( 5,23):143, ( 5,24):144, ( 5,25):145, ( 5,26):146, ( 5,27):147, ( 5,28):148, ( 5,29):149, ( 5,30):150, ( 5,31):151,
 ( 6, 1):152, ( 6, 2):153, ( 6, 3):154, ( 6, 4):155, ( 6, 5):156, ( 6, 6):157, ( 6, 7):158, ( 6, 8):159, ( 6, 9):160, ( 6,10):161, ( 6,11):162, ( 6,12):163, ( 6,13):164, ( 6,14):165, ( 6,15):166, ( 6,16):167, ( 6,17):168, ( 6,18):169, ( 6,19):170, ( 6,20):171, ( 6,21):172, ( 6,22):173, ( 6,23):174, ( 6,24):175, ( 6,25):176, ( 6,26):177, ( 6,27):178, ( 6,28):179, ( 6,29):180, ( 6,30):181,
 ( 7, 1):182, ( 7, 2):183, ( 7, 3):184, ( 7, 4):185, ( 7, 5):186, ( 7, 6):187, ( 7, 7):188, ( 7, 8):189, ( 7, 9):190, ( 7,10):191, ( 7,11):192, ( 7,12):193, ( 7,13):194, ( 7,14):195, ( 7,15):196, ( 7,16):197, ( 7,17):198, ( 7,18):199, ( 7,19):200, ( 7,20):201, ( 7,21):202, ( 7,22):203, ( 7,23):204, ( 7,24):205, ( 7,25):206, ( 7,26):207, ( 7,27):208, ( 7,28):209, ( 7,29):210, ( 7,30):211, ( 7,31):212,
 ( 8, 1):213, ( 8, 2):214, ( 8, 3):215, ( 8, 4):216, ( 8, 5):217, ( 8, 6):218, ( 8, 7):219, ( 8, 8):220, ( 8, 9):221, ( 8,10):222, ( 8,11):223, ( 8,12):224, ( 8,13):225, ( 8,14):226, ( 8,15):227, ( 8,16):228, ( 8,17):229, ( 8,18):230, ( 8,19):231, ( 8,20):232, ( 8,21):233, ( 8,22):234, ( 8,23):235, ( 8,24):236, ( 8,25):237, ( 8,26):238, ( 8,27):239, ( 8,28):240, ( 8,29):241, ( 8,30):242, ( 8,31):243,
 ( 9, 1):244, ( 9, 2):245, ( 9, 3):246, ( 9, 4):247, ( 9, 5):248, ( 9, 6):249, ( 9, 7):250, ( 9, 8):251, ( 9, 9):252, ( 9,10):253, ( 9,11):254, ( 9,12):255, ( 9,13):256, ( 9,14):257, ( 9,15):258, ( 9,16):259, ( 9,17):260, ( 9,18):261, ( 9,19):262, ( 9,20):263, ( 9,21):264, ( 9,22):265, ( 9,23):266, ( 9,24):267, ( 9,25):268, ( 9,26):269, ( 9,27):270, ( 9,28):271, ( 9,29):272, ( 9,30):273,
 (10, 1):274, (10, 2):275, (10, 3):276, (10, 4):277, (10, 5):278, (10, 6):279, (10, 7):280, (10, 8):281, (10, 9):282, (10,10):283, (10,11):284, (10,12):285, (10,13):286, (10,14):287, (10,15):288, (10,16):289, (10,17):290, (10,18):291, (10,19):292, (10,20):293, (10,21):294, (10,22):295, (10,23):296, (10,24):297, (10,25):298, (10,26):299, (10,27):300, (10,28):301, (10,29):302, (10,30):303, (10,31):304,
 (11, 1):305, (11, 2):306, (11, 3):307, (11, 4):308, (11, 5):309, (11, 6):310, (11, 7):311, (11, 8):312, (11, 9):313, (11,10):314, (11,11):315, (11,12):316, (11,13):317, (11,14):318, (11,15):319, (11,16):320, (11,17):321, (11,18):322, (11,19):323, (11,20):324, (11,21):325, (11,22):326, (11,23):327, (11,24):328, (11,25):329, (11,26):330, (11,27):331, (11,28):332, (11,29):333, (11,30):334,
 (12, 1):335, (12, 2):336, (12, 3):337, (12, 4):338, (12, 5):339, (12, 6):340, (12, 7):341, (12, 8):342, (12, 9):343, (12,10):344, (12,11):345, (12,12):346, (12,13):347, (12,14):348, (12,15):349, (12,16):350, (12,17):351, (12,18):352, (12,19):353, (12,20):354, (12,21):355, (12,22):356, (12,23):357, (12,24):358, (12,25):359, (12,26):360, (12,27):361, (12,28):362, (12,29):363, (12,30):364, (12,31):365
}

#LEAP YEAR DOY <--> DOM conversions
g_dLeapConv = {
   1:( 1, 1),   2:( 1, 2),   3:( 1, 3),   4:( 1, 4),   5:( 1, 5),   6:( 1, 6),   7:( 1, 7),   8:( 1, 8),   9:( 1, 9),  10:( 1,10),  11:( 1,11),  12:( 1,12),  13:( 1,13),  14:( 1,14),  15:( 1,15),  16:( 1,16),  17:( 1,17),  18:( 1,18),  19:( 1,19),  20:( 1,20),  21:( 1,21),  22:( 1,22),  23:( 1,23),  24:( 1,24),  25:( 1,25),  26:( 1,26),  27:( 1,27),  28:( 1,28),  29:( 1,29),  30:( 1,30),  31:( 1,31),
  32:( 2, 1),  33:( 2, 2),  34:( 2, 3),  35:( 2, 4),  36:( 2, 5),  37:( 2, 6),  38:( 2, 7),  39:( 2, 8),  40:( 2, 9),  41:( 2,10),  42:( 2,11),  43:( 2,12),  44:( 2,13),  45:( 2,14),  46:( 2,15),  47:( 2,16),  48:( 2,17),  49:( 2,18),  50:( 2,19),  51:( 2,20),  52:( 2,21),  53:( 2,22),  54:( 2,23),  55:( 2,24),  56:( 2,25),  57:( 2,26),  58:( 2,27),  59:( 2,28),  60:( 2,29),
  61:( 3, 1),  62:( 3, 2),  63:( 3, 3),  64:( 3, 4),  65:( 3, 5),  66:( 3, 6),  67:( 3, 7),  68:( 3, 8),  69:( 3, 9),  70:( 3,10),  71:( 3,11),  72:( 3,12),  73:( 3,13),  74:( 3,14),  75:( 3,15),  76:( 3,16),  77:( 3,17),  78:( 3,18),  79:( 3,19),  80:( 3,20),  81:( 3,21),  82:( 3,22),  83:( 3,23),  84:( 3,24),  85:( 3,25),  86:( 3,26),  87:( 3,27),  88:( 3,28),  89:( 3,29),  90:( 3,30),  91:( 3,31),
  92:( 4, 1),  93:( 4, 2),  94:( 4, 3),  95:( 4, 4),  96:( 4, 5),  97:( 4, 6),  98:( 4, 7),  99:( 4, 8), 100:( 4, 9), 101:( 4,10), 102:( 4,11), 103:( 4,12), 104:( 4,13), 105:( 4,14), 106:( 4,15), 107:( 4,16), 108:( 4,17), 109:( 4,18), 110:( 4,19), 111:( 4,20), 112:( 4,21), 113:( 4,22), 114:( 4,23), 115:( 4,24), 116:( 4,25), 117:( 4,26), 118:( 4,27), 119:( 4,28), 120:( 4,29), 121:( 4,30),
 122:( 5, 1), 123:( 5, 2), 124:( 5, 3), 125:( 5, 4), 126:( 5, 5), 127:( 5, 6), 128:( 5, 7), 129:( 5, 8), 130:( 5, 9), 131:( 5,10), 132:( 5,11), 133:( 5,12), 134:( 5,13), 135:( 5,14), 136:( 5,15), 137:( 5,16), 138:( 5,17), 139:( 5,18), 140:( 5,19), 141:( 5,20), 142:( 5,21), 143:( 5,22), 144:( 5,23), 145:( 5,24), 146:( 5,25), 147:( 5,26), 148:( 5,27), 149:( 5,28), 150:( 5,29), 151:( 5,30), 152:( 5,31),
 153:( 6, 1), 154:( 6, 2), 155:( 6, 3), 156:( 6, 4), 157:( 6, 5), 158:( 6, 6), 159:( 6, 7), 160:( 6, 8), 161:( 6, 9), 162:( 6,10), 163:( 6,11), 164:( 6,12), 165:( 6,13), 166:( 6,14), 167:( 6,15), 168:( 6,16), 169:( 6,17), 170:( 6,18), 171:( 6,19), 172:( 6,20), 173:( 6,21), 174:( 6,22), 175:( 6,23), 176:( 6,24), 177:( 6,25), 178:( 6,26), 179:( 6,27), 180:( 6,28), 181:( 6,29), 182:( 6,30),
 183:( 7, 1), 184:( 7, 2), 185:( 7, 3), 186:( 7, 4), 187:( 7, 5), 188:( 7, 6), 189:( 7, 7), 190:( 7, 8), 191:( 7, 9), 192:( 7,10), 193:( 7,11), 194:( 7,12), 195:( 7,13), 196:( 7,14), 197:( 7,15), 198:( 7,16), 199:( 7,17), 200:( 7,18), 201:( 7,19), 202:( 7,20), 203:( 7,21), 204:( 7,22), 205:( 7,23), 206:( 7,24), 207:( 7,25), 208:( 7,26), 209:( 7,27), 210:( 7,28), 211:( 7,29), 212:( 7,30), 213:( 7,31),
 214:( 8, 1), 215:( 8, 2), 216:( 8, 3), 217:( 8, 4), 218:( 8, 5), 219:( 8, 6), 220:( 8, 7), 221:( 8, 8), 222:( 8, 9), 223:( 8,10), 224:( 8,11), 225:( 8,12), 226:( 8,13), 227:( 8,14), 228:( 8,15), 229:( 8,16), 230:( 8,17), 231:( 8,18), 232:( 8,19), 233:( 8,20), 234:( 8,21), 235:( 8,22), 236:( 8,23), 237:( 8,24), 238:( 8,25), 239:( 8,26), 240:( 8,27), 241:( 8,28), 242:( 8,29), 243:( 8,30), 244:( 8,31),
 245:( 9, 1), 246:( 9, 2), 247:( 9, 3), 248:( 9, 4), 249:( 9, 5), 250:( 9, 6), 251:( 9, 7), 252:( 9, 8), 253:( 9, 9), 254:( 9,10), 255:( 9,11), 256:( 9,12), 257:( 9,13), 258:( 9,14), 259:( 9,15), 260:( 9,16), 261:( 9,17), 262:( 9,18), 263:( 9,19), 264:( 9,20), 265:( 9,21), 266:( 9,22), 267:( 9,23), 268:( 9,24), 269:( 9,25), 270:( 9,26), 271:( 9,27), 272:( 9,28), 273:( 9,29), 274:( 9,30),
 275:(10, 1), 276:(10, 2), 277:(10, 3), 278:(10, 4), 279:(10, 5), 280:(10, 6), 281:(10, 7), 282:(10, 8), 283:(10, 9), 284:(10,10), 285:(10,11), 286:(10,12), 287:(10,13), 288:(10,14), 289:(10,15), 290:(10,16), 291:(10,17), 292:(10,18), 293:(10,19), 294:(10,20), 295:(10,21), 296:(10,22), 297:(10,23), 298:(10,24), 299:(10,25), 300:(10,26), 301:(10,27), 302:(10,28), 303:(10,29), 304:(10,30), 305:(10,31),
 306:(11, 1), 307:(11, 2), 308:(11, 3), 309:(11, 4), 310:(11, 5), 311:(11, 6), 312:(11, 7), 313:(11, 8), 314:(11, 9), 315:(11,10), 316:(11,11), 317:(11,12), 318:(11,13), 319:(11,14), 320:(11,15), 321:(11,16), 322:(11,17), 323:(11,18), 324:(11,19), 325:(11,20), 326:(11,21), 327:(11,22), 328:(11,23), 329:(11,24), 330:(11,25), 331:(11,26), 332:(11,27), 333:(11,28), 334:(11,29), 335:(11,30),
 336:(12, 1), 337:(12, 2), 338:(12, 3), 339:(12, 4), 340:(12, 5), 341:(12, 6), 342:(12, 7), 343:(12, 8), 344:(12, 9), 345:(12,10), 346:(12,11), 347:(12,12), 348:(12,13), 349:(12,14), 350:(12,15), 351:(12,16), 352:(12,17), 353:(12,18), 354:(12,19), 355:(12,20), 356:(12,21), 357:(12,22), 358:(12,23), 359:(12,24), 360:(12,25), 361:(12,26), 362:(12,27), 363:(12,28), 364:(12,29), 365:(12,30), 366:(12,31),
 
 ( 1, 1):  1, ( 1, 2):  2, ( 1, 3):  3, ( 1, 4):  4, ( 1, 5):  5, ( 1, 6):  6, ( 1, 7):  7, ( 1, 8):  8, ( 1, 9):  9, ( 1,10): 10, ( 1,11): 11, ( 1,12): 12, ( 1,13): 13, ( 1,14): 14, ( 1,15): 15, ( 1,16): 16, ( 1,17): 17, ( 1,18): 18, ( 1,19): 19, ( 1,20): 20, ( 1,21): 21, ( 1,22): 22, ( 1,23): 23, ( 1,24): 24, ( 1,25): 25, ( 1,26): 26, ( 1,27): 27, ( 1,28): 28, ( 1,29): 29, ( 1,30): 30, ( 1,31): 31,
 ( 2, 1): 32, ( 2, 2): 33, ( 2, 3): 34, ( 2, 4): 35, ( 2, 5): 36, ( 2, 6): 37, ( 2, 7): 38, ( 2, 8): 39, ( 2, 9): 40, ( 2,10): 41, ( 2,11): 42, ( 2,12): 43, ( 2,13): 44, ( 2,14): 45, ( 2,15): 46, ( 2,16): 47, ( 2,17): 48, ( 2,18): 49, ( 2,19): 50, ( 2,20): 51, ( 2,21): 52, ( 2,22): 53, ( 2,23): 54, ( 2,24): 55, ( 2,25): 56, ( 2,26): 57, ( 2,27): 58, ( 2,28): 59, ( 2,29): 60,
 ( 3, 1): 61, ( 3, 2): 62, ( 3, 3): 63, ( 3, 4): 64, ( 3, 5): 65, ( 3, 6): 66, ( 3, 7): 67, ( 3, 8): 68, ( 3, 9): 69, ( 3,10): 70, ( 3,11): 71, ( 3,12): 72, ( 3,13): 73, ( 3,14): 74, ( 3,15): 75, ( 3,16): 76, ( 3,17): 77, ( 3,18): 78, ( 3,19): 79, ( 3,20): 80, ( 3,21): 81, ( 3,22): 82, ( 3,23): 83, ( 3,24): 84, ( 3,25): 85, ( 3,26): 86, ( 3,27): 87, ( 3,28): 88, ( 3,29): 89, ( 3,30): 90, ( 3,31): 91,
 ( 4, 1): 92, ( 4, 2): 93, ( 4, 3): 94, ( 4, 4): 95, ( 4, 5): 96, ( 4, 6): 97, ( 4, 7): 98, ( 4, 8): 99, ( 4, 9):100, ( 4,10):101, ( 4,11):102, ( 4,12):103, ( 4,13):104, ( 4,14):105, ( 4,15):106, ( 4,16):107, ( 4,17):108, ( 4,18):109, ( 4,19):110, ( 4,20):111, ( 4,21):112, ( 4,22):113, ( 4,23):114, ( 4,24):115, ( 4,25):116, ( 4,26):117, ( 4,27):118, ( 4,28):119, ( 4,29):120, ( 4,30):121,
 ( 5, 1):122, ( 5, 2):123, ( 5, 3):124, ( 5, 4):125, ( 5, 5):126, ( 5, 6):127, ( 5, 7):128, ( 5, 8):129, ( 5, 9):130, ( 5,10):131, ( 5,11):132, ( 5,12):133, ( 5,13):134, ( 5,14):135, ( 5,15):136, ( 5,16):137, ( 5,17):138, ( 5,18):139, ( 5,19):140, ( 5,20):141, ( 5,21):142, ( 5,22):143, ( 5,23):144, ( 5,24):145, ( 5,25):146, ( 5,26):147, ( 5,27):148, ( 5,28):149, ( 5,29):150, ( 5,30):151, ( 5,31):152,
 ( 6, 1):153, ( 6, 2):154, ( 6, 3):155, ( 6, 4):156, ( 6, 5):157, ( 6, 6):158, ( 6, 7):159, ( 6, 8):160, ( 6, 9):161, ( 6,10):162, ( 6,11):163, ( 6,12):164, ( 6,13):165, ( 6,14):166, ( 6,15):167, ( 6,16):168, ( 6,17):169, ( 6,18):170, ( 6,19):171, ( 6,20):172, ( 6,21):173, ( 6,22):174, ( 6,23):175, ( 6,24):176, ( 6,25):177, ( 6,26):178, ( 6,27):179, ( 6,28):180, ( 6,29):181, ( 6,30):182,
 ( 7, 1):183, ( 7, 2):184, ( 7, 3):185, ( 7, 4):186, ( 7, 5):187, ( 7, 6):188, ( 7, 7):189, ( 7, 8):190, ( 7, 9):191, ( 7,10):192, ( 7,11):193, ( 7,12):194, ( 7,13):195, ( 7,14):196, ( 7,15):197, ( 7,16):198, ( 7,17):199, ( 7,18):200, ( 7,19):201, ( 7,20):202, ( 7,21):203, ( 7,22):204, ( 7,23):205, ( 7,24):206, ( 7,25):207, ( 7,26):208, ( 7,27):209, ( 7,28):210, ( 7,29):211, ( 7,30):212, ( 7,31):213,
 ( 8, 1):214, ( 8, 2):215, ( 8, 3):216, ( 8, 4):217, ( 8, 5):218, ( 8, 6):219, ( 8, 7):220, ( 8, 8):221, ( 8, 9):222, ( 8,10):223, ( 8,11):224, ( 8,12):225, ( 8,13):226, ( 8,14):227, ( 8,15):228, ( 8,16):229, ( 8,17):230, ( 8,18):231, ( 8,19):232, ( 8,20):233, ( 8,21):234, ( 8,22):235, ( 8,23):236, ( 8,24):237, ( 8,25):238, ( 8,26):239, ( 8,27):240, ( 8,28):241, ( 8,29):242, ( 8,30):243, ( 8,31):244,
 ( 9, 1):245, ( 9, 2):246, ( 9, 3):247, ( 9, 4):248, ( 9, 5):249, ( 9, 6):250, ( 9, 7):251, ( 9, 8):252, ( 9, 9):253, ( 9,10):254, ( 9,11):255, ( 9,12):256, ( 9,13):257, ( 9,14):258, ( 9,15):259, ( 9,16):260, ( 9,17):261, ( 9,18):262, ( 9,19):263, ( 9,20):264, ( 9,21):265, ( 9,22):266, ( 9,23):267, ( 9,24):268, ( 9,25):269, ( 9,26):270, ( 9,27):271, ( 9,28):272, ( 9,29):273, ( 9,30):274,
 (10, 1):275, (10, 2):276, (10, 3):277, (10, 4):278, (10, 5):279, (10, 6):280, (10, 7):281, (10, 8):282, (10, 9):283, (10,10):284, (10,11):285, (10,12):286, (10,13):287, (10,14):288, (10,15):289, (10,16):290, (10,17):291, (10,18):292, (10,19):293, (10,20):294, (10,21):295, (10,22):296, (10,23):297, (10,24):298, (10,25):299, (10,26):300, (10,27):301, (10,28):302, (10,29):303, (10,30):304, (10,31):305,
 (11, 1):306, (11, 2):307, (11, 3):308, (11, 4):309, (11, 5):310, (11, 6):311, (11, 7):312, (11, 8):313, (11, 9):314, (11,10):315, (11,11):316, (11,12):317, (11,13):318, (11,14):319, (11,15):320, (11,16):321, (11,17):322, (11,18):323, (11,19):324, (11,20):325, (11,21):326, (11,22):327, (11,23):328, (11,24):329, (11,25):330, (11,26):331, (11,27):332, (11,28):333, (11,29):334, (11,30):335,
 (12, 1):336, (12, 2):337, (12, 3):338, (12, 4):339, (12, 5):340, (12, 6):341, (12, 7):342, (12, 8):343, (12, 9):344, (12,10):345, (12,11):346, (12,12):347, (12,13):348, (12,14):349, (12,15):350, (12,16):351, (12,17):352, (12,18):353, (12,19):354, (12,20):355, (12,21):356, (12,22):357, (12,23):358, (12,24):359, (12,25):360, (12,26):361, (12,27):362, (12,28):363, (12,29):364, (12,30):365, (12,31):366
}

g_dDaysInMon =     {1:31, 2:28, 3:31, 4:30, 5:31, 6:30, 7:31, 8:31, 9:30, 10:31, 11:30, 12:31}
g_dDaysInMonLeap = {1:31, 2:29, 3:31, 4:30, 5:31, 6:30, 7:31, 8:31, 9:30, 10:31, 11:30, 12:31}

g_nSecInYear =     365*24*60*60
g_nSecInYearLeap = 366*24*60*60

g_nSecInDay =      24*60*60

##############################################################################
def today():
	return time.strftime('%Y-%m-%d')

##############################################################################
class DOYdate:

	def __init__(self, nYear=None, nDOY_nMonth=None, nDay=None):
		"""Initializer, insure day of year is legal for given year
		If no arguments are given assume the current day.
		"""
		
		#If this is a three argument version just call the three arg constructor
		# below.
		if nYear != None and nDOY_nMonth != None and nDay != None:
			return self._init_from_dom(nYear, nDOY_nMonth, nDay)
		
		nDOY = nDOY_nMonth
		
		#It's both empty or both complete
		if (nYear == None and nDOY != None) or (nYear != None and nDOY == None):
			raise ValueError('Year and DOY must both be supplied, or must both be None')
			
		if nYear == None:
			tTm = time.gmtime()
			self.nYear = tTm[0]
			self.nDOY = tTm[7]
			
		else:
		
			if nDOY < 1:
				raise ValueError('Day of Year starts at 1')
			if isLeapYear(nYear):
				if nDOY > 366:
					raise ValueError('Leap year max day is 366')
			else:
				if nDOY > 365:
					raise ValueError('Non leap year max day is 365')
				
			self.nYear = nYear
			self.nDOY = nDOY
	
	
	def _init_from_dom(self, nYear, nMonth, nDay):
		"""Initialize from Year month day input."""
		if nYear == None or nMonth == None or nDay == None:
			raise ValueError('Constructor requires three non-None arguments')
		
		self.nYear = nYear
		
		if nMonth < 1 or nMonth > 12:
			raise ValueError('Month is out of range (1-12): %d'%nMonth)
		
		if isLeapYear(nYear):
			if nDay < 1 or nDay > g_dDaysInMonLeap[nMonth]:
				raise ValueError('Day is out of range (1-%d):%d'%(
				                 g_dDaysInMonLeap[nMonth], nDay))
									  
			(self.nDOY) = g_dLeapConv[ (nMonth, nDay) ]
			
		else:
			if nDay < 1 or nDay > g_dDaysInMon[nMonth]:
				raise ValueError('Day is out of range (1-%d):%d'%(
				                 g_dDaysInMon[nMonth], nDay))
	
			(self.nDOY) = g_dConv[ (nMonth, nDay) ]	
		
		
	def __str__(self):
		"""Date to year string, with leading zeros.
		
		If date is 0AD to 9999AD the output is 7 characters long
		"""
		
		sDOY = ""
		sYear = ""
		
		if self.nDOY < 10:
			sDOY = "00%i"%self.nDOY
		elif self.nDOY < 100:
			sDOY = "0%i"%self.nDOY
		else:
			sDOY = "%i"%self.nDOY
		
		nTmp = abs(self.nYear)
		if nTmp > 999:
			sYear = "%i"%nTmp
		elif nTmp > 99:
			sYear = "0%i"%nTmp
		elif nTmp > 9:
			sYear = "00%i"%nTmp
		else:
			sYear = "000%i"%nTmp
		
		if self.nYear > -1:
			return "%s%s"%(sYear,sDOY)
		else:
			return "-%s%s"%(sYear,sDOY)
	
	
	def __ne__(self, other):
		"""Override of != operator so that comparison accross
		types is possible. (doesn't assume other type is DOYdate)"""
		if type(self) != type(other):
			return True
		
		if self.nYear != other.nYear:
			return True
		
		if self.nDOY != other.nDOY:
			return True
		
		return False
		
		
	def __eq__(self, other):
		"""Override of == operator so that comparison accross
		types is possible. (doesn't assume other type is DOYdate)"""
		if type(self) != type(other):
			return False
		
		if self.nYear == other.nYear and self.nDOY == other.nDOY:
			return True
		else:
			return False
	
	
	def __cmp__(self, other):
		"""Override of the comparison operators < >
		returns neg. int. if self < other, pos. int if self > other
		and 0 if self = other.  Cannot compare across types.
		"""
		
		if self.nYear != other.nYear:
			return self.nYear - other.nYear
			
		elif self.nDOY != other.nDOY:
			return self.nDOY - other.nDOY
			
		else:
			return 0
	
#	def __sub__(self, other):
#		"""Get a time range in units of days.  The + operator is not
#		defined as time differences make since, but a date is just a
#		point in time and has no particular absolute value.  If you
#		really want one you do myDate - DOYdate(0,1).  But remember
#		there is no day 0.
#		"""
#		lDays = 
		
		
	
	def incDay(self):
		"""Increment the day by one"""
		
		nMaxDay = 365
		if isLeapYear(self.nYear):
			nMaxDay = 366
		
		if (self.nDOY + 1) <= nMaxDay:
			self.nDOY += 1
		else:
			self.nYear += 1
			self.nDOY = 1
			
	
	def decDay(self):
		"""Decrement the day by one"""
		
		if(self.nDOY - 1) > 0:
			self.nDOY -= 1
		else:
			self.nYear -= 1
			
			if isLeapYear(self.nYear):
				self.nDOY = 366
			else:
				self.nDOY = 365
			
	def isLeap(self):
		return isLeapYear(self.nYear)

		
	def getDOM(self):
		"""Return current date as a day of month three tuple (YYYY,MM,DD)"""
		(nM, nDOM) =  (None, None)
		if isLeapYear(self.nYear):
			(nM, nDOM) = g_dLeapConv[self.nDOY]
		else:
			(nM, nDOM) = g_dConv[self.nDOY]
		
		return (self.nYear, nM, nDOM)


	def getDOMstr(self, sSep='-'):
		"""Return date as a day of month string. YYYY-MM-DD
		
		sSep = separator string, defaults to '-'
		"""
		(nM, nDOM) =  (None, None)
		if isLeapYear(self.nYear):
			(nM, nDOM) = g_dLeapConv[self.nDOY]
		else:
			(nM, nDOM) = g_dConv[self.nDOY]
			
		return "%04i%s%02i%s%02i"%(self.nYear, sSep, nM, sSep, nDOM)
		
		
	def getDOYstr(self, sSep='-'):
		"""Return date as a doy of year string with a seperator YYYY-DDD
		
		sSep = separator string, defaults = '-'
		"""
		return "%04i%s%03i"%(self.nYear, sSep, self.nDOY)
		
		
	def getDOMpDOYstr(self, sSep='-'):
		"""Return the date as a combined day of month and day of year str
		the format if the string is: YYYY-MM-DD (DOY)
		
		     sSep = separator stirng, defaults to '-'
		"""
		(nM, nDOM) =  (None, None)
		if isLeapYear(self.nYear):
			(nM, nDOM) = g_dLeapConv[self.nDOY]
		else:
			(nM, nDOM) = g_dConv[self.nDOY]
		
		return "%04i%s%02i%s%02i (%03i)"%(self.nYear, sSep, nM, sSep,
		                                  nDOM, self.nDOY)
		

def strToDOYdate(sDate):
	"""Given a string in YYYYDDD format return a DOYdate object.
	
	Notes: 001 is the first day of the year.  
	       The last three digits will be considered the day of year.
	       For positive years the	minimum acceptable string is 4 chars
	       For negative years the minimum acceptable string is 5 chars
	"""
	s = sDate
	if len(s) < 4:
		raise ValueError('strToDOYdate: string too short')
		
	bNeg = False
	if s[0] == '-':
		bNeg = True
		s = s[1:]
	
	if s[0] == '+':
		s = s[1:]
	
	if len(s) < 4:
		raise ValueError("strToDOYdate: string too short, '%s'."%sDate)
		
	if not s.isdigit():
		raise ValueError("strToDOYdate: all chars after the 1st must be digits, '%s'."%sDate)

	nDOY = int(s[-3:])
	nYear = int(s[:-3])
	if bNeg:
		nYear *= -1
	
	return DOYdate(nYear, nDOY)


def domStr2Date(sDate):
	"""given a string thats starts with the pattern YYYY?MM?DD where
	? is some kind of separater, return a new DOYdate object"""
	
	return DOYdate(int(sDate[:4]), int(sDate[5:7]), int(sDate[8:10]))
	
def str2date(sDate):
	"""If the number of digits is == 7 assume DOY, else assume DOM"""
	lType = [" ", " ", " ", " ", " ", " ", " ", " ", " ", " "]
	i = 0
	for c in sDate[:10]:
		if c.isdigit():
			lType[i] = 'd'
		else:
			lType[i] = '-'
		i += 1
	sType = "".join(lType)
	
 	if sType == "dddd-dd-dd":
		return DOYdate(int(sDate[:4]), int(sDate[5:7]), int(sDate[8:10]))
	elif sType[:8] == "dddd-ddd":
		return DOYdate(int(sDate[:4]), int(sDate[5:8]))
	elif sType[:7] == "ddddddd":
		return DOYdate(int(sDate[:4]), int(sDate[4:7]))
	else:
		raise ValueError("Don't understand the format for date %s"%sDate)
	
	return None

##############################################################################
def mkDateRange(doyStart, doyEnd):
	"""Given a start day and an *exclusive* end day, generate a date list"""
	lDates = []
	doyTmp = deepcopy(doyStart)
	while doyTmp < doyEnd:
		lDates.append(deepcopy(doyTmp))
		doyTmp.incDay()
		
	return lDates
	

##############################################################################
def setupLogging(sLogLevel, sFile=None, bStderr=True):
	"""Utility to setup standard python logger.
	sLogLevel - Logging level, starts with one of: C,c,E,e,W,w,I,i,D,d 
	           [critical,error,warning,info,debug]
				
	bStderror - Log to standard error, if False don't log to standard error
	
	sFile - If not empty, open the stated file for logging.
	
	Returns: None, can throw IOerror.
	"""

	sLevel = sLogLevel.lower()
	nLevel = logging.WARNING
	
	sDateFmt = '%Y-%m-%d %H:%M:%S'
	sFileFmt = '%(asctime)s %(levelname)-8s: %(message)s'
	sConFmt = '%(levelname)-8s: %(message)s'
	
	if sLevel.startswith("c"):
		nLevel = logging.CRITICAL
	elif sLevel.startswith("e"):
		nLevel = logging.ERROR
	elif sLevel.startswith("i"):
		nLevel = logging.INFO
	elif sLevel.startswith("d"):
		nLevel = logging.DEBUG
		sFileFmt = '%(asctime)s %(name)-12s %(levelname)-8s: %(message)s'
		sConFmt = '%(name)-12s %(levelname)-8s: %(message)s'
	
	#Logging options:  Console,File|File|Console|None(acually console force crit)
	rootLogger = logging.getLogger('')
	rootLogger.setLevel(nLevel)
	
	
	if sFile != None:
		fileHdlr = logging.FileHandler(sFile, 'w')
		formatter = logging.Formatter(sFileFmt, sDateFmt)
		fileHdlr.setFormatter(formatter)
		rootLogger.addHandler(fileHdlr)
	
	#For all cases except file only logging we need a console handler
	if bStderr or (not bStderr and (sFile == "")):
		conHdlr = logging.StreamHandler(sys.stderr)
		formatter = logging.Formatter(sConFmt, sDateFmt)
		conHdlr.setFormatter(formatter)
		rootLogger.addHandler(conHdlr)
	
		if not bStderr and sFile == "":
			#In the special case that we are not supposed to log at all, make a
			#console handler that never reports anything.
			logging.addLevelName(60, 'NEVER')
			rootLogger.setLevel(60)
		
	return rootLogger

##############################################################################
def setupSELogging(sLogLevel, sFile=None, bStderr=True):
	"""Utility to setup standard python logger.
	sLogLevel - Logging level, starts with one of: C,c,E,e,W,w,I,i,D,d 
	           [critical,error,warning,info,debug]
				
	bStderr - Log to standard error, if False don't log to standard error
	
	sFile - If not empty, open the stated file for logging.
	
	Returns: None, can throw IOerror.
	"""

	sLevel = sLogLevel.lower()
	nLevel = logging.WARNING
	
	sDateFmt = '%Y-%m-%d %H:%M:%S'
	sFileFmt = '%(asctime)s %(levelname)-8s: %(message)s'
	sConFmt = '%(levelname)-8s: %(message)s'
	
	if sLevel.startswith("c"):
		nLevel = logging.CRITICAL
	elif sLevel.startswith("e"):
		nLevel = logging.ERROR
	elif sLevel.startswith("i"):
		nLevel = logging.INFO
	elif sLevel.startswith("d"):
		nLevel = logging.DEBUG
#		sFileFmt = '%(asctime)s %(name)-12s %(levelname)-8s: %(message)s'
#		sConFmt = '%(asctime)s %(name)-12s %(levelname)-8s: %(message)s'
		sFileFmt = '%(asctime)s %(levelname)-8s: %(message)s'
		sConFmt = '%(asctime)s %(levelname)-8s: %(message)s'

	
	#Logging options:  Console,File|File|Console|None(acually console force crit)
	rootLogger = logging.getLogger('')
	rootLogger.setLevel(nLevel)
	
	
	if sFile != None:
		fileHdlr = logging.FileHandler(sFile, 'w')
		formatter = logging.Formatter(sFileFmt, sDateFmt)
		fileHdlr.setFormatter(formatter)
		rootLogger.addHandler(fileHdlr)
	
	#For all cases except file only logging we need a console handler
	if bStderr or (not bStderr and (sFile == "")):
		conHdlr = logging.StreamHandler(sys.stderr)
		formatter = logging.Formatter(sConFmt, sDateFmt)
		conHdlr.setFormatter(formatter)
		rootLogger.addHandler(conHdlr)
	
		if not bStderr and sFile == None:
			#In the special case that we are not supposed to log at all, make a
			#console handler that never reports anything.
			logging.addLevelName(60, 'NEVER')
			rootLogger.setLevel(60)
		
	return rootLogger

##############################################################################
def symLinkCkSize(sSrcPath, sDestPath):
	"""Symlink a file, make sure it's not zero length and return it's size
	"""
	nSize = os.path.getsize(sSrcPath)
	if nSize == 0L:
		# Assume if it's zero it's a file cause directory "files" always
		# have the . and .. entries
		raise RuntimeError("Zero length file detected: '%s'"%sSrcPath)
		
	if not os.path.isfile(sSrcPath):
		raise RuntimeError("%s: File does not exist.  (Is this a directory?)"%sSrcPath)
	
	if not os.path.isdir(os.path.dirname(sDestPath)):
		os.makedirs(os.path.dirname(sDestPath))
		
	os.symlink(sSrcPath, sDestPath)
	return nSize

##############################################################################
class recursionError(StandardError):
	"""Little exception class to denote reaching a recursion limit, didn't
	see one build into the standard library"""

	def __init__(self, nMax, sMsg=None):
		self.sMsg = sMsg
		self.nMax = nMax

	def __str__(self):
		if self.sMsg == None:
			return "Maximum recursion depth %s exceeded"%self.nMax
		else:
			return "%s Maximum recursion depth %s exceeded"%(self.sMsg, self.nMax)


##############################################################################
def getDirSizeLk(sDir, nDepth=20, _n=0):
	"""An os.path.walk style function that follows symlinks.
	
	sDir -- is the directory to walk, which may be a symlink
	
	nDepth -- to avoid circular links, the maximum directory depth that will
	     be searched, defualt 20.
			
	_n -- variable for recursive calls, not ment for outside callers.
			
	Returns -- (Size,Files) of the contained files in bytes as a long integer.
	"""
	if nDepth < 1:
		raise ValueError("In getDirSizeLk, nDepth must be at least 1")
	
	nCurDepth = _n + 1
	if nCurDepth > nDepth:
		raise recursionError("In getDirSizeLk:", nDepth)
	
	nSize = 0L
	nFiles = 0
	
	if not os.path.isdir(sDir):
		return (nSize, nFiles)
		
	for sItem in os.listdir(sDir):
		sPath = os.path.join(sDir, sItem)
		
		if os.path.isfile(sPath):
			nSize += os.path.getsize(sPath)
			nFiles += 1
			
		elif os.path.isdir(sPath):
			(nSzTmp, nFTmp) = getDirSizeLk(sPath, nDepth, nCurDepth)
			nSize += nSzTmp
			nFiles += nFTmp
		
	return (nSize, nFiles)


##############################################################################
def rmRecursive(sDir, nDepth=20, _n=0):
	"""Remove a directory tree recursively, take care to only remove links
	not what they point to."""
	
	if nDepth < 1:
		raise ValueError("In rmRecursive, nDepth must be at least 1")
	
	nCurDepth = _n + 1
	if nCurDepth > nDepth:
		raise recursionError("In rmRecursive:", nDepth)
		
	if os.path.islink(sDir):
		os.remove(sDir)
		return None
	
	if os.path.isfile(sDir):
		raise ValueError("In rmRecursive, sDir must be a directory or a link.")

	for sItem in os.listdir(sDir):
		sPath = os.path.join(sDir, sItem)
		if os.path.islink(sPath) or os.path.isfile(sPath):
			os.remove(sPath)
		elif os.path.isdir(sPath):
			rmRecursive(sPath, nDepth, nCurDepth)
	
	os.rmdir(sDir)
	
	return None

##############################################################################
def symWalk(sRoot, fileCallBack = None, dirCallBack = None, nMaxDepth=20, _n=0):
	"""Walk a directory tree, following symlinks.  For each file encountered
	call the fileCallback which looks like:
	
	   bContinue fileCallback(sAbsPathToFile)
		
	For each directory encountered 1st call the dir callback which looks like
	
	   (bContinue, sNewDirPath) dirCallBack(sAbsPathToDir)
		
	And then walk into the directory.  If either call back returns False for
	bContinue, walking is halted.  The dir callback *must* return the current
	abs name of the directory for which it was called.  Because it is okay
	of the dir callback to change the directory name, or to delete it altogether.
	
	If the dir is deleted by the dir callback then "" should be returned for
	sNewDirName.
	
	nMaxDepth defaults to 20 and is the maximum recursion limit for decending
	a directory tree.  If the directory tree is deeper than nMaxDepth then
	a util.recursionError will be thrown.
	
	Returns:  True if walking was not halted, False if a halt was sent by
	one of the callback functions.
	"""
	
	_sRoot = sRoot
	if nMaxDepth < 1:
		raise ValueError("In find, nMaxDepth must be at least 1")
	
	nCurDepth = _n + 1
	if nCurDepth > nMaxDepth:
		raise recursionError("In symWalk:", nMaxDepth)
	
	lItems = os.listdir(_sRoot)
	lItems.sort()
	
	for sItem in os.listdir(_sRoot):
		sPath = os.path.join(_sRoot, sItem)
		
		if os.path.isfile(sPath) and fileCallBack != None:
			if not fileCallBack(sPath):
				return False
		
		elif os.path.isdir(sPath):
			
			if dirCallBack != None:
				(bContinue, sPath) = dirCallBack(sPath)
				if not bContinue:
					return False
					
				if sPath == "" or sPath == None:
					continue
			
			if not symWalk(sPath, fileCallBack, dirCallBack, nMaxDepth, nCurDepth):
				return False
			
	return True

##############################################################################
# DEFS for find:
FILES     = True
NOFILES   = False
DIRS      = True
NODIRS    = False
CASESEN   = True
CASEINSEN = False

def find(sPtn, sRoot="", bFiles=True, bDirs=True, bCaseSen=False, nDepth=20, _n=0):
	"""Walk a directory tree and find stuff, *follows symlinks*, returns a
	list of abs paths that match.
	
	   sPtn - a pattern with the wild cards: * ? [seq] [!seq] see fnmatch
		       for more information.
	   
		sRoot - the root location to start checking.  If sRoot="" then 
		        os.getcwd() is the start.
		
		bFiles - check file names (default true)
		
		bDirs  - check dir names (default true)
		
		bCaseSen - do a case sensitive comparison (default false)
		
		nDepth - max directory recursion depth (default 20)
		
		_n     - internal variable, please don't mess with this.
		returns:
		
		  A list of absolute pathnames that match.
	"""
	lMatchList = []
	
	if not bFiles and not bDirs:
		raise ValueError("In find, at least one of bFiles or bDirs must be True.")
	
	_sRoot = sRoot
	if nDepth < 1:
		raise ValueError("In find, nDepth must be at least 1")
	
	nCurDepth = _n + 1
	if nCurDepth > nDepth:
		raise recursionError("In find:", nDepth)
	
	if _sRoot == "":
		_sRoot = os.getcwd()
	
	for sItem in os.listdir(_sRoot):
		sPath = pjoin(_sRoot, sItem)
		if (os.path.isfile(sPath) and bFiles) or \
		   (os.path.isdir(sPath) and bDirs):
			if bCaseSen:
				if fnmatch.fnmatchcase(sPath, sPtn):
					lMatchList.append(sPath)
			else:
				if fnmatch.fnmatch(sPath, sPtn):
					lMatchList.append(sPath)
	
		if os.path.isdir(sPath):
			lMatchList += find(sPtn, sPath, bFiles, bDirs, bCaseSen, nDepth, nCurDepth)
		
	return lMatchList

##############################################################################
def relPath(sFrom, sTo, sOutSep=os.sep, sSep=os.sep):
	"""Generate a reltive path string from file sFrom to file sTo.  sFrom is
	assumed to be a directory, sTo may be a file or directory.  Note, no
	actual files or directories need to exist on the disk at the time the function
	is called, but the strings must obey file and directory conventions.
	
		sSep = the path component separator used in sFrom and sTo
		sOutSep = the path component separator that should be used in the
		          generated output path.
	"""

	if not os.path.isabs(sFrom) or not os.path.isabs(sTo):
		raise ValueError, ('relFilePath: sFrom and sTo must be absolute paths.')
	
	lFrom = sFrom.split(sSep)
	lTo = sTo.split(sSep)[:-1]
	sOut = ""
	
	#Go up if need be
	iCommon = -1
	for i in range(len(lFrom) - 1, -1, -1):
		if len(lTo) <= i or lTo[i] != lFrom[i]:
			sOut += '..%s'%sOutSep
		else:
			iCommon = i
			break
	
	#Now go down:
	for i in range(iCommon+1, len(lTo)):
		sOut += "%s%s"%(lTo[i], sOutSep)
		
	#Last add file part
	lTmp = sTo.split(sSep)[-1:]
	sOut += lTmp[0]
	
	return sOut
	

##############################################################################	
def bytes2str(nBytes, prefer=None, bUse1024=False):
	"""Print some number of bytes with human readable suffixes
	with 1 to 4 digits of precision
	
	prefer - The preferred report unit, one of ('B','K','M','G','T')
	    for Bytes, KiloBytes, MegaBytes, GigaBytes, TeraBytes...
		 if prefer is None, then the a 1 to 3 digit representation will
		 be printed with auto selected units.
	"""
	
	n1K = 1000L
	if bUse1024:
		n1K = 1024L
	n1Kminus1 = n1K - 1L
	
	suf = ['B ','KB','MB','GB','TB']
	if nBytes < 0:
		raise ValueError("szToStr, nBytes cannot be negative")
	
	nPrnBytes = nBytes
	i = 0
	for i in range(0,len(suf) - 1):
		if prefer != None and suf[i].startswith(prefer):
			break; 
		
		if nPrnBytes > n1Kminus1:
			nPrnBytes /= n1K
			i += 1
		else:
			break
			
	return "%3d %s"%(nPrnBytes, suf[i])
		

##############################################################################
def ordnalName(nNum):
	"""Create an ordinal name for an integer, examples include:
	
	   1 - first, 17 - seventeenth, 33 - 33rd, 34567 - 34567th
	"""
	
	lName = ['', 'first','second','third','forth','fifth','sixth','seventh',
	   'eight','ninth','tenth','eleventh','twelfth','thirteenth','fourteenth',
		'fifteenth','sixteenth','seventeenth','eighteenth','nineteenth']
	
	dSuf={0:'th',1:'st',2:'nd',3:'rd',4:'th',5:'th',6:'th',7:'th',8:'th',9:'th'}
	
	
	if nNum < 1:
		raise ValueError("ordinalNames: nNum must be greater than zero.")
		
	elif nNum < 20:
		return lName[nNum]
		
	else:
		s = str(nNum)
		return "%s%s"%(s, dSuf[ int(s[-1:]) ])


##############################################################################
def makeFromTemplate(sTmpltDir, sStageDir, dDocReplace, suffix='.r'):

	""" Copy files from a directory tree to another and replace certian params.
	
	sTmpltDir = Directory root under which to find template files
	sStageDir = Directory root under which to copy files
	dDocReplace = a dictionary of parameters to replace.
	suffix = any file ending in this suffix will be copied, but with
	     the suffix removed for the output file.

	returns:  The number of bytes copied across all files
	
	Notes:  To put a replaceable paramter in a document add blocks like
	        '%(param-name)s' in the text.  Where param-name is a string
			  key for the given dictionary.
	"""	
	log = logging.getLogger('makeFromTemplate')
	
	sTemplateTop = os.path.abspath(sTmpltDir) #Make sure using abs paths
	sStageTop = os.path.abspath(sStageDir)
	
	# Search template directory for .r files, copy with param replace to
	# equivalent destination in stage directory.  Assumption:
	# Root of template vol (eqivalent to) Root of stage vol
	nSize = 0L
	for sTmptDir, lDirs, lFiles in os.walk(sTemplateTop):
		for sFile in lFiles:
			if sFile.endswith(suffix) and len(sFile) > 2:
				sOutDir = sTmptDir.replace(sTemplateTop, sStageTop, 1)
				
				if not os.path.isdir(sOutDir):
					os.makedirs(sOutDir)
				
				sPathIn = pjoin(sTmptDir, sFile)
				log.info("Subbing: '%s'."%sPathIn)
				
				fIn = file(sPathIn, 'rb')
				sFmt = fIn.read()
				fIn.close()
				
				sPathOut = pjoin(sOutDir, sFile[: -len(suffix) ])
				log.debug("  to-->: '%s'."%sPathOut)
				
				fOut = file(sPathOut, 'wb')
				fOut.write(sFmt%dDocReplace)
				fOut.close()
				
				nSize = os.path.getsize(sPathOut)
	
	return nSize

##############################################################################
#  VERSIONED path handlers.

def _getVerVector(sPath):
	# Not supposed to be called directly, returns a list of integers for the
	# versioned files found on disk, where the file name itself maps to int 0

	if os.path.exists("%s,0"%sPath):
		raise RuntimeError('%s,0 should not exist for files versioned via mkVerFileName.'%sPath)
	
	lMatch = []
	if os.path.exists(sPath):
		lMatch.append("%s,0"%sPath)  #,0 is just a placeholder never actually used.
		
	lMatch += glob.glob("%s,[1-9]"%sPath)  #"name.txt,1"
	lMatch += glob.glob("%s,[1-9][0-9]"%sPath)
	lMatch += glob.glob("%s,[1-9][0-9][0-9]"%sPath)
	
	#Cut off the file name and , part.
	lVers = []
	for sName in lMatch:
		lVers.append( int(sName.replace("%s,"%sPath, "", 1)) )
	
	lVers.sort()
	
	return lVers

def mkVerFileName(sPath):
	"""If file 'sPath' already exists, make a new file name as '[sPath],n'
	
		Here n is the smallest posible integer that would be 1 higher then all
		other files with the pattern '[sPath],n' on the directory.
	"""
	
	lVers = _getVerVector(sPath)
	if len(lVers) < 1:
		return sPath
		
	nVer = lVers[-1]
	nVer += 1
	if nVer > 999:
		sTmp = 'mkVerFileName only supports upto 1000 versions of a file, '
		sTmp += 'including the original'
		raise RuntimeError(sTmp)
		
	return "%s,%i"%(sPath, nVer)	
	

def getVerFile(sPath, bLowest=False, bThrowNone=False):
	"""Return the a version of a file where versions are tracked as '*,n'
	
		sPath -- the path without the ,n.
		
		bLowest -- Get the lowest version not the highest.
		
		bThrowNone - If true will throw if there is no version of sPath around
		
		Returns: the highest existing file version, may return None if there's
		      no version of file and bThrowNone is False.
				
		NOTE: There is no sPath,0 file sPath by itself is version 0
	"""
	iIndex = -1
	if bLowest:
		iIndex = 0
	
	lVers = _getVerVector(sPath)
	
	if len(lVers) < 1:
		if bThrowNone:
			raise RuntimeError("Couldn't find any version of %s"%sPath)
		return None
	
	if lVers[iIndex] == 0:
		return sPath
	else:
		return "%s,%i"%(sPath, lVers[iIndex])
	

def getLowestVerFile(sPath, bThrowNone=False):
	"""Wrapper around getVerFile where bLowest=True"""
	return getVerFile(sPath, True, bThrowNone)

def getHighestVerFile(sPath, bThrowNone=False):
	"""Wrapper around getVerFile where bLowest=False"""
	return getVerFile(sPath, False, bThrowNone)


##############################################################################
def pdsList(lIn, nOffset, sSetStart='{', sSetStop='}'):
	"""Given a list, create a PDS list text string with proper spacing and CR/LF
	"""
	
	# Adjust so that N/A always has quotes
	_lIn = deepcopy(lIn)
	for i in xrange(0, len(_lIn)):
		if _lIn[i] == 'N/A':
			_lIn[i] = '"N/A"'
	
	if len(_lIn) < 1:
		raise ValueError("pdsList: Input list must be at least 1 element long")
		
	if len(_lIn) < 2:		
		return str(_lIn[0])

	sFmt1st = "%s%%s,\r\n"%sSetStart
	sFmtMid = "%%%is %%s,\r\n"%nOffset
	sFmtLst = "%%%is %%s%s"%(nOffset, sSetStop)
	
	sOut = None
	for i in range(0,len(_lIn)):
		if i == 0:
			sOut = sFmt1st%_lIn[i]
		elif i == (len(lIn) - 1):
			sOut += sFmtLst%(" ", _lIn[i])
		else:
			sOut += sFmtMid%(" ", _lIn[i])
	
	return sOut	

##############################################################################
def linesInFile(sPath):
	fIn = file(sPath, 'rb')
	
	i = 0
	for sLine in fIn:
		i += 1
	
	fIn.close()
	
	return i


def getFileHash(sPath):
	hasher = hashlib.md5()
	fIn = file(sPath, 'rb')
	sIn = fIn.read(32768)
	while len(sIn) > 0:
		hasher.update(sIn)
		sIn = fIn.read(32768)
	fIn.close()

	return hasher.hexdigest()

###############################################################################
def md5DirTree(sRoot, bIncRoot=False, nDepth=20, _n=0, _sTrimRoot=None, _nFiles=0):
	"""md5hash a directory tree, will walk links unlike os.walk
	
		sRoot - Root of tree, all files at or below this location are hashed
		
		sIncRoot - Include the ROOT path in outputed dictionary keys, by default
		
		nDepth - The maximum directory depth to walk.
		
		Returns dict of strings in format: d['file/path'] = 'hash hex string'
	"""
	
	sTrimRoot = _sTrimRoot  # Basically, preserve trim path on recursive calls
	if sTrimRoot == None:
		sTrimRoot = sRoot
	
	if nDepth < 1:
		raise ValueError("In md5DirTree, nDepth must be at least 1")
	
	nCurDepth = _n + 1
	if nCurDepth > nDepth:
		raise RecursionLimit("In md5DirTree:", nDepth)
	
	nFiles = _nFiles
	
	if os.path.isfile(sRoot):
		raise ValueError("In md5DirTree, sDir must be a directory or a link.")

	dRet = {}
	for sItem in os.listdir(sRoot):
		sPath = os.path.join(sRoot, sItem)
		
		if os.path.isfile(sPath):
		
			# It's a file, hash it
			hasher = hashlib.md5()
			fIn = file(sPath, 'rb')
			sIn = fIn.read(32768) #Read 32K at a time
			while len(sIn) > 0:
				hasher.update(sIn)
				sIn = fIn.read(32768)
			fIn.close()
			
			if bIncRoot:
				sKey = sPath
			else:
				sKey = sPath.replace("%s%s"%(sTrimRoot,os.sep), '', 1)
			
			dRet[sKey] = hasher.hexdigest()
			nFiles += 1
		
			if nFiles != 0 and (nFiles % 500) == 0:
				log = logging.getLogger('md5DirTree')
				log.info("%5i files hashed..."%nFiles)
			
		elif os.path.isdir(sPath):
		
			# it's a dir, merge results from sub-call
			dTmp = md5DirTree(sPath, bIncRoot, nDepth, nCurDepth, sTrimRoot, nFiles)
			nFiles += len(dTmp)
			for sKey, sVal in dTmp.iteritems():
				dRet[sKey] = sVal
	

	if nCurDepth == 1:
		log = logging.getLogger('md5DirTree')
		log.info("%5i total files hashed"%nFiles)
	return dRet

############################################################################
def genMD5hashLists(sStageDir):
	"""Run through the hole disk and generate MD5 hashs
	
		Outputs files EXTRAS/MD5LF.TXT or EXTRAS/MD5CRLF.TXT which are not
		themselves included in the hashes
	"""
	log = logging.getLogger('genMD5hashLists')
	
	log.info('Generating MD5 hash lists, this could take a while...')
	dHashes = md5DirTree(sStageDir)
	
	sOutLF = pjoin(sStageDir, 'EXTRAS', 'MD5LF.TXT')
	fOutLF = file(sOutLF, 'wb')
	sOutCRLF = pjoin(sStageDir, 'EXTRAS', 'MD5CRLF.TXT')
	fOutCRLF = file(sOutCRLF, 'wb')
	
	lKeys = dHashes.keys()
	lKeys.sort()
	
	for sKey in lKeys:
		fOutLF.write("%s  %s\n"%(dHashes[sKey], sKey))
		fOutCRLF.write("%s  %s\r\n"%(dHashes[sKey], sKey))
	
	fOutLF.close()
	fOutCRLF.close()
	nSize = os.path.getsize(sOutLF)
	nSize += os.path.getsize(sOutCRLF)
	return nSize

############################################################################
def spaceFormat(sTxt, nStart, nEnd):
	"""Take a string of text and break it into lines that are spaced over
	by at least nStart and end before nEnd and end in the given newline
	character.
	
	If the existing text already has this property, then don't change it.
	
	If sTxt is empty, just return the empty string, not a spaced over
	empty string.
	
	Note: nEnd is the column BEFORE the \\r\\n.
	"""
	if sTxt == "":
		return ""
	
	log = logging.getLogger('spaceFormat')
	
	if nStart < 1:
		sTmp = "spaceFormat - 1st column is 1 so nStart < 1 is not defined."
		log.error(sTmp)
		raise ValueError, sTmp
	
	if (nEnd - nStart) < 10:
		sTmp = "spaceFormat - Column must be at least 10 chars wide, "+\
		       "nStart = %i, nEnd = %i."%(nStart, nEnd)
		log.error(sTmp)
		raise ValueError, sTmp
	
	# 1st see if can be used as is, human formatting is better than
	# most programs.
	lLines = sTxt.split('\r\n')
	bOkay = True
	for sLine in lLines:
		if sLine == "":  #Ignore the empty line at end of \r\n
			continue
	
		if len(sLine) > (nEnd+2):  #Check too long
			#print 'too long'
			bOkay = False
			break
			
		if sLine[:nStart-1] != " "*(nStart-1):  #check no spacing
			#print 'insufficent spacing: "%s"'%sLine[:nStart-1]
			bOkay = False
			break
			
		if sLine.find('\t') != -1: #check tab chars
			#print 'has tab chars'
			bOkay = False
			break
	
	if bOkay:
		return sTxt
	
	# Didn't pass, so fix it up.
	rapper = textwrap.TextWrapper()
	rapper.replace_whitespace = True
	rapper.width = nEnd - nStart + 1
	rapper.break_long_words = True
	rapper.expand_tabs = True
	lLines2 = rapper.wrap(sTxt)
	sOut = ""
	for sLine in lLines2:
		#print sLine
		sOut += "%s%s\r\n"%(" "*(nStart - 1), sLine)
	
	return sOut

##############################################################################
def lnBreak(sText, sEndl, sPrefix, nMaxCol):
	"""Simple line breaking function, doesn't attempt to preserve original
	text"""
	
	if nMaxCol - len(sPrefix) < 30:
		raise ValueError("Not enough room for text, max_length - prefix_length = %d"%\
		                 nMaxCol - len(sPrefix) )
	
	lLines = []
	sLine = deepcopy(sPrefix)
	lWords = sText.split()
	
	for sWord in lWords:
	
		if len(sWord) + len(sLine) + 1 > nMaxCol:
			lLines.append("%s%s"%(sLine.rstrip(), sEndl))
			
			sLine = "%s%s "%(sPrefix, sWord)
		else:
			sLine += "%s "%sWord
		
		if sWord.endswith('.') or sWord.endswith(':'):
			sLine += " "
	
	if sLine != sPrefix:
		lLines.append("%s%s"%(sLine.rstrip(), sEndl))
	
	return lLines

############################################################################
# dos format

def chkPDSLines(sPath, nLen=-1):
	"""Check a pds file text file to see if it has any line length
	or embedded tab errors.
	
	Returns the number of errors, and logs them.
	
	nLen defaults to 72 for .lbl, .fmt, .cat
	
	checks for CRLF ending.
	"""

	nErr = 0
	if nLen < 40:
		sTmp = sPath.lower()
		if sTmp.endswith('.lbl') or sTmp.endswith('.fmt') or\
		   sTmp.endswith('.cat'):
			nLen = 74
		else:
			nLen = 80
	else:
		nLen += 2
	
	log = logging.getLogger('chkPDSLines')
	
	fIn = file(sPath, 'rb')	
	nLine = 0
	for sLine in fIn:
		nLine += 1
		
		if not sLine.endswith('\r\n'):
			log.warning("%s,%d: doesn't end in CRLF."%(sPath, nLine))
			nErr += 1
		
		if sLine.count('\t') > 0:
			log.warning('%s,%d: embedded tab.'%(sPath, nLine))
			nErr += 1
			
		if len(sLine) > nLen:
			log.warning('%s,%d: line length > %d chars.'%(sPath,nLine,nLen-2))
			nErr += 1
				
	
	fIn.close()
	return nErr


def fmtPDSTextNoRightPad(sPath, nLen=-1):
	"""Format a pds text file so that it has no padding on 
	the right.  Raise an error if the input file has embedded tabs
	or the line (without right whitespace) is > 78 chars long.
	
	The original file is backed up as: *.bak[,vN].
	
	sPath - full path to file
	
	nLen  - max legal line length without carrage return line
	        feed.  Defaults to 78, but some files are only
			  supposed to be 72 chars long.
	
	Returns: Name of old file if successful, None otherwise.
	"""
	
	if nLen < 40:
		sTmp = sPath.lower()
		if sTmp.endswith('.lbl') or sTmp.endswith('.fmt') or\
		   sTmp.endswith('.cat'):
			nLen = 72
		else:
			nLen = 78
	
	log = logging.getLogger('rmFileRightPad')
	#Try move:
	sBack = mkVerFileName("%s.bak"%sPath)
	
	try:
		os.rename(sPath, sBack)	
	except OSError, e:
		log.error("Couldn't make backup file %s"%sBack)
		return None
	
	try:
		fOut = file(sPath, 'wb')
		fIn = file(sBack, 'rb')	
	
		nLine = 0
		for sLine in fIn:
			nLine += 1
			sOut = sLine.rstrip()
			
			if sOut.count('\t') > 0:
				log.warning('%s,%d: embedded tab.'%(sPath, nLine))
			
			if len(sOut) > nLen:
				log.warning('%s,%d: line length > %d chars.'%(sPath,nLine,nLen))
				
			fOut.write("%s\r\n"%sOut)
	
		fOut.close()
		fIn.close()
		
	except IOError, e:
		log.error('%s: %s'%(sPath, str(e)))
		
		# Try to recover backup, let exceptions flow up if can't
		# recover backup file.
		fOut.close()
		fIn.close()
		os.rename(sBack, sPath)
		
		return None
	
	return sBack
	
