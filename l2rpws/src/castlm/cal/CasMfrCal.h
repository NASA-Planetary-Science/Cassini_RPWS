#ifndef CasMfrCal_h
#define CasMfrCal_h

#ifdef __cplusplus
extern "C" {
#endif



/* 
  RPWS Cassini Medium Frequency Receiver Calibration File


Modification List:
  Feburary 11, 2003 raj 
    copy the origional calibration file dated November 16, 1998 into a header
    file for static inclusion into the program.

*/


/* converts from data numbers to Vrms for each band and step, 70 includes */
#include "CasMfrDn2Vrms.h"



/* step center frequencies in hertz */
static float CasMfr_Frq[3][32]={
{ 23.89f, 26.25f, 29.15f, 32.42f,  36.27f,  40.89f,  46.51f,  52.82f,
  60.43f, 69.46f, 80.05f, 92.34f, 107.20f, 124.30f, 144.80f, 169.00f,
   0.00f,  0.00f,  0.00f,  0.00f,   0.00f,   0.00f,   0.00f,   0.00f,  
   0.00f,  0.00f,  0.00f,  0.00f,   0.00f,   0.00f,   0.00f,   0.00f },  

{ 192.11f,  214.63f,   225.98f,  237.40f,  260.42f,  272.02f,  295.43f,  307.23f,
  331.04f,  355.11f,   379.46f,  404.09f,  429.01f,  454.22f,  492.58f,  518.53f,
  558.04f,  598.25f,   639.20f,  680.91f,  723.38f,  766.65f,  825.61f,  870.82f,
  932.46f,  995.71f,  1060.64f, 1144.25f, 1213.20f, 1302.08f, 1375.44f, 1470.09f },

{ 1536.89f, 1671.81f, 1785.10f, 1899.17f, 2083.33f,  2269.55f,  2316.43f,  2481.55f,
  2648.30f, 2816.71f, 3011.24f, 3208.00f, 3432.08f,  3659.09f,  3992.29f,  4279.24f,
  4651.27f, 4813.11f, 5086.11f, 5419.21f, 5758.48f,  6162.32f,  6575.06f,  7027.51f,
  7490.92f, 7997.75f, 8518.02f, 9120.06f, 9705.63f, 10165.01f, 11078.04f, 11799.33f }
};


/* Bandwidth for each mfr step in hertz */
static float CasMfr_Bandwidth[3][32];

static float CasMfr_MagPreAmpGain[3][32];

/* Conversion factors for the magnetic search coils, Vrms -> nT */
static float CasMfr_Bx_Vrms2nT[3][32]={
{ 0.0922f, 0.0973f, 0.1030f, 0.1088f, 0.1149f, 0.1215f, 0.1285f, 0.1340f,
  0.1379f, 0.1420f, 0.1444f, 0.1468f, 0.1483f, 0.1489f, 0.1495f, 0.1500f,
  0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f,
  0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f },

{ 0.1505f, 0.1506f, 0.1505f, 0.1504f, 0.1503f, 0.1502f, 0.1501f, 0.1500f, 
  0.1499f, 0.1498f, 0.1497f, 0.1496f, 0.1495f, 0.1494f, 0.1493f, 0.1492f, 
  0.1490f, 0.1488f, 0.1487f, 0.1485f, 0.1483f, 0.1482f, 0.1479f, 0.1478f, 
  0.1476f, 0.1474f, 0.1473f, 0.1471f, 0.1470f, 0.1469f, 0.1468f, 0.1466f },

{ 0.1466f, 0.1464f, 0.1463f, 0.1462f, 0.1459f, 0.1456f, 0.1455f, 0.1452f, 
  0.1450f, 0.1447f, 0.1445f, 0.1443f, 0.1440f, 0.1437f, 0.1434f, 0.1432f,
  0.1428f, 0.1427f, 0.1424f, 0.1419f, 0.1415f, 0.1409f, 0.1404f, 0.1399f,
  0.1388f, 0.1377f, 0.1366f, 0.1354f, 0.1343f, 0.1325f, 0.1255f, 0.1204f }

};



/* Conversion factors for the magnetic search coils, Vrms -> nT */
static float CasMfr_Bz_Vrms2nT[3][32]={
{ 0.0914f, 0.0965f, 0.1021f, 0.1079f, 0.1139f, 0.1204f, 0.1273f, 0.1329f,
  0.1369f, 0.1410f, 0.1434f, 0.1458f, 0.1473f, 0.1479f, 0.1484f, 0.1490f,
  0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 
  0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f, 0.0000f },

{ 0.1495f, 0.1495f, 0.1494f, 0.1493f, 0.1492f, 0.1491f, 0.1490f, 0.1489f,
  0.1488f, 0.1487f, 0.1485f, 0.1484f, 0.1483f, 0.1482f, 0.1481f, 0.1480f,
  0.1478f, 0.1476f, 0.1475f, 0.1473f, 0.1472f, 0.1471f, 0.1469f, 0.1468f,
  0.1467f, 0.1466f, 0.1464f, 0.1462f, 0.1461f, 0.1459f, 0.1458f, 0.1456f },

{ 0.1455f, 0.1453f, 0.1452f, 0.1450f, 0.1447f, 0.1443f, 0.1442f, 0.1439f,
  0.1436f, 0.1433f, 0.1430f, 0.1428f, 0.1425f, 0.1422f, 0.1418f, 0.1415f,
  0.1411f, 0.1409f, 0.1406f, 0.1400f, 0.1394f, 0.1387f, 0.1380f, 0.1373f,
  0.1356f, 0.1338f, 0.1321f, 0.1303f, 0.1286f, 0.1266f, 0.1203f, 0.1156f }

};




#ifdef __cplusplus
}  /* Close scope of 'extern "C" declaration */
#endif

#endif  /* CasMfrCal_h */
