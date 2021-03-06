#ifndef CasWfdrCal_h
#define CasWfdrCal_h

#ifdef __cplusplus
extern "C" {
#endif



/* 
  RPWS Cassini Low Frequency Digital Receiver Calibration File


Modification List:
  Feburary 11, 2003 raj 
    copy the origional calibration file dated November 16f, 1998 into a header
    file for static inclusion into the program.
	
  Aug, 2016 cwp
    Updated constants to be floats instead of downconvert doubles.
*/


static float CasLfdr_Frq[32]={
  0.195f, 0.390f,  0.586f,  0.781f,  0.977f,  1.172f,  1.367f,  1.563f,
  1.758f, 1.953f,  2.148f,  2.344f,  2.539f,  2.734f,  2.930f,  3.125f,
  3.320f, 3.515f,  3.711f,  4.004f,  4.590f,  5.371f,  6.250f,  7.227f,
  8.398f, 9.766f, 11.328f, 13.184f, 15.332f, 17.871f, 20.898f, 24.316f
};
		
static float CasMfdr_Frq[32]={
   13.93f,  27.86f,  41.86f,  55.79f,   69.79f,   83.71f,   97.64f,  111.64f,
  125.57f, 139.50f, 153.43f, 167.43f,  181.36f,  195.29f,  209.29f,  223.21f, 
  237.14f, 251.07f, 265.07f, 286.00f,  327.86f,  383.64f,  446.43f,  516.21f,
  592.71f, 697.57f, 809.14f, 941.71f, 1095.14f, 1276.50f, 1492.71f, 1736.86f
};


static float CasLfdr_Bandwidth[32]={   
  0.2045f, 0.2045f, 0.2045f, 0.2045f, 0.2045f, 0.2045f, 0.2045f, 0.2045f, 
  0.2045f, 0.2045f, 0.2045f, 0.2045f, 0.2045f, 0.2045f, 0.2045f, 0.2045f,
  0.2045f, 0.2045f, 0.2045f, 0.2871f, 0.5719f, 0.5719f, 0.7373f, 0.7373f,
  1.0429f, 1.0429f, 1.3182f, 1.4466f, 1.8386f, 2.0797f, 2.5488f, 2.6172f 
};

static float CasMfdr_Bandwidth[32]={
  14.6071f, 14.6071f, 14.6071f,  14.6071f,  14.6071f,  14.6071f,  14.6071f,  14.6071f,
  14.6071f, 14.6071f, 14.6071f,  14.6071f,  14.6071f,  14.6071f,  14.6071f,  14.6071f,
  14.6071f, 14.6071f, 14.6071f,  20.5071f,  40.8500f,  40.8500f,  52.6643f,  52.6643f,
  74.4929f, 74.4929f, 94.1571f, 103.3286f, 131.3286f, 148.5500f, 182.0571f, 186.9429f
};



/* converts from Vrms to nT for the Bx Search Coil, [Vrms/nT] */
static float CasLfdr_Bx_Vrms2nT[32]={
  0.00064f, 0.00163f, 0.00264f, 0.00362f, 0.00455f, 0.00547f, 0.00640f, 0.00733f,
  0.00826f, 0.00920f, 0.01015f, 0.01110f, 0.01205f, 0.01301f, 0.01396f, 0.01492f,
  0.01587f, 0.01682f, 0.01776f, 0.01917f, 0.02195f, 0.02554f, 0.02947f, 0.03382f,
  0.03907f, 0.04511f, 0.05176f, 0.05923f, 0.06728f, 0.07596f, 0.08507f, 0.09389f
};

/* converts from Vrms to nT for the By Search Coil, [Vrms/nT] */
static float CasLfdr_By_Vrms2nT[32]={
  0.00063f, 0.00161f, 0.00262f, 0.00358f, 0.00449f, 0.00540f, 0.00631f, 0.00723f,
  0.00816f, 0.00909f, 0.01002f, 0.01096f, 0.01190f, 0.01285f, 0.01379f, 0.01473f,
  0.01567f, 0.01661f, 0.01754f, 0.01893f, 0.02167f, 0.02522f, 0.02910f, 0.03340f,
  0.03860f, 0.04459f, 0.05117f, 0.05856f, 0.06651f, 0.07508f, 0.08410f, 0.09284f
};

/* converts from Vrms to nT for the Bz Search Coil, [Vrms/nT] */
static float CasLfdr_Bz_Vrms2nT[32]={
  0.00063f, 0.00161f, 0.00262f, 0.00359f, 0.00451f, 0.00542f, 0.00634f, 0.00727f,
  0.00820f, 0.00913f, 0.01008f, 0.01102f, 0.01197f, 0.01292f, 0.01386f, 0.01481f,
  0.01575f, 0.01669f, 0.01763f, 0.01903f, 0.02178f, 0.02533f, 0.02924f, 0.03356f,
  0.03878f, 0.04480f, 0.05140f, 0.05881f, 0.06679f, 0.07537f, 0.08439f, 0.09311f
};


/* converts from Vrms to nT for the Bx Search Coil, 0.1474 [Vrms/nT] */
static float CasMfdr_Bx_Vrms2nT[32]={
  0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f,
  0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f,
  0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f,
  0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f, 0.1474f
};

/* converts from Vrms to nT for the By Search Coil, 0.1467 [Vrms/nT] */
static float CasMfdr_By_Vrms2nT[32]={
  0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f,
  0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f,
  0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f,
  0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f, 0.1467f
};

/* converts from Vrms to nT for the Bz Search Coil, 0.1466 [Vrms/nT] */
static float CasMfdr_Bz_Vrms2nT[32]={
  0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f,
  0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f,
  0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f,
  0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f, 0.1466f
};


static float CasWfdr_Counts[256];

  /*  0dB,     10dB,     20dB,     30dB */
static float CasLfdr_Gain[32][4] = {
  {  51.0f,     79.0f,     89.0f,     83.0},
  { 410.0f,    637.0f,    724.0f,    673.0},
  { 872.0f,   2049.0f,   1595.0f,   1610.0},
  {1623.0f,   3015.0f,   4098.0f,   3981.0},
  {1972.0f,   5679.0f,   5855.0f,   5738.0},
  {2791.0f,   6792.0f,   9836.0f,   9836.0},
  {3004.0f,   9134.0f,  12061.0f,  12178.0},
  {3641.0f,   9719.0f,  16394.0f,  16862.0},
  {3762.0f,  11710.0f,  18267.0f,  19438.0},
  {4248.0f,  12061.0f,  22482.0f,  24356.0},
  {4369.0f,  13700.0f,  24357.0f,  27167.0},
  {4733.0f,  14360.0f,  29208.0f,  33102.0},
  {4733.0f,  15577.0f,  30181.0f,  36023.0},
  {5097.0f,  15577.0f,  34076.0f,  40891.0},
  {5097.0f,  16551.0f,  35049.0f,  42838.0},
  {5340.0f,  16551.0f,  37970.0f,  47706.0},
  {5340.0f,  17038.0f,  38944.0f,  50627.0},
  {5340.0f,  17038.0f,  41864.0f,  54521.0},
  {5461.0f,  18011.0f,  42838.0f,  56955.0},
  {6835.0f,  21980.0f,  55950.0f,  77930.0},
  {6957.0f,  23479.0f,  59947.0f,  89920.0},
  {7201.0f,  23479.0f,  64943.0f, 103908.0},
  {7323.0f,  24478.0f,  67440.0f, 118894.0},
  {7445.0f,  24478.0f,  71936.0f, 133881.0},
  {7567.0f,  25477.0f,  73934.0f, 151865.0},
  {7811.0f,  25977.0f,  77931.0f, 171847.0},
  {8055.0f,  26976.0f,  81927.0f, 191829.0},
  {8037.0f,  26820.0f,  83441.0f, 206616.0},
  {7794.0f,  26324.0f,  79468.0f, 210589.0},
  {7550.0f,  25330.0f,  79468.0f, 218536.0},
  {7794.0f,  26324.0f,  81454.0f, 234430.0},
  {7185.0f,  23592.0f,  75494.0f, 218536.0}
};






#ifdef __cplusplus
}  /* Close scope of 'extern "C" declaration */
#endif

#endif  /* CasWfdrCal_h */
