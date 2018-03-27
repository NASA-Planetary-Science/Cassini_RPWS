
#ifndef CasType_h
#define CasType_h

#ifdef __cplusplus
extern "C"
{
#endif

#include <Cext.h>

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/* unused mini-packet ids : 3,5,6,9,A,C */
#define CasMp_Mask   (0xF0000000)
#define CasMp_Stim   (0x00000000)
#define CasMp_Mfr    (0x10000000)
#define CasMp_Hfr    (0x20000000)
#define CasMp_Id3    (0x30000000)       /* unused mini-packet id */
#define CasMp_Lp     (0x40000000)
#define CasMp_Id5    (0x50000000)       /* unused mini-packet id */
#define CasMp_Id6    (0x60000000)       /* unused mini-packet id */
#define CasMp_Wfdr   (0x70000000)       /* low freq. digital receiver 25Hz/2.5KHz */
#define CasMp_Wfr    (0x80000000)
#define CasMp_Id9    (0x90000000)       /* unused mini-packet id */
#define CasMp_Id10   (0xA0000000)       /* unused mini-packet id */
#define CasMp_Dust   (0xB0000000)
#define CasMp_Id12   (0xC0000000)       /* unused mini-packet id */
#define CasMp_Mro    (0xD0000000)
#define CasMp_Wbr    (0xE0000000)
#define CasMp_Fill   (0xF0000000)

#define CasRcv_Mask  (0xFF000000)
#define CasMpMask    (0xF0000000)
#define CasModeMask  (0xFF000000)
#define CasBadPacket (0x00800000)

#define CasAntMask        ((Ulong)0x00000FFF)
#define CasAntAll         ((Ulong)0x00000FFF)
#define CasAntAny         ((Ulong)0x00000FFF)
#define CasAntNone        ((Ulong)0x00000000)
#define CasAntExEz        ((Ulong)0x0000000F)
#define CasAntExAny       ((Ulong)0x00000007)
#define CasAntExMask      ((Ulong)0x00000007)
#define CasAntExp         ((Ulong)0x00000001)
#define CasAntEu          ((Ulong)0x00000001)
#define CasAntExm         ((Ulong)0x00000002)
#define CasAntEv          ((Ulong)0x00000002)
#define CasAntEx          ((Ulong)0x00000004)
#define CasAntEz          ((Ulong)0x00000008)
#define CasAntEw          ((Ulong)0x00000008)
#define CasAntEzMask      ((Ulong)0x00000008)
#define CasAntBx          ((Ulong)0x00000010)
#define CasAntBy          ((Ulong)0x00000020)
#define CasAntBz          ((Ulong)0x00000040)
#define CasAntLMRp        ((Ulong)0x00000080)
#define CasAntLMRm        ((Ulong)0x00000100)
#define CasAntLPs         ((Ulong)0x00000200)
#define CasAntHF          ((Ulong)0x00000400)

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
#define CasWfdr_BadPacket    0x00800000
#define CasWfdr_ModeMask     0xFF000000
#define CasWfdr_Lfdr         0x71000000
#define CasWfdr_Mfdr         0x72000000

#define CasWfdr_Anything     0x0070F000
#define CasWfdr_Log          0x00100000
#define CasWfdr_Lin          0x00200000
#define CasWfdr_Fake         0x00400000
#define CasWfdr_DPFzero      0x00080000

#define CasWfdr_SizeMask     0x0000F000
#define CasWfdr_256          0x00001000
#define CasWfdr_512          0x00002000
#define CasWfdr_1024         0x00004000
#define CasWfdr_2048         0x00008000

#define CasLfdr_AnyMode      0x7F000000 /* alias for logical receiver type */
#define CasLfdr_Normal       0x71000000 /* alias for logical receiver type */
#define CasMfdr_Normal       0x72000000 /* alias for logical receiver type */

#define CasWfdr_AntMask      0x00000FFF
#define CasLfdr_AntMask      0x00000FFF
#define CasMfdr_AntMask      0x00000FFF

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

/* mfr packet header was redefined with the flight software upload, ver2.4
     on June 1, 2000: 2000-153T08:06:32 (0x4FC87A44). */
#define CasMfr_MagicDate   (0x4FC87A44)

#define CasMfr_BadPacket    0x00800000
#define CasMfr_ModeMask     0xFF000000
#define CasMfr_AnyMode      0x1F000000
#define CasMfr_Normal       0x11000000
#define CasMfr_FastToggle   0x12000000

#define CasMfr_BandMask     0x000FF000
#define CasMfr_AntMask      0x00000FFF

/* normal mode */
#define CasMfr_BandMask     0x000FF000
#define CasMfr_Band1Sweep1  0x00001000
#define CasMfr_Band1Sweep2  0x00002000
#define CasMfr_Band2Sweep1  0x00004000
#define CasMfr_Band2Sweep2  0x00008000
#define CasMfr_Band3Sweep1  0x00010000
#define CasMfr_Band3Sweep2  0x00020000
#define CasMfr_Band3Sweep3  0x00040000
#define CasMfr_Band3Sweep4  0x00080000
#define CasMfr_Band1        0x00003000
#define CasMfr_Band2        0x0000C000
#define CasMfr_Band3        0x000F0000
#define CasMfr_Band123      0x000FF000

/* fast toggle */
#define CasMfr_FstTog1st    0x00035000
#define CasMfr_FstTog2nd    0x000CA000

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
#define CasHfr_BadPacket   0x00800000
#define CasHfr_ModeMask    0xFF000000
#define CasHfr_AnyMode     0x2F000000
#define CasHfr_Analysis    0x21000000
#define CasHfr_Calibration 0x22000000
#define CasHfr_Sounder     0x24000000
#define CasHfr_Millisecond 0x28000000

/* Analysis Mode */
#define CasHfr_BandMask    0x0001F000
#define CasHfr_AntMask     0x00000FFF
#define CasHfr_BandAll     0x0001F000
#define CasHfr_BandA       0x00001000
#define CasHfr_BandB       0x00002000
#define CasHfr_BandC       0x00004000
#define CasHfr_BandHF1     0x00008000
#define CasHfr_BandHF2     0x00010000
#define CasHfr_BandABC     0x00007000
#define CasHfr_BandHF12    0x00018000
#define CasHfr_BandABC12   0x0001F000
#define CasHfr_NoBand      0x00000000
#define CasHfr_BandsNone   0x00000000

/* Calibration Mode*/



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
#define CasWfr_BadPacket    0x00800000
#define CasWfr_ModeMask     0xFF000000
#define CasWfr_Lband        0x81000000
#define CasWfr_Hband        0x82000000

#define CasWfr_AnyMode      0x007FF000
#define CasWfr_Ch0          0x00001000
#define CasWfr_Ch1          0x00002000
#define CasWfr_Ch2          0x00004000
#define CasWfr_Ch3          0x00008000
#define CasWfr_Ch4          0x00010000
#define CasWfr_Ch01         0x00020000
#define CasWfr_Ch234        0x00040000
#define CasWfr_Ch01234      0x00080000
#define CasWfr_Ch012        0x00100000  /* L.P. Mode */

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
#define CasWbr_BadPacket    0x00800000
#define CasWbr_ModeMask     0xFF000000
#define CasWbr_Lband        0x81000000
#define CasWbr_Hband        0x82000000

#define CasWbr_AnyMode      0x007FF000
#define CasWbr_AgcActive    0x00400000

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */



#define CasMpii_Mask                 0x000000FF /* cds fill for sure */
#define CasMpii_ErrorMask            0x0000003F /* mask off error bits */
#define CasMpii_Processed            0x00000080 /* touched by mpii */
#define CasMpii_Segmented            0x00000040 /* re-segmented by mpii */
#define CasMpii_CdsFillMaybe         0x00000004 /* Guess at cds fill */
#define CasMpii_MpiiFill             0x00000002 /* cds fill for sure */
#define CasMpii_CdsFill              0x00000001 /* cds fill for sure */

#define CasMpus_Mask                 0x0000FF00 /* mpus status bits */
#define CasMpus_ErrorMask            0x00003F00 /* mask off error bits */
#define CasMpus_Processed            0x00008000 /* touched by mpus */
#define CasMpus_Segmented            0x00004000 /* segmented packets */
#define CasMpus_Error                0x00002000 /* error during construction */
#define CasMpus_ZeroFilled           0x00001000 /* zero fill data inserted */
#define CasMpus_LengthGuess          0x00000800 /* guess at the correct len */
#define CasMpus_LastSegment          0x00000400 /* missing last segment */
#define CasMpus_MiddleSegment        0x00000200 /* missing middle segment(s) */
#define CasMpus_FirstSegment         0x00000100 /* missing first segment */

#define CasMeander_Mask              0x00FF0000 /* hfr meander status bits */
#define CasMeander_ErrorMask         0x003F0000 /* mask off error bits */
#define CasMeander_Processed         0x00800000 /* touched by meander decomp */
#define CasMeander_Decompressed      0x00400000 /* actually meander decomped */
#define CasMeander_DataTermination   0x00200000 /* data not terminated */
#define CasMeander_HeaderErrors      0x00100000 /* header errs/not term. */
#define CasMeander_ZeroFilled        0x00080000 /* zero fill */
#define CasMeander_BufferOverRun     0x00040000 /* out of data to decompress */
#define CasMeander_MakeClassError    0x00020000 /* invalid class */
#define CasMeander_GetBitsError      0x00010000 /* out of range bits */

#define CasRice_Mask                 0xFF000000 /* rice status bits */
#define CasRice_ErrorMask            0x3F000000 /* rice status bits */
#define CasRice_Processed            0x80000000 /* touched by rice */
#define CasRice_Decompressed         0x40000000 /* actually decompressed */
#define CasRice_Errors               0x20000000 /* decompression errors */
#define CasRice_FoundZeros           0x10000000 /* detected unmarked zeros */
#define CasRice_BackupOperator       0x08000000 /* Psi3,1 involked */
#define CasRice_CodeId_7             0x04000000 /* Psi1,7 involked,8bit data */
#define CasRice_CodeId_6             0x02000000 /* Psi1,6 involked,8bit data */
#define CasRice_InvalidOperator      0x01000000 /* 10,11,12,13,14 */

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
  char *CasTlm_Version (void);

  char *CasMpus_DecodeGndStatus (Ulong nStatus);
  char *CasHfrMeander_DecodeGndStatus (Ulong nStatus);

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

/*      OBSOLETE - OBSOLETE - OBSOLETE - OBSOLETE - OBSOLETE - OBSOLETE     */

#define CasSciMask   ((ULONG)0x0000FFFF)
#define CasSciStim   ((ULONG)0x00000001)
#define CasSciMFR    ((ULONG)0x00000002)
#define CasSciHFR    ((ULONG)0x00000004)
#define CasSciID3    ((ULONG)0x00000008)
#define CasSciLP     ((ULONG)0x00000010)
#define CasSciID5    ((ULONG)0x00000020)
#define CasSciID6    ((ULONG)0x00000040)
#define CasSciLFDR   ((ULONG)0x00000080)
#define CasSciWFR    ((ULONG)0x00000100)
#define CasSciID9    ((ULONG)0x00000200)
#define CasSciID10   ((ULONG)0x00000400)
#define CasSciDust   ((ULONG)0x00000800)
#define CasSciID12   ((ULONG)0x00001000)
#define CasSciMRO    ((ULONG)0x00002000)
#define CasSciWBR    ((ULONG)0x00004000)
#define CasSciFill   ((ULONG)0x00008000)

/*      OBSOLETE - OBSOLETE - OBSOLETE - OBSOLETE - OBSOLETE - OBSOLETE     */

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

#ifdef __cplusplus
}                                       /* Close scope of 'extern "C"' declaration which encloses file. */
#endif
#endif                                  /* CasType_h */
