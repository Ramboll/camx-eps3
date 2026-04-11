c*** EMCVTPRF.INC
c
c-----------------------------------------------------------------------
c
c    Include file for the profile weight data used by the EMSCVT
c    program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for cross reference data:
c-----------------------------------------------------------------------
c
c   MXWPRF   I   maximum number of compounds for profile
c
      integer*4 MXWPRF
c
      parameter( MXWPRF  = 500 )
c
c-----------------------------------------------------------------------
c   Profile weight data:
c-----------------------------------------------------------------------
c
c   icmpcd   I   saroad code of compound in weight profile
c   nwate    I   number of compounds making up profile
c   prfcod   C   profile code
c   wtfrac   R   weight fraction of compound in profile (grams per gram)
c
      character*(LENPRF)  prfcod
      integer*4           icmpcd(MXWPRF)
      integer*4           nwate
      real*4              wtfrac(MXWPRF)
c
      common /prfchr/ prfcod
      common /prfdat/ nwate, icmpcd, wtfrac
