c*** USRLBASE.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for user input parameters and variables for LBASE
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Paramters for USERIN switches:
c-----------------------------------------------------------------------
c
c     GRMSTR  C  string for input units in grams
c     TONSTR  C  string for input units in tons
c     GRMCVT  R  conversion factor for grams to tons
c     TONCVT  R  conversion factor for tons to tons
c
      character*(KEYLEN) GRMSTR
      character*(KEYLEN) TONSTR
      real*4             GRMCVT
      real*4             TONCVT
c
      parameter( GRMSTR = 'GRAMS')
      parameter( TONSTR = 'TONS')
      parameter( GRMCVT = 1/9.071847e+05)
      parameter( TONCVT = 1.0 )
c
c-----------------------------------------------------------------------
c    USERIN variables:
c-----------------------------------------------------------------------
c
c   cvtunit  R   conversion factor for input units to grams
c
      real*4  cvtunit
c
      common /usrdat/ cvtunit
