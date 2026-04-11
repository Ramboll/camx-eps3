c*** USRGRDEM.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for user input parameters and variables for GRDEM
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    USERIN variables:
c-----------------------------------------------------------------------
c
c   luam     L   flag to determine if output emissions format
c                (.TRUE. = UAM format, .FALSE. = EMBR format )
c   lmeds    L   flag to determine if output is MEDS
c   lcomp    L   flag to determine if output is compressed EMBR
c   lsrgmult L   flag to determine if secondary surrogate codes 
c                should be used (.TRUE. = use seondary/tertiary codes)
c
      logical luam
      logical lcomp
      logical lmeds
      logical lsrgmult
c
      common /usrdat/ luam, lcomp, lmeds, lsrgmult
c
c-----------------------------------------------------------------------
c    MEDS packet variables:
c-----------------------------------------------------------------------
c
c   scen     C   character string for scenario descriptor
c   medlst   C   list of species to output to MEDS file
c   imapmed  I   index into species list of MEDS output species
c   facmed   R   factor to be applied to the emissions to
c                get the output of kg
c
      character*8  scen
      character*10 medlst(MXSPEC)
      integer*4    imapmed(MXSPEC)
      real*4       facmed(MXSPEC)
c
      common /medchr/ scen, medlst
      common /meddat/ imapmed, facmed
