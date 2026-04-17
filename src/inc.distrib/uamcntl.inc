c*** UAMCNTL.INC 
c
c-----------------------------------------------------------------------
c
c   Variables and common blocks for the /UAMCNTL/ packet of the USERIN
c   file
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c   jan 2018 gwilson added varaibles for species gas flag
c
c-----------------------------------------------------------------------
c   /UAMCNTL/ Variables:
c-----------------------------------------------------------------------
c
c   nspecs   I   number of species in species array
c   spcnam   C   species name array
c   lgas     L   .TRUE. if species is a gas species
c
      character*10 spcnam(MXSPEC)
      integer*4    nspecs
      logical*4    lgas(MXSPEC)
c
      common /spcchr/  spcnam
      common /spcdat/  nspecs, lgas
c
c   begtim   I   beginning time of episode
c   endtim   I   ending time of episode
c   fnote    C   emissions file note
c   ibgdat   I   beginning Julian date of episode
c   ibgcal   I   beginning calandar date of episode
c   iendat   I   ending Julian date of episode
c   iencal   I   ending calandar date of episode
c   itzone   I   time zone
c
      character*60 fnote
      integer*4    ibgdat
      integer*4    ibgcal
      integer*4    iendat
      integer*4    iencal
      real*4       begtim
      real*4       endtim
      integer      itzone
c
      common /timchr/ fnote
      common /timdat/ ibgdat, iendat, begtim, endtim, ibgcal, iencal,
     &                itzone
