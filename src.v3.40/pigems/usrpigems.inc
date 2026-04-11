c*** USRPIGEMS.INC version 2.0 09/01/2007
c
c-----------------------------------------------------------------------
c
c    Include file for user input parameters and variables for PIGEMS
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Variables for Co-location distance 
c-----------------------------------------------------------------------
c
c   colodist  R  co-location distance (meters)
c   colocpol  C  pollutant that is used for co-lopcation 
c   idxcoloc  I  index of the colocation pollutant in pollutant list
c
      real*4       colodist
      character*10 colocpol
      integer*4    idxcoloc
c
      common /colochr/ colocpol
      common /colodat/ colodist, idxcoloc
c
c-----------------------------------------------------------------------
c    Parameters for /THRESHOLD/ packet arrays:
c-----------------------------------------------------------------------
c
c   MXTHRSH   I   maximum number of threshold values
c
      integer*4 MXTHRSH
c
      parameter( MXTHRSH = 100 )
c
c-----------------------------------------------------------------------
c    /THRESHOLD MAP/ packet variables:
c-----------------------------------------------------------------------
c
c   nthrshmap I   number of threshold codes/values provided
c   thrshcty  C   county in which this threshold is applied
c   ithrsmap  I   numeric code for this threshold value
c
      character*10 thrshcty(MXTHRSH)
      integer*4    ithrshmap(MXTHRSH)
      integer*4    nthrshmap
c
      common /thmchr/ thrshcty
      common /thmdat/ ithrshmap, nthrshmap
c
c-----------------------------------------------------------------------
c    /THRESHOLD VALUE/ packet variables:
c-----------------------------------------------------------------------
c
c   nthresh   I   number of threshold codes/values provided
c   thrshspc  C   species to be used to test the threshold
c   ithrshcd  I   numeric code for this threshold value
c   idxthspc  I   index of species in global arrays
c   thrshval  R   threshold value for this species and code
c
      character*10 thrshspc(MXTHRSH)
      integer*4    ithrshcd(MXTHRSH)
      integer*4    nthresh
      integer*4    idxthspc(MXTHRSH)
      real*4       thrshval(MXTHRSH)
c
      common /thrchr/ thrshspc
      common /thrdat/ ithrshcd, nthresh, thrshval, idxthspc
c
c-----------------------------------------------------------------------
c    Parameters for /COMBINE STACKS/ packet arrays:
c-----------------------------------------------------------------------
c
c   MXSNEW    I   maximum number of "new" stacks to be created
c   MXCOMB    I   maximum number of stacks that be combined to make
c                 a new stack
c
      integer*4 MXSNEW
      integer*4 MXCOMB
c
      parameter( MXSNEW = 1000 )
      parameter( MXCOMB = 100  )
c
c-----------------------------------------------------------------------
c    /COMBINE STACKS/ packet variables:
c-----------------------------------------------------------------------
c
c  ctynew     C   county ID for the new stack
c  facnew     C   facility ID for new stack
c  stidnew    C   stack ID form new stack
c  lpignew    L   flag to determine if this source should be PiGged
c  inewlst    I   index of the new stack into master stack list
c  hgtnew     R   stack height for this new stack (negative means
c                 calculate from combined stacks)
c  dianew     R   stack diameter for this new stack
c  velnew     R   gas exit velocity for this new stack
c  tptnew     R   gas exit temperature for this new stack 
c  xlocnew    R   X-coordinate for this new stack
c  ylocnew    R   Y-coordinate for this new stack
c  nstknew    I   number of new stacks to create
c  ctycomb    C   county ID for stack to combine to create new stack
c  faccomb    C   facility ID for stack to combine to create new stack
c  stidcomb   C   stack ID for stack to combine to create new stack
c  nstkcomb   I   number of stacks to combine to make this new stack
c  hgtcomb    R   stack height for the stack to combine
c  diacomb    R   stack diameter for the stack to combine
c  velcomb    R   gas exit velocity for the stack to combine
c  tptcomb    R   gas exit temperature for the stack  to combine
c  xloccomb   R   X-coordinate for the stack to combine
c  yloccomb   R   Y-coordinate for the stack to combine
c  crtcomb    R   maximum criteria pollutant for the stack to combine
c  spccomb    R   speciated pollutants for the stack to combine
c  ifilenew   I   number of the input file that originally contained the stack
c  lcolonew   L   flag that determines if the source was created via
c                 colocation
c  idxmaster  I   index of the "NEW" stack in the master stack list
c
      character*10 ctynew(MXSNEW)
      character*10 facnew(MXSNEW)
      character*10 stidnew(MXSNEW)
      logical      lpignew(MXSNEW)
      integer*4    inewlist(MXSNEW)
      real*4       hgtnew(MXSNEW)
      real*4       dianew(MXSNEW)
      real*4       velnew(MXSNEW)
      real*4       tptnew(MXSNEW)
      real*4       xlocnew(MXSNEW)
      real*4       ylocnew(MXSNEW)
      integer*4    nstknew
      character*10 ctycomb(MXSNEW,MXCOMB)
      character*10 faccomb(MXSNEW,MXCOMB)
      character*10 stidcomb(MXSNEW,MXCOMB)
      integer*4    nstkcomb(MXSNEW)
      real*4       hgtcomb(MXSNEW,MXCOMB)
      real*4       diacomb(MXSNEW,MXCOMB)
      real*4       velcomb(MXSNEW,MXCOMB)
      real*4       tptcomb(MXSNEW,MXCOMB)
      real*4       xloccomb(MXSNEW,MXCOMB)
      real*4       yloccomb(MXSNEW,MXCOMB)
      real*4       crtcomb(MXSNEW,MXCOMB,MXCRIT)
      real*4       spccomb(MXSNEW,MXCOMB,MXSPEC,MXHOUR)
      integer*4    ifilenew(MXSNEW,MXCOMB)
      logical*4    lcolonew(MXSNEW)
      integer*4    idxmaster(MXSNEW)
c
      common /combchr/ ctynew, facnew, stidnew, ctycomb, faccomb, 
     &                 stidcomb
c
      common /combdat/ lpignew, inewlist, ifilenew, hgtnew, dianew, 
     &                 velnew, tptnew, xlocnew, ylocnew, nstknew, 
     &                 nstkcomb, hgtcomb, diacomb, velcomb, tptcomb, 
     &                 xloccomb, yloccomb, crtcomb, spccomb, lcolonew,
     *                 idxmaster
c
c-----------------------------------------------------------------------
c    Variables for the mask file:
c-----------------------------------------------------------------------
c
c   imskval   I  2-D gridded array of threshold values by grid cell
c
      integer*4 imskval(MXCELX,MXCELY)         
c
      common /mskdat/ imskval
