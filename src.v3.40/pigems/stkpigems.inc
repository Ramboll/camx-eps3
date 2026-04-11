c*** STKPIGEMS.INC version 2.0 09/01/2007
c
c-----------------------------------------------------------------------
c
c    Include file for stack list variables for PIGEMS program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c
c-----------------------------------------------------------------------
c    Parameters for arrays bounds:
c-----------------------------------------------------------------------
c
c   MXSTKLST   I   maximum number of stacks in master stack list
c
      integer*4 MXSTKLST
c
      parameter( MXSTKLST = 100000 )
c
c-----------------------------------------------------------------------
c    Variables for Master stack list:
c-----------------------------------------------------------------------
c
c   ctylist    C   county code
c   faclist    C   facility ID
c   stklist    C   stack ID
c   lpiglist   L   flag to determine if this source should be PiGged
c   hgtlist    R   stack height
c   dialist    R   stack diameter
c   vellist    R   gas exit velocity
c   tptlist    R   gas exit temperature
c   xloclist   R   X-coordinate
c   yloclist   R   Y-coordinate
c   nstklist   I   number of stacks in the list
c   ilaststk   I   index of the last stack that was matched
c   ifilestk   I   index of the input file that originally contained the stack
c   lcolocat   L   flag that determnines if a stack is collocated 
c                  with another
c   lprdo      L   flag to indicate source has plume rise distribution override feature
c   idxpigcrt  I   index of the pollutant that was used to PiG the source
c
      character*10 ctylist(MXSTKLST)
      character*10 faclist(MXSTKLST)
      character*10 stklist(MXSTKLST)
      logical      lpiglist(MXSTKLST)
      real*4       hgtlist(MXSTKLST)
      real*4       dialist(MXSTKLST)
      real*4       vellist(MXSTKLST)
      real*4       tptlist(MXSTKLST)
      real*4       xloclist(MXSTKLST)
      real*4       yloclist(MXSTKLST)
      integer*4    nstklist
      integer*4    ilaststk
      integer*4    ifilestk(MXSTKLST)
      integer*4    idxpigcrt(MXSTKLST)
      logical      lcolocat(MXSTKLST)
      logical      lprdo(MXSTKLST)
c
      common /lstchr/ ctylist, faclist, stklist
c
      common /lstdat/ lpiglist, hgtlist, dialist, vellist, tptlist,
     &                xloclist, yloclist, nstklist, ilaststk,
     &                ifilestk, idxpigcrt, lcolocat, lprdo
c
c-----------------------------------------------------------------------
c    Variables for Emissions values
c-----------------------------------------------------------------------
c
c   emsmax  R  maximum emissions for each stack on any day,
c              by criteria pollutant
c   emsstk  R  speicated emissions by stack for day to be output
c
      real*4 emsmax(MXSTKLST,MXCRIT) 
      real*4 emsstk(MXSTKLST,MXSPEC,MXHOUR) 
c
      common /emsdat/ emsmax, emsstk          
