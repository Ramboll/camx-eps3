C**** FNDVSTK.F 
c
      function fndvstk(
     &                  fipin, ascin, vslin, ijclin )
      integer*4 fndvstk
c
c-----------------------------------------------------------------------
c
c     This routine searches through the array of stacks for an exact
c     match of the data in the argument list.
c
c   Arguments:
c     Inputs:
c       fipin     C   FIPS county code to match
c       ascin     C   ASC-SCC code to match
c       vslin     C   vessel type to match 
c       ijclin    I   cell to match
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'layers.inc'
      include 'pstshpstk.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*5  fipin
      character*10 ascin
      character*10 vslin
      integer*4    ijclin
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
      logical*4 lmatch
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndvstk = 0
c
c   --- loop over all stacks in the list ---
c
      do i=1,nstack
c
c   --- assume that this stack matches ---
c
        lmatch = .TRUE.
c
c   ---- compare the values and reset flag, if needed ---
c
        if( fipin .NE. fipstk(i) ) lmatch = .FALSE.
        if( ascin .NE. ascstk(i) ) lmatch = .FALSE.
        if( vslin .NE. vslstk(i) ) lmatch = .FALSE.
        if( ijclin .NE. ijclstk(i) ) lmatch = .FALSE.
c
c   --- if the match is still true, set return value and return ----
c
        if( lmatch ) then
           fndvstk = i
           goto 9999
        endif
c
c   --- no match, next stack ---
c
      enddo
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
