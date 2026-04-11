C**** FNDLIST.F version 2.0 09/01/2007
c
      function fndlist(
     &                 fipin, plant, stack, hghtin, diamin, tempin, 
     &                                       velocin, xlocin, ylocin )
      integer*4 fndlist
c
c-----------------------------------------------------------------------
c
c     This routine searches through the master stack arrays for a match 
c     with data in the argument list.  If found, the index of the stack 
c     in the array is returned. A return value of zero indicates
c     no match was made.
c
c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code of source
c       plant    C   NEDS facility id code of source
c       stack    C   stack id of source
c       hghtin   R   stack height
c       diamin   R   stack diameter
c       tempin   R   stack temperature
c       velocin  R   stack velocity
c       xlocin   R   X-coordinate of stack location
c       ylocin   R   Y-coordinate of stack location
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'stkpigems.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      character*10 fipin
      character*10 plant
      character*10 stack
      real*4       hghtin
      real*4       diamin
      real*4       tempin
      real*4       velocin
      real*4       xlocin
      real*4       ylocin
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      character*10 stktmp
      integer*4    istart, iend, incrament, idxstk, i
      logical      lfound
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndlist = 0
      if( nstklist .EQ. 0 ) goto 9999
      iend = nstklist
      incrament = 1
      istart = ilaststk
c
c   --- if we start with the last stack found, it's more efficient ---
c
 111  continue
      if( istart .LE. 0 ) istart = 1
c
c   ---- loop through the array ---
c
      do i=istart,iend,incrament
c
c   ---- if match, set return value and return right away ---
c
         lfound = .TRUE.
         if( fipin .NE. ctylist(i) ) lfound = .FALSE.
         if( plant .NE. faclist(i) ) lfound = .FALSE.
         if( stack .NE. stklist(i) ) lfound = .FALSE.
         if( hghtin .NE. hgtlist(i) ) lfound = .FALSE.
         if( ABS(diamin) .NE. ABS(dialist(i)) ) lfound = .FALSE.
         if( tempin .NE. tptlist(i) ) lfound = .FALSE.
         if( velocin .NE. vellist(i) ) lfound = .FALSE.
         if( xlocin .NE. xloclist(i) ) lfound = .FALSE.
         if( ylocin  .NE. yloclist(i) ) lfound = .FALSE.
c
c   --- if everyghing matches, set return value and exit ---
c
         if( lfound ) then
             fndlist = i
             ilaststk = i
             goto 9999
         endif
c
c   --- otherwise try the next stack ---
c
      enddo
c
c   --- end of all of the stacks, go back and look at
c       the rest of the list ---
c
      if( istart .EQ. 1 ) then
          ilaststk = nstklist - 1
          goto 9999
      else
          istart = iend 
          iend = 1
          incrament = -1
          goto 111
      endif
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
