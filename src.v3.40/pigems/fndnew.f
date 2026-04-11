C**** FNDNEW.F version 2.0 09/01/2007
c
      function fndnew( 
     &                  fipin, plant, stack )
      integer*4 fndnew
c
c-----------------------------------------------------------------------
c
c     This routine searches through the list of new stacks that will
c     be created by combining existing stacks.  If found, the index of 
c     the new stack in the combined stack array is returned. A return 
c     value of zero indicates no match was made.
c
c   Arguments:
c     Outputs:
c     Inputs:
c       fipin    C   FIPS code of source
c       plant    C   NEDS facility id code of source
c       stack    C   stack id of source
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
      include 'usrpigems.inc'
      include 'stkpigems.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      character*10 fipin
      character*10 plant
      character*10 stack
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 i, j
      logical   lfound
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndnew = 0
      if( nstknew .EQ. 0 ) goto 9999
c
c   ---- loop through the array of new sttacks ---
c
      do i=1,nstknew
c
c   ---- if match, set return value and return right away ---
c
         lfound = .TRUE.
         if( fipin .NE. ctynew(i) ) lfound = .FALSE.
         if( plant .NE. facnew(i) ) lfound = .FALSE.
         if( stack .NE. stidnew(i) ) lfound = .FALSE.
c
c   --- if everyghing matches, set return value and exit ---
c
        if( lfound ) then
            fndnew = i
            goto 9999
        endif
c
c   --- otherwise try the next stack ---
c
      enddo
c
c   ---- no match, return zero ----
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
