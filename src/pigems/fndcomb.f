C**** FNDCOMB.F version 2.0 09/01/2007
c
      function fndcomb( idxcomb,
     &                            fipin, plant, stack )
      integer*4 fndcomb
c
c-----------------------------------------------------------------------
c
c     This routine searches through the list of stacks that are to be
c     combined to make a new stack, searching for the identifiers in
c     the argument list.  If found, the index of the new stack 
c     in the combined stack array is returned. A return value of zero 
c     indicates no match was made. The position of this particular
c     stack in the list of those to be combined is passed back in the
c     argument list.
c
c   Arguments:
c     Outputs:
c       idxcomb  I   index of this source in list of stacks to combine
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
      integer*4    idxcomb
      character*10 fipin
      character*10 plant
      character*10 stack
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
      integer*4 i, j
      logical   lfound
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndcomb = 0
      if( nstknew .EQ. 0 ) goto 9999
c
c   ---- loop through the array of new sttacks ---
c
      do i=1,nstknew
c
c  --- loop through the stacks to create this new stack ---
c
         do j=1,nstkcomb(i)
c
c   ---- if match, set return value and return right away ---
c
            lfound = .TRUE.
            if( fipin .NE. ctycomb(i,j) ) lfound = .FALSE.
            if( plant .NE. faccomb(i,j) ) lfound = .FALSE.
            if( stack .NE. stidcomb(i,j) ) lfound = .FALSE.
c
c   --- if everyghing matches, set return value and exit ---
c
            if( lfound ) then
                idxcomb = j
                fndcomb = i
                goto 9999
            endif
c
c   --- otherwise try the next stack ---
c
         enddo
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
