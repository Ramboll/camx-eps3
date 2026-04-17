C**** FNDMAP.F version 2.0 09/01/2007
c
      function fndmap(
     &                 fipin, idxpol )
      integer*4 fndmap
c
c-----------------------------------------------------------------------
c
c     This routine searches through the list of mappings for threshold
c     values and finds the "best" match of county.  The mapping must
c     also have the same pollutant.  The pollutant index is compared
c     against the argument before it is considered for a match.
c
c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code of source
c       idxpol   I   index of pollutant in list
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
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      character*10 fipin
      integer*4    idxpol
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
      integer*4 fndint
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      character*5 fipglb, fipst
      integer*4   idxglb, idxst, idxthrsh, i, j
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data fipglb /'00000'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndmap = 0
      idxglb = 0
      idxst = 0
      fipst = fipin(1:2)//'000'
c
c   ---- loop through the array of codes ---
c
      do 10 i=1,nthrshmap
c
c   --- find the index of this code in the theshold value arrays ---
c
         idxthrsh = 0
         do j=1,nthresh
           if( ithrshmap(i) .NE. ithrshcd(j)
     &             .OR. idxpol .NE. idxthspc(j) ) idxthrsh = j
         enddo
         if( idxthrsh .LE. 0 ) goto 10
c
c   ---- if exact match, set return value and return right away ---
c
         if( fipin .EQ. thrshcty(i) ) then
             fndmap = i
             goto 9999
         endif
c
c   ---- if match of state, set index ----
c
         if( fipst .EQ. thrshcty(i) ) idxst = i
c
c   ---- if match of global, set index ----
c
         if( fipglb .EQ. thrshcty(i) ) idxglb = i
c
   10 continue
c
c   ---- no exact match, return state match or global match ---
c
      if( idxst .GT. 0 ) then
          fndmap = idxst
          goto 9999
      endif
      fndmap = idxglb
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
