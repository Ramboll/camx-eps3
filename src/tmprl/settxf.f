C**** SETTXF.F 
c
      function settxf(
     &                fipin, plant, stack, point, scccd )
      integer*4 settxf
c
c-----------------------------------------------------------------------
c
c     This routine searches through the arrays of cross reference data
c     seaching for an exact match of the input data.  If no match is
c     found a value of zero is returned.
c
c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code to match
c       plant    C   neds plant id code to match
c       stack    C   stack id code to match
c       point    C   point identification code
c       scccd    C   ASC-SCC code to match
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
      include 'hash.inc'
      include 'tmprlprf.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------

      character*10 fipin
      character*10 plant
      character*10 stack
      character*3  point
      character*10 scccd
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4   i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value, and value for matching state ----
c
      settxf = 0
c
c   ---- loop through the array of codes ---
c
      do 10 i=npfxrf,1,-1
c
c   ---- if ASC/SCC code is not exact match and code is not global,
c        it cannot be a match ----
c
         if( fipin .NE. fipxrf(i) ) goto 10
         if( plant .NE. facxrf(i) ) goto 10
         if( stack .NE. stkxrf(i) ) goto 10
         if( point .NE. pntxrf(i) ) goto 10
         if( scccd .NE. sccxrf(i) ) goto 10
c
c   ---- match was found ---
c
         settxf = i
         goto 9999
   10 continue
c
c   ---- no match, return zero ---
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
