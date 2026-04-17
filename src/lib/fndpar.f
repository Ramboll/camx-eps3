C**** FNDPAR.F 
c  
      function fndpar(
     &                fipin, scccd, fipar, sccar, nfips )
      integer*4 fndpar
c
c-----------------------------------------------------------------------
c
c     This routine searches through two arrays for a match of the FIPS
c     and SCC code pair.  A global match of the fips code is acceptable
c
c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code to match
c       scccd    C   SCC code to match
c       fipar    C   array of FIPS codes to search
c       sccar    C   array of SCC codes to search
c       nfips    I   number of code in the array
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*5  fipin
      character*10 scccd
      integer*4    nfips
      character*5  fipar(nfips)
      character*10 sccar(nfips)
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   fipglb   C   character string for global FIPS code
c   fipst    C   character string for FIPS code for match of state
c   idxglb   I   index of match of FIPS global code
c   idxst    I   index of match of FIPS state code
c
      character*5 fipglb, fipst
      integer*4   idxst, idxglb, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data fipglb /'00000'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value, and value for matching state ----
c
      fndpar = 0
      idxst = 0
      idxglb = 0
      fipst = fipin(1:2)//'000'
c
c   ---- loop through the array of codes ---
c
      do 10 i=1,nfips
c
c   ---- check for match of SCC code ---
c
         if( scccd .NE. sccar(i) ) goto 10
c
c   ---- match of global FIPS code ----
c
         if( fipglb .EQ. fipar(i) ) then
             idxglb = i
             goto 10
         endif
c
c   ---- match of state FIPS code ----
c
         if( fipst .EQ. fipar(i) ) then
             idxst = i
             goto 10
         endif
c
c   ---- exact match, set return value and exit ---
c
         if( fipin .EQ. fipar(i) ) then
             fndpar = i
             goto 9999
         endif
c
   10 continue
c
c   ---- check for match on state FIPS code ---
c
      fndpar = idxglb
      if( idxst .GT. 0 ) fndpar = idxst
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
