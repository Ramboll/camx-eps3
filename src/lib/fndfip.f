C**** FNDFIP.F 
c
      function fndfip(
     &                 fipin, nfipcd, fipcd)
      integer*4 fndfip
c
c-----------------------------------------------------------------------
c
c     This routine searches through an array of FIPS codes for a match
c     with the input FIPS code.  A search is made for an exact match,
c     a match of just the state, or a match of the global code "00000".
c     the return value is the index of the matched code in the input
c     array.  The return value will be the index of the exact match if
c     found, the index of the match of state if found and no exact match
c     was made or the index of the global code.  A return value of zero
c     indicates no match of any kind was made.
c
c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code to match
c       nfipcd   C   number of FIPS codes in the array to search
c       fipcd    C   array of FIPS codes to search
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(5) fipin
      integer*4     nfipcd
      character*(5) fipcd(*)
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   fipglb   C   variable for storing global code
c   fipst    C   variable for storing code to match state
c   idxglb   I   index of global code in array
c   idxst    I   index of match for state
c
      character*5 fipglb, fipst
      integer*4   idxglb, idxst, i
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
      fndfip = 0
      idxglb = 0
      idxst = 0
      fipst = fipin(1:2)//'000'
c
c   ---- loop through the array of codes ---
c
      do i=1,nfipcd
c
c   ---- if exact match, set return value and return right away ---
c
         if( fipin .EQ. fipcd(i) ) then
             fndfip = i
             goto 9999
         endif
c
c   ---- if match of state, set index ----
c
         if( fipst .EQ. fipcd(i) ) idxst = i
c
c   ---- if match of global, set index ----
c
         if( fipglb .EQ. fipcd(i) ) idxglb = i
c
      enddo
c
c   ---- no exact match, return state match or global match ---
c
      if( idxst .GT. 0 ) then
          fndfip = idxst
          goto 9999
      endif
      fndfip = idxglb
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
