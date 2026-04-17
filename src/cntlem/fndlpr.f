C**** FNDLPR.F 
c
      function fndlpr(
     &                lngin, sccin, lngar, sccar, nlngs )
      integer*4 fndlpr
c
c-----------------------------------------------------------------------
c
c     This routine searches through an array for a match of the
c     "long FIPS"  code and ASC(SCC) code pair.  A global match of the
c     FIPS code is acceptable.
c     A long FIPS code is a FIPS code together with a subregion code.
c
c        The index of FIPS/subregion matches are stored as follows:
c
c                idxmch      FIPS      sub     ASC
c                           ------    ------  -----
c                  1        Global    Global  Exact
c                  2        Global    Exact   Exact
c                  3        State     Global  Exact
c                  4        State     Exact   Exact
c                  5        Exact     Global  Exact
c                  6        Exact     Exact   Exact
c
c   Arguments:
c
c     Inputs:
c       lngin    C   FIPS code to match
c       sccin    C   ASC(SCC) code to match
c       lngar    C   array of FIPS codes to search
c       sccar    C   array of ASC(SCC) code to search
c       nlngs    I   number of code in the array
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      character*10 lngin
      character*10 sccin
      integer*4    nlngs
      character*10 lngar(nlngs)
      character*10 sccar(nlngs)
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c   lngglb   C   character string for global FIPS code
c
      character*10 lngglb(6)
      integer*4    idxmch(6), i, j
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data lngglb /'00000     ','00000     ','00000     ',
     &             '00000     ','00000     ','00000     '/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndlpr = 0
      do i=1,6
        idxmch(i) = 0
      enddo
c
c   ---- set up arrays for comparing long FIPS code ---
c
      lngglb(2)(6:10) = lngin(6:10)
      lngglb(3)(1:2) = lngin(1:2)
      lngglb(4)(1:2) = lngin(1:2)
      lngglb(4)(6:10) = lngin(6:10)
      lngglb(5)(1:5) = lngin(1:5)
      lngglb(6) = lngin
c
c   ---- loop through the array of codes ---
c
      do 10 i=1,nlngs
c
c   ---- if ASC code does not match, skip entry ---
c
         if( sccin .NE. sccar(i) ) goto 10
c
c   ---- loop through global FIPS, setting index value if found ---
c
         do j=1,6
            if( lngar(i) .EQ. lngglb(j) ) idxmch(j) = i
         enddo
c
c   ---- next array entry ----
c
   10 continue
c
c   ---- return best match ----
c
      do i=6,1,-1
         if( idxmch(i) .GT. 0 ) then
             fndlpr = idxmch(i)
             goto 9999
         endif
      enddo
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
