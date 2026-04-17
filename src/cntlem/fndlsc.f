C**** FNDLSC.F 
c
      function fndlsc(
     &                lngin, sicin, sccin, lngar, sicar, sccar, nlngs )
      integer*4 fndlsc
c
c-----------------------------------------------------------------------
c
c     This routine searches through two arrays for a match of the
c     "long FIPS" and ASC(SCC) code pair.  A global match of the
c     FIPS code is acceptable.  A long FIPS code is a FIPS code together
c     with a subregion code.
c
c        The index of FIPS/subregion matches are stored as follows:
c
c                idxmch      FIPS      sub    ASC(SCC)   SIC
c                           ------    ------   ------   -----
c                  1        Global    Global    Exact   Global
c                  2        Global    Global    Global  Exact
c                  3        Global    Global    Exact   Exact
c                  4        Global    Exact     Exact   Global
c                  5        Global    Exact     Global  Exact
c                  6        Global    Exact     Exact   Exact
c                  7        State     Global    Exact   Global
c                  8        State     Global    Global  Exact
c                  9        State     Global    Exact   Exact
c                 10        State     Exact     Exact   Global
c                 11        State     Exact     Global  Exact
c                 12        State     Exact     Exact   Exact
c                 13        Exact     Global    Exact   Global
c                 14        Exact     Global    Global  Exact
c                 15        Exact     Global    Exact   Exact
c                 16        Exact     Exact     Exact   Global
c                 17        Exact     Exact     Global  Exact
c                 18        Exact     Exact     Exact   Exact
c
c   Arguments:
c
c     Inputs:
c       lngin    C   FIPS code to match
c       sccin    C   SCC code to match
c       sicin    C   SIC code to match
c       lngar    C   array of FIPS codes to search
c       sccar    C   array of SCC codes to search
c       sicar    C   array of SIC codes to match
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
      character*4  sicin
      integer*4    nlngs
      character*10 lngar(nlngs)
      character*10 sccar(nlngs)
      character*4  sicar(nlngs)
c
c-----------------------------------------------------------------------
c   Local Parameters:
c-----------------------------------------------------------------------
c
c  BLNSIC  C   blank string the length of SIC codes
c  BLNSCC  C   blank string the length of SIC codes
c
      character*10 BLNSCC
      character*4  BLNSIC
c
      parameter( BLNSCC = '          ' )
      parameter( BLNSIC = '    ' )
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c   lngglb   C   character string for global FIPS code
c
      character*10 lngglb(6)
      integer*4    idxmch(18), istart, i, j
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
c   ---- initialize return value ---
c
      fndlsc = 0
      do i=1,18
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
c   ---- set the starting value in match arrays ---
c
         if( sccin .EQ. sccar(i) .AND. sicar(i) .EQ. BLNSIC ) then
             istart = 1
         else if( sccar(i) .EQ. BLNSCC .AND. sicin .EQ. sicar(i) ) then
             istart = 2
         else if( sccin .EQ. sccar(i) .AND. sicin .EQ. sicar(i) ) then
             istart = 3
         else
             goto 10
         endif
c
c   ---- loop through global FIPS, setting index value if found ---
c
         do j=1,6
            if( lngar(i) .EQ. lngglb(j) ) idxmch( (j-1)*3+istart ) = i
         enddo
c
c   ---- next array entry ----
c
   10 continue
c
c   ---- return best match ----
c
      do i=18,1,-1
         if( idxmch(i) .GT. 0 ) then
             fndlsc = idxmch(i)
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
