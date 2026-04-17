C**** FNDLNG.F 
c
      function fndlng(idxcod,
     &                        lngin, lngar, nlngs )
      integer*4 fndlng
c
c-----------------------------------------------------------------------
c
c     This routine searches through an array for a match of the
c     "long FIPS"  code.  A global match of the FIPS code is acceptable.
c     A long FIPS code is a FIPS code together with a subregion code.
c
c        The index of FIPS/subregion matches are stored as follows:
c
c                idxmch      FIPS      sub
c                           ------    ------
c                  1        Global    Global
c                  2        Global    Exact
c                  3        State     Global
c                  4        State     Exact
c                  5        Exact     Global
c                  6        Exact     Exact
c
c   Arguments:
c     Outputs:
c       idxcod   I   array of indexes of matches
c     Inputs:
c       lngin    C   FIPS code to match
c       lngar    C   array of FIPS codes to search
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
      integer*4    idxcod(*)
      character*10 lngin
      integer*4    nlngs
      character*10 lngar(nlngs)
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c   lngglb   C   character string for global FIPS code
c
      character*10 lngglb(6)
      integer*4    idxfnd, idxmch(6), i, j
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
      fndlng = 0
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
      do i=1,nlngs
c
c   ---- loop through global FIPS, setting index value if found ---
c
         do j=1,6
            if( lngar(i) .EQ. lngglb(j) ) idxmch(j) = i
         enddo
c
c   ---- next array entry ----
c
      enddo
c
c   ---- return best match ----
c
      idxfnd = 0
      do i=6,1,-1
         if( idxmch(i) .GT. 0 ) then
             idxfnd = idxmch(i)
             goto 111
         endif
      enddo
      if( idxfnd .LE. 0 ) goto 9999
c
c  --- get all other hours with this match ---
c
 111  continue
      do 10 i=1,nlngs
         if( lngar(i) .NE. lngar(idxfnd) ) goto 10
         fndlng = fndlng + 1
         idxcod(fndlng) = i
 10   continue
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
