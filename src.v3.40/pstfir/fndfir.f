C**** FNDFIR.F
c
      function fndfir(
     &                 fipin, stkdia, utme, utmn )
      integer*4 fndfir
c
c-----------------------------------------------------------------------
c
c     This routine searches through the stack arrays for a match with
c     data in the argument list.  If found, the index of the stack in
c     the array is returned. A return value of zero indicates
c     no match was made.
c
c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code of source
c       stkdia   R   stack diameter
c       utme     R   UTM easting coordinate
c       utmn     R   UTM northing coordinate
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
      include 'cntylst.inc'
      include 'pstfirstk.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      character*5  fipin
      real*4      stkdia
      real*4      utme
      real*4      utmn
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
      integer*4 idxfip, i, j
      logical*4 lfnd1, lfnd2, lwarn
c
c-----------------------------------------------------------------------
c   Data and save statements:
c-----------------------------------------------------------------------
c
      data lwarn /.FALSE./
      save lwarn
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndfir = 0
      if( nstack .EQ. 0 ) goto 9999
c
c   ---- find the index of county in array ---
c
      idxfip = fndchr ( fipin, 5, fipcd, ncdfip )
c
c   ---- loop through the array ---
c
      do i=1,nstack
c
c   ---- if match, set return value and return right away ---
c
         lfnd1  = .TRUE.
         lfnd2  = .TRUE.
         if( fipin .NE. infips(i) ) lfnd1 = .FALSE.
         if( stkdia .NE. stdiam(i) ) lfnd2 = .FALSE.
         if( utme .NE. xloc(i) ) lfnd2 = .FALSE.
         if( utmn .NE. yloc(i) ) lfnd2 = .FALSE.
c
         if( lfnd1 .AND. lfnd2 ) then
             fndfir = i
             goto 9999
         endif
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
