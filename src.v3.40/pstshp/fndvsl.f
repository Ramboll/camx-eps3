C**** FNDVSL.F 
c
      function fndvsl(
     &                  asccd, vessel )
      integer*4 fndvsl
c
c-----------------------------------------------------------------------
c
c     This routine searches through the arrays of Vessel/Stack cross 
c     reference data searching for a match of the input data.  If the
c     vessel type is an exact match, this will override any matches
c     of the ASC code. 
c     The best match will follow the following heirarchy:
c
c      idxmch   Vessel      ASC(SCC)
c              ------    --------
c        1     Exact     Exact
c        2     Exact     Global(8:10)
c        3     Exact     Global(5:10)
c        4     Exact     Global(3:10)
c        5     Exact     Global
c        6     Exact     Exact
c        7     Global    Global(8:10)
c        8     Global    Global(5:10)
c        9     Global    Global(3:10)
c        10    Global    Global
c
c   Arguments:
c     Inputs:
c       asccd     C   ASC-SCC code to match
c       vessel    C   FIPS code to match
c     Outputs:
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'layers.inc'
      include 'pstshpstk.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 asccd
      character*10 vessel
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c  fndchr   I  returns the index of a string in an array of strings
c
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c   BLNKVSL   C   character string for global (blank) vessel type
c
      character*10 BLNKVSL
c
      parameter( BLNKVSL = '          ' )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   fipglb   C   character string for global FIPS code
c   idxmch   I   array of indexes of matches
c
      character*10 ascglb(5)
      integer*4    idxmch(12), istart, idxvsl, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data ascglb/'0000000000','0000000000','0000000000','0000000000',
     &            '0000000000'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value, and values for matches ---
c
      fndvsl = 0
      do i=1,12
        idxmch(i) = 0
      enddo
c
c  --- set up arrays for comparing the ASC codes ----
c
      ascglb(2)(1:2) = asccd(1:2)
      ascglb(3)(1:4) = asccd(1:4)
      ascglb(4)(1:7) = asccd(1:7)
      ascglb(5) = asccd
c
c   ---- loop through the array of codes ---
c
      do 10 i=1,nvslmap
c
c   ---- set the starting value in match arrays,based on ASC match ---
c
         istart = 0
         istart = fndchr( ascmap(i), 10, ascglb, 5 )
         if( istart .EQ. 0 ) goto 10
c
c   ---- check the vessel type and set the offset ----
c
         if( vslmap(i) .EQ. vessel ) then
            idxvsl = 2
         else if( vslmap(i) .EQ. BLNKVSL ) then
            idxvsl = 1
         else
            goto 10
         endif
         idxmch((idxvsl-1)*5+istart ) = i
   10 continue
c
c   ---- loop through the array of matches for closest match ---
c
      do i=12,1,-1
        if( idxmch(i) .GT. 0 ) then
            fndvsl = idxmch(i)
            goto 9999
        endif
      enddo
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
