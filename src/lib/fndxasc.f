C**** FNDXASC.F 
c
      function fndxasc(
     &                  fipin, asccd )
      integer*4 fndxasc
c
c-----------------------------------------------------------------------
c
c     This routine searches through the arrays of cross reference data
c     seaching for a match of the input data.  Any match of global
c     codes is kept until the end and the closest match is used. This
c     version is used for area source codes.
c
c      idxmch   FIPS      ASC(SCC)
c              ------    --------
c        1     Global    Global(1:10)
c        2     Global    Global(3:10)
c        3     Global    Global(5:10)
c        4     Global    Global(8:10)
c        5     Global    Exact 
c        6     State     Global(1:10)
c        7     State     Global(3:10)
c        8     State     Global(5:10)
c        9     State     Global(8:10)
c       10     State     Exact  
c       11     Exact     Global(1:10)
c       12     Exact     Global(3:10)
c       13     Exact     Global(5:10)
c       14     Exact     Global(8:10)
c       15     Exact     Exact  
c
c   Arguments:
c     Inputs:
c       fipin    C   FIPS code to match
c       asccd    C   ASC-SCC code to match
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
      include 'sccxref.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*5  fipin
      character*10 asccd
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
c   Local variables:
c-----------------------------------------------------------------------
c
c   fipglb   C   character string for global FIPS code
c   idxmch   I   array of indexes of matches
c
      character*10 ascglb(5)
      character*5  fipglb(3)
      integer*4    idxmch(15), istart, idxfip, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data fipglb /'00000','00000','00000'/
      data ascglb/'0000000000','0000000000','0000000000','0000000000',
     &            '0000000000'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value, and value for matching state ----
c
      fndxasc = 0
      do i=1,15
        idxmch(i) = 0
      enddo
c
c   ---- set up arrays for comparing long FIPS code ---
c
      fipglb(2)(1:2) = fipin(1:2)
      fipglb(3) = fipin
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
      do 10 i=1,npfxrf
c
c   ---- set the starting value in match arrays ---
c
         istart = 0
         istart = fndchr( sccxrf(i), 10, ascglb, 5 )
         if( istart .EQ. 0 ) goto 10
c
c   ---- get index of global FIPS array ----
c
         idxfip = fndchr( fipxrf(i), 5, fipglb, 3 )
         if( idxfip .GT. 0 ) then
             idxmch((idxfip-1)*5+istart ) = i
         endif
   10 continue
c
c   ---- loop through the array of matches for closest match ---
c
      do i=15,1,-1
        if( idxmch(i) .GT. 0 ) then
            fndxasc = idxmch(i)
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
