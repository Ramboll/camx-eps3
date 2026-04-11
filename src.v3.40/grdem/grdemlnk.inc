c**** GRDEMLNK.INC 
c
c-----------------------------------------------------------------------
c
c   Include file for link data used by the GRDEM program
c   the links are stored in a "linked" list. (oh, i kill myself!)
c
c-----------------------------------------------------------------------
c   Array bounds parameters:
c-----------------------------------------------------------------------
c
c   NLTYPE   I  number of link types
c
      integer*4 NLTYPE
c
      parameter( NLTYPE = 5   )
c
c-----------------------------------------------------------------------
c   Link type parameters:
c-----------------------------------------------------------------------
c
c   IDXAP    I  index airport link surrgoate
c   IDXHW    I  index highway link surrogate
c   IDXPT    I  index aquatic porst surrogate
c   IDXRR    I  index railroad link surrogate
c   ITYPAP   C  airport link surrgoate code
c   ITYPHW   C  hihgway link surrogate code
c   ITYPPT   C  aquatic porst surrogate code
c   ITYPRR   C  railroad link surrogate code
c
      integer*4 IDXAP
      integer*4 IDXDS
      integer*4 IDXHW
      integer*4 IDXPT
      integer*4 IDXRR
      character*3 TYPAP
      character*3 TYPDS
      character*3 TYPHW
      character*3 TYPPT
      character*3 TYPRR
c
      parameter( IDXHW = 1 )
      parameter( IDXRR = 2 )
      parameter( IDXAP = 3 )
      parameter( IDXPT = 4 )
      parameter( IDXDS = 5 )
      parameter( TYPHW = "101" )
      parameter( TYPRR = "102" )
      parameter( TYPAP = "103" )
      parameter( TYPPT = "104" )
      parameter( TYPDS = "105" )
c
c-----------------------------------------------------------------------
c   Link data common variables:
c-----------------------------------------------------------------------
c
c   frclnk   R  fraction of total link in cell
c   ibeglk   I  pointer to beginning of county in link array
c   icelnk   I  grid cell crossed by the link
c   dftyp    C  array of link type codes
c   iendlk   I  pointer to ending of county in link array
c   clktyp   C  array for storing each link type
c   nlink    I  number of links in stored in the arrays
c   nlncty   I  number of links for each county
c   tlink    R  total link length of each type in the county
c
      character*3 clktyp(MXLKCL)
      character*3 dftyp(NLTYPE)
      integer*4   ibeglk(MXFIP)
      integer*4   icelnk(MXLKCL)
      integer*4   iendlk(MXFIP)
      integer*4   nlink
      integer*4   nlncty(MXFIP)
      real*4      frclnk(MXLKCL)
      real*4      tlink(NLTYPE,MXFIP)
c
      common /lnkchr/ dftyp, clktyp
      common /lnkdat/ nlink, nlncty, ibeglk, iendlk, icelnk,
     &                frclnk, tlink
