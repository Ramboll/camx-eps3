C**** FNDSRC.F 
c
      function fndsrc( idxcod,
     &                     lngin, plant, stkin, pntin, sicin, sccin )
      integer*4 fndsrc
c
c-----------------------------------------------------------------------
c
c     This routine searches through two arrays for a match of the
c     source identification data for the /SOURCE CATEGORY/ packet.
c     Global matches are acceptable.
c
c        The index of FIPS/Subregion matches are stored as follows:
c
c      idxmch   FIPS      SUB    Plant    Stack   Point  ASC(SCC)  SIC
c              ------    ------  -----    ------  -----  --------  ---
c        1     Global    Global  Global   Global  Global  Global1  Global
c        2     Global    Global  Global   Global  Global  Global2  Global
c        3     Global    Global  Global   Global  Global  Global3  Global
c        4     Global    Global  Global   Global  Global  Global4  Global
c        5     Global    Global  Global   Global  Global  Exact    Global
c        6     Global    Global  Global   Global  Global  Global1  Exact
c        7     Global    Global  Global   Global  Global  Global2  Exact
c        8     Global    Global  Global   Global  Global  Global3  Exact
c        9     Global    Global  Global   Global  Global  Global4  Exact
c       10     Global    Global  Global   Global  Global  Exact    Exact
c       11     Global    Exact   Global   Global  Global  Global1  Global
c       12     Global    Exact   Global   Global  Global  Global2  Global
c       13     Global    Exact   Global   Global  Global  Global3  Global
c       14     Global    Exact   Global   Global  Global  Global4  Global
c       15     Global    Exact   Global   Global  Global  Exact    Global
c       16     Global    Exact   Global   Global  Global  Global1  Exact
c       17     Global    Exact   Global   Global  Global  Global2  Exact
c       18     Global    Exact   Global   Global  Global  Global3  Exact
c       19     Global    Exact   Global   Global  Global  Global4  Exact
c       20     Global    Exact   Global   Global  Global  Exact    Exact
c       21     State     Global  Global   Global  Global  Global1  Global
c       22     State     Global  Global   Global  Global  Global2  Global
c       23     State     Global  Global   Global  Global  Global3  Global
c       24     State     Global  Global   Global  Global  Global4  Global
c       25     State     Global  Global   Global  Global  Exact    Global
c       26     State     Global  Global   Global  Global  Global1  Exact
c       27     State     Global  Global   Global  Global  Global2  Exact
c       28     State     Global  Global   Global  Global  Global3  Exact
c       29     State     Global  Global   Global  Global  Global4  Exact
c       30     State     Global  Global   Global  Global  Exact    Exact
c       31     State     Exact   Global   Global  Global  Global1  Global
c       32     State     Exact   Global   Global  Global  Global2  Global
c       33     State     Exact   Global   Global  Global  Global3  Global
c       34     State     Exact   Global   Global  Global  Global4  Global
c       35     State     Exact   Global   Global  Global  Exact    Global
c       36     State     Exact   Global   Global  Global  Global1  Exact
c       37     State     Exact   Global   Global  Global  Global2  Exact
c       38     State     Exact   Global   Global  Global  Global3  Exact
c       39     State     Exact   Global   Global  Global  Global4  Exact
c       40     State     Exact   Global   Global  Global  Exact    Exact
c       41     Exact     Global  Global   Global  Global  Global1  Global
c       42     Exact     Global  Global   Global  Global  Global2  Global
c       43     Exact     Global  Global   Global  Global  Global3  Global
c       44     Exact     Global  Global   Global  Global  Global4  Global
c       45     Exact     Global  Global   Global  Global  Exact    Global
c       46     Exact     Global  Global   Global  Global  Global1  Exact
c       47     Exact     Global  Global   Global  Global  Global2  Exact
c       48     Exact     Global  Global   Global  Global  Global3  Exact
c       49     Exact     Global  Global   Global  Global  Global4  Exact
c       50     Exact     Global  Global   Global  Global  Exact    Exact
c       51     Exact     Exact   Global   Global  Global  Global1  Global
c       52     Exact     Exact   Global   Global  Global  Global2  Global
c       53     Exact     Exact   Global   Global  Global  Global3  Global
c       54     Exact     Exact   Global   Global  Global  Global4  Global
c       55     Exact     Exact   Global   Global  Global  Exact    Global
c       56     Exact     Exact   Global   Global  Global  Global1  Exact
c       57     Exact     Exact   Global   Global  Global  Global2  Exact
c       58     Exact     Exact   Global   Global  Global  Global3  Exact
c       59     Exact     Exact   Global   Global  Global  Global4  Exact
c       60     Exact     Exact   Global   Global  Global  Exact    Exact
c       61     Exact     Global  Exact    Global  Global  Global1  Global
c       62     Exact     Global  Exact    Global  Global  Global2  Global
c       63     Exact     Global  Exact    Global  Global  Global3  Global
c       64     Exact     Global  Exact    Global  Global  Global4  Global
c       65     Exact     Global  Exact    Global  Global  Exact    Global
c       66     Exact     Global  Exact    Global  Global  Global1  Exact
c       67     Exact     Global  Exact    Global  Global  Global2  Exact
c       68     Exact     Global  Exact    Global  Global  Global3  Exact
c       69     Exact     Global  Exact    Global  Global  Global4  Exact
c       70     Exact     Global  Exact    Global  Global  Exact    Exact
c       71     Exact     Exact   Exact    Global  Global  Global1  Global
c       72     Exact     Exact   Exact    Global  Global  Global2  Global
c       73     Exact     Exact   Exact    Global  Global  Global3  Global
c       74     Exact     Exact   Exact    Global  Global  Global4  Global
c       75     Exact     Exact   Exact    Global  Global  Exact    Global
c       76     Exact     Exact   Exact    Global  Global  Global1  Exact
c       77     Exact     Exact   Exact    Global  Global  Global2  Exact
c       78     Exact     Exact   Exact    Global  Global  Global3  Exact
c       79     Exact     Exact   Exact    Global  Global  Global4  Exact
c       80     Exact     Exact   Exact    Global  Global  Exact    Exact
c       81     Exact     Global  Exact    Exact   Global  Global1  Global
c       82     Exact     Global  Exact    Exact   Global  Global2  Global
c       83     Exact     Global  Exact    Exact   Global  Global3  Global
c       84     Exact     Global  Exact    Exact   Global  Global4  Global
c       85     Exact     Global  Exact    Exact   Global  Exact    Global
c       86     Exact     Global  Exact    Exact   Global  Global1  Exact
c       87     Exact     Global  Exact    Exact   Global  Global2  Exact
c       88     Exact     Global  Exact    Exact   Global  Global3  Exact
c       89     Exact     Global  Exact    Exact   Global  Global4  Exact
c       90     Exact     Global  Exact    Exact   Global  Exact    Exact
c       91     Exact     Exact   Exact    Exact   Global  Global1  Global
c       92     Exact     Exact   Exact    Exact   Global  Global2  Global
c       93     Exact     Exact   Exact    Exact   Global  Global3  Global
c       94     Exact     Exact   Exact    Exact   Global  Global4  Global
c       95     Exact     Exact   Exact    Exact   Global  Exact    Global
c       96     Exact     Exact   Exact    Exact   Global  Global1  Exact
c       97     Exact     Exact   Exact    Exact   Global  Global2  Exact
c       98     Exact     Exact   Exact    Exact   Global  Global3  Exact
c       99     Exact     Exact   Exact    Exact   Global  Global4  Exact
c      100     Exact     Exact   Exact    Exact   Exact   Exact    Exact
c      101     Exact     Global  Exact    Exact   Exact   Global1  Global
c      102     Exact     Global  Exact    Exact   Exact   Global2  Global
c      103     Exact     Global  Exact    Exact   Exact   Global3  Global
c      104     Exact     Global  Exact    Exact   Exact   Global4  Global
c      105     Exact     Global  Exact    Exact   Exact   Exact    Global
c      106     Exact     Global  Exact    Exact   Exact   Global1  Exact
c      107     Exact     Global  Exact    Exact   Exact   Global2  Exact
c      108     Exact     Global  Exact    Exact   Exact   Global3  Exact
c      109     Exact     Global  Exact    Exact   Exact   Global4  Exact
c      110     Exact     Global  Exact    Exact   Exact   Exact    Exact
c      111     Exact     Exact   Exact    Exact   Exact   Global1  Global
c      112     Exact     Exact   Exact    Exact   Exact   Global2  Global
c      113     Exact     Exact   Exact    Exact   Exact   Global3  Global
c      114     Exact     Exact   Exact    Exact   Exact   Global4  Global
c      115     Exact     Exact   Exact    Exact   Exact   Exact    Global
c      116     Exact     Exact   Exact    Exact   Exact   Global1  Exact
c      117     Exact     Exact   Exact    Exact   Exact   Global2  Exact
c      118     Exact     Exact   Exact    Exact   Exact   Global3  Exact
c      119     Exact     Exact   Exact    Exact   Exact   Global4  Exact
c      120     Exact     Exact   Exact    Exact   Exact   Exact    Exact
c
c   Arguments:
c     Outputs:
c       idxcod   I   array of matches
c     Inputs:
c       lngin    C   FIPS code to match
c       plant    C   NEDS plant id
c       stkin    C   stack id code
c       pntin    C   input point id
c       sccin    C   SCC code to match
c       sicin    C   SIC code to match
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c   16Jan2014 mmj - Corrected idxmch offsets 
c                 - Added the look for next within FIPS/ASC match
c                   (Need to search through entire list to find best
c                    match)
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'hash.inc'
      include 'ctldsc.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4    idxcod(*)
      character*10 lngin
      character*10 plant
      character*10 stkin
      character*10 sccin
      character*4  sicin
      character*10 pntin
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   External functions: 
c-----------------------------------------------------------------------
c
c   mkhash_pair  I  returns the hash value for the FIPS/ASC pair
c
      integer*4 mkhash_pair                        
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c  BLANK   C   blank string the length of 10
c  BLNSIC  C   blank string the length of SIC codes
c
      character*10 BLANK 
      character*4  BLNSIC
c
      parameter( BLANK  = '          ' )
      parameter( BLNSIC = '    ' )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   lngglb   C   character string for global FIPS code
c
      character*10 lngglb(6), ascglb(5)
      integer*4    idxfnd, idxmch(120), istart, i, idxhash, idxfrst
      integer*4    idxnext, iasc, ifip
      logical*4    lfound
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data lngglb /'00000     ','00000     ','00000     ',
     &             '00000     ','00000     ','00000     '/
      data ascglb/'0000000000','0000000000','0000000000','0000000000',
     &            '0000000000'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ---
c
      fndsrc = 0
      do i=1,120
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
c  --- set up arrays for comparing the ASC codes ----
c
      ascglb(2)(1:2) = sccin(1:2)
      ascglb(3)(1:4) = sccin(1:4)
      ascglb(4)(1:7) = sccin(1:7)
      ascglb(5) = sccin
c
c   --- loop over the possible matches for ASC code ---
c
      do iasc=1,5
c
c   ---- loop over possible matches of FIPS code ---
c
         do 10 ifip=1,6
c
c   ---- find the hash value for this pair ---
c
            idxfrst = mkhash_pair( lngglb(ifip)(1:5), ascglb(iasc) )
            idxhash = idxhsh_src_first_pair(idxfrst)
c
c   ---- if this index is not initialized, we are at
c        the end of the hash entries for this pair ----
c
 111        continue
            if( idxhash .EQ. IMISS ) goto 10
c
c   ---- set the starting value in match arrays ---
c
            if( srcscc(idxhash) .EQ. ascglb(iasc) ) then
                istart = iasc
c
c  --- no match here, get next entry for this hash ----
c
            else
                idxnext = idxhsh_src_next_pair(idxhash)
                idxhash = idxnext
                goto 111
            endif
c
c  --- found a match on ASC, try to match SIC ---
c
            if( sicin .EQ. srcsic(idxhash) ) then
              istart = istart + 5
            else
              if( srcsic(idxhash) .NE. BLANK ) then
c
c  --- no match here either, get next entry for this hash ----
c
                idxnext = idxhsh_src_next_pair(idxhash)
                idxhash = idxnext
                goto 111
              endif
            endif
c
c  --- must be a match for ASC code, check for FIPS match ---
c
            if( srcfip(idxhash) .EQ. lngglb(ifip) ) then
c
c   --- if exact match on long FIPS code, check for global on plant
c       and stack ---
c
               if( ifip .EQ. 5 .OR. ifip .EQ. 6 ) then
                  if( srcplt(idxhash) .EQ. BLANK
     &                    .AND. stksrc(idxhash) .EQ. BLANK )
     &                        idxmch( 40+(ifip-5)*4+istart ) = idxhash
                  if( srcplt(idxhash) .EQ. plant
     &                       .AND. stksrc(idxhash) .EQ. BLANK )
     &                        idxmch( 60+(ifip-5)*4+istart ) = idxhash
                  if(srcplt(idxhash) .EQ. plant .AND. 
     &                                stksrc(idxhash) .EQ. stkin) then
                      if( srcpnt(idxhash) .EQ. BLANK ) then
                          idxmch( 80+(ifip-5)*4+istart ) = idxhash
                      else if( srcpnt(idxhash) .EQ. pntin ) then
                          idxmch( 100+(ifip-5)*4+istart ) = idxhash
                      endif
                  endif
c --- go to next list and check match ---
                  idxnext = idxhsh_src_next_pair(idxhash)
                  idxhash = idxnext
                  goto 111 
               else
                  idxmch( (ifip-1)*4+istart ) = idxhash
               endif
c
c   ---- get next entry for this hash ----
c
            else 
               idxnext = idxhsh_src_next_pair(idxhash)
               idxhash = idxnext
               goto 111
            endif
   10    continue
c
c  --- get next match for ASC code ---
c
      enddo
c
c   ---- return best match ----
c
  222 continue
      idxfnd = 0
      do i=120,1,-1
         if( idxmch(i) .GT. 0 ) then
             idxfnd = idxmch(i)
             goto 333
         endif
      enddo
      if( idxfnd .LE.  0 ) goto 9999
c
c  --- get all other hours with this match ---
c
 333  continue
c
c   ---- find the hash value for the best-match record ---
c
      idxfrst = mkhash_pair( srcfip(idxfnd)(1:5), srcscc(idxfnd) )
      idxhash = idxhsh_src_first_pair(idxfrst)
c
c  --- check if finished with all hash entries for this pair ---
c
 444  continue
      if( idxhash .EQ. IMISS ) goto 9999
c
c  --- compare current entry to the best-match found ---
c
      lfound = .TRUE.
      if( srcfip(idxhash) .NE. srcfip(idxfnd) ) lfound = .FALSE.
      if( srcplt(idxhash) .NE. srcplt(idxfnd) ) lfound = .FALSE.
      if( stksrc(idxhash) .NE. stksrc(idxfnd) ) lfound = .FALSE.
      if( srcpnt(idxhash) .NE. srcpnt(idxfnd) ) lfound = .FALSE.
      if( srcscc(idxhash) .NE. srcscc(idxfnd) ) lfound = .FALSE.
      if( srcsic(idxhash) .NE. srcsic(idxfnd) ) lfound = .FALSE.
      if( lfound ) then
         fndsrc = fndsrc + 1
         idxcod(fndsrc) = idxhash
      endif
      idxnext = idxhsh_src_next_pair(idxhash)
      idxhash = idxnext
      goto 444
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
