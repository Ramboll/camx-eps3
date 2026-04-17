C**** FNDXCPT.F 
c 
      function fndxcpt(
     &                 fipin, plant, stack, pntid, scccd, crtin )
      integer*4 fndxcpt
c
c-----------------------------------------------------------------------
c
c     This routine searches through the arrays of cross reference data
c     seaching for a match of the input data.  We start the search
c     for the best match, as soon as we get a match we return.
c     It is assumed that no exact matches to plant code can be applied
c     without an exact match of FIPS code and no exact match of a
c     stack code can be applied without an exact match of plant code.
c     The criteria pollutant must always be an exact match.
c     In addition, the FIPS hierarchy and point source SCC hierarchy
c     are supported.  Global indicates a blank field or all zero code.
c
c     The best match hierarch is:
c        FIPS     Plant   Stack  Point   SCC       Pollutant
c        --------------------------------------------------
c        Exact    Exact   Exact  Exact   Exact       Exact
c        Exact    Exact   Exact  Exact   Global      Exact
c        Exact    Exact   Exact  Global  Exact       Exact
c        Exact    Exact   Exact  Global  Global      Exact
c        Exact    Exact   Global Global  Exact       Exact
c        Exact    Exact   Global Global  Global      Exact
c        Exact    Global  Global Global  Exact       Exact
c        Exact    Global  Global Global  SCC[1:6]    Exact
c        Exact    Global  Global Global  SCC[1:3]    Exact
c        Exact    Global  Global Global  SCC[1:1]    Exact
c        Exact    Global  Global Global  Global      Exact
c        State    Global  Global Global  Exact       Exact
c        State    Global  Global Global  SCC[1:6]    Exact
c        State    Global  Global Global  SCC[1:3]    Exact
c        State    Global  Global Global  SCC[1:1]    Exact
c        State    Global  Global Global  Global      Exact
c        Global   Global  Global Global  Exact       Exact
c        Global   Global  Global Global  SCC[1:6]    Exact
c        Global   Global  Global Global  SCC[1:3]    Exact
c        Global   Global  Global Global  SCC[1:1]    Exact
c        Global   Global  Global Global  Global      Exact

c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code to match
c       plant    C   NEDS plant id code to match
c       stack    C   stack id code to match
c       pntid    C   point id code to match
c       scccd    C   ASC-SCC code to match
c       crtin    C   criteria pollutant code to match
c
c-----------------------------------------------------------------------
c   LOG:
c           mjimenez  Wed Apr 23 09:09:56 PDT 2008
c             Started with library routine fndxrf
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

      character*5  fipin
      character*10 plant
      character*10 stack
      character*10 pntid
      character*10 scccd
      character*10 crtin
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
c    BLANK   C   blank string
c
      character*10 BLANK
c
      parameter( BLANK = '          ' )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   fipglb   C   character string for global FIPS code
c   nexact   I   index of exact match
c   nxtbst   I   index of next best match
c
      character*10 sccglb(5), sccptg
      character*5  fipglb(3)
      integer*4    nexact, nxtbst, idxmch(15), istart, idxfip, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data fipglb /'00000','00000','00000'/
      data sccptg /'  00000000'/
      data sccglb/'0000000000','  00000000','  00000000','  00000000',
     &            '  00000000'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ---
c
      fndxcpt = 0
      nexact = 0
      nxtbst = 0
c
c  --- if the xref data has no point specific ids skip ahead ---
c
      if ( .NOT.lchkfac ) goto 111
c
c  ---- if doing point ID codes, look for a match ---
c
      if( lxrfpnt ) then
         do i=1,npfxrf
            if( pntxrf(i) .NE. BLANK ) then
              if( fipin .EQ. fipxrf(i) .AND. plant .EQ. facxrf(i) .AND. 
     &            stack .EQ. stkxrf(i) .AND. pntid .EQ. pntxrf(i) .AND.
     &            crtin .EQ. crtxrf(i) )  then
                     if ( scccd .EQ. sccxrf(i) )then
                          fndxcpt = i
                          goto 9999
                     else if ( sccxrf(i) .EQ. sccptg .OR.
     &                         sccxrf(i) .EQ. BLANK )  then
                          nxtbst = i
                     endif
              endif
            endif
         enddo
         if ( nxtbst .GT. 0 ) then
            fndxcpt = nxtbst
            goto 9999
         endif
      endif
c
c     --- check for match through stack id ---
c
      do i=1,npfxrf
         if( fipin .EQ. fipxrf(i) .AND. plant .EQ. facxrf(i) .AND. 
     &       stack .EQ. stkxrf(i) .AND.  crtin .EQ. crtxrf(i) )  then
             if ( scccd .EQ. sccxrf(i) )then
                  fndxcpt = i
                  goto 9999
             else if ( sccxrf(i) .EQ. sccptg .OR.
     &                 sccxrf(i) .EQ. BLANK )  then
                  nxtbst = i
             endif
         endif
      enddo
      if ( nxtbst .GT. 0 ) then
         fndxcpt = nxtbst
         goto 9999
      endif
c
c     --- check for match through plant id ---
c
      do i=1,npfxrf
         if( fipin .EQ. fipxrf(i) .AND. plant .EQ. facxrf(i) .AND. 
     &       BLANK .EQ. stkxrf(i) .AND.  crtin .EQ. crtxrf(i) )  then
                if ( scccd .EQ. sccxrf(i) )then
                     fndxcpt = i
                     goto 9999
                else if ( sccxrf(i) .EQ. sccptg .OR.
     &                    sccxrf(i) .EQ. BLANK )  then
                     nxtbst = i
                endif
         endif
      enddo
      if ( nxtbst .GT. 0 ) then
         fndxcpt = nxtbst
         goto 9999
      endif
c
c     --- no point-specific data lookup starts here ---
  111 continue
c
c     --- initialize return value, and value for matching state ---
c
      do i=1,15
        idxmch(i) = 0
      enddo
c
c     --- set up arrays for comparing FIPS code ---
c
      fipglb(2)(1:2) = fipin(1:2)
      fipglb(3) = fipin
c
c     --- set up arrays for comparing the SCC codes       ---
c     --- note that the char index assumes leading blanks ---
c     --- for point SCC (since right justified on read)   ---
c
      sccglb(2)(3:3) = scccd(3:3)
      sccglb(3)(3:5) = scccd(3:5)
      sccglb(4)(3:8) = scccd(3:8)
      sccglb(5) = scccd
c
c     --- loop through the array of codes ---
c
      do 200 i=1,npfxrf
c
c        --- exact pollutant code match required ---
c
         if ( crtin .NE. crtxrf(i) ) goto 200
c
c        --- set the starting value in match arrays ---
c
         istart = 0
         istart = fndchr( sccxrf(i), 10, sccglb, 5 )
         if( istart .EQ. 0 ) goto 200
c
c        --- get index of global FIPS array ----
c
         idxfip = fndchr( fipxrf(i), 5, fipglb, 3 )
         if( idxfip .GT. 0 ) then
             idxmch((idxfip-1)*5+istart ) = i
         endif
  200 continue
c
c     --- loop through the array of matches for closest match ---
c
      do i=15,1,-1
        if( idxmch(i) .GT. 0 ) then
            fndxcpt = idxmch(i)
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
