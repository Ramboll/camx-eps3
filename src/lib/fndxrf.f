C**** FNDXRF.F 
c 
      function fndxrf(
     &                 fipin, plant, stack, pntid, scccd )
      integer*4 fndxrf
c
c-----------------------------------------------------------------------
c
c     This routine searches through the arrays of cross reference data
c     seaching for a match of the input data.  Any match of global
c     codes is kept until the end and the closest match is used. It
c     is assumed that no exact matches to plant code can be applied
c     without an exact match of FIPS code and no exact match of a
c     stack code can be applied without an exact match of plant code.
c     The index of a match is stored in an array so that the higher
c     the array element the closer the match.  The matches are stored
c     as follows:
c                                FIPS     Plant   Stack   ASC-SCC
c                               -------------------------------
c               idxmch(1)       Global    Global  Global  Exact
c               idxmch(2)       State     Global  Global  Exact
c               idxmch(3)       Exact     Global  Global  Exact
c               idxmch(4)       Exact     Exact   Global  Exact
c
c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code to match
c       plant    C   NEDS plant id code to match
c       stack    C   stack id code to match
c       pntid    C   point id code to match
c       scccd    C   ASC-SCC code to match
c
c-----------------------------------------------------------------------
c   LOG:
c
c         1. gwilson      Tue Apr 26 10:56:37 PDT 2005
c            Added point ID to lookup table
c
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

      character*5  fipin
      character*10 plant
      character*10 stack
      character*10 pntid
      character*10 scccd
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
c   idxmch   I   array of indexes of matches
c   fipglb   C   character string for global FIPS code
c   fipst    C   character string for FIPS code for match of state
c
      character*5 fipglb, fipst
      integer*4   idxmch(4), i
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
      fndxrf = 0
      fipst = fipin(1:2)//'000'
      do i=1,4
        idxmch(i) = 0
      enddo
c
c  ---- if doing point ID codes, look for a match ---
c
      if( lxrfpnt ) then
         do i=1,npfxrf
            if( pntxrf(i) .NE. BLANK ) then
              if( fipin .EQ. fipxrf(i) .AND. plant .EQ. facxrf(i) .AND. 
     &            stack .EQ. stkxrf(i) .AND. pntid .EQ. pntxrf(i) .AND.
     &                                      scccd .EQ. sccxrf(i) ) then
                   fndxrf = i
                   goto 9999
              endif
            endif
         enddo
      endif
c
c   ---- loop through the array of codes ---
c
      do 10 i=1,npfxrf
c
c   ---- check for match of ASC-SCC, if global ASC-SCC code then all
c        other codes must be exact ---
c
         if( scccd .NE. sccxrf(i) .OR. sccxrf(i) .EQ. BLANK ) goto 10
         if( sccxrf(i) .EQ. BLANK ) then
             if( plant .NE. facxrf(i) ) goto 10
             if( fipin .NE. fipxrf(i) ) goto 10
         endif
c
c   ---- check for match of FIPS code first global, the state then
c        exact ---
c
         if( fipglb .EQ. fipxrf(i) ) then
             idxmch(1) = i
             goto 10
         endif
         if( fipst .EQ. fipxrf(i) ) then
             idxmch(2) = i
             goto 10
         endif
         if( fipin .NE. fipxrf(i) ) goto 10
c
c   ---- FIPS is exact match so check the plant and stack codes,
c        first global plant, then exact plant ---
c
         if( facxrf(i) .EQ. BLANK ) then
             idxmch(3) = i
             goto 10
         endif
         if( plant .NE. facxrf(i) ) goto 10
         if( stkxrf(i) .EQ. BLANK ) then
             idxmch(4) = i
             goto 10
         endif
         if( stkxrf(i) .EQ. stack ) then
             fndxrf = i
             goto 9999
         endif
c
   10 continue
c
c   ---- loop through the array of matches for closest match ---
c
      do i=4,1,-1
         if( idxmch(i) .GT. 0 ) then
             fndxrf = idxmch(i)
             goto 9999
         endif
      enddo
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
