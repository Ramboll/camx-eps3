C**** FNDFXF.F 
c
      function fndfxf(
     &                 fipin, plant, stkid, point, scccd, idxpol )
      integer*4 fndfxf
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
c
c      idxmch   FIPS      SUB    Plant    Stack   Point  ASC(SCC)
c              ------    ------  -----    ------  -----  --------
c        1     Global    Global  Global   Global  Global  Global
c        2     Global    Global  Global   Global  Global  Exact 
c        3     Global    Exact   Global   Global  Global  Global  
c        4     Global    Exact   Global   Global  Global  Exact  
c        5     State     Global  Global   Global  Global  Global  
c        6     State     Global  Global   Global  Global  Exact  
c        7     State     Exact   Global   Global  Global  Global  
c        8     State     Exact   Global   Global  Global  Exact  
c        9     Exact     Global  Global   Global  Global  Global  
c       10     Exact     Global  Global   Global  Global  Exact  
c       11     Exact     Exact   Global   Global  Global  Global  
c       12     Exact     Exact   Global   Global  Global  Exact  
c       13     Exact     Global  Exact    Global  Global  Global  
c       14     Exact     Global  Exact    Global  Global  Exact  
c       15     Exact     Exact   Exact    Global  Global  Global  
c       16     Exact     Exact   Exact    Global  Global  Exact  
c       17     Exact     Global  Exact    Exact   Global  Global  
c       18     Exact     Global  Exact    Exact   Global  Exact  
c       19     Exact     Exact   Exact    Exact   Global  Global  
c       20     Exact     Exact   Exact    Exact   Global  Exact  
c       21     Exact     Global  Exact    Exact   Exact   Global  
c       22     Exact     Global  Exact    Exact   Exact   Exact  
c       23     Exact     Exact   Exact    Exact   Exact   Global  
c       24     Exact     Exact   Exact    Exact   Exact   Exact  
c
c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code to match
c       plant    C   NEDS plant id code to match
c       stkid    C   stack id code to match
c       point    C   point id code
c       scccd    C   ASC-SCC code to match
c       idxpol   I   index of pollutant codes in pollutant arrays
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
      include 'tpfacprf.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 fipin
      character*10 plant
      character*10 stkid
      character*10 point
      character*10 scccd
      integer*4    idxpol
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
c    BLANK    C   blank string of same length as plant id for comparison
c
      character*10 BLANK
c
      parameter( BLANK  = '          ' )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   fipglb   C   character string for global FIPS code
c   idxmch   I   array of indexes of matches
c
      character*10 fipglb(6)
      integer*4    idxmch(24), istart, idxlng, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data fipglb /'00000     ','00000     ','00000     ',
     &             '00000     ','00000     ','00000     '/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value, and value for matching state ----
c
      fndfxf = 0
      do i=1,24
        idxmch(i) = 0
      enddo
c
c   ---- set up arrays for comparing long FIPS code ---
c
      fipglb(2)(6:10) = fipin(6:10)
      fipglb(3)(1:2) = fipin(1:2)
      fipglb(4)(1:2) = fipin(1:2)
      fipglb(4)(6:10) = fipin(6:10)
      fipglb(5)(1:5) = fipin(1:5)
      fipglb(6) = fipin
c
c   ---- loop through the array of codes ---
c
      do 10 i=npfxrf,1,-1
c
c   ---- skip this record if no data for this pollutant ---
c
cgwilson         if( imnxrf(i,idxpol) .EQ. IMISS .OR.
cgwilson     &                        iwkxrf(i,idxpol) .EQ. IMISS .OR.
cgwilson     &                           idyxrf(i,idxpol) .EQ. IMISS ) goto 10
c
c   ---- check for all global ---
c
         if( sccxrf(i) .EQ. BLANK .AND. stkxrf(i) .EQ. BLANK .AND.
     &       facxrf(i) .EQ. BLANK .AND. fipxrf(i) .EQ. fipglb(1) ) then
              idxmch(1) = i
              goto 10
         endif
c
c   ---- set the starting value in match arrays ---
c
         istart = 0
         if( sccxrf(i) .EQ. BLANK ) then
             istart = 1
         else if( scccd .EQ. sccxrf(i) ) then
             istart = 2
         else
             goto 10
         endif
c
c   ---- get index of global FIPS array ----
c
         idxlng = fndchr( fipxrf(i), 10, fipglb, 6 )
c
c   --- if exact match on long FIPS code, check for global on plant
c       and stack ---
c
         if( idxlng .EQ. 5 .OR. idxlng .EQ. 6 ) then
            if( facxrf(i) .EQ. BLANK .AND. stkxrf(i) .EQ. BLANK )
     &                             idxmch( 8+(idxlng-5)*2+istart ) = i
            if( facxrf(i) .EQ. plant .AND. stkxrf(i) .EQ. BLANK )
     &                            idxmch( 12+(idxlng-5)*2+istart ) = i
            if(facxrf(i) .EQ. plant .AND. stkxrf(i) .EQ. stkid) then
               if( pntxrf(i) .EQ. BLANK ) then
                   idxmch( 16+(idxlng-5)*2+istart ) = i
               else if( pntxrf(i) .EQ. point ) then
                       idxmch( 20+(idxlng-5)*2+istart ) = i
               endif
            endif
         else
            if( idxlng .GT. 0 ) idxmch( (idxlng-1)*2+istart ) = i
         endif
c
   10 continue
c
c   ---- loop through the array of matches for closest match ---
c
      do i=24,1,-1
         if( idxmch(i) .GT. 0 ) then
             fndfxf = idxmch(i)
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
