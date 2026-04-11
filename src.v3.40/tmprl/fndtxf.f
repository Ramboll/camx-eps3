C**** FNDTXF.F 
c
      subroutine fndtxf( idxtpl,
     &                         fipsin, plntin, stack, pntin, sccin )
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
c     Outputs:
c       idxtpl   I   array by species of index into match
c     Inputs:
c       fipsin   C   FIPS code to match
c       plntin   C   NEDS plant id code to match
c       stack    I   stack id code to match
c       pntin    C   point id code
c       sccin    C   ASC-SCC code to match
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
      include 'embrdat.inc'
      include 'hash.inc'
      include 'tmprlprf.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4    idxtpl(MXCRIT)
      character*10 fipsin
      character*10 plntin
      character*10 stack
      character*10 pntin
      character*10 sccin
c
c-----------------------------------------------------------------------
c  External functions:
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
c    BLANK    C   blank string for comparison
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
c   idxmch   I   array of indexes of matches
c
      character*10 fipglb(6), ascglb(2)
      integer*4    idxmch(MXCRIT,24), idxpol, istart, idxhash, idxfrst
      integer*4    idxnext, iasc, ifip, i
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
      do idxpol=1,ncrthd
         idxtpl(idxpol) = 0
         do i=1,24
          idxmch(idxpol,i) = 0
        enddo
      enddo
c
c   ---- set up arrays for comparing long FIPS code ---
c
      fipglb(2)(6:10) = fipsin(6:10)
      fipglb(3)(1:2) = fipsin(1:2)
      fipglb(4)(1:2) = fipsin(1:2)
      fipglb(4)(6:10) = fipsin(6:10)
      fipglb(5)(1:5) = fipsin(1:5)
      fipglb(6) = fipsin
c
      ascglb(1) = BLANK
      ascglb(2) = sccin
c
c   --- loop over the possible matches for ASC code ---
c
      do iasc=1,2
c
c   ---- loop over possible matches of FIPS code ---
c
         do 10 ifip=1,6
c
c   ---- find the hash value for this pair ---
c
            idxfrst = mkhash_pair( fipglb(ifip)(1:5), ascglb(iasc) )
            idxhash = idxhsh_tpl_first_pair(idxfrst)
c
c   ---- if this index is not initialized, we are at
c        the end of the hash entries for this pair ----
c
 111        continue
            if( idxhash .EQ. IMISS ) goto 10
c
c   ---- check for all global ---
c
            if( sccxrf(idxhash) .EQ. BLANK .AND. 
     &               stkxrf(idxhash) .EQ. BLANK .AND.  
     &                        facxrf(idxhash) .EQ. BLANK .AND. 
     &                           fipxrf(idxhash) .EQ. fipglb(1) ) then
                do 20 idxpol=1,ncrthd
c
c   ---- skip this record if no data for this pollutant ---
c
                    if( imnxrf(idxhash,idxpol) .EQ. IMISS .OR.
     &                    iwkxrf(idxhash,idxpol) .EQ. IMISS .OR.
     &                       idyxrf(idxhash,idxpol) .EQ. IMISS ) goto 20
                    idxmch(idxpol,1) = idxhash
   20            continue
                 goto 10
            endif
c
            istart = 0
            if( sccxrf(idxhash) .EQ. BLANK ) then
               istart = 1
            else if( sccin .EQ. sccxrf(idxhash) ) then
               istart = 2
            else
c
c  --- no match here, get next entry for this hash ----
c
               idxnext = idxhsh_tpl_next_pair(idxhash)
               idxhash = idxnext
               goto 111
            endif
c
c   ---- see if it matches this FIPS code ---
c
            if( fipxrf(idxhash) .EQ. fipglb(ifip) ) then
c
c   --- if exact match on long FIPS code, check for global on plant
c       and stack ---
c
               do 30 idxpol=1,ncrthd
c
c   ---- skip this record if no data for this pollutant ---
c
                 if( imnxrf(idxhash,idxpol) .EQ. IMISS .OR.
     &                     iwkxrf(idxhash,idxpol) .EQ. IMISS .OR.
     &                       idyxrf(idxhash,idxpol) .EQ. IMISS ) goto 30
                 if( ifip .EQ. 5 .OR. ifip .EQ. 6 ) then
                    if( facxrf(idxhash) .EQ. BLANK .AND.
     &                  stkxrf(idxhash) .EQ. BLANK )
     &                    idxmch(idxpol, 8+(ifip-5)*2+istart ) = idxhash
                    if( facxrf(idxhash) .EQ. plntin .AND. 
     &                  stkxrf(idxhash) .EQ. BLANK )
     &                   idxmch(idxpol, 12+(ifip-5)*2+istart ) = idxhash
                    if( facxrf(idxhash) .EQ. plntin .AND. 
     &                    stkxrf(idxhash) .EQ. stack) then
                       if( pntxrf(idxhash) .EQ. BLANK ) then
                         idxmch(idxpol, 16+(ifip-5)*2+istart ) = idxhash
                       else if( pntxrf(idxhash) .EQ. pntin ) then
                         idxmch(idxpol, 20+(ifip-5)*2+istart ) = idxhash
                       endif
                    endif
                 else
                    idxmch(idxpol, (ifip-1)*2+istart ) = idxhash
                 endif
   30          continue
c
c   ---- get next entry for this hash ----
c
            else
               idxnext = idxhsh_tpl_next_pair(idxhash)
               idxhash = idxnext
               goto 111
            endif
c
c   --- next global FIPS ---
c
   10    continue
c
c   --- next global ASC ---
c
      enddo
c
c   ---- loop through the array of matches for closest match ---
c
      do i=24,1,-1
         do idxpol=1,ncrthd
            if( idxtpl(idxpol) .LE. 0 .AND. 
     &                            idxmch(idxpol,i) .GT. 0 ) then
                idxtpl(idxpol) = idxmch(idxpol,i)
            endif
         enddo
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
