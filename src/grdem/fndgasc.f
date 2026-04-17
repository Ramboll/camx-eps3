C**** FNDGASC.F 
c
      function fndgasc(
     &                  fipin, asccd )
      integer*4 fndgasc
c
c-----------------------------------------------------------------------
c
c     This routine searches through the arrays of cross reference data
c     seaching for a match of the input data.  Any match of global
c     codes is kept until the end and the closest match is used. This
c     version is used for area source codes.
c
c      idxmch   FIPS      SUB     ASC(SCC)
c              ------    ------   --------
c        1     Global    Global    Global(1:10)
c        2     Global    Global    Global(3:10)
c        3     Global    Global    Global(5:10)
c        4     Global    Global    Global(8:10)
c        5     Global    Global    Exact 
c        6     Global    Exact     Global(1:10)
c        7     Global    Exact     Global(3:10)
c        8     Global    Exact     Global(5:10)
c        9     Global    Exact     Global(8:10)
c       10     Global    Exact     Exact  
c       11     State     Global    Global(1:10)
c       12     State     Global    Global(3:10)
c       13     State     Global    Global(5:10)
c       14     State     Global    Global(8:10)
c       15     State     Global    Exact  
c       16     State     Exact     Global(1:10)
c       17     State     Exact     Global(3:10)
c       18     State     Exact     Global(5:10)
c       19     State     Exact     Global(8:10)
c       20     State     Exact     Exact  
c       21     Exact     Global    Global(1:10)
c       22     Exact     Global    Global(3:10)
c       23     Exact     Global    Global(5:10)
c       24     Exact     Global    Global(8:10)
c       25     Exact     Global    Exact  
c       26     Exact     Exact     Global(1:10)
c       27     Exact     Exact     Global(3:10)
c       28     Exact     Exact     Global(5:10)
c       29     Exact     Exact     Global(8:10)
c       30     Exact     Exact     Exact  
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
      include 'hash.inc'
      include 'grdemsrg.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 fipin
      character*10 asccd
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c  fndchr      I  returns the index of a string in an array of strings
c  mkhash_pair I  returns the hash value for the FIPS/ASC pair
c
      integer*4 fndchr
      integer*4 mkhash_pair
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   fipglb   C   character string for global FIPS code
c   idxmch   I   array of indexes of matches
c
      character*10 fipglb(6), ascglb(5)
      integer*4    idxmch(30), istart, idxlng, idxhash, idxfrst
      integer*4    idxnext, iasc, ifip, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data fipglb /'00000     ','00000     ','00000     ',
     &             '00000     ','00000     ','00000     '/
      data ascglb/'0000000000','0000000000','0000000000','0000000000',
     &            '0000000000'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value, and value for matching state ----
c
      fndgasc = 0
      do i=1,30
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
c  --- set up arrays for comparing the ASC codes ----
c
      ascglb(2)(1:2) = asccd(1:2)
      ascglb(3)(1:4) = asccd(1:4)
      ascglb(4)(1:7) = asccd(1:7)
      ascglb(5) = asccd
c
c   ---- loop over possible matches of ASC code ---
c
      do iasc=1,5
c
c   ---- loop over possible matches of ASC code ---
c
         do 10 ifip=1,6
c
c   ---- find the hash value for this pair ---
c
            idxfrst = mkhash_pair( fipglb(ifip)(1:5), ascglb(iasc) )
            idxhash = idxhsh_xrf_first_pair(idxfrst)
c
c   ---- if this index is not initialized, we are at
c        the end of the hash entries for this pair ----
c
 111        continue
            if( idxhash .EQ. IMISS ) goto 10
c
c   ---- set the starting value in match arrays ---
c
            if( sccxrf(idxhash) .EQ. ascglb(iasc) ) then
                istart = iasc
c
c  --- no match here, get next entry for this hash ----
c
            else
                idxnext = idxhsh_xrf_next_pair(idxhash)
                idxhash = idxnext
                goto 111
            endif
c
c   ---- get index of global FIPS array ----
c
            if( fipxrf(idxhash) .EQ. fipglb(ifip) ) then
               idxmch((ifip-1)*5+istart ) = idxhash
            else
c
c   ---- get next entry for this hash ----
c
              idxnext = idxhsh_xrf_next_pair(idxhash)
              idxhash = idxnext
              goto 111
            endif
c
c   --- next global FIPS code ---
c
   10   continue
c
c   --- next global ASC code ---
c
      enddo
c
c   ---- loop through the array of matches for closest match ---
c
  222 continue
      do i=30,1,-1
        if( idxmch(i) .GT. 0 ) then
            fndgasc = idxmch(i)
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
