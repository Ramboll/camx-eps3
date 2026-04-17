C**** CTLDSC.F 
c 
      subroutine ctldsc( idxasc )
c
c-----------------------------------------------------------------------
c
c    Applies "discretionary" controls as specified by the user.  The
c    glossary values for the SIC/SCC pair is retrieved and the control
c    arrays are searched for a match on the various codes.  If no match
c    is found, the emissions values are unchanged.
c
c    Argument Declaration.
c     Outputs:
c     Inputs:
c       idxasc I index of SIC/SCC pair in totals arrays
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
      include 'sccxref.inc'
      include 'hash.inc'
      include 'cntlem.inc'
      include 'ctldsc.inc'
      include 'ctltot.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 idxasc
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c  fndlng  I  returns the index of a "long FIPS" code in arrays
c  fndsrc  I  returns index match for /SOURCE CATEGORY/ controls
c  fndeaxct I  returns exact match for /SOURCE CATEGORY/ controls
c
      integer*4 fndlng
      integer*4 fndsrc
      integer*4 fndexact
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c
c  BLANK   C   blank string the length of 10
c
      character*10 BLANK
c
      parameter( BLANK  = '          ' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   curfip  C   current embrdat fip/subregion code
c
      character*10 curfip
      integer*4    nfound, idxcod(MXHOUR*5)
      integer*4    ihbeg, ihend, idxexact, idx, i
      real*4       factor(MXCRIT)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- concat FIP and subregion code ---
c
      curfip(1:5) = fipin
      curfip(6:10) = sbrgn
c
c  --- /SOURCE CATEGORY/ controls ---
c
      if( ncdsrc .GT. 0 ) then
c
c  --- if only doing exact matches (and this is not a point
c      source) look for exact match of source category ---
c
        if( lexact .AND. plant .EQ. BLANK .AND. 
     &                  stkid .EQ. BLANK .AND. point .EQ. BLANK ) then
c
c  --- only check if we know this code has some matches ----
c
            if( lcodctl(idxasc) ) then
                idxexact = fndexact( idxasc, curfip, scccd, 
     &                                                ibegin, iendin )
                if( idxexact .GT. 0 ) then
                   ctlflg(idxasc)(IFLSRC:IFLSRC) = 'X'
                   do i=1,ncrthd
                     if( emshr(i,ibegin) .GT. 0. ) emshr(i,ibegin) =
     &                            emshr(i,ibegin) * srcfac(i,idxexact)
                   enddo
                   do i=1,nspchd
                       if( spchr(i,ibegin) .GT. 0. ) spchr(i,ibegin) =
     &                    spchr(i,ibegin) * srcfac(ispcrt(i),idxexact)
                   enddo
                endif
            endif
c
c  --- try best match criteria ---
c
        else
           nfound = fndsrc ( idxcod,
     &                curfip, plant, stkid, point, siccd, scccd )
           if( nfound .GT. 0 ) then
c
c  --- check for correct span of hours ---
c
               do idx=1,nfound
                 ihbeg = ihrsrc(idxcod(idx),1)
                 ihend = ihrsrc(idxcod(idx),2)
                 if( ihend .GE. ibegin+1 .OR. ihbeg .LE. iendin ) then
                    ctlflg(idxasc)(IFLSRC:IFLSRC) = 'X'
                    do i=1,ncrthd
                      factor(i) = srcfac(i,idxcod(idx))
                    enddo
                    call aplctl(ihbeg,ihend,factor)
                 endif
              enddo
           endif
        endif
      endif
c
c  --- /FIPS CODE/ controls ---
c
      if( ncdcty .GT. 0 ) then
        nfound = fndlng ( idxcod,
     &                           curfip, ctyfip, ncdcty )
        if( nfound .GT. 0 ) then
c
c  --- check for correct span of hours ---
c
           do idx=1,nfound
             ihbeg = ihrcty(idxcod(idx),1)
             ihend = ihrcty(idxcod(idx),2)
             if( ihend .GE. ibegin+1 .OR. ihbeg .LE. iendin ) then
                ctlflg(idxasc)(IFLCTY:IFLCTY) = 'X'
                do i=1,MXCRIT
                   factor(i) = ctyfac(i,idxcod(idx))
                enddo
                call aplctl(ihbeg,ihend,factor)
             endif
           enddo
        endif
      endif
c
c   --- set error flag to sucess ---
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
