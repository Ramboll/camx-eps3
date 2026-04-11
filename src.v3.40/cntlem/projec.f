C**** PROJRC.F 
c
      subroutine projec( idxasc )
c
c-----------------------------------------------------------------------
c
c    Applies growth factors to get the projected emissions.  The record
c    type is checked, and either the PPT or PAM control factors are
c    applied to the matching SIC code.  For point sources the 2 digit
c    SIC code is used to match controls.  For area sources the entire
c    ASC code is checked for a match and if not found a check is made
c    for match on 4 digit SIC code.
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
      include 'ctlreg.inc'
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
c  fndlcd  I  returns the index of a "long FIPS" code and character
c             code in dual arrays in arrays
c
      integer*4 fndlcd
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   curfip  C   current embrdat fip/subregion code
c
      character*10  curfip, tmcd
      integer*4     idxcod, i
      real*4        factor(MXSPEC)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- concat fip and subregion code ---
c
      curfip(1:5) = fipin
      curfip(6:10) = sbrgn
c
c  --- check record type ---
c
      if( rectyp .EQ. 'E' .OR. rectyp .EQ. 'L') then
c
c  --- /PROJECT PTS/ ---
c
          if( ncdppt .GT. 0 ) then
c
c  --- try to match 4, 3 or 2 digit SIC code  ---
c
             idxcod = fndlcd (
     &                     curfip, siccd, 4, pptfip, pptsic, ncdppt )
             if( idxcod .LE. 0)
     &          idxcod = fndlcd (
     &                     curfip, siccd, 3, pptfip, pptsic, ncdppt )
             if( idxcod .LE. 0)
     &          idxcod = fndlcd (
     &                     curfip, siccd, 2, pptfip, pptsic, ncdppt )
c
c  --- if neither 4,3,2 digit SIC codes match,
c                                  then try 8 digit SCC match  ---
c
             if( idxcod .LE. 0) then
                tmcd(1:10) = scccd(1:10)
                call lftjst( tmcd )
                idxcod = fndlcd (
     &                        curfip, tmcd, 8, pptfip, pptsic, ncdppt )
             endif
c
c  --- if found a match, apply factors ---
c
             if( idxcod .GT. 0 ) then
c
c  --- check for correct span of hours ---
c
                  ctlflg(idxasc)(IFLPRJ:IFLPRJ) = 'X'
                  do i=1,MXCRIT
                    factor(i) = pptfac(idxcod)
                  enddo
                  call aplctl(1,MXHOUR,factor)
             endif
          endif
c
c  --- /PROJECT AMS/ ---
c
      else
          if( ncdpam .GT. 0 ) then
c
c  --- try match on ASC code ---
c
              idxcod = fndlcd (
     &                    curfip, scccd, 10, pamfip, pamsic, ncdpam )
              if( idxcod .LE. 0 ) then
c
c  --- no match on ASC code, try match on sic code ---
c
                  idxcod = fndlcd (
     &                    curfip, siccd, 4, pamfip, pamsic, ncdpam )
              endif
c
c  --- if no SIC match, try to fit 1st 4 characters of ASC code ---
c
              if( idxcod .LE. 0 ) then
                 idxcod = fndlcd (
     &                       curfip, scccd, 4, pamfip, pamsic, ncdpam )
              endif
c
c  --- if found a match, apply factors ---
c
              if( idxcod .GT. 0 ) then
c
c  --- check for correct span of hours ---
c
                  ctlflg(idxasc)(IFLPRJ:IFLPRJ) = 'X'
                  do i=1,MXCRIT
                     factor(i) = pamfac(idxcod)
                  enddo
                  call aplctl(1,MXHOUR,factor)
              endif
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
