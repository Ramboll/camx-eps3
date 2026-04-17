C**** APLCTL.F 
c
      subroutine aplctl(ihbeg,ihend,factor)
c
c-----------------------------------------------------------------------
c
c    This routine is responsible for applying the controls.  The hourly
c    emissions are stored in an global array to be written to EMBR later.
c
c    Argument Declaration.
c     Outputs:
c     Inputs:
c       ihbeg    I   beginning hour of control factor
c       ihend    I   ending hour of control factor
c       factor   R   control factor to apply
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
      include 'ctltot.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ihbeg
      integer*4 ihend
      real*4    factor(MXCRIT)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 ihr, i 
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      do ihr=MAX(ihbeg,ibegin),MIN(ihend,iendin)
        do i=1,ncrthd
          if( emshr(i,ihr) .GT. 0. .AND. factor(i) .GE. 0.) 
     &                        emshr(i,ihr) = emshr(i,ihr) * factor(i)
        enddo
        if( nspchd .GT. 0 ) then
          do i=1,nspchd
             if( spchr(i,ihr) .GT. 0. .AND. factor(ispcrt(i)) .GE. 0.) 
     &                spchr(i,ihr) = spchr(i,ihr) * factor(ispcrt(i))
          enddo
        endif
      enddo
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
      return
      end
