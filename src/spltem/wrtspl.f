C**** WRTSPL.F
c
      subroutine wrtspl( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the output data for the SPLTEM module
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      08/14/97  --mmj--  original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospltem.inc'
      include 'spltem.inc'
      include 'wgtfrac.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) sname
      integer*4         jerr, isrdcd
      integer*4         nh, iw, ic, ix, iy
      integer*4         ijcell
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- loop through emission array and write data
c
      do 40 nh = 0, MXHOUR-1
c
         do 30 ic = 1, ncells
c
               ijcell = ijlst(ic)
               ix = int( ijcell/1000 )
               iy = mod( ijcell, 1000 )
c
               do 10 iw = 1, isrdcnt
                  if ( emslow(ic,nh,iw) .GT. 0. )  then
                     isrdcd = isrdlst(iw)
                     if ( strmin(srdlng(iw)) .GT. 1 ) then
                        sname  = srdlng(iw)
                     else
                        sname  = srdnam(iw)
                     endif
                     call wrtems ( jerr,
     &                            IOWDAT, nh, ix, iy, isrdcd, sname,
     &                                        emslow(ic,nh,iw) )
                     if ( jerr .NE. ISUCES ) goto 9999
                  endif
                  if ( lelev )  then
                     if ( emsele(ic,nh,iw) .GT. 0. )  then
                        isrdcd = isrdlst(iw)
                        if ( strmin(srdlng(iw)) .GT. 1 ) then
                           sname  = srdlng(iw)
                        else
                           sname  = srdnam(iw)
                        endif
                        call wrtems ( jerr,
     &                            IOWELE, nh, ix, iy, isrdcd, sname,
     &                                        emsele(ic,nh,iw) )
                     if ( jerr .NE. ISUCES ) goto 9999
                     endif
                  endif
   10          continue
   20       continue
   30    continue
   40 continue
c
c   --- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
