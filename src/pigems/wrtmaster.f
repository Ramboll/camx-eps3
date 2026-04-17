C**** WRTMASTER.F version 2.0 09/01/2007
c
      subroutine wrtmaster(ierr)
c
c-----------------------------------------------------------------------
c
c    This routine writes the master stack list.
c
c    Argument Declaration.
c     Outputs:
c        ierr  I  error code
c     Inputs:
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
      include 'uamregn.inc'
      include 'embrdat.inc'
      include 'iopigems.inc'
      include 'usrpigems.inc'
      include 'stkpigems.inc'
      include 'pigemstot.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr    
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*10 keypts, name
      integer*4    inew, i
      real*4       diaout, emsval
      logical      lpigout
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- set the error flag ---
c
      ierr = IFAIL
c
c  --- Pigged sources are at the top ---
c
      lpigout = .TRUE.
c
c  --- sources that are to be combined ---
c
  111 continue
      do inew=1,nstknew
        if( lpignew(inew) .EQV. lpigout ) then
            keypts = 'NEW'
            write(IOWSTK,9000,ERR=7000) keypts, ctynew(inew), 
     &                facnew(inew), stidnew(inew), hgtnew(inew), 
     &                   dianew(inew), tptnew(inew), velnew(inew), 
     &                                  xlocnew(inew), ylocnew(inew)
            keypts = ' '
            do i=1,nstkcomb(inew)
               if( lcolonew(inew) ) then
                   write(IOWSTK,9001,ERR=7000) keypts, ctycomb(inew,i), 
     &               faccomb(inew,i), stidcomb(inew,i), hgtcomb(inew,i), 
     &                diacomb(inew,i), tptcomb(inew,i), velcomb(inew,i), 
     &                               xloccomb(inew,i), yloccomb(inew,i),
     &                         crtnam(idxcoloc),crtcomb(inew,i,idxcoloc)
               else
                   write(IOWSTK,9001,ERR=7000) keypts, ctycomb(inew,i), 
     &               faccomb(inew,i), stidcomb(inew,i), hgtcomb(inew,i), 
     &                diacomb(inew,i), tptcomb(inew,i), velcomb(inew,i), 
     &                               xloccomb(inew,i), yloccomb(inew,i)
               endif
               if( lpignew(inew) ) ntotstk(ifilenew(inew,i),ICDPIG) =
     &                              ntotstk(ifilenew(inew,i),ICDPIG) + 1
            enddo
        endif
      enddo
c
c  --- single sources ---
c
      do i=1,nstklist
        if( .NOT. lcolocat(i) .AND. lpiglist(i) .EQV. lpigout ) then
            keypts = 'ORIGINAL'
            diaout = dialist(i)
            if( lpigout ) then
                diaout = -ABS( dialist(i) )
                ntotstk(ifilestk(i),ICDPIG) =
     &                            ntotstk(ifilestk(i),ICDPIG) + 1
            endif
            if( lpiglist(i) ) then
                write(IOWSTK,9001,ERR=7000) keypts, ctylist(i), 
     &                faclist(i), stklist(i), hgtlist(i), 
     &                   diaout, tptlist(i), vellist(i), 
     &                           xloclist(i), yloclist(i),
     &                  crtnam(idxpigcrt(i)), emsmax(i,idxpigcrt(i))
            else
                write(IOWSTK,9001,ERR=7000) keypts, ctylist(i), 
     &                faclist(i), stklist(i), hgtlist(i), 
     &                   diaout, tptlist(i), vellist(i), 
     &                           xloclist(i), yloclist(i)
            endif
        endif
      enddo
c
c  --- changew the pig flag and do it again ---
c
      if( lpigout ) then
          lpigout = .FALSE.
          goto 111
      endif
c
c  --- set error flag and return ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                'ERROR:  Writing master stack header file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',
     &                                  masterfl(:strmin(masterfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(4(A10,1X),F10.1,1X,F10.3,1X,4(F10.0,1X))
 9001 format(4(A10,1X),F10.1,1X,F10.3,1X,4(F10.0,1X),A10,1X,F10.3)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
