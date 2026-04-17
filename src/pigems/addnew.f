C**** ADDNEW.F version 2.0 09/01/2007
c
      subroutine addnew( ierr )
c
c-----------------------------------------------------------------------
c
c    This routine inserts all of the "NEW" stacks at the top of the stack
c    list.  Later, a routine will be called to sort the list to ensure
c    that all of the PiGs are at the top of the list.
c
c    Argument Declaration.
c     Outputs:
c       ierr  I error flag
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
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'iopigems.inc'
      include 'usrpigems.inc'
      include 'stkpigems.inc'
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
      integer*4 fndlist    
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 idxlst, i, j, k, l
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   ---- find the position of this stack in master list ---
c
      do i=1,nstknew
         idxlst = fndlist( ctynew(i), facnew(i), stidnew(i),  hgtnew(i), 
     &                                  dianew(i), tptnew(i), velnew(i), 
     &                                          xlocnew(i), ylocnew(i) )
c
c  --- now load the emissions for all stacks that are combined
c      to make this stack ---
c
         if( idxlst .GT. 0 ) then
            hgtlist(idxlst) = hgtnew(i)
            dialist(idxlst) = dianew(i)
            tptlist(idxlst) = tptnew(i)
            vellist(idxlst) = velnew(i)
            xloclist(idxlst) = xlocnew(i)
            yloclist(idxlst) = ylocnew(i)
            lpiglist(idxlst) = lpignew(i)
            do j=1,nspecs
              do k=1,MXHOUR
                emsstk(idxlst,j,k) = 0.
                do l=1,nstkcomb(i)
                  emsstk(idxlst,j,k) = emsstk(idxlst,j,k) + 
     &                                       spccomb(i,l,j,k)
                enddo
              enddo
            enddo
         endif
c
c  --- next "NEW" stack ---
c
      enddo
c
c  --- all files processed, return ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Creating the inventory ',
     &                                                          'file.'
      write(ioerr,'(A)',ERR=9999) 'Number of stacks exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSTKLST
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter ',
     &                                        'MXSTKLST and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(3(A10,1X),95X,A10,1X,100(:,F12.0))
 9000 format(T10,A,T30,2A)
 9001 format(T10,A,T30,2A,T60,A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
