C**** RDSTKMAX.F version 2.0 09/01/2007
c
      subroutine rdstkmax( ierr )
c
c-----------------------------------------------------------------------
c
c    This routine reads all of the stack emissions files and calculates
c    the maximum emissions for each stack over all of the days.
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
c   strmin  I   returns the actual length of a string
c   fndcomb I   returns the index of a stack in array of combined stacks
c   fndlist I   returns the index of a stack in array of master list
c   fndchr  I   returns the index of a string in a list of strings
c
      integer*4 strmin
      integer*4 fndcomb
      integer*4 fndlist
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) string, fname, action
      character*10      ctytmp, factmp, stidtmp, crttmp
      integer*4         irec, idxlist, ifile, idxnew, idxcomb, idxcrt
      integer*4         idattmp, i, j, k
      real*4            hgttmp, diatmp, veltmp, tpttmp, crtval
      real*4            xloctmp, yloctmp
      logical           lsame, lwarn
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      lwarn = .FALSE.
c
c  --- if there are no stack files, just exit ---
c
      if( nemsfl .LE. 0 ) then
         ierr = ISUCES
         goto 9999
      endif
c
c  --- initialize the max emissions arrays ---
c
      do i=1,nstklist
         do j=1,ncrthd
           emsmax(i,j) = -9.
         enddo 
      enddo
      do i=1,nstknew
         do j=1,nstkcomb(i)
            do k=1,ncrthd
               crtcomb(i,j,k) = -9.
            enddo
         enddo
      enddo
c
c  --- loop over all of the input stack list files ---
c
      do ifile=1,nemsfl
c
c   --- open the file ---
c
         fname = iemsfl(ifile)
         action = 'Opening input emissions files.'
         open(IOREMS,file=iemsfl(ifile),status='OLD',ERR=7003)
         write(IOWSTD,'(2A)') 'Reading: ',
     &                        iemsfl(ifile)(:strmin(iemsfl(ifile)))
         irec = 0
c
c  ---- read a record from the file and load into local variables ---
c
 111     continue
         irec = irec + 1
         read(IOREMS,'(A)',ERR=7000,END=222) string
         read(string,8000,ERR=7000) ctytmp, factmp, stidtmp, hgttmp, 
     &                      diatmp, tpttmp, veltmp, xloctmp, yloctmp,
     &                                           idattmp, crttmp, crtval
c
c  ---- find the index of this pollutant in the arrays ---
c
         idxcrt = fndchr(crttmp, 10, crtnam, ncrthd)
         if( idxcrt .LE. 0 ) then
            if( .NOT. lwarn ) then
                write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
                write(ioerr,'(2A)',ERR=9999) 'WARNING:  The stack ',
     &              'emissions files contain pollutants that are not in'
                write(ioerr,'(2A)',ERR=9999) '          the ',
     &                                   '/CRITERIA POLLUTANT/ packet.'
                write(ioerr,'(2A)',ERR=9999) 'File read: ',
     &                             iemsfl(ifile)(:strmin(iemsfl(ifile)))
                write(ioerr,9000,ERR=9999) 'Pollutant Name',': ',crttmp
                write(ioerr,'(2A)',ERR=9999) 'This is just the first ',
     &              'occurance of this warning.  There may be other'
                write(ioerr,'(2A)',ERR=9999) 'records with invalid ',
     &                 'pollutants.  All such records will be skipped.'
                write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
                nwarn = nwarn + 1
                lwarn = .TRUE.
            endif
            goto 111
         endif
c
c  --- check to see if this stack is to be combined to
c      make a new stack ---
c
         idxnew = fndcomb( idxcomb, ctytmp, factmp, stidtmp )
         if( idxnew .GT. 0 ) then
c
c  --- put the daily max into combination arrays to be added later ---
c
            crtcomb(idxnew,idxcomb,idxcrt) = 
     &                    MAX( crtval,crtcomb(idxnew,idxcomb,idxcrt) )
c
c  --- stack should be in list as is, find it ---
c
         else
           idxlist = fndlist( ctytmp, factmp, stidtmp,  hgttmp, diatmp,
     &                                tpttmp, veltmp, xloctmp, yloctmp )
           if( idxlist .LE. 0 ) goto 7001
c
c  --- calculate the max for this stack ---
c
           emsmax(idxlist,idxcrt) = MAX( crtval, emsmax(idxlist,idxcrt))
         endif
c
c  --- get the next record ---
c
         goto 111
c
c  --- entire file read, close it and get the next stack list file ---
c
 222    continue
        close(IOREMS)
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
      write(ioerr,'(/,A,I4)',ERR=9999) 
     &           'ERROR: Reading stack emissions file at record: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File read: ',
     &                             iemsfl(ifile)(:strmin(iemsfl(ifile)))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  The emissions files ',
     &          'contain stacks that are not in the master stack list.'
      write(ioerr,'(2A)',ERR=9999) 'File read: ',
     &                             iemsfl(ifile)(:strmin(iemsfl(ifile)))
      write(ioerr,'(2A)',ERR=9999) 'This stack has no match in the ',
     &                                            'master stack list.'
      write(ioerr,9001,ERR=9999) 'County ID',': ',ctytmp
      write(ioerr,9001,ERR=9999) 'Facility ID',': ',factmp
      write(ioerr,9001,ERR=9999) 'Stack ID',': ',stidtmp
      write(ioerr,9004,ERR=9999) 'Height',': ',hgttmp
      write(ioerr,9004,ERR=9999) 'Diameter',': ',diatmp
      write(ioerr,9004,ERR=9999) 'Temperature',': ',tpttmp
      write(ioerr,9004,ERR=9999) 'Velocity',': ',veltmp
      write(ioerr,9005,ERR=9999) 'Location',': ',xloctmp,yloctmp
      write(ioerr,'(2A)',ERR=9999) 'File read: ',
     &                             iemsfl(ifile)(:strmin(iemsfl(ifile)))
      write(ioerr,'(2A,//)',ERR=9999) 'Please make sure the input ',
     &                              'files are from the same inventory.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(3(A10,1X),6(F10.0,1X),I6,1X,A10,1X,F10.0)
 9000 format(T10,A,T30,2A)
 9001 format(T10,A,T30,2A,T60,A)
 9004 format(T10,A,T30,A,F10.4)
 9005 format(T10,A,T30,A,'(',F10.2,',',F10.2,')')
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
