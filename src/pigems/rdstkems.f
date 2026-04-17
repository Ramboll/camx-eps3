C**** RDSTKEMS.F version 2.0 09/01/2007
c
      subroutine rdstkems( ierr )
c
c-----------------------------------------------------------------------
c
c    This routine reads all of the stack emissions files and loads the
c    arrays for writing the output files.
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
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
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
      character*(4*MXSTR) string, fname, action
      character*10      ctytmp, factmp, stidtmp, spctmp
      integer*4         irec, idxlist, ifile, idxnew, idxcomb, idxspc
      integer*4         i, j, k, l
      real*4            spcval, emsval(MXHOUR), hgttmp, diatmp, tpttmp
      real*4            veltmp, xloctmp, yloctmp
      logical           lwarn
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
c  --- if there are no stack emission files, just exit ---
c
      if( nemsfl .LE. 0 ) then
         ierr = ISUCES
         goto 9999
      endif
c
c  --- loop over all of the input emissions files ---
c
      do ifile=1,nemsfl
c
c   --- open the file ---
c
         fname = iemsfl(ifile)
         action = 'Opening input emissions files.'
         open(IOREMS,file=iemsfl(ifile),status='OLD',ERR=7003)
         write(IOWSTD,'(2A)') 'Reading emission file: ',
     &                        iemsfl(ifile)(:strmin(iemsfl(ifile))) 
c  added flush for qa - feb 2015
         flush(IOWSTD)

         irec = 0
c
c  ---- read a record from the file and load into local variables ---
c
 111     continue
         irec = irec + 1
         read(IOREMS,'(A)',ERR=7000,END=222) string
         read(string,8000,ERR=7001) ctytmp, factmp, stidtmp,
     &                       hgttmp, diatmp, tpttmp, veltmp,  xloctmp, 
     &                           yloctmp, spctmp, (emsval(i),i=1,MXHOUR)
c
c  ---- find the index of this pollutant in the arrays ---
c
         idxspc = fndchr(spctmp, 10, spcnam, nspecs)
         if( idxspc .LE. 0 ) then
            if( .NOT. lwarn ) then
                write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
                write(ioerr,'(2A)',ERR=9999) 'WARNING:  The stack ',
     &                'emissions files contain species that are not in'
                write(ioerr,'(2A)',ERR=9999) '          the ',
     &                                          '/SPECIES LIST/ packet.'
                write(ioerr,'(2A)',ERR=9999) 'File read: ',
     &                             iemsfl(ifile)(:strmin(iemsfl(ifile)))
                write(ioerr,9000,ERR=9999) 'Species Name',': ',spctmp
                write(ioerr,'(2A)',ERR=9999) 'This is just the first ',
     &              'occurance of this warning.  There may be other'
                write(ioerr,'(2A)',ERR=9999) 'records with invalid ',
     &                   'species.  All such records will be skipped.'
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
           if( hgttmp .LT.0.0 .AND. veltmp .LT. 0.0 ) goto 7004
           ntotstk(ifile,ICDCOM) = ntotstk(ifile,ICDCOM) + 1
           ntotstk(ifile,ICDSTK) = ntotstk(ifile,ICDSTK) + 1
c
c  --- put the daily max into combination arrays to be added later ---
c
           do i=1,MXHOUR
              spccomb(idxnew,idxcomb,idxspc,i) = 
     &               spccomb(idxnew,idxcomb,idxspc,i) + emsval(i)
              if( ispcrt(idxspc) .GT. 0 ) then
                 emstot(ifile,ispcrt(idxspc)) = 
     &                        emstot(ifile,ispcrt(idxspc)) + 
     &                            emsval(i) * molwt(idxspc) * CVTTON
               endif
           enddo
c
c  --- stack should be in list as is, find it ---
c
         else
           idxlist = fndlist( ctytmp, factmp, stidtmp, hgttmp, diatmp, 
     &                                tpttmp, veltmp, xloctmp, yloctmp )
           ntotstk(ifile,ICDSTK) = ntotstk(ifile,ICDSTK) + 1
           if( idxlist .LE. 0 ) goto 7002
c
c  --- add the emissions for this stack ---
c
           do i=1,MXHOUR
              emsstk(idxlist,idxspc,i) = 
     &                    emsstk(idxlist,idxspc,i) + emsval(i)
              if( ispcrt(idxspc) .GT. 0 ) then
                emstot(ifile,ispcrt(idxspc)) = 
     &                        emstot(ifile,ispcrt(idxspc)) + 
     &                            emsval(i) * molwt(idxspc) * CVTTON
              endif
           enddo
         endif
c
c  --- get the next record ---
c
         goto 111

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
      write(ioerr,'(/,A,I4)',ERR=9999)
     &           'ERROR: Reading stack emissions file at record: ',irec
      write(ioerr,'(2A)',ERR=9999) 'File read: ',
     &                             iemsfl(ifile)(:strmin(iemsfl(ifile)))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7002 continue
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
 7004 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Attempt to combine a ',
     &          'stack with plume rise distribution override feature.'
      write(ioerr,'(2A,/)',ERR=9999) '       Remove stack from the ',
     &          ' master stack list of sources to be combined.'
      write(ioerr,'(2A,/)',ERR=9999) '       County ', ctytmp
      write(ioerr,'(2A,/)',ERR=9999) '       Facility ', factmp
      write(ioerr,'(2A,/)',ERR=9999) '       Stack ', stktmp
      goto 9999
c
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(3(A10,1X),6(F10.0,1X),29X,A10,100(:,F12.0))
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
