C**** RDSTKNEW.F version 2.0 09/01/2007
c
      subroutine rdstknew( ierr )
c
c-----------------------------------------------------------------------
c
c    This routine reads any additional input stack list files and 
c    add the stacks to the end of the supplied master stack list.
c    These sources cannot be PiGGed but will be added to the inventory.
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
c   fndlist I   returns the index of a stack in array of master list
c
      integer*4 strmin
      integer*4 fndlist
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) string, fname, action
      character*10      keywrd, ctytmp, factmp, stktmp
      integer*4         irec, idxlist, ifile, i
      real*4            hgttmp, diatmp, veltmp, tpttmp
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
      irec = 0
      lwarn = .FALSE.
c
c  --- if there are no stack files, just exit ---
c
      if( nstkfl .LE. 0 ) then
         ierr = ISUCES
         goto 9999
      endif
c
c  --- loop over all of the input stack list files ---
c
      do ifile=1,nstkfl
c
c  --- open the file ---
c
         fname = istkfl(ifile)
         action = 'Opening input stack files.'
         open(IORSTK,file=istkfl(ifile),status='OLD',ERR=7003)
         write(IOWSTD,'(2A)') 'Reading: ',
     &                        istkfl(ifile)(:strmin(istkfl(ifile)))
c
c  ---- read a record from the file and load into local variables ---
c
 111     continue
         irec = irec + 1
         read(IORSTK,'(A)',ERR=7000,END=222) string
         read(string,8000,ERR=7001) keywrd, ctytmp, factmp, stktmp,
     &              hgttmp, diatmp, tpttmp, veltmp, xloctmp, yloctmp
c
c  --- check to see if this stack is already in the list --
c
         idxlist = fndlist( ctytmp, factmp, stktmp,  hgttmp, diatmp,
     &                                tpttmp, veltmp, xloctmp, yloctmp )
c
c  --- not in the list, so add it ---
c
         if( idxlist .LE. 0 ) then
            nstklist = nstklist + 1
            if( nstklist .GT. MXSTKLST ) goto 7002
            ctylist(nstklist) = ctytmp
            faclist(nstklist) = factmp
            stklist(nstklist) = stktmp
            hgtlist(nstklist) = hgttmp
            dialist(nstklist) = diatmp
            tptlist(nstklist) = tpttmp
            vellist(nstklist) = veltmp
            xloclist(nstklist) = xloctmp
            yloclist(nstklist) = yloctmp
            lpiglist(nstklist) = .FALSE.
            lcolocat(nstklist) = .FALSE. 
            if( dialist(nstklist) .LT. 0 ) lpiglist(nstklist) = .TRUE.
            lprdo(nstklist) = .FALSE.
            if( hgtlist(nstklist).LT.0 .AND. vellist(nstklist).LT.0 )
     &                                        lprdo(nstklist) = .TRUE.
            ifilestk(nstklist) = ifile
         endif
c
c  --- get the next record ---
c
         goto 111

c  --- entire file read, close it and get the next stack list file ---
c
 222    continue
        close(IORSTK)
      enddo
c
c  --- all files processed, return ---
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
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR: Reading stack file at record: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File read: ',
     &                          istkfl(ifile)(:strmin(istkfl(ifile)))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR: Reading stack file at record: ',irec
      write(ioerr,'(2A)',ERR=9999) 'File read: ',
     &                          istkfl(ifile)(:strmin(istkfl(ifile)))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading the master stack ',
     &                                                      'list file.'
      write(ioerr,'(A)',ERR=9999) 'Number of stacks exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSTKLST
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter ',
     &                                        'MXSTKLST and recompile.'
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
 8000 format(4(A10,1X),6(F10.0,1X))
 9000 format(T10,A,T40,2A)
 9001 format(T10,A,T40,2A,T60,A)
 9002 format(T10,A,T40,A,F10.3,T60,F10.3)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
