C**** RDMASTER.F version 2.0 09/01/2007
c
      subroutine rdmaster( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the master stack list file for the PIGEMS program.  
c    The master stack list contains the stack identification and
c    stack parameters for all of the sources that are to be included
c    in the inventory.  The list must be consistent from day to 
c    day to ensure that the PiGs are treated properly.
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
c      01/23/06	-gwilson-	Fixed bug that was causing the last new
c                               stacked to be skipped.
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
c   fndnew I   returns the index of a stack in array of new stacks
c
      integer*4 strmin
      integer*4 fndlist
      integer*4 fndnew
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   KEYNEW   C   flag that deteremines that the stack is newly created
c
      character*10 KEYNEW
c
      parameter( KEYNEW = 'NEW       ')         
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) string
      character*10      keywrd, ctytmp, factmp, stktmp
      integer*4         irec, idxlist, idxnew, i
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
      idxnew = -9
c
c   --- if the master stack list file was not supplied, just exit ---
c
      if( .NOT. lmaster ) then
          ierr = ISUCES
          goto 9999
      endif 
      write(IOWSTD,'(2A)') 'Reading master stack file: ',
     &                        masterfl(:strmin(masterfl))
c
c  ---- read a record from the file and load into local variables ---
c
 111  continue
      irec = irec + 1
      read(IORMSTR,'(A)',ERR=7000,END=222) string
      read(string,8000,ERR=7001) keywrd, ctytmp, factmp, stktmp, 
     &               hgttmp, diatmp, tpttmp, veltmp, xloctmp, yloctmp
      call lftjst( keywrd )
      call low2up( keywrd )
      call lftjst( ctytmp )
      call lftjst( factmp )
      call lftjst( stktmp )
c
c  --- if the keyword is blank and the index for a new
c      stack has been set, this is a stack that should be combined ---
c
      if( keywrd .EQ. ' ' .AND. idxnew .GT. 0 ) then
         nstkcomb(idxnew) = nstkcomb(idxnew) + 1
         if( nstkcomb(idxnew) .GT. MXCOMB ) goto 7005
         ctycomb(idxnew,nstkcomb(idxnew)) = ctytmp
         faccomb(idxnew,nstkcomb(idxnew)) = factmp
         stidcomb(idxnew,nstkcomb(idxnew)) = stktmp
         hgtcomb(idxnew,nstkcomb(idxnew)) = hgttmp
         diacomb(idxnew,nstkcomb(idxnew)) = diatmp
         tptcomb(idxnew,nstkcomb(idxnew)) = tpttmp
         velcomb(idxnew,nstkcomb(idxnew)) = veltmp
         xloccomb(idxnew,nstkcomb(idxnew)) = xloctmp
         yloccomb(idxnew,nstkcomb(idxnew)) = yloctmp
         goto 111
      endif
c
c  --- if this is a new stack, find it in the array of combined stacks ---
c
      idxnew = 0
      if( keywrd .EQ. KEYNEW ) then
         idxnew = fndnew( ctytmp, factmp, stktmp )
         if( idxnew .LE. 0. ) then
            nstknew = nstknew + 1
            if( nstknew  .GT. MXSNEW ) goto 7003
            idxnew = nstknew
            ctynew(nstknew) = ctytmp
            facnew(nstknew) = factmp
            stidnew(nstknew) = stktmp
            hgtnew(nstknew) = hgttmp
            dianew(nstknew) = diatmp
            tptnew(nstknew) = tpttmp
            velnew(nstknew) = veltmp
            xlocnew(nstknew) = xloctmp
            ylocnew(nstknew) = yloctmp
         endif
      endif
c
c  --- check to see if this stack is already in the list --
c
      idxlist = fndlist( ctytmp, factmp, stktmp, hgttmp, diatmp,
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
         if( dialist(nstklist) .LT. 0 ) lpiglist(nstklist) = .TRUE.
         lprdo(nstklist)    = .FALSE.
         if( hgtlist(nstklist).LT.0 .AND. vellist(nstklist).LT.0 ) 
     &                                     lprdo(nstklist) = .TRUE.
         if( idxnew .GT. 0 ) idxmaster(idxnew) = nstklist
c
c  --- already in the list, this shouldn't happen ---
c
      else
          lsame = .TRUE.
          if( ctytmp .NE. ctylist(idxlist) ) lsame = .FALSE.
          if( factmp .NE. faclist(idxlist) ) lsame = .FALSE.
          if( stktmp .NE. stklist(idxlist) ) lsame = .FALSE.
          if( hgttmp .NE. hgtlist(idxlist) ) lsame = .FALSE.
          if( diatmp .NE. dialist(idxlist) ) lsame = .FALSE.
          if( tpttmp .NE. tptlist(idxlist) ) lsame = .FALSE.
          if( veltmp .NE. vellist(idxlist) ) lsame = .FALSE.
          if( xloctmp .NE. xloclist(idxlist) ) lsame = .FALSE.
          if( yloctmp .NE. yloclist(idxlist) ) lsame = .FALSE.
          if( lsame ) then
             goto 6000 
          else
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
             if( dialist(nstklist) .LT. 0 ) lpiglist(nstklist) = .TRUE.
             lprdo(nstklist)    = .FALSE.
             if( hgtlist(nstklist).LT.0 .AND. vellist(nstklist).LT.0 ) 
     &                                         lprdo(nstklist) = .TRUE.
             goto 111
          endif
      endif
c
c  --- get the next record ---
c
      goto 111

c  --- entire file read ---
c
 222  continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      if( .NOT. lwarn ) then
          write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
          write(ioerr,'(2A)',ERR=9999) 'WARNING:  The master stack ',
     &                              'list contains duplicate records.'
          write(ioerr,'(2A)',ERR=9999) 'There are multiple instances ',
     &                                        'of the following stack:'
          write(ioerr,9000,ERR=9999) 'County ID',': ',ctytmp
          write(ioerr,9000,ERR=9999) 'Facility ID',': ',factmp
          write(ioerr,9000,ERR=9999) 'Stack ID',': ',stktmp
          write(ioerr,9004,ERR=9999) 'Height',': ',hgttmp
          write(ioerr,9004,ERR=9999) 'Diameter',': ',diatmp
          write(ioerr,9004,ERR=9999) 'Temperature',': ',tpttmp
          write(ioerr,9004,ERR=9999) 'Velocity',': ',veltmp
          write(ioerr,9005,ERR=9999) 'Location',': ',xloctmp,yloctmp
          write(ioerr,'(2A)',ERR=9999) 'This is just the first ',
     &              'occurance of this warning.  There may be other'
          write(ioerr,'(2A)',ERR=9999) 'stacks with duplicate ',
     &                    'records.  All duplicates will be skipped.'
          write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
          nwarn = nwarn + 1
          lwarn = .TRUE.
      endif
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999) 
     &           'ERROR: 1 Reading master stack file at record: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File read: ',
     &                                       masterfl(:strmin(masterfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR: Reading master stack file at record: ',irec
      write(ioerr,'(2A)',ERR=9999) 'File read: ',
     &                                       masterfl(:strmin(masterfl))
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
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading the master stack ',
     &                                                      'list file.'
      write(ioerr,'(A)',ERR=9999)
     &                        'Number of "NEW" stacks exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSNEW
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSNEW ',
     &                                                  'and recompile.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading the master stack ',
     &                                                      'list file.'
      write(ioerr,'(A)',ERR=9999)
     &                   'Number of new stacks to combine exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXCOMB
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXCOMB ',
     &                                                  'and recompile.'
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,//)',ERR=9999)  string(:strmin(string))
      write(ioerr,9000,ERR=9999) 'New County ID',':',ctynew(nstknew)
      write(ioerr,9000,ERR=9999) 'New Facility ID',':',facnew(nstknew)
      write(ioerr,9000,ERR=9999) 'New Stack ID',':',stidnew(nstknew)
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(4(A10,1X),6(F10.0,1X))
 9000 format(T10,A,T40,2A)
 9001 format(T10,A,T40,2A,T60,A)
 9002 format(T10,A,T40,A,F10.3,T60,,F10.3)
 9003 format(T10,A,T40,A,F10.1,T60,,F10.1)
 9004 format(T10,A,T40,A,F10.4)
 9005 format(T10,A,T40,A,'(',F10.2,',',F10.2,')')
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
