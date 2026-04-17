C**** FNDSTK.F 
c
      function fndstk(
     &                 fipin, plant, stack,  stkdia, stkgev,
     &                             stkhgt, stktmp, utme, utmn )
      integer*4 fndstk
c
c-----------------------------------------------------------------------
c
c     This routine searches through the stack arrays for a match with
c     data in the argument list.  If found, the index of the stack in
c     the array is returned. A return value of zero indicates
c     no match was made.
c
c   Arguments:
c
c     Inputs:
c       fipin    C   FIPS code of source
c       plant    C   NEDS facility id code of source
c       stack    C   stack id of source
c       stkdia   R   stack diameter
c       stkgev   R   stack gas exit velocity
c       stkhgt   R   stack height
c       stktmp   R   stack exit gas temperature
c       utme     R   UTM easting coordinate
c       utmn     R   UTM northing coordinate
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'cntylst.inc'
      include 'pstpntstk.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      character*5  fipin
      character*10 plant
      character*10 stack
      real*4      stkdia
      real*4      stkgev
      real*4      stkhgt
      real*4      stktmp
      real*4      utme
      real*4      utmn
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
c   fndhash_fips I   returns index of FIPS code in an hash table
c
      integer*4 fndhash_fips
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 idxfip, i, j
      logical*4 lfnd1, lfnd2, lwarn
c
c-----------------------------------------------------------------------
c   Data and save statements:
c-----------------------------------------------------------------------
c
      data lwarn /.FALSE./
      save lwarn
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndstk = 0
      if( nstack .EQ. 0 ) goto 9999
c
c   ---- find the index of county in array ---
c
      idxfip = fndhash_fips( fipin )
      if( istkpt(idxfip) .EQ. 0 ) istkpt(idxfip) = nstack
c
c   ---- loop through the array ---
c
      do i=istkpt(idxfip),nstack
c
c   ---- if match, set return value and return right away ---
c
         lfnd1  = .TRUE.
         lfnd2  = .TRUE.
         if( fipin .NE. infips(i) ) lfnd1 = .FALSE.
         if( plant .NE. inplnt(i) ) lfnd1 = .FALSE.
         if( stack .NE. stckin(i) ) lfnd1 = .FALSE.
         if( stkdia .NE. stdiam(i) ) lfnd2 = .FALSE.
         if( stkhgt .NE. stkht(i) ) lfnd2 = .FALSE.
         if( stktmp .NE. stextp(i) ) lfnd2 = .FALSE.
         if( utme .NE. xloc(i) ) lfnd2 = .FALSE.
         if( utmn .NE. yloc(i) ) lfnd2 = .FALSE.
         if( stkgev*3600. .NE. stexvl(i) ) lfnd2 = .FALSE.
c
c  --- if stack parameters are inconsistent, write a message ---
c
         if( lfnd1 .AND. lfnd2 ) then
             fndstk = i
             goto 9999
         endif
c
c  --- if stack parameters are inconsistent, write a message ---
c
         if( lfnd1 .AND. .NOT. lfnd2 .AND. .NOT. lwarn ) then
            write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',j=1,40)
            write(ioerr,'(3A)',ERR=9999)'WARNING:  EMBR file contains ',
     &        'stacks with matching ID but different stack parameters.'
            write(ioerr,'(10X,2A)',ERR=9999) 'County ID: ',fipin
            write(ioerr,'(10X,2A)',ERR=9999) 'Plant ID: ',plant
            write(ioerr,'(10X,2A)',ERR=9999) 'Stack ID: ',stack
            write(ioerr,'(25X,2A)',ERR=9999) 
     &                       'First Record    Second Record'
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999) 
     &                                    'Diameter  ',stkdia,stdiam(i)
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999) 
     &                                    'Height    ',stkhgt,stkht(i)
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999) 
     &                                 'Exit Temp ',stktmp,stextp(i)
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999) 
     &                            'Exit Vel  ',stkgev,stexvl(i)/3600.
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999) 
     &                                     'X-Coord   ',utme,xloc(i)
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999) 
     &                                     'Y-Coord   ',utmn,yloc(i)
            write(ioerr,'(3A)',ERR=9999)'These records will be ',
     &                                  'treated as separate stacks. '
            write(ioerr,'(3A)',ERR=9999)'This is just the first ',
     &                              'occurance.  There may be others.'
            write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',j=1,40)
            nwarn = nwarn + 1
            lwarn = .TRUE.
         endif
      enddo
c
c   ---- no match, return zero ----
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
