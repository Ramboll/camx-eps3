C**** RDUSPC.F
c
      subroutine rduspc( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file, looking for the /SPCEMS/ packet.
c    Also includes packets:  /SPECIES LIST/
c                            /COUNTY/
c                            /OUTPUT tables/
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
      include 'embrdat.inc'
      include 'uamcntl.inc'
      include 'iospcems.inc'
      include 'spcems.inc'
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
c   fndchr  I  find a string in an array (library)
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c   keywrd  C   temporary storage for seaching fo keyword
c   respon  C   character string for temporary storage
c
      character*(5*MXSTR) string, strtmp
      character*(KEYLEN)  keywrd, keyin
      character*(MXSTR)   respon
      character*40        cdate
      character*10        module
      character*10        clcnew, minuend, subtra
      integer*4           jerr, irec, i
      integer*4           idxt, idxm, idxs
      logical*4           found
c
c-----------------------------------------------------------------------
c   data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/SPCEMS/'/
      data module /'SPCEMS'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume success ---
c
      ierr = IFAIL
c
c   --- call routine to read species list packet ---
c
      call rdspec( jerr,
     &                   IORINP, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read county list ---
c
      call rdcnty( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to get the output tables requested ---
c
      call rdotbl( jerr, 
     &                   IORINP, module, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- echo data to message file ---
c
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7000) VERSON, cdate
c
c   --- call routine to find /SPCEMS/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7001
c
c   --- read the records ---
c
c     --- report profile tables? ---
      irec = 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( respon(1:3) .EQ. YES ) then
          lprtpf = .TRUE.
          write(IOWMSG,9000,ERR=7000) 'Write totals by profile code',YES
      else if( respon(1:3) .EQ. NO ) then
          lprtpf = .FALSE.
          write(IOWMSG,9000,ERR=7000) 'Write totals by profile code',NO
      else
          goto 7005
      endif
c
c     --- force profile match? ---
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( respon(1:3) .EQ. YES ) then
          lreqpf = .TRUE.
          write(IOWMSG,9000,ERR=7000)
     &                   'Force profile match',YES
      else if( respon(1:3) .EQ. NO ) then
          lreqpf = .FALSE.
          write(IOWMSG,9000,ERR=7000)
     &                   'Force profile match',NO
      else
          goto 7006
      endif
c
c     --- apply conversion factors?  from  -->  to ---
      lconv = .FALSE.
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      keyin = string(:KEYLEN)
      call low2up( keyin )
      call lftjst( keyin )
      if( keyin .EQ. KEYEND ) goto 222 

      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if (strmin(respon) .GT. 1)  then
         call token (jerr, fromcrt, respon)
         if( jerr .NE. ISUCES ) goto 7008
         call token (jerr, tocrt, respon)
         if( jerr .NE. ISUCES ) goto 7008
c
c        --- check that 'from' criteria is in our inventory ---
         idxfrm = fndchr ( fromcrt, 10, crtnam, ncrthd )
         if ( idxfrm .EQ. 0 ) goto 7007

         lconv = .TRUE.
      endif
c
c     --- calculate new species from existing criteria pollutants ? ---
c
      lpart = .FALSE.
      npmcs = 0
  111 continue
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      keyin = string(:KEYLEN)
      call low2up( keyin )
      call lftjst( keyin )
      if( keyin .EQ. KEYEND ) goto 222 

c     --- parse string of new species to compute from existing; A=B-C ---
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if (strmin(respon) .GT. 1)  then
c
c        --- extract the name of new species to be calculated ---
         found = .FALSE.
         do i=1,LEN(respon)
            if( respon(i:i) .EQ. "=" ) then
                found = .TRUE.
                clcnew = respon(:i-1)
                call lftjst( clcnew )
                strtmp = respon(i+1:)
                call lftjst( strtmp )
            endif
         enddo
         if( .NOT.found ) goto 7012

         idxt = fndchr ( clcnew, 10, spcnam, nspecs )
         if ( idxt .EQ. 0 ) goto 7009
c
c        --- extract the name of minuend and subtrahend from equation ---
         found = .FALSE.
         do i=1,LEN(strtmp)
            if( strtmp(i:i) .EQ. "-" ) then
                found = .TRUE.
                minuend = strtmp(:i-1)
                subtra = strtmp(i+1:)
                call lftjst( subtra )
            endif
         enddo
         if( .NOT.found ) goto 7012
c
         idxm = fndchr ( minuend, 10, crtnam, ncrthd )
         if ( idxm .EQ. 0 ) goto 7010
         idxs = fndchr ( subtra, 10, crtnam, ncrthd )
         if ( idxs .EQ. 0 ) goto 7011

         lpart = .TRUE.
         npmcs = npmcs + 1
         if ( npmcs .GT. MXCMPT ) goto 7013
c
         idxnew(npmcs) = idxt
         pmnew(npmcs)  = clcnew
         idxmin(npmcs) = idxm
         idxsub(npmcs) = idxs
         lpmM(idxm) = .TRUE.

         goto 111
      endif
c
c   ---- finish writing to message file ---
c
  222 continue

      if ( lconv ) then
          write(IOWMSG,9000,ERR=7000)
     &                   'Apply conversion factors',YES
          write(IOWMSG,9000,ERR=7000)
     &                   '                 From',fromcrt
          write(IOWMSG,9000,ERR=7000)
     &                   '                 To',tocrt
      else
          write(IOWMSG,9000,ERR=7000)
      endif
c
      if ( lpart )  then
          write(IOWMSG,9000,ERR=7000)
     &                   'Calculate new from existing ',YES
          do i = 1, npmcs
             write(IOWMSG,9003,ERR=7000)
     &               spcnam(idxnew(i))(:strmin(spcnam(idxnew(i)))),
     &               crtnam(idxmin(i))(:strmin(crtnam(idxmin(i)))),
     &               crtnam(idxsub(i))(:strmin(crtnam(idxsub(i))))
          enddo
c         --- write message concerning reporting of PMs ---
          write(IOWMSG,9004,ERR=7000) 
     &      'Note -',
     &      'This new compound is not reported in the optional profile',
     &      'code tables below since it is independent of a profile',
     &      'code.  The criteria emissions for the component compounds',
     &      'are carried through EPS3.'
      else
          write(IOWMSG,9000,ERR=7000)
     &                   'Calculate new from existing ',NO
      endif
c
c     --- write output species to file ---
c
      write(IOWMSG,'(/)',ERR=7000) 
      do i = 1, nspecs
         if ( i .EQ. 1 )  then
          write(IOWMSG,9000,ERR=7000) 'Species output list',
     &                                 spcnam(i)
         else
          write(IOWMSG,9000,ERR=7000) ' ',spcnam(i)
         endif
      enddo
c
c   ---- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading the USERIN file.'
      write(ioerr,'(3A,//)',ERR=9999) 'This program requires the ',
     &                     keywrd(:strmin(keywrd)),' packet of USERIN.'
      ierr = IFAIL
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I5,//)',ERR=9999) 'Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A,I3)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)
     &          'Invalid response to write totals by profile flag: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A,I3)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)
     &            'Invalid response to force profile match flag: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
 7007 continue
      write(ioerr,'(/,3A,I3)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(3A)',ERR=9999)
     &            'Specified pollutant converting from: ',TRIM(fromcrt),
     &                      ' is not found in the input inventory.'
      write(ioerr,'(A)',ERR=9999) 'Correct the packet and rerun.'
      write(ioerr,'(2A)',ERR=9999) 'A blank line ',
     &           'indicates no conversion factors are applied.'
      goto 9999
c
 7008 continue
      write(ioerr,'(/,3A,I3)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(2A)',ERR=9999) 'String read: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(2A//)',ERR=9999) 'Specify valid FROM and TO ',
     &        'criteria pollutant names to apply conversion factors.'
      goto 9999
c
 7009 continue
      write(ioerr,'(/,3A,I3)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(2A)',ERR=9999) 'New pollutant must be specified ',
     &                    'in /SPECIES LIST/ packet which includes:'
      do i = 1, nspecs
         write(ioerr,'(5X,A)',ERR=9999) spcnam(i)
      enddo
      write(ioerr,'(2A)',ERR=9999) 'String read: ',
     &                                       string(21:strmin(string))
      goto 9999
c
 7010 continue
      write(ioerr,'(/,3A,I3)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(3A)',ERR=9999) 
     &     'The option to compute ', clcnew(:strmin(clcnew)), 
     &     ' has been specified.'
      write(ioerr,'(2A/A)',ERR=9999) 
     &     'The minuend component of new computed pollutant must ',
     &     'exist in criteria pollutant list on EMBR header.',
     &     'Options are:'
      do i = 1, ncrthd
         write(ioerr,'(5X,A)',ERR=9999) crtnam(i)
      enddo
      write(ioerr,'(2A)',ERR=9999) 'String read: ',
     &                                       string(21:strmin(string))
      goto 9999
c
 7011 continue
      write(ioerr,'(/,3A,I3)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(3A)',ERR=9999) 
     &     'The option to compute ', clcnew(:strmin(clcnew)), 
     &     ' has been specified.'
      write(ioerr,'(2A/A)',ERR=9999) 
     &     'The subtrahend component of new computed pollutant must ',
     &     'exist in criteria pollutant list on EMBR header.',
     &     'Options are:'
      do i = 1, ncrthd
         write(ioerr,'(5X,A)',ERR=9999) crtnam(i)
      enddo
      write(ioerr,'(2A)',ERR=9999) 'String read: ',
     &                                       string(21:strmin(string))
      goto 9999
c
 7012 continue
      write(ioerr,'(/,3A,I3)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(/,2A)',ERR=9999) '       while attempting ',
     &                                  'to parse input line.'
      write(ioerr,'(/,A)',ERR=9999) '       Format expected: A = B - C'
      write(ioerr,'(/,2A)',ERR=9999) '       Where A is new species ',
     &          'computed from criteria B minus criteria C.'
      write(ioerr,'(/,2A)',ERR=9999) '      Record read: ', string
      goto 9999
c
 7013 continue
      write(ioerr,'(/,3A,I3)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(2A)',ERR=9999) 'ERROR:  Number of new ',
     &          'calculated species exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXCMPT
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXCMPT ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(T10,A,T40,:,':',A)
 9001 format(T10,A,T40,:,':',I6)
 9002 format(T10,A,T40,:,':',I5,:,I5,:,I5)
 9003 format(46X,A,' = ',A,' - ',A )
 9004 format(46X,58('-'),5(/,46X,A),/,46X,58('-'))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
