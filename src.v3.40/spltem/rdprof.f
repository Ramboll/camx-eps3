C**** RDPROF.F
c 
      subroutine rdprof( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the profile weight data and sotres it in common to be used
c    by the EMSCVT program.  The cross reference data is read and a
c    search is made in the existing cross reference arrays for a match.
c    If no match is found, the data is added to the cross reference
c    arrays.  The profile weight data is then read and checked for
c    validity.
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
      include 'iospltem.inc'
      include 'spltem.inc'
      include 'sccxref.inc'
      include 'emcvtprf.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c    rgtjst  --  right justifies a character string (library)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c    strmin  I   returns actual length of string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c    factmp  C   temporary variable for facility id
c    fiptmp  C   temporary variable for FIPS code
c    irec    I   record counter
c    stktmp  C   temporary variable for stack id
c    prftmp  C   temporary variable for profile code
c    scctmp  C   temporary variable for scc code
c    string  C   temporary variable for reading data
c    totprf  R   total weight for entire profile
c
      character*(5*MXSTR) string
      character*(LENPRF)  prftmp
      character*10        scctmp, factmp, stktmp
      character*5         fiptmp
      integer*4           irec, nxref, i
      real*4              totprf
c
c-----------------------------------------------------------------------
c   Data and save statements:
c-----------------------------------------------------------------------
c
      save irec
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
c
c   --- incrament counter and check for array overflow ---
c
      prfcod = ' '
      nxref = 0
  111 continue
c
c   --- read record as character string ---
c
      irec = irec + 1
      read(IORPRW,'(A)',ERR=7000,END=333) string
c
c   --- assume longer record is cross reference record ---
c
      if( string(1:1) .EQ. 'x' .OR. string(1:1) .EQ. 'X' ) then
          read(string,'(A5,1X,3(A10,1X),A)',ERR=7001) fiptmp, factmp, 
     &                                          stktmp, scctmp, prftmp
          call rgtjst( scctmp )
          call lftjst( prftmp )
          call lftjst( stktmp )
          call lftjst( factmp )

c
c   --- verify that this is same profile code ---
c
          if( prfcod .EQ. '              ' ) prfcod = prftmp
          if( prfcod .NE. prftmp ) goto 7002
          nxref = nxref + 1
c
c   --- find index of data in arrays ---
c
          do i=1,npfxrf
             if( fipxrf(i) .EQ. fiptmp .AND. facxrf(i) .eq. factmp .and.
     &          stkxrf(i) .EQ. stktmp .AND. sccxrf(i) .eq. scctmp ) then
                   if( prfxrf(i) .NE. prfcod ) then
                       write(IOWMSG,9000,ERR=7006)
     &                       'WARNING:  Changing cross reference of: ',
     &                                     fiptmp,factmp,stktmp,scctmp,
     &                                     ' from profile: ',prfxrf(i),
     &                                           ' to profile: ',prfcod
                       prfxrf(i) = prfcod
                   endif
                   goto 111
             endif
          enddo
          npfxrf = npfxrf + 1
          if( npfxrf .GT. MXREF ) goto 7003
          prfxrf(npfxrf) = prfcod
          fipxrf(npfxrf) = fiptmp
          facxrf(npfxrf) = factmp
          stkxrf(npfxrf) = stktmp
          sccxrf(npfxrf) = scctmp
          goto 111
      endif
c
c   --- found first profile weight record, read the profile weights ---
c
      nwate = 1
      prftmp = string(:LENPRF)
      call lftjst( prftmp )
      read(string(LENPRF+2:),'(I5,1X,F10.0)',ERR=7004) icmpcd(1), 
     &                                                   wtfrac(1)
      if( nxref .GT. 0 .AND. prftmp .NE. prfcod ) goto 7002
c
      write(IOWMSG,'(3A,I8)',ERR=7006) 'Number of new ASC-SCC/profile ',
     &              'cross reference records for profile: ',prftmp,nxref
c
      prfcod = prftmp
      totprf = wtfrac(1)
  222 continue
      irec = irec + 1
      read(IORPRW,'(A)',ERR=7000,END=333) string
c
c   --- keep reading profile weights while string is short or on same
c       profile ---
c
      nwate = nwate + 1
      if( nwate .GT. MXWPRF ) goto 7005
      if( string(1:1) .NE. 'x' .AND. string(1:1) .NE. 'X' ) then
          prftmp = string(1:LENPRF)
          call lftjst( prftmp )
          read(string(LENPRF+2:),'(I5,1X,F10.0)') icmpcd(nwate), 
     &                                               wtfrac(nwate)
          if( prftmp .EQ. prfcod ) then
              totprf = totprf + wtfrac(nwate)
              goto 222
          endif
      endif
c
c   --- decrement counter and backspace the file to get last record
c       for next profile ---
c
      nwate = nwate - 1
      backspace(IORPRW,ERR=7000)
c
c   --- check for within valid range and renormalize if not ---
c
c
c   --- check for within valid range and renormalize if not ---
c     
      if( ABS(1.0 - totprf) .GT. epslon ) then
         write(IOWMSG,9001,ERR=7006)
     &           'WARNING:  Total weight from profile: ',prfcod,
     &                        ' outside acceptable range: ',totprf,
     &                                   'Processing data as is.'
      else
         do 20 i=1,nwate
            if( totprf .GT. 0 ) wtfrac(i) = wtfrac(i) / totprf
  20     continue
      endif
c
c   --- set error flag to sucess ----
c
      ierr = ISUCES
      goto 9999
c
c   --- set error flag to end of data ---
c
  333 continue
c
c   --- check for within valid range and renormalize if not ---
c       makse sure the last prifile is checked
c     
      if( ABS(1.0 - totprf) .GT. epslon ) then
         write(IOWMSG,9001,ERR=7006)
     &           'WARNING:  Total weight from profile: ',prfcod,
     &                        ' outside acceptable range: ',totprf,
     &                                   'Processing data as is.'
      else
         do i=1,nwate
            if( totprf .GT. 0 ) wtfrac(i) = wtfrac(i) / totprf
      enddo
      endif
      ierr = IEOD
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading input',
     &                          ' weight profiles file at line: ',irec
      write(ioerr,'(2A)',ERR=9999) 'File:',prwtfl(:strmin(prwtfl))
      write(ioerr,'(2A,/,A,//)',ERR=9999) 'Make sure that the profile ',
     &    'code and all the stack ID codes are','of the correct length.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading input',
     &                          ' weight profiles file at line: ',irec
      write(ioerr,'(2A)',ERR=9999) 'File:',prwtfl(:strmin(prwtfl))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A)',ERR=9999) string(:strmin(string))
      write(ioerr,'(2A,/,A,//)',ERR=9999) 'Make sure that the profile ',
     &    'code and all the stack ID codes are','of the correct length.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading input',
     &                                    ' weight profiles file.'
      write(ioerr,'(2A)',ERR=9999) 'File:',prwtfl(:strmin(prwtfl))
      write(ioerr,'(3A)',ERR=9999) 'There are no data records for ',
     &                              'profile code: ',prfcod
      write(ioerr,'(2A)',ERR=9999) 'Please remove all cross ',
     &                 'reference records for this profile code.'
      write(ioerr,'(/,2A,/,A,//)',ERR=9999) 'Make sure that the ',
     &            'profile code and all the stack ID codes are',
     &                                     'of the correct length.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading input',
     &                                    ' weight profiles file.'
      write(ioerr,'(2A)',ERR=9999) 'Number of ASC-SCC/profile ',
     &                   'cross reference records exceeds maximum.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXREF
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXREF ',
     &                                                  'and recompile.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading input',
     &                          ' weight profiles file at line: ',irec
      write(ioerr,'(2A)',ERR=9999) 'File:',prwtfl(:strmin(prwtfl))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A)',ERR=9999) string(:strmin(string))
      write(ioerr,'(10X,2A,I2,//)',ERR=9999) 'Make sure the profile ',
     &                                   'codes are of length: ',LENPRF
      goto 9999
c
 7005 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading input',
     &                                    ' weight profiles file.'
      write(ioerr,'(2A)',ERR=9999) 'Number of compounds in profile ',
     &                                               'exceeds maximum.'
      write(ioerr,'(10X,2A)',ERR=9999) 'Profile code: ',prfcod
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXWPRF
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXWPRF ',
     &                                                  'and recompile.'
      goto 9999
c
 7006 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(//,1X,A,4(A,1X),/,15X,4A)
 9001 format(//,1X,3A,F20.6,/,1X,A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
