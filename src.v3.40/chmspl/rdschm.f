C**** RDSCHM.F 
c
      subroutine rdschm( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the splits file.
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
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
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'sccxref.inc'
      include 'iochmspl.inc'
      include 'splits.inc'
      include 'totchm.inc'
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
c   fndchr  I   returns the index of an character string in an array
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c   Local Parameters:
c-----------------------------------------------------------------------
c
c   DEFPRF   C    profile code for default profile (overall average)
c
      character*(LENPRF) DEFPRF
c
      parameter( DEFPRF = '0000' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(5*MXSTR) string
      integer*4           i, irec, idxuse
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      nfac = 0
      irec = 1
c
c  --- read species information ---
c
      read(IORSPL,'(A)',ERR=7000) string
      read(string,'(I10)',ERR=7001) nsplit
      if( nsplit .GT. MXSPEC ) goto 7002
      do i=1,nsplit
          irec = irec + 1
          read(IORSPL,'(A)',ERR=7000) string
          read(string,'(A10,1X,F5.0,1X,A10)',ERR=7001) 
     &                            splnam(i),splmol(i),splcrt(i)
          call lftjst( splnam(i) )
          call lftjst( splcrt(i) )
      enddo
c
c  --- set up index of criteria pollutants ---
c
      do 10 i=1,nsplit
c
c  --- skip all of the checks if this species is not requested ---
c
          idxuse = fndchr( splnam(i), 10, spcnam, nspecs )
          if( idxuse .LE. 0 ) goto 10
c
c  --- find the index of criteria pollutant in the arrays ---
c
          indcrt(idxuse) = fndchr( splcrt(i), 10, crtnam, ncrthd )
          if( indcrt(idxuse) .LE. 0. ) goto 7003
          lsplcrt( indcrt(idxuse) ) = .TRUE.
   10 continue
c
c  --- read factors to end of file ---
c
      idefsp = 0
  111 continue
      irec = irec + 1
      nfac = nfac + 1
      if( nfac .GT. MXSPL ) goto 7004
      read(IORSPL,'(A)',ERR=7005,END=222) string
      read(string,'(A,100(:,F10.0))',ERR=7006) prfspl(nfac),
     &                                 (spltab(nfac,i),i=1,nsplit)
      call lftjst( prfspl(nfac) )
c
c  --- check for default profile ---
c
      if( prfspl(nfac) .EQ. DEFPRF ) idefsp = nfac
c
c  --- if this is the first record, write out the note about
c      profile code length ---
c
      if( nfac .EQ. 1 ) goto 6000
c
c  --- get next record ---
c
      goto 111
c
c  --- read the entire file, add a "default" profile for output tables ---
c
  222 continue
      prfspl(nfac) = "Deflt"
c
c  --- write count to message file ---
c
      write(IOWMSG,'(/,1X,A,I8)',ERR=7007)
     &                  'Number of species in the splits file:', nsplit
      write(IOWMSG,'(/,1X,A,I8)',ERR=7007)
     &                 'Number of factors read from splits file:', nfac
      if( idefsp .LE. 0 ) write(IOWMSG,'(/,A)',ERR=9999)
     &                               'No default profile is available.'
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning Messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(ioerr,'(/,5X,70A)',ERR=9999) ('*',i=1,70)
      write(ioerr,'(5X,A,T75,A)',ERR=9999) '*','*'
      write(ioerr,'(5X,3A,I2,T75,A)',ERR=9999) '*','  NOTE: This ',
     &   'version is compiled for profile codes of length: ',LENPRF,'*'
      write(ioerr,'(5X,3A,T75,A)',ERR=9999) '*','  Please verify ',
     &             'that this is the correct length for your data.','*'
      write(ioerr,'(5X,3A,T75,A)',ERR=9999) '*','  The first record ',
     &                'of your split factors file is shown below: ','*'
      write(ioerr,'(5X,A,T75,A)',ERR=9999) '*','*'
      write(ioerr,'(5X,70A,/)',ERR=9999) ('*',i=1,70)
      write(ioerr,'(/,A,/)',ERR=9999) string(:strmin(string))
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &        'ERROR:  Reading header of splits file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &        'ERROR:  Reading header of splits file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                         string(:strmin(string))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Reading splits file.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      write(ioerr,'(A)',ERR=9999)
     &       'ERROR:  Number of species in splits file exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Species in file: ',nsplit
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSPEC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPEC ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Reading splits file.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      write(ioerr,'(2A)',ERR=9999) 'ERROR:  Criteria pollutant ',
     &           'listed in splits file not found in the EMBR file.'
      write(ioerr,'(10X,2A)',ERR=9999) 'Citeria pollutant: ',splcrt(i)
      write(ioerr,'(10X,2A)',ERR=9999) 'Split species    : ',splnam(i)
      write(ioerr,'(2A,//)',ERR=9999) 'Remove the species from the ',
     &                                   'splits file and try again.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Reading splits file.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      write(ioerr,'(2A)',ERR=9999) 'ERROR:  Number of splitting ',
     &                            'factors in splits file exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSPL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPL ',
     &                                                  'and recompile.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &        'ERROR:  Reading data of splits file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      goto 9999
c
 7006 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &        'ERROR:  Reading data of splits file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                         string(:strmin(string))
      goto 9999
c
 7007 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
