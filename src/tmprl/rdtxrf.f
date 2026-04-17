C**** RDTXRF.F 
c
      subroutine rdtxrf( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the ASC-SCC/temporal profiles cross reference file and stores
c    the data in common variables to be used by the TMPRL program.
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
      include 'hash.inc'
      include 'iotmprl.inc'
      include 'tmprlprf.inc'
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
c   fndint      I  returns the index of an integer in an array of integers
c   fndhash_tpl I  returns the index of exact match in cross reference
c                  data in hash table
c   strmin      I  returns actual length of string (min of 1)
c
      integer*4 fndint
      integer*4 fndhash_tpl
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   factmp   C   NEDS plant id read from file
c   fiptmp   C   FIPS code read from file
c   idytmp   I   diurnal profile code read from file
c   imntmp   I   monthly profile code read from file
c   inpol    I   pollutant code read from file
c   staktmp  C   stack id read from file
c   iwktmp   I   weekly profile code read from file
c   pnttmp   C   temporary variable for reading point id
c   scctmp   C   scc code read from file
c
      character*(5*MXSTR) string
      character*10        scctmp, lngfip, staktmp, factmp, pnttmp
      character*5         fiptmp, sbrtmp
      integer*4           inpol, imntmp, iwktmp, idytmp
      integer*4           idxpol, idxscc, irec, jerr, i
      logical*4           l6000
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize the hash table entries ---
c
      do i=1,MXHASH_PAIR
         idxhsh_tpl_first_pair(i) = IMISS
      enddo
      do i=1,MXREF
         idxhsh_tpl_next_pair(i) = IMISS
      enddo
c
c   --- set error flag ---
c
      ierr = IFAIL
      l6000 = .FALSE.
c
c   --- initialize the array counter ---
c
      npfxrf = 0
      irec = 1
c
c   --- read next record ---
c
  111 continue
      read(IORXRF,'(A)',ERR=7000,END=222) string
      read(string,8000,ERR=7001,END=222) fiptmp, sbrtmp, factmp, 
     &         staktmp, pnttmp, scctmp, inpol, imntmp, iwktmp, idytmp
      irec = irec + 1
      call rgtjst( scctmp )
      call lftjst( factmp )
      call lftjst( sbrtmp )
      call lftjst( staktmp )
      call lftjst( pnttmp )
c
c   --- create long FIPS code ----
c
      lngfip(1:5) = fiptmp
      lngfip(6:10) = sbrtmp
c
c   --- get pollutant index ---
c
      idxpol = fndint ( inpol, ncrthd, isarod )
      if( idxpol .LE. 0 .AND. inpol .NE. 0 ) goto 6000
c
c   --- get hash entry for this pair, it will be added
c       to the array of not found  ---
c
      idxscc = fndhash_tpl( jerr, lngfip, factmp, staktmp, 
     &                                           pnttmp, scctmp )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- update the codes for this entry ---
c
      fipxrf(idxscc) = lngfip
      facxrf(idxscc) = factmp
      stkxrf(idxscc) = staktmp
      pntxrf(idxscc) = pnttmp
      sccxrf(idxscc) = scctmp
      if ( inpol .EQ. 0 ) then
         do i=1,MXCRIT
            imnxrf(idxscc,i) = imntmp
            iwkxrf(idxscc,i) = iwktmp
            idyxrf(idxscc,i) = idytmp
         enddo
      else
         imnxrf(idxscc,idxpol) = imntmp
         iwkxrf(idxscc,idxpol) = iwktmp
         idyxrf(idxscc,idxpol) = idytmp
      endif
c
c   --- read next record ---
c
      goto 111
  222 continue
c
c   --- processed entire file, echo to message file ---
c
      if( npfxrf .LE. 0 ) goto 7003
      write(IOWMSG,9000,ERR=7004) 'Number of ASC-SCC/temporal',
     &                ' profile cross reference records read: ',irec-1
      write(IOWMSG,9000,ERR=7004) 'Number of ASC-SCC/temporal',
     &                ' profile cross reference entries: ',npfxrf
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      if( .NOT. l6000 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  ASC-SCC/temporal ',
     &                      'profile cross reference file contains ',
     &                                       'unknown pollutant codes.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(A,I10)',ERR=9999) 'Unknown code: ',inpol
         write(ioerr,'(2A)',ERR=9999) 'All records with pollutant ',
     &                          'not in the inventory will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6000 = .TRUE.
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 
     &         'ERROR:  Reading ASC-SCC/temporal profile cross ',
     &                                'reference file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',xrffl(:strmin(xrffl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 
     &         'ERROR:  Reading ASC-SCC/temporal profile cross ',
     &                                'reference file at line:',irec
      write(ioerr,'(2A)',ERR=9999) 'File: ',xrffl(:strmin(xrffl))
      write(ioerr,'(A)',ERR=9999) 'Line read: '
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading ',
     &              'ASC-SCC/temporal profile cross reference file.'
      write(ioerr,'(/,2A)',ERR=9999) 'No ASC-SCC/temporal ',
     &                       'profile cross reference records found.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',xrffl(:strmin(xrffl))
      goto 9999
c
 7004 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c mj 8000 format(2(A5,1X),4(A10,1X),I5,I4,2(1X,I3))
 8000 format(2(A5,1X),4(A10,1X),I5,3I4)
 9000 format(/,T10,2A,I5)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
