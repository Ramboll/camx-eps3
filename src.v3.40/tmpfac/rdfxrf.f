C**** RDFXRF.F 
c
      subroutine rdfxrf( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the ASC-SCC/temporal profiles cross reference file and stores
c    the data in common variables to be used by the TMPFAC program.
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
      include 'iotmpfac.inc'
      include 'tpfacprf.inc'
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
c   fndint   I  returns the index of an integer in an array of integers
c   setfxf   I  returns the index of cross reference data in arrays
c   strmin   I  returns actual length of string
c
      integer*4 fndint
      integer*4 setfxf
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   factmp   C   NEDS plant id read from file
c   fiptmp   C   FIPS code read from file
c   sbrtmp   C   Subregion code read from file
c   lngfip   C   Long FIPS code -- FIPS code and subregion code
c   idytmp   I   diurnal profile code read from file
c   imntmp   I   monthly profile code read from file
c   inpol    I   pollutant code read from file
c   staktmp  C   stack id read from file
c   iwktmp   I   weekly profile code read from file
c   pnttmp   C   point indentification code
c   scctmp   C   SCC code read from file
c
      character*(5*MXSTR) string
      character*10        scctmp, lngfip, factmp, staktmp, pnttmp
      character*5         fiptmp, sbrtmp
      integer*4           inpol, imntmp, iwktmp, idytmp
      integer*4           idxpol, idxscc, irec, i, j
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- initialize the xref arrays to missing ---
c
      npfxrf = 0
      do i=1,MXREF
        do j=1,MXPOL
          idyxrf(i,j) = IMISS
          imnxrf(i,j) = IMISS
          iwkxrf(i,j) = IMISS
          lglob(i,j) = .FALSE.
        enddo
      enddo
c
c   --- if there is no file to read, just return ---
c
      if( .NOT. lxref ) then
         ierr = ISUCES
         goto 9999
      endif 
c
c   --- initialize the array counter ---
c
      irec = 0
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
c   --- create the long FIPS code ---
c
      lngfip(1:5) = fiptmp
      lngfip(6:10) = sbrtmp
c
c   --- get pollutant index, if it does not exist add it ---
c
      idxpol = fndint ( inpol, ncdpol, ipolcd )
      if( idxpol .LE. 0 .AND. inpol .NE. 0 ) then
         ncdpol = ncdpol + 1
         if( ncdpol .GT. MXPOL ) goto 7002
         ipolcd(ncdpol) = inpol
         idxpol = ncdpol
      endif
c
c   --- search array for existing XREF data ---
c
      idxscc = setfxf( lngfip, factmp, staktmp, pnttmp, scctmp )
      if( idxscc .LE. 0 ) then
          npfxrf = npfxrf + 1
          if( npfxrf .GT. MXREF ) goto 7003
          fipxrf(npfxrf) = lngfip
          facxrf(npfxrf) = factmp
          stkxrf(npfxrf) = staktmp
          pntxrf(npfxrf) = pnttmp
          sccxrf(npfxrf) = scctmp
          idxscc = npfxrf
      endif
      if( inpol .EQ. 0 ) then
         do idxpol=1,MXPOL
           if( imnxrf(idxscc,idxpol)  .LE. 0 ) 
     &                                imnxrf(idxscc,idxpol) = imntmp
           if( iwkxrf(idxscc,idxpol)  .LE. 0 ) 
     &                                iwkxrf(idxscc,idxpol) = iwktmp
           if( idyxrf(idxscc,idxpol)  .LE. 0 ) 
     &                               idyxrf(idxscc,idxpol) = idytmp
           lglob(idxscc,idxpol) = .TRUE.
         enddo
      endif
c
c   --- read next record ---
c
      goto 111
  222 continue
c
c   --- processed entire file, echo to message file ---
c
      if( npfxrf .LE. 0 ) goto 7004
      write(IOWMSG,'(/,T10,A,A,I5)',ERR=7005) 'Number of temporal',
     &                ' profile cross reference records read: ',irec
      write(IOWMSG,'(/,T10,A,A,I5)',ERR=7005) 'Number of temporal',
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
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A,I8)',ERR=9999)
     &         'ERROR:  Reading input temporal profile cross ',
     &                                'reference file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',inxrfl(:strmin(inxrfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999)
     &         'ERROR:  Reading input temporal profile cross ',
     &                                'reference file at line:',irec
      write(ioerr,'(2A)',ERR=9999) 'File: ',inxrfl(:strmin(inxrfl))
      write(ioerr,'(A)',ERR=9999) 'Line read: '
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading ',
     &              'input temporal profile cross reference file.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &                                  'pollutant codes exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXPOL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXPOL ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading ',
     &              'input temporal profile cross reference file.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &          'temporal profile cross reference records exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXREF
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXREF ',
     &                                                  'and recompile.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading ',
     &                    'temporal profile cross reference file.'
      write(ioerr,'(2A)',ERR=9999) 'No temporal ',
     &                       'profile cross reference records found.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',inxrfl(:strmin(inxrfl))
      goto 9999
c
 7005 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(2(A5,1X),4(A10,1X),I5,I4,2(I4))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
