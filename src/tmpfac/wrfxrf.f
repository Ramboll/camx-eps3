C**** WRFXRF.F 
c
      subroutine wrfxrf( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the newly computed ASC-SCC/temporal profiles cross
c    data to the cross reference file
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
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin   I  returns actual length of string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 imnsam, iwksam, idysam, irec, i, j
      logical*4 lsame
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
c   --- loop through each cross reference record ---
c
      do i=1,npfxrf
c
c   --- see if all pollutants are the same ---
c
         imnsam = IMISS
         iwksam = IMISS
         idysam = IMISS
         do j=1,MXPOL
            if( imnsam .EQ. IMISS .AND. imnxrf(i,j) .NE. IMISS )
     &                                           imnsam = imnxrf(i,j)
            if( iwksam .EQ. IMISS .AND. iwkxrf(i,j) .NE. IMISS )
     &                                           iwksam = iwkxrf(i,j)
            if( idysam .EQ. IMISS .AND. idyxrf(i,j) .NE. IMISS )
     &                                           idysam = idyxrf(i,j)
         enddo
         lsame = .TRUE.
         do j=1,MXPOL
            if( imnxrf(i,j) .NE. IMISS .AND. 
     &                imnxrf(i,j) .NE. imnsam ) lsame = .FALSE.
            if( iwkxrf(i,j) .NE. IMISS .AND. 
     &                iwkxrf(i,j) .NE. iwksam ) lsame = .FALSE.
            if( imnxrf(i,j) .NE. IMISS .AND. 
     &                idyxrf(i,j) .NE. idysam ) lsame = .FALSE.
         enddo 
c
c  --- if they are all the same, write out a global record ---
c
         if( lsame ) then
            write(IOWXRF,9000,ERR=7000) fipxrf(i)(1:5), fipxrf(i)(6:10),
     &                     facxrf(i), stkxrf(i), pntxrf(i), sccxrf(i),  
     &                                 0, imnsam, iwksam, idysam
            irec = irec + 1
         else
c
c   --- should write one pollutant per record ---
c
             do 10 j=1,ncdpol
c
c   --- if any codes are missing, skip this entry ---
c
                if( imnxrf(i,j) .EQ. IMISS .OR.
     &                    iwkxrf(i,j) .EQ. IMISS .OR.
     &                                  idyxrf(i,j) .EQ. IMISS ) goto 10
c
c   --- write the record ---
c
                write(IOWXRF,9000,ERR=7000) fipxrf(i)(1:5), 
     &                    fipxrf(i)(6:10), facxrf(i), stkxrf(i), 
     &                    pntxrf(i), sccxrf(i), ipolcd(j), imnxrf(i,j), 
     &                                         iwkxrf(i,j), idyxrf(i,j)
                irec = irec + 1
   10        continue
         endif
c
      enddo
c
c  --- echo number of records to cross nessage file ---
c
      write(IOWMSG,'(/,T10,2A,I8)',ERR=7001) 'Number of cross ',
     &                            'reference records written: ',irec
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999)
     &         'ERROR:  Writing output temporal profile cross ',
     &                                               'reference file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',oxrfl(:strmin(oxrfl))
      goto 9999
c
 7001 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(2(A5,1X),4(A10,1X),I5.5,I4,2(I4))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
