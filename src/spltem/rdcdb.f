C**** RDCDB.F
c
      subroutine rdcdb( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the compound data base file extracting just the species ids
c    and names
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
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
      include 'wgtfrac.inc'
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
c   fndint   I   returns the index of an integer in an array of integers
c   strmin   I   returns the actual length of a string
c
      integer*4 fndint
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   NVOC   C   string for reactivity flag
c   RVOC   C   string for reactivity flag
c
      character*4 NVOC
      character*4 RVOC
c
      parameter( NVOC = 'TOG ' )
      parameter( RVOC = 'VOC ' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(5*MXSTR)  string
      character*(5*MXSPEC) tmpstr
      character*(MXSTR)    fulltmp
      character*20         spctmp
      character*4          reatmp
      integer*4            irec, nspcmp, i, ibnd, ispccd, idxsrd
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
c  --- read the species names for this data base ---
c
      irec = irec + 1
      read(IORCDB,'(A)',ERR=7000,END=7001) string
      read(string,'(I10)',ERR=7002) nspcmp
c
c  --- read and skip species names ---
c
      do i=1,nspcmp
         irec = irec + 1
         read(IORCDB,'(A)',ERR=7000,END=7001) string
      enddo
c
c  --- read each compound record ---
c
      ibnd = nspcmp*5
  111 continue
      irec = irec + 1
      read(IORCDB,'(A)',ERR=7000,END=222) string
      read(string,8000,ERR=7002) ispccd, spctmp, tmpstr(1:ibnd), 
     &                           reatmp, fulltmp

      idxsrd = fndint ( ispccd, isrdcnt, isrdlst )
      if( idxsrd .GT. 0 ) then
         call lftjst( spctmp )
         call lftjst( fulltmp )
         srdnam(idxsrd) = spctmp
         srdlng(idxsrd) = fulltmp
c
c        --- set reactivity flag ---
         call low2up( reatmp )
         call lftjst( reatmp )
         if( reatmp .EQ. RVOC ) then
            lreact(idxsrd) = .TRUE.
         else if( reatmp .EQ. NVOC ) then
            lreact(idxsrd) = .FALSE. 
         else    
            goto 7006
         endif   
      endif


      goto 111
c
c  --- echo data to message file ---
c
  222 continue
c
      write(IOWMSG,'(/,T10,A,I8)',ERR=7006) 
     &  'Number of compound database records read: ', irec-1
c
c
c  --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,2A,I8)',ERR=9999) 'ERROR: Reading chemical',
     &                       ' compound database file at line: ',irec
      write(IOWSTD,'(2A,//)',ERR=9999)  'File: ',cdbfl(:strmin(cdbfl))
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,2A,I8)',ERR=9999) 'ERROR: Reading chemical',
     &                       ' compound database file at line: ',irec
      write(IOWSTD,'(2A)',ERR=9999)  'File: ',cdbfl(:strmin(cdbfl))
      write(IOWSTD,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,2A,I8)',ERR=9999) 'ERROR: Reading chemical',
     &                       ' compound database file at line: ',irec
      write(IOWSTD,'(2A)',ERR=9999)  'File: ',cdbfl(:strmin(cdbfl))
      write(IOWSTD,'(A)',ERR=9999) 'Line read:'
      write(IOWSTD,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,2A)',ERR=9999) 'ERROR: Reading chemical',
     &                                   ' compound database file.'
      write(IOWSTD,'(A)',ERR=9999) 'Invalid reactivity flag found.'
      write(IOWSTD,'(2A)',ERR=9999)  'File: ',cdbfl(:strmin(cdbfl))
      write(IOWSTD,'(A)',ERR=9999) 'Line read:'
      write(IOWSTD,'(A,//)',ERR=9999) string(:strmin(string))
      write(IOWSTD,'(10X,2A)',ERR=9999) 'Flag read: ',reatmp
      write(IOWSTD,'(10X,A)',ERR=9999) 'Acceptable responses are: '
      write(IOWSTD,'(20X,A)',ERR=9999) RVOC
      write(IOWSTD,'(20X,A,//)',ERR=9999) NVOC
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(IOWSTD,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(I5,A20,A,1X,A4,11x,A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
