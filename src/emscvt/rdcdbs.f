C**** RDCDBS.F 
c
      subroutine rdcdbs( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the compound data base file and stores the data in common
c    blocks to be used by the EMSCVT program.
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
      include 'ioemscvt.inc'
      include 'usremcvt.inc'
      include 'cmpdbase.inc'
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
c   fndchr   I   returns the index of a string in an array of strings
c   strmin   I   returns the actual length of a string
c
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   MXBND  I   maximum length of string containin bonding fractions
c   NTOX   C   string for toxic compound flag
c   NVOC   C   string for reactivity flag
c   RVOC   C   string for reactivity flag
c   TOX    C   string for toxic compound flag
c
      character*1 NTOX
      character*1 TOX
      character*4 NVOC
      character*4 RVOC
      integer*4   MXBND
c
      parameter( MXBND = MXSPEC*5 )
      parameter( NTOX = ' ' )
      parameter( NVOC = 'TOG ' )
      parameter( RVOC = 'VOC ' )
      parameter( TOX = 'T' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   bndtmp   C    temporary string for reading bonding fractions
c   ibnd     I    actual length of string containing bonding fractions
c   reatmp   C    temporary string for reading reactivity flag
c   spctmp   C    temporary string for read species names
c   toxtmp   C    temporary string for reading toxic flag
c
      character*(5*MXSTR) string
      character*(MXBND)   bndtmp
      character*10        spctmp
      character*4         reatmp
      character*1         toxtmp
      integer*4           irec, ibnd, i
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
      if( nspcmp .GT. MXSPEC ) goto 7003
c
c  --- read each species name and store in array ---
c
      do i=1,nspcmp
         irec = irec + 1
         read(IORCDB,'(A)',ERR=7000,END=7001) string
         spctmp = string(:10)
         call low2up( spctmp )
         call lftjst( spctmp )
         spccmp(i) = spctmp
      enddo
c
c  --- check for compatibilty of species lists ---
c
      do i=1,nspcmp
         idxspl(i) = fndchr( spccmp(i), 10, spclst, nspecs )
         if( idxspl(i) .LE. 0 ) goto 7004
         idxcmp(idxspl(i)) = i
      enddo
c
c  --- read each compound record ---
c
      ncmpnd = 0
      ibnd = nspcmp*5
  111 continue
      ncmpnd = ncmpnd + 1
      if( ncmpnd .GT. MXCOMP ) goto 7005
      irec = irec + 1
      read(IORCDB,'(A)',ERR=7000,END=222) string
      read(string,8000,ERR=7002) isarod(ncmpnd), bndtmp(1:ibnd),
     &                                    reatmp, molcmp(ncmpnd), toxtmp
c
c  --- set reactivity flag ---
c
      call low2up( reatmp )
      call lftjst( reatmp )
      if( reatmp .EQ. RVOC ) then
         lreact(ncmpnd) = .TRUE.
      else if( reatmp .EQ. NVOC ) then
         lreact(ncmpnd) = .FALSE.
      else
         goto 7006
      endif
c
c  --- set toxic flag ---
c
      call low2up( toxtmp )
      if( toxtmp .EQ. TOX ) then
         ltoxic(ncmpnd) = .TRUE.
      else if( toxtmp .EQ. NTOX )  then
         ltoxic(ncmpnd) = .FALSE.
      else
         goto 7007
      endif
c
c  --- read bonding fractions from string ---
c
      read(bndtmp,8001,ERR=7002) (bndfrc(ncmpnd,i),i=1,nspcmp)
      goto 111
c
c  --- echo data to message file ---
c
  222 continue
      ncmpnd = ncmpnd - 1
      write(IOWMSG,'(/,A,I8)',ERR=7008)
     &         'Number of compounds read from compound data base: ',
     &                                                 ncmpnd
      write(IOWMSG,'(/,T20,A)',ERR=7008)
     &                  'Species list from compound data base'
      do i=1,nspcmp
        write(IOWMSG,'(T20,2A)',ERR=7008) '             ',spccmp(i)
      enddo
      write(IOWMSG,'(//)',ERR=7008)
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
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading chemical',
     &                       ' compound database file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',cdbsfl(:strmin(cdbsfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading chemical',
     &                       ' compound database file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',cdbsfl(:strmin(cdbsfl))
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading chemical',
     &                       ' compound database file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',cdbsfl(:strmin(cdbsfl))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading chemical',
     &                                   ' compound database file.'
      write(ioerr,'(A)',ERR=9999) 'Number of species exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Species in file  : ',nspcmp
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSPEC
      write(ioerr,'(3A,//)',ERR=9999) 'Increase the parameter MXSPEC ',
     &                                                  'and recompile.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading chemical',
     &                                   ' compound database file.'
      write(ioerr,'(2A)',ERR=9999) 'Species in compound data ',
     &                         'base not among requested species.'
      write(ioerr,'(2A)',ERR=9999) 'Species name: ',spccmp(i)
      write(ioerr,'(2A)',ERR=9999) 'Either update the database file ',
     &                   'or add this species to the user input file.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading chemical',
     &                                   ' compound database file.'
      write(ioerr,'(A)',ERR=9999) 
     &                  'Number of chemical compounds exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXCOMP
      write(ioerr,'(3A,//)',ERR=9999) 'Increase the parameter MXCOMP ',
     &                                                  'and recompile.'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading chemical',
     &                                   ' compound database file.'
      write(ioerr,'(A)',ERR=9999) 'Invalid reactivity flag found.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',cdbsfl(:strmin(cdbsfl))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      write(ioerr,'(10X,2A)',ERR=9999) 'Flag read: ',reatmp
      write(ioerr,'(10X,A)',ERR=9999) 'Acceptable responses are: '
      write(ioerr,'(20X,A)',ERR=9999) RVOC
      write(ioerr,'(20X,A,//)',ERR=9999) NVOC
      goto 9999
c
 7007 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading chemical',
     &                                   ' compound database file.'
      write(ioerr,'(A)',ERR=9999) 'Invalid toxic flag found.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',cdbsfl(:strmin(cdbsfl))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      write(ioerr,'(10X,2A)',ERR=9999) 'Flag read: ',toxtmp
      write(ioerr,'(10X,A)',ERR=9999) 'Acceptable responses are: '
      write(ioerr,'(20X,A)',ERR=9999) TOX
      write(ioerr,'(20X,A,//)',ERR=9999) NTOX
      goto 9999
c
 7008 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(I5,20X,A,1X,A4,F10.0,A1)
 8001 format(300(:,F5.0))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
