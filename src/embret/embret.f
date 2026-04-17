C**** EMBRET.F 
c
      program embret
c
c-----------------------------------------------------------------------
c
c    This program retrieves the data records in an EMBR file and converts
c    them to EMAR format.  The header and trailer are also written to
c    the EMAR file.  The data can be retrieved by a number of criteria
c    specified in the user input file.  If no user input file is given,
c    i.e. a blank string on input, then the entire file is retrieved.
c    The user input file, which is in packet format, supports the
c    following packets:
c
c      /COUNTY/     --   only records in the specified county
c      /SUBRGN/     --   only records in the specified subregion
c      /SIC/        --   only records with the specified SIC
c      /ASC-SCC/    --   only records with the specified ASC-SCC
c      /PLANT/      --   only records matching the specified plant id
c      /SUBGRID/    --   only those cells within the specified subgrid
c
c    Calling Sequence:
c
c            EMBRET
c              |----- init_hash_msgtot
c              |----- opnret
c              |----- rduret
c              |         |----- fndkey
c              |
c              |----- hdrret
c              |         |----- getime
c              |
c              |----- rdembr
c              |----- totret
c              |----- wremar
c              |----- wtlret
c              |         |----- wrctbl
c              |
c              |----- clsret
c
c   clsret  --  closes all file units
c   fndkey  --  searches a file for a keyword (library)
c   getime  --  gets the curretn date and time (library)
c   hdrret  --  reads and writes the header data
c   init_hash_msgtot  -- initializes hash table indexes
c   opnret  --  reads filename and opens files
c   rdembr  --  reads one EMBR data record (library)
c   rduret  --  reads the user input file
c   totret  --  stores emissions in totals arrays
c   wrctbl  --  write one table of totals
c   wrembr  --  writes on EMAR data record (library)
c   wtlret  --  writes the tables of processed emissions
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
      include 'cntylst.inc'
      include 'msgtot.inc'
      include 'ioembret.inc'
      include 'embret.inc'
      include 'embretot.inc'
      include 'uamcntl.inc'
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   fndchr   I  returns index of character string in array
c
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 ierr, idx, irec, idxfip, i
c
c-----------------------------------------------------------------------
c    Entry Point:
c-----------------------------------------------------------------------
c
c
c  --- initialize the tables outputs to produce all tables ---
c
      lcrtcty(IDXFMT) = .TRUE.
      lcrtasc(IDXFMT) = .TRUE.
      lcrtfcd(IDXFMT) = .TRUE.
      lcrthr(IDXFMT) = .TRUE.
      lcrthcd(IDXFMT) = .TRUE.
      lcrtfac(IDXFMT) = .TRUE.
      lcrtstk(IDXFMT) = .TRUE.
      lcrtpnt(IDXFMT) = .TRUE.
c
      lspccty(IDXFMT) = .TRUE.
      lspcasc(IDXFMT) = .TRUE.
      lspcfcd(IDXFMT) = .TRUE.
      lspchr(IDXFMT) = .TRUE.
      lspchcd(IDXFMT) = .TRUE.
      lspcfac(IDXFMT) = .TRUE.
      lspcstk(IDXFMT) = .TRUE.
      lspcpnt(IDXFMT) = .TRUE.
      lspcpid = .TRUE.
c
      lcrtcty(IDXCSV) = .FALSE.
      lcrtasc(IDXCSV) = .FALSE.
      lcrtfcd(IDXCSV) = .FALSE.
      lcrthr(IDXCSV) = .FALSE.
      lcrthcd(IDXCSV) = .FALSE.
      lcrtfac(IDXCSV) = .FALSE.
      lcrtstk(IDXCSV) = .FALSE.
      lcrtpnt(IDXCSV) = .FALSE.
      lcrtpid = .FALSE.
c
      lspccty(IDXCSV) = .FALSE.
      lspcasc(IDXCSV) = .FALSE.
      lspcfcd(IDXCSV) = .FALSE.
      lspchr(IDXCSV) = .FALSE.
      lspchcd(IDXCSV) = .FALSE.
      lspcfac(IDXCSV) = .FALSE.
      lspcstk(IDXCSV) = .FALSE.
      lspcpnt(IDXCSV) = .FALSE.
      lspcpid = .FALSE.
c  --- call routine to initialize the hash table indexes ---
c
      call init_hash_msgtot( )
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opnret(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      if( lusrin ) then
         call rduret(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to read and write the EMBR header ---
c
      call hdrret(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- read an EMBR record ---
c
      irec = 1
  111 continue
      call rdembr(ierr,
     &                 IORMBR, irec )
      if( ierr .EQ. IEOD ) goto 222
      if( ierr .NE. ISUCES ) goto 9999
      irec = irec + 1
c
c   --- call routine to get index of FIPS code in county list ---
c
      idxfip = fndchr ( fipin, 5, fipcd, ncdfip )
      if( idxfip .LE. 0 ) then
         ncdfip = ncdfip + 1
         if( ncdfip .GT. MXFIP ) goto 7000
         idxfip = ncdfip
         fipcd(idxfip) = fipin
      endif
c
c   --- store input emissions in arrays ---
c
      call totret( ierr,IDXIN,emin,spcpol,ICDIN,idxfip,
     &                                            scccd,invbeg,invend)
c
c  --- check for match of /COUNTY/ packet ---
c
      if( ncdcty .GT. 0 ) then
          idx = fndchr( fipin, 5, ctyscd, ncdcty )
          if( idx .LE. 0 ) goto 111
      endif
c
c  --- check for match of /SIC/ packet ---
c
      if( ncdsic .GT. 0 ) then
          idx = fndchr( siccd, 4, rsiccd, ncdsic )
          if( idx .LE. 0 ) goto 111
      endif
c
c  --- check for match of /ASC-SCC/ packet ---
c
      if( ncdscc .GT. 0 ) then
          idx = fndchr( scccd, 10, rscccd, ncdscc )
          if( idx .LE. 0 ) goto 111
      endif
c
c  --- check for match of /PLANT/ packet ---
c
      if( ncdpln .GT. 0 ) then
          idx = fndchr( plant, 10, plntcd, ncdpln )
          if( idx .LE. 0 ) goto 111
      endif
c
c  --- check within range of /SUBGRID/ packet ---
c
      if( nsub .GT. 0 ) then
          do i=1,nsub
             if( INT(ijcell/1000) .GE. INT(ibegcl(i)/1000) .AND.
     &           MOD(ijcell,1000) .GE. MOD(ibegcl(i),1000) .AND.
     &           INT(ijcell/1000) .LE. INT(iendcl(i)/1000) .AND.
     &           MOD(ijcell,1000) .LE. MOD(iendcl(i),1000) )  goto 333
          enddo
      goto 111
      endif
c
c  --- call routine to add totals ---
c
  333 continue
      do i=1,ncrthd
        emout(i) = emin(i)
      enddo
      do i=1,MXSPEC
        spcout(i) = spcpol(i)
      enddo
      call totret( ierr,IDXOUT,emin,spcpol,ICDOUT,idxfip,
     &                                            scccd,invbeg,invend)
c
c  --- call routine to write record and read next record ---
c
      call wremar( ierr,
     &                   IOWEMR, nrec(IDXOUT) )
      goto 111
c
c  --- call routine to read and write EMBR trailer ---
c
  222 continue
      call traret(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals ---
c
      call wtlret(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- looks successful, write message and reset flag ---
c
      write(IOWSTD,'(//,2A,//)',ERR=9999) 'Successful completion of ',
     &                                                       VERSON
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Procesing EMBR file.'
      write(ioerr,'(A)',ERR=9999) 'Number of counties exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXFIP
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXFIP ',
     &                                                 'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Exit Point:
c-----------------------------------------------------------------------
c
c  --- call routine to close files ---
c
 9999 continue
      call clsret()
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
