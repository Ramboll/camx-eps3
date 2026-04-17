C**** HDRSPL.F
c
      subroutine hdrspl( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the EMBR header and stores the variables in common varibales
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
c      08/18/97  --mmj--  original development
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospltem.inc'
      include 'embrdat.inc'
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
c     rdhdr   --  reads the input EMBR header (library)
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   KEYDAT  C   character string for /DATA/ keyword
c
       character*(KEYLEN) KEYDAT
c
       parameter( KEYDAT = '/DATA/' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c   string  C   character string for temporary storage
c
      character*(LENREC)  string
      character*(MXSTR)   cdate
      integer*4           jerr, irec
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- call routine to read the first part of the header ---
c
      call rdhdr(jerr,
     &                 IORMBR )
      if( jerr .NE. ISUCES ) goto 9999

      call getime( cdate )
      write(IOWMSG,9001,ERR=7001) VERSON, cdate
      write(IOWMSG,9002,ERR=7001) '    Extracted from EMBR header'
      write(IOWMSG,9002,ERR=7001) 'UTM origin (km)',rgutme,rgutmn
      write(IOWMSG,9003,ERR=7001) 'UTM zone',irgzn
      write(IOWMSG,9002,ERR=7001) 'Grid cell width (km)',dxrgn,dyrgn
      write(IOWMSG,9003,ERR=7001) 'Number of cells',nxrgn,nyrgn
c
c   --- read rest of header until /DATA/ is found ---
c
         irec = 4
  111    continue
         irec = irec + 1
         read(IORMBR,END=7001,ERR=7001) string
         if( string(1:KEYLEN) .EQ. KEYDAT ) goto 222
         goto 111
c
c   --- set error flag to success ---
c
  222 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Errror Messages:
c-----------------------------------------------------------------------
c
 7001 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR:  Reading EMBR file.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(1X,A)
 9001 format(/,T20,A,A40,/)
 9002 format(T10,A,T40,:,': (',F8.2,',',F8.2,')')
 9003 format(T10,A,T40,:,':',I5,I5)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
