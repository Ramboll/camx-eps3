C**** HDRCTL.F 
c
      subroutine hdrctl( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the header and stores the variables in common varibales
c    then it creates the program name/date/time string and writes
c    the rest of the header
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
      include 'iocntlem.inc'
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
c     getime  --  makes system call to get date and time (library)
c     rdhdr   --  reads the input EMBR header (library)
c     filhdr  --  writes the new EMBR header (library)
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   string  C   character string for temporary storage
c   cdate   C   character string for date and time
c   jerr    I   error flag for subroutine calls
c
      character*(LENREC) string
      character*40       cdate
      integer*4          jerr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      curofl = ombrfl
      curifl = imbrfl
c
c   --- call routine to get date and time string, and create header
c       string ---
c
      call getime(cdate)
      string = VERSON // cdate
c
c   --- call routine to read the first part of the header ---
c
      call rdhdr(jerr,
     &                 IORMBR )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- no header data changes, so write the new header ---
c
      call filhdr(jerr, string,
     &                          IORMBR, IOWMBR )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
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
