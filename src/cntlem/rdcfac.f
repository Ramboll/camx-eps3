C**** CTLDSC.F 
c
      subroutine rdcfac( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the control factor file and stores the data in common
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
      include 'hash.inc'
      include 'ctlreg.inc'
      include 'ctldsc.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   temporary storage for seaching fo keyword
c   nmxctl  I   temporary variable for max array dimension
c   jerr    I   error flag for subroutine calls
c
      character*(KEYLEN) keywrd
      integer*4          jerr, nmxctl
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
      keywrd = '/PROJECT PTS/'
      nmxctl = MXPPT
      call rdpfac(jerr, ncdppt, pptfip, pptsic, pptfac, nmxctl, keywrd)
      if( jerr .NE. ISUCES ) goto 9999
c
      keywrd = '/PROJECT AMS/'
      nmxctl = MXPAM
      call rdpfac(jerr, ncdpam, pamfip, pamsic,  pamfac, nmxctl, keywrd)
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- read in discretionary controls ---
c
      keywrd = '/SOURCE CATEGORY/'
      nmxctl = MXSRC
      call rdsrc(jerr)
      if( jerr .NE. ISUCES) goto 9999
c
      keywrd = '/FIPS CODE/'
      call rdfips(jerr)
      if( jerr .NE. ISUCES ) goto 9999
c
c    ---set error flag to sucess and return ---
c
      ierr = ISUCES
      go to 9999
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
