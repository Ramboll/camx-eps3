C**** WRTSTKCNT.F version 2.0 09/01/2007
c
      subroutine wrtstkcnt(ierr)
c
c-----------------------------------------------------------------------
c
c    This routine writes the master stack list.
c
c    Argument Declaration.
c     Outputs:
c        ierr  I  error code
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
      include 'uamregn.inc'
      include 'embrdat.inc'
      include 'iopigems.inc'
      include 'usrpigems.inc'
      include 'stkpigems.inc'
      include 'pigemstot.inc'
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
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*80 title
      character*20 units, codtit
      character*15 codnam(MXSTKFL)
      character*20 name(ICDMSK)
      integer*4    jerr, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- set the error flag ---
c
      ierr = IFAIL
      curofl = msgfl
c
c  --- setup the labels ---
c
      title = 'Count of Stacks Processed'
      units = ' '
      codtit = 'Input File'
      do i=1,nstkfl
          write(codnam(i),'(A,I4.4)') 'Stack file ',i
      enddo
      name(ICDSTK) = 'Total Stacks'
      name(ICDPIG) = 'Flagged for PiG'
      name(ICDCOL) = 'Co-located'
      name(ICDCOM) = 'User Combined'
      name(ICDMSK) = 'Mask File Applied'
c
c   --- call routine to output table of counts ---
c
      call wrttbl( jerr,
     &              IOWMSG, title, units, MXSTKFL, nstkfl, 15,
     &               codnam, codtit, ICDMSK, name, ntotstk, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- set error flag and return ---
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
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
