C**** WRTEMSTOT.F version 2.0 09/01/2007
c
      subroutine wrtemstot(ierr)
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
      character*25 codnam(MXSTKFL)
      character*10 name(MXCRIT)
      integer*4    jerr, numfiles, icdlen, i
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
      title = 'Table of Emissions Processed'
      units = ' '
      codtit = 'Input File'
      do i=1,nemsfl
          write(codnam(i),'(A,I4.4)') 'Emiss file ',i
      enddo
      codnam(nemsfl+1) = 'Statics sources file'
      numfiles = nemsfl + 1
      icdlen = 25
      if( .NOT. lstaticfl ) then
        numfiles = nemsfl
        icdlen = 15
      endif
      do i=1,ncrthd
         name(i) = crtnam(i)
      enddo
c
c   --- call routine to output table of processed emissions ---
c
      call wrctbl8( jerr,
     &              IOWMSG, title, units, MXEMSFL+1, numfiles, icdlen,
     &               codnam, codtit, ncrthd, name, emstot, .TRUE. )
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
