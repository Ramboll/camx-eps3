C**** HDRSPC.F
c 
      subroutine hdrspc( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the header of the output EMBR file which includes
c    the new variables spclst, molwt and spcrit.
c    These come from the /SPECIES LIST/ input packet (uamcntl.inc).
c    All other variables come from the existing header.
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
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
      include 'iospcems.inc'
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'factors.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    library Subroutines Called:
c-----------------------------------------------------------------------
c
c   wrhdr  writes the header of an EMBR file
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c    MULTI    C   special string to indicate multiple parents
c
      character*10 MULTI
c
      parameter( MULTI  = 'MULTIPRNTS' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c    cdate  C  character string for holding date/tine
c    hdrver C  character string for holding version and date/time
c    jerr   I  error code for subroutines called
c
      character*(LENREC) hdrver
      character*40       cdate
      integer*4          jerr, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      curofl = infl
      curifl = outfl
c
c  --- Move species list from uamcntl.inc to the embrdat.inc common block.---
c  --- If a species has more than one parent then set the parent name ---
c  --- to MULTI.  This is necessary as CNTLEM requires a species to have ---
c  --- a single parent in order to appropriately apply controls.  ---
      nspchd = nspecs
      do i=1,nspecs
          spclst(i) = spcnam(i)
          if ( ncntpar(i) .GT. 1 ) then
             spcrit(i) = MULTI
          else
             spcrit(i) = crtnam(ispcrt(i))
             if ( ncntpar(i).GT.0  
     &               .AND.  ispcrt(i).NE.iparnt(i) )  
     &          spcrit(i) = crtnam(iparnt(i))
          endif
      enddo
c
c   --- call routine to get date and time string, and create header
c       string ---
c
      call getime(cdate)
      hdrver = VERSON // cdate
c
c  --- call routine to read and write the EMBR header ---
c
      call filhdr(jerr, hdrver,
     &                          IOREMB, IOWEMB)
      if(jerr .NE. ISUCES) goto 9999
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
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
