C**** HDRCHM.F 
c 
      subroutine hdrchm( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the header of the output EMBR file
c    the variables spclst, molwt and spcrit are new.
c    These come from the splits common block.
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
      include 'embrdat.inc'
      include 'sccxref.inc'
      include 'uamcntl.inc'
      include 'iochmspl.inc'
      include 'splits.inc'
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
c  --- move variables from the splits.inc common block
c      to the embrdat.inc common block
c
      nspchd = nspecs
      do i=1,nspecs
          spclst(i) = spcnam(i)
          if( indspl(i) .GT. 0 ) then
              molwt(i) = splmol(indspl(i))
              spcrit(i) = splcrt(indspl(i))
              ispcrt(i) = indcrt(i)
          else 
             if( idflst(i) .GT. 0 ) then
                 molwt(i) = dftmlwt(idflst(i))
                 spcrit(i) = dftcrit(idflst(i))
                 ispcrt(i) = idfcrt(idflst(i))
             else
                 goto 7000
             endif 
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
      nspchd = 0
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A,/,A)',ERR=9999) 'ERROR:  Species in species ',
     &                         'list is not in the splits file or in',
     &                                       '/DEFAULT SPLITS/ packet.'
      write(ioerr,'(10X,2A)',ERR=9999) 'Species    : ',spcnam(i)
      write(ioerr,'(2A,/,A,//)',ERR=9999) 'Remove the species from ',
     &  'the species list or supply data','for how it should be split.'
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
