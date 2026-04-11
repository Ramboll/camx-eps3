c*** IOSPLTEM.INC
c
c-----------------------------------------------------------------------
c
c    include file for I/O parameters and variables for SPLTEM program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     08/11/97  -mmj-   original development
c
c-----------------------------------------------------------------------
c    program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   c   character string paramater conatining version of the
c                CHMSPL program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 SPLTEM utility v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORMBR   i   unit number for input EMBR file
c   IORINP   i   unit number for user input file
c   IORPRW   i   unit number for weight profiles
c   IORXRF   i   unit number for SCC cross reference file
c   IORCDB   i   unit number for the compound database (species names)
c   IOWDAT   i   unit number for output lowlevel data file
c   IOWELE   i   unit number for output elevated data file
c   IOWMSG   i   unit number for message file
c
      integer*4 IORMBR
      integer*4 IORINP
      integer*4 IORPRW
      integer*4 IORXRF
      integer*4 IORCDB
      integer*4 IOWDAT
      integer*4 IOWELE
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IORMBR = 11 )
      parameter ( IORPRW = 12 )
      parameter ( IORXRF = 13 )
      parameter ( IORCDB = 14 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWDAT = 21 )
      parameter ( IOWELE = 22 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   cdbfl    c   compound database filename
c   elefl    c   output elevated data filename
c   infl     c   input EMBR filename
c   msgfl    c   message filename
c   outfl    c   output lowlevel data filename
c   uinpfl   c   user input filename
c   wtfrfl   c   weight profile filename
c   xrffl    c   scc cross reference filename
c
      character*(MXSTR) cdbfl
      character*(MXSTR) elefl
      character*(MXSTR) infl
      character*(MXSTR) msgfl
      character*(MXSTR) outfl
      character*(MXSTR) uinpfl
      character*(MXSTR) prwtfl
      character*(MXSTR) xrffl
c
      common /iofil/ uinpfl, infl, prwtfl, xrffl, cdbfl,
     &               msgfl, outfl, elefl
c
c-----------------------------------------------------------------------
c   flags for optional files:
c-----------------------------------------------------------------------
c
c    lelev    l   flag for output elevated data provided
c
      logical*4 lelev
c
      common /usedat/ lelev
