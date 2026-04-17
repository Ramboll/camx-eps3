c*** IOEMSCVT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for EMSCVT program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater conatining version of the
c                EMSCVT program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 EMSCVT utility v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORCDB   I   unit number for compound data base
c   IORPRW   I   unit number for profile weight file
c   IORSPL   I   unit number for input splitting factors file
c   IORUIN   I   unit number for user input file
c   IORXRF   I   unit number for input XREF file
c   IORCTL   I   unit number for control factor file
c   IOWMSG   I   unit number for message file
c   IOWSPL   I   unit number for output splitting factors file
c   IOWXRF   I   unit number for output XREF file
c   IOWPRO   I   unit number for output GSPRO formatted speciation factors
c   IOWCNV   I   unit number for output GXCNV formatted VOC-to-TOG factors
c
      integer*4 IORCDB
      integer*4 IORPRW
      integer*4 IORSPL
      integer*4 IORUIN
      integer*4 IORXRF
      integer*4 IORCTL
      integer*4 IOWMSG
      integer*4 IOWSPL
      integer*4 IOWXRF
      integer*4 IOWPRO
      integer*4 IOWCNV
c
      parameter ( IORUIN = 10 )
      parameter ( IORCDB = 11 )
      parameter ( IORPRW = 12 )
      parameter ( IORXRF = 13 )
      parameter ( IORSPL = 14 )
      parameter ( IORCTL = 15 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWXRF = 21 )
      parameter ( IOWSPL = 22 )
      parameter ( IOWPRO = 23 )
      parameter ( IOWCNV = 24 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   cdbsfl   C   compound data base filename
c   inxrfl   C   input XREF filename
c   ctlfl    C   control factors filename
c   isplfl   C   input split factors filename
c   msgfl    C   message output filename
c   osplfl   C   output temporal filename
c   oxrfl    C   output XREF filename
c   prwtfl   C   profile weight input filename
c   uinfl    C   user input filename
c
      character*(MXSTR) cdbsfl
      character*(MXSTR) inxrfl
      character*(MXSTR) ctlfl
      character*(MXSTR) isplfl
      character*(MXSTR) msgfl
      character*(MXSTR) osplfl
      character*(MXSTR) oxrfl
      character*(MXSTR) prwtfl
      character*(MXSTR) uinfl
      character*(MXSTR) ogspfl
      character*(MXSTR) ogscfl
c
      common /iofil/ uinfl, prwtfl, cdbsfl, inxrfl, isplfl, msgfl,
     &               oxrfl, osplfl, ctlfl, ogspfl, ogscfl
c
c-----------------------------------------------------------------------
c   Other variables:
c-----------------------------------------------------------------------
c
c   lsplit L    true if using an existing splitting factors file
c   lxref  L    true if using an existing XREF file
c   lctlfl L    true if using a control factors file
c   loxrfl L    true if writing an output XREF file
c   lgspfl L    true if writing GSPRO and GSCNV output files
c
      logical*4 lxref
      logical*4 lsplit
      logical*4 lctlfl
      logical*4 loxrfl
      logical*4 lgspfl
c
      common /facdat/ lxref, lsplit, lctlfl, loxrfl, lgspfl
