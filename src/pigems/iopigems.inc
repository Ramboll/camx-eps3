c*** IOPIGEMS.INC version 2.0 09/01/2007
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for PIGEMS program
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
c                PSTPNT program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 PIGEMS module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORINP   I   unit number for user input file
c   IORMSTR  I   unit number for the input master stack list
c   IORSTK   I   base unit number for input stack parameters files
c   IOREMS   I   base unit number for input point source emissions files
c   IORMSK   I   unit number for the threshold mask file
c   IORSTAT  I   unit number for the model-ready file of "static" sources
c   IOWSTK   I   unit number for output master stack file
c   IOWEMS   I   unit number for output emissions file
c   IOWMSG   I   unit number for message file
c
      integer*4 IORINP
      integer*4 IORMSTR
      integer*4 IORSTK
      integer*4 IOREMS
      integer*4 IORMSK
      integer*4 IORSTAT
      integer*4 IOWSTK      
      integer*4 IOWEMS
      integer*4 IOWMSG
c
      parameter ( IORINP   = 10 )
      parameter ( IORMSTR  = 11 )
      parameter ( IORSTK   = 12 )
      parameter ( IOREMS   = 13 )
      parameter ( IORMSK   = 14 )
      parameter ( IORSTAT  = 15 )
      parameter ( IOWMSG   = 20 )
      parameter ( IOWEMS   = 21 )
      parameter ( IOWSTK   = 22 )
c
c-----------------------------------------------------------------------
c    Parameters for number of inpuit files:
c-----------------------------------------------------------------------
c
c   MXSTKFL   I   maximum number of stack input files
c   MXEMSFL   I   maximum number of emissions input files
c
      integer MXSTKFL         
      integer MXEMSFL         
c
      parameter( MXSTKFL = 300 )
      parameter( MXEMSFL = 300 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   uinpfl     C   user input filename
c   masterfl   C   input master stack list file
c   istkfl     C   array of filenames for input stack files
c   iemsfl     C   array of filenames for input emissions files
c   maskfl     C   threshold mask file
c   staticfl   C   model-ready file containing the static sources
c   oemsfl     C   output binary point sources emissions filename
c   ncf_oemsfl C   output NetCDF point sources emissions filename
c   ostkfl     C   output master stacklist filename
c   msgfl      C   message filename
c
c   nstkfl        I   number of input stack files read
c   nemsfl        I   number of input emissions files read
c   lmaster       L   .TRUE. if master stack list file was supplied
c   lmaskfl       L   .TRUE. if threshold mask file was provided
c   lstaticfl     L   .TRUE. if model-ready static sources file is supplied
c   lnetcdf_out   L   flag for output NetCDF point source emissions file
c   ncf_iowemis   I   file handle for output NetCDF file
c   loemsfl       L   flag for binary point source emissions file
c
      character*(MXSTR) uinpfl
      character*(MXSTR) masterfl
      character*(MXSTR) istkfl(MXSTKFL)
      character*(MXSTR) iemsfl(MXEMSFL)
      character*(MXSTR) maskfl
      character*(MXSTR) staticfl
      character*(MXSTR) oemsfl
      character*(MXSTR) ncf_oemsfl
      character*(MXSTR) ostkfl
      character*(MXSTR) msgfl
c
      integer*4 nstkfl
      integer*4 nemsfl
      logical*4 lmaster
      logical*4 lmaskfl
      logical*4 lstaticfl
      logical*4 lnetcdf_out
      integer*4 ncf_iowemis
      logical*4 loemsfl
c
      common /iofil/ uinpfl, masterfl, istkfl, iemsfl, maskfl, staticfl,
     &               oemsfl, ncf_oemsfl, ostkfl, msgfl
c
      common /ioflg/ nstkfl, nemsfl, lmaster, lmaskfl, lstaticfl, 
     &               lnetcdf_out, ncf_iowemis, loemsfl
