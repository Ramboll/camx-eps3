c*** SPCEMS.INC
c
c-----------------------------------------------------------------------
c
c    Include file for SPCEMS program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c     Parameters:
c-----------------------------------------------------------------------
c
c     MXCMPT   I   maximum number of newly computed species
c                  specified in /SPCEMS/ packet
c
      integer*4 MXCMPT
c
      parameter (MXCMPT=5)
c
c-----------------------------------------------------------------------
c   user options:
c-----------------------------------------------------------------------
c
c   fromcrt  C   name of criteria pollutant to apply conversion factors
c                'converting FROM this'
c   tocrt    C   name of criteria pollutant to be generated
c                'converting TO this'
c   pmnew    C   list of output species names computed from criterias
c   idxfrm   I   index of fromcrt to criteria pollutant list (embr header)
c   idxmin   I   list of index of minuend to criteria pollutant list (embr header)
c   idxsub   I   list of index of subtrahend to criteria pollutant list (embr header)
c   idxnew   I   list of index of new calculated to output species list /SPECIES LIST/
c   lprtpf   L   flag to determine if tables by profile should be printed
c   lreqpf   L   flag to determine if all profiles must match an entry
c                in the GSPRO file or if a default profile '0000' can be used
c   lconv    L   flag to determine if applying conversion factors file
c   lpart    L   flag to determine if calculating pollutant from two existing
c                criteria pollutants
c   lpmM     L   flag to determine if criteria pollutant is used in new = A-B
c   npmcs    I   number of new calculated species
c
      character*10 fromcrt
      character*10 tocrt
      character*10 pmnew(MXCMPT)
      integer*4    idxfrm
      integer*4    idxmin(MXCMPT)
      integer*4    idxsub(MXCMPT)
      integer*4    idxnew(MXCMPT)
      integer*4    npmcs
      logical*4    lprtpf
      logical*4    lreqpf
      logical*4    lconv
      logical*4    lpart
      logical*4    lpmM(MXCRIT)
c
      common /chrspc/ fromcrt, tocrt, pmnew
      common /datspc/ lprtpf, lreqpf, lconv, lpart,
     &                idxfrm, lpmM, 
     &                npmcs, idxmin, idxsub, idxnew
