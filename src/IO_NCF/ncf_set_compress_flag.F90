subroutine ncf_set_compress_flag()
!
!     This routine just sets the compression flag for NetCDF based on the
!     the compiler directive. It needs to be a F90 routine so the preprocssor
!     directives can be used.
!
!     Modifications:
!        none
!
!     Input arguments:
!        none
!
!     Output arguments:
!        none
!
!     Routines called:
!        none
!
!
!     Called by:
!        READNML
!
use eps3ncf_incs
implicit none

ncf_compress = .FALSE.

#ifdef CHUNK
ncf_compress = .TRUE.
#endif

return
end

