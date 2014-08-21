#include "fintrf.h"
#include "okada_wrapper/DC3D.f"
C======================================================================
#if 0
C     
C     DC3Dwrapper.F
C     .F file needs to be preprocessed to generate .f equivalent
C     
#endif
C     
C     DC3Dwrapper.f
C
C     Wraps the DC3D fortran function. This function computes the half
C     space displacements and displacement gradients for a strike-slip,
C     dip-slip, tensile, or inflationary displacement on a rectangular
C     surface at depth.
C     
C     Seven arguments are required:
C     alpha = (lambda + mu) / (lambda + 2 * mu)
C     xo = 3-vector representing the observation point (x, y, z in the
C          original)
C     depth = the depth of the fault origin
C     dip = the dip-angle of the rectangular dislocation surface
C     strike_width = the along-strike range of the surface (al1,al2 in
C                    the original)
C     dip_width = the along-dip range of the surface (aw1, aw2 in the 
C                 original)
C     dislocation = 3-vector representing the direction of motion on the
C                   surface (DISL1, DISL2, DISL3)
C     
C     Three outputs are provided:
C     success - a return code from DC3D=0 if normal, 1 if singular, 2
C               if a positive z value for the observation point was given
C     u - 3-vector representing the displacement at the observation
C         point. for example, u(2) = u_y
C     grad_u = the 3x3 tensor representing the partial derivatives of
C              the displacement, for example, grad_u(1, 2) = d^2u/dxdy
C     
C     DC3D routine written by Y.OKADA ... SEP.1991
C     Mex wrapper written by Ben Thompson 2014.
C
C======================================================================
C     Gateway routine
      subroutine mexFunction(nlhs, plhs, nrhs, prhs)

C     Declarations
      implicit none

C     mexFunction arguments:
      mwPointer plhs(*), prhs(*)
      integer nlhs, nrhs

C     Function declarations:
      mwPointer mxGetPr
      mwPointer mxCreateDoubleMatrix
      integer mxIsNumeric
      integer mxGetM
      integer mxGetN

C     Argument pointers:
      mwPointer alpha_pointer
      mwPointer xo_pointer
      mwPointer depth_pointer
      mwPointer dip_pointer
      mwPointer strike_width_pointer
      mwPointer dip_width_pointer
      mwPointer dislocation_pointer

C     Output argument pointers
      mwPointer u_pointer
      mwPointer grad_u_pointer
      mwPointer retval_pointer

C     Intermediate values
      real*8 xo(3)
      real*8 strike_width(2)
      real*8 dip_width(2)
      real*8 dislocation(4)

C     Arguments for DC3D:
      real*8 alpha
      real*8 x, y, z
      real*8 depth
      real*8 dip
      real*8 al1
      real*8 al2
      real*8 aw1
      real*8 aw2
      real*8 disl1
      real*8 disl2
      real*8 disl3

C     Outputs from DC3D:
      real*4 ux
      real*4 uy
      real*4 uz
      real*4 uxx
      real*4 uyx
      real*4 uzx
      real*4 uxy
      real*4 uyy
      real*4 uzy
      real*4 uxz
      real*4 uyz
      real*4 uzz
      integer*4 iret

C     Intermediate outputs
      real*8 u(3)
      real*8 grad_u(3, 3)
      real*8 retval

C-----------------------------------------------------------------------
C     Check for proper number of arguments. 
      if(nrhs .ne. 7) then
         call mexErrMsgIdAndTxt ('MATLAB:DC3Dwrapper:nInput',
     +                           'Seven inputs required.')
      elseif(nlhs .gt. 3) then
         call mexErrMsgIdAndTxt ('MATLAB:DC3Dwrapper:nOutput',
     +                           'Three outputs required')
      endif

C     Grab the elastic moduli ratio (lambda + mu) / (lambda + 2*mu)
C     Check that the input is a scalar.
      if(mxIsNumeric(prhs(1)) .eq. 0 .or. mxGetM(prhs(1)) .ne. 1 .or. 
     +   mxGetN(prhs(1)) .ne. 1) then
         call mexErrMsgIdAndTxt ('MATLAB:DC3Dwrapper:alpha',
     +                           'Alpha must be a real scalar.')
      endif
      alpha_pointer = mxGetPr(prhs(1))
      call mxCopyPtrToReal8(alpha_pointer, alpha, 1)

C     Grab the observation point
      if(mxIsNumeric(prhs(2)) .eq. 0 .or. mxGetM(prhs(2)) * 
     +   mxGetN(prhs(2)) .ne. 3) then
         call mexErrMsgIdAndTxt ('MATLAB:DC3Dwrapper:xo',
     +                           'the observation point must be a real
     +3-vector.')
      endif
      xo_pointer = mxGetPr(prhs(2))
      call mxCopyPtrToReal8(xo_pointer, xo, 3)
      x = xo(1)
      y = xo(2)
      z = xo(3)

C     Grab the depth
      if(mxIsNumeric(prhs(3)) .eq. 0 .or. mxGetM(prhs(3)) .ne. 1 .or. 
     +   mxGetN(prhs(3)) .ne. 1) then
         call mexErrMsgIdAndTxt ('MATLAB:DC3Dwrapper:depth',
     +                           'Depth must be a real scalar.')
      endif
      depth_pointer = mxGetPr(prhs(3))
      call mxCopyPtrToReal8(depth_pointer, depth, 1)

C     Grab the dip
      if(mxIsNumeric(prhs(4)) .eq. 0 .or. mxGetM(prhs(4)) .ne. 1 .or. 
     +   mxGetN(prhs(4)) .ne. 1) then
         call mexErrMsgIdAndTxt ('MATLAB:DC3Dwrapper:dip',
     +                           'Dip must be a real scalar.')
      endif
      dip_pointer = mxGetPr(prhs(4))
      call mxCopyPtrToReal8(dip_pointer, dip, 1)

C     Grab the strike_width
      if(mxIsNumeric(prhs(5)) .eq. 0 .or. mxGetM(prhs(5)) * 
     +   mxGetN(prhs(5)) .ne. 2) then
         call mexErrMsgIdAndTxt ('MATLAB:DC3Dwrapper:strike_width',
     +                           'the strike_width must be a real 
     +2-vector.')
      endif
      strike_width_pointer = mxGetPr(prhs(5))
      call mxCopyPtrToReal8(strike_width_pointer, strike_width, 2)
      al1 = strike_width(1)
      al2 = strike_width(2)

C     Grab the dip_widths
      if(mxIsNumeric(prhs(6)) .eq. 0 .or. mxGetM(prhs(6)) * 
     +   mxGetN(prhs(6)) .ne. 2) then
         call mexErrMsgIdAndTxt ('MATLAB:DC3Dwrapper:dip_width',
     +                           'the dip_width must be a real 
     +2-vector.')
      endif
      dip_width_pointer = mxGetPr(prhs(6))
      call mxCopyPtrToReal8(dip_width_pointer, dip_width, 2)
      aw1 = dip_width(1)
      aw2 = dip_width(2)

C     Grab the potencies
      if(mxIsNumeric(prhs(7)) .eq. 0 .or. mxGetM(prhs(7)) * 
     +   mxGetN(prhs(7)) .ne. 3) then
         call mexErrMsgIdAndTxt ('MATLAB:DC3Dwrapper:dislocation',
     +                           'the potency must be a real 4-vector.')
      endif
      dislocation_pointer = mxGetPr(prhs(7))
      call mxCopyPtrToReal8(dislocation_pointer, dislocation, 3)
      disl1 = dislocation(1)
      disl2 = dislocation(2)
      disl3 = dislocation(3)

      call DC3D(sngl(alpha), 
     +           sngl(x), 
     +           sngl(y), 
     +           sngl(z), 
     +           sngl(depth), 
     +           sngl(dip), 
     +           sngl(al1), 
     +           sngl(al2), 
     +           sngl(aw1), 
     +           sngl(aw2), 
     +           sngl(disl1), 
     +           sngl(disl2), 
     +           sngl(disl3), 
     + ux, uy, uz, uxx, uyx, uzx, uxy, uyy, uzy, uxz, uyz, uzz, iret)

C     Tests to check that the input files are being properly handled
C     u(1) = dislocation(1)
C     u(2) = dislocation(2)
C     u(3) = dislocation(3)
C     grad_u(1, 1) = strike_width(1)
C     grad_u(2, 1) = strike_width(2)
C     grad_u(3, 1) = dip_width(1)
C     grad_u(1, 2) = dip_width(2)
C     grad_u(2, 2) = xo(1)
C     grad_u(3, 2) = xo(2)
C     grad_u(1, 3) = xo(3)
C     grad_u(2, 3) = depth
C     grad_u(3, 3) = dip
C       
      retval = dble(iret)
      u(1) = dble(ux)
      u(2) = dble(uy)
      u(3) = dble(uz)
      grad_u(1, 1) = dble(uxx)
      grad_u(2, 1) = dble(uyx)
      grad_u(3, 1) = dble(uzx)
      grad_u(1, 2) = dble(uxy)
      grad_u(2, 2) = dble(uyy)
      grad_u(3, 2) = dble(uzy)
      grad_u(1, 3) = dble(uxz)
      grad_u(2, 3) = dble(uyz)
      grad_u(3, 3) = dble(uzz)

      plhs(1) = mxCreateDoubleMatrix(1, 1, 0)
      plhs(2) = mxCreateDoubleMatrix(3, 1, 0)
      plhs(3) = mxCreateDoubleMatrix(3, 3, 0)

      retval_pointer = mxGetPr(plhs(1))
      u_pointer = mxGetPr(plhs(2))
      grad_u_pointer = mxGetPr(plhs(3))
      call mxCopyReal8ToPtr(retval, retval_pointer, 1)
      call mxCopyReal8ToPtr(u, u_pointer, 3)
      call mxCopyReal8ToPtr(grad_u, grad_u_pointer, 9)
      return
      end
