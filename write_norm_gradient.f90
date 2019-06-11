program write_norm

implicit none

integer :: NITER, nline, i, j
double precision, allocatable, dimension(:) :: gradient_rho, gradient_vp, gradient_vs
double precision, allocatable, dimension(:) :: norm

character(len=150) :: dir, filename, arg
integer :: ios
double precision :: dummy1, dummy2, dummy3, dummy4, dummy5
character(len=150) :: dummy_character

! read input arguments
if( iargc() .ne. 2 ) stop 'USAGE: write_misfit NITER DIR'
j=1; call getarg(j, arg); read(arg,*,iostat=ios) NITER;
if (ios /= 0) stop 'gradient reading NITER'
j=2; call getarg(j, dir);
if (ios /= 0) stop 'gradient reading DIR'
dir = trim(dir)

! dynamically allocate arrays
filename = trim(dir) // '/output/model_true'
open(unit=3,file=filename,status='old',action='read')
nline = 0
do j=1,2147483647
   read(3,*,iostat=ios) dummy1, dummy2, dummy3, dummy4, dummy5
   if (ios /= 0) exit
   nline=nline+1
enddo
close(3)

allocate( gradient_rho(nline) )
allocate( gradient_vp(nline) )
allocate( gradient_vs(nline) )
allocate( norm(1:NITER) )

gradient_rho(:) = 0.0d0
gradient_vp(:) = 0.0d0
gradient_vs(:) = 0.0d0
norm(:) = 0.0d0


! read subsequent models
do i = 1, NITER

   write(dummy_character,'(i4.4)') i
   filename = trim(dir) // '/output/gradient_' // trim(ADJUSTL(dummy_character))

   open(unit=4,file=filename,status='old',action='read')
   do j = 1,nline
      read(4,*) dummy1, dummy2, gradient_rho(j), gradient_vp(j), gradient_vs(j)
   enddo
   close(4)
   norm(i) = (DOT_PRODUCT(gradient_vs(:),gradient_vs(:)))**0.5
   norm(i) = (DOT_PRODUCT(gradient_vs(:),gradient_vs(:))/nline)**0.5

   print *, i, &
      (DOT_PRODUCT(gradient_rho(:),gradient_rho(:))/nline)**0.5, &
      (DOT_PRODUCT(gradient_vp(:),gradient_vp(:))/nline)**0.5, &
      (DOT_PRODUCT(gradient_vs(:),gradient_vs(:))/nline)**0.5

enddo

end program

