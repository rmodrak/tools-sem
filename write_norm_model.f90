program write_norm

implicit none

integer, parameter :: CUSTOM_REAL = 4
integer, parameter :: IIN = 4

integer :: NITER, nline, i, j
real(kind=CUSTOM_REAL), allocatable, dimension(:) :: target_rho, target_vp, target_vs, &
 model_rho, model_vp, model_vs, error_rho, error_vp, error_vs
double precision, allocatable, dimension(:) :: error_rho_L1, error_vp_L1, &
  error_vs_L1, error_rho_L2, error_vp_L2, error_vs_L2

character(len=150) :: dir, filename, arg
integer :: ios, filesize
character(len=150) :: suffix

! read input arguments
if( iargc() .ne. 2 ) stop 'USAGE: write_misfit NITER DIR'
j=1; call getarg(j, arg); read(arg,*,iostat=ios) NITER;
if (ios /= 0) stop 'Error reading NITER'
j=2; call getarg(j, dir);
if (ios /= 0) stop 'Error reading DIR'
dir = trim(dir)


! dynamically allocate arrays
filename = trim(dir) // '/output/model_true/proc000000_rho.bin'
open(unit=3,file=filename,status='old',action='read')
inquire(3, size=filesize)
close(3)
nline=(filesize-8)/4

allocate( target_rho(nline) )
allocate( target_vp(nline) )
allocate( target_vs(nline) )
allocate( model_rho(nline) )
allocate( model_vp(nline) )
allocate( model_vs(nline) )
allocate( error_rho(nline) )
allocate( error_vp(nline) )
allocate( error_vs(nline) )

allocate( error_rho_L1(0:NITER) )
allocate( error_vp_L1(0:NITER) )
allocate( error_vs_L1(0:NITER) )
allocate( error_rho_L2(0:NITER) )
allocate( error_vp_L2(0:NITER) )
allocate( error_vs_L2(0:NITER) )

target_rho(:) = 0.0d0
target_vp(:) = 0.0d0
target_vs(:) = 0.0d0
model_rho(:) = 0.0d0
model_vp(:) = 0.0d0
model_vs(:) = 0.0d0
error_rho(:) = 0.0d0
error_vp(:) = 0.0d0
error_vs(:) = 0.0d0

error_rho_L1(:) = 0.0d0
error_vp_L1(:) = 0.0d0
error_vs_L1(:) = 0.0d0
error_rho_L2(:) = 0.0d0
error_vp_L2(:) = 0.0d0
error_vs_L2(:) = 0.0d0


! read target model
filename = trim(dir) // '/output/model_true/proc000000_rho.bin'
open(unit=IIN,file=filename,status='old',action='read',form='unformatted')
read(IIN) target_rho
close(IIN)

filename = trim(dir) // '/output/model_true/proc000000_vp.bin'
open(unit=IIN,file=filename,status='old',action='read',form='unformatted')
read(IIN) target_vp
close(IIN)

filename = trim(dir) // '/output/model_true/proc000000_vs.bin'
open(unit=IIN,file=filename,status='old',action='read',form='unformatted')
read(IIN) target_vs
close(IIN)



! read subsequent models
do i = 0, NITER

   if (i==0) then
       suffix = 'init'
   else
       write(suffix,'(i4.4)') i
   endif

   !filename =trim(dir) // '/output/model_'//trim(suffix) // '/proc000000_rho.bin'
   !open(unit=IIN,file=filename,status='old',action='read',form='unformatted')
   !read(IIN) model_rho
   !close(IIN)

   filename =trim(dir) // '/output/model_'//trim(suffix) //'/proc000000_vp.bin'
   open(unit=IIN,file=filename,status='old',action='read',form='unformatted')
   read(IIN) model_vp
   close(IIN)

   filename =trim(dir) // '/output/model_'//trim(suffix) //'/proc000000_vs.bin'
   open(unit=IIN,file=filename,status='old',action='read',form='unformatted')
   read(IIN) model_vs
   close(IIN)

   !error_rho(:)=target_rho(:)-model_rho(:)
   error_vp(:)=target_vp(:)-model_vp(:)
   error_vs(:)=target_vs(:)-model_vs(:)

   !error_rho_L1(i) = SUM(abs(error_rho(:)))/nline
   error_vp_L1(i) = SUM(abs(error_vp(:)))/nline
   error_vs_L1(i) = SUM(abs(error_vs(:)))/nline

   !error_rho_L2(i) = (DOT_PRODUCT(error_rho(:),error_rho(:))/nline)**0.5
   error_vp_L2(i) = (DOT_PRODUCT(error_vp(:),error_vp(:))/nline)**0.5
   error_vs_L2(i) = (DOT_PRODUCT(error_vs(:),error_vs(:))/nline)**0.5

   print *, i, &
       !error_rho_L2(i),&
       error_vp_L2(i),&
       error_vs_L2(i)

enddo

!filename = trim(dir) // '/output.stats/error_rho_L1'
!open(unit=6,file=filename,status='unknown',action='write')
!do i = 0, NITER
!    write(6,*) error_rho_L1(i)
!enddo
!close(6)

!filename = trim(dir) // '/output.stats/error_vp_L1'
!open(unit=6,file=filename,status='unknown',action='write')
!do i = 0, NITER
    write(6,*) error_vp_L1(i)
!enddo
!close(6)

!filename = trim(dir) // '/output.stats/error_vs_L1'
!open(unit=6,file=filename,status='unknown',action='write')
!do i = 0, NITER
!    write(6,*) error_vp_L1(i)
!enddo
!close(6)


filename = trim(dir) // '/output.stats/error_rho_L2'
open(unit=6,file=filename,status='unknown',action='write')
do i = 0, NITER
    write(6,*) error_rho_L2(i)
enddo
close(6)

filename = trim(dir) // '/output.stats/error_vp_L2'
open(unit=6,file=filename,status='unknown',action='write')
do i = 0, NITER
    write(6,*) error_vp_L2(i)
enddo
close(6)

filename = trim(dir) // '/output.stats/error_vs_L2'
open(unit=6,file=filename,status='unknown',action='write')
do i = 0, NITER
    write(6,*) error_vs_L2(i)
enddo
close(6)



end program

