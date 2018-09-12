! Copyright 2004, Magnus Hagdorn
! 
! This file is part of proj4.
! 
! proj4 is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2 of the License, or
! (at your option) any later version.
! 
! proj4 is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with proj4; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

program testproj
  use proj4
  implicit none

!  character(len=256) :: str_arg
!  integer(c_int), target :: int_arg
!  type(c_ptr)        :: int_ptr
!  !integer(c_int)        :: int_ptr
!  integer(kind=4)    :: stat
!
!  int_arg = 256
!  stat = test_out_f(str_arg, int_arg, int_ptr)
!  print*, int_arg, trim(str_arg) 
!  print*, "aqui..."
!  !int_arg = 256
!  !int_ptr = c_loc(int_arg)
!  !str_arg = "hello!"//C_NULL_CHAR
!  stat = test_in_f(str_arg, int_ptr, int_arg)
!  print*, int_arg, trim(str_arg), stat 

!  integer :: status
!  type(prj90_projection) :: proj
!  character(len=256) :: params
!  real(kind=kind(1.0d0)) :: lam0,phi0,x0,y0
  real(kind=kind(1.0d0)) :: lam1(2),phi1(2),x1(2),y1(2)
  real(kind=kind(1.0d0)), target :: xx(2),yy(2),zz(2)

  type(c_ptr)     :: x_ptr, y_ptr, z_ptr
  integer(c_long) :: point_count
  integer(c_int)  :: point_offset
  !type(projPJ_f)  :: srcdefn, dstdefn
  integer(c_int)  :: stat
  type(c_ptr) :: srcdefn, dstdefn

!  params = '+proj=aea '//&
!           '+ellps=WGS84 '//&
!           '+lat_1=52.8333320617676 '//&
!           '+lat_2=68.1666641235352 '//&
!           '+lon_0=33.5'//&
!           '+lat_0=60.5 '//&
!           '+x_0=1903970.98145531 '//&
!           '+y_0=898179.31322811 '
!
!  status=prj90_init(proj,params)
!  if (status.ne.PRJ90_NOERR) then
!     write(*,*) prj90_strerrno(status)
!     stop
!  end if
!
!!  lam0 = 7.0
!!  phi0 = 49.0
!!  status = prj90_fwd(proj,lam0,phi0,x0,y0)
!!  if (status.ne.PRJ90_NOERR) then
!!     write(*,*) prj90_strerrno(status)
!!     stop
!!  end if  
!!  write(*,*) lam0,phi0,x0,y0
!!  status = prj90_inv(proj,x0,y0,lam0,phi0)
!!  if (status.ne.PRJ90_NOERR) then
!!     write(*,*) prj90_strerrno(status)
!!     stop
!!  end if    
!!  write(*,*) x0,y0,lam0,phi0
!
!  lam1 = [59.92093, 60.92093]
!  phi1 = [71.9509, 72.9509]
!  status = prj90_fwd(proj,lam1,phi1,x1,y1)
!  if (status.ne.PRJ90_NOERR) then
!     write(*,*) prj90_strerrno(status)
!     stop
!  end if  
!  write(*,*) lam1,phi1,x1,y1
!  status = prj90_inv(proj,x1,y1,lam1,phi1)
!  if (status.ne.PRJ90_NOERR) then
!     write(*,*) prj90_strerrno(status)
!     stop
!  end if    
!
!  status = prj90_free(proj)

  stat = pj_init_plus_f('+proj=aea '//&
                        '+ellps=WGS84 '//&
                        '+lat_1=52.8333320617676 '//&
                        '+lat_2=68.1666641235352 '//&
                        '+lon_0=33.5'//&
                        '+lat_0=60.5 '//&
                        '+x_0=1903970.98145531 '//&
                        '+y_0=898179.31322811'//&
                        C_NULL_CHAR, srcdefn)

  stat = pj_init_plus_f('+proj=latlong +ellps=WGS84 '//&
                        '+datum=WGS84'//C_NULL_CHAR, dstdefn)

  x1 = [2810000.3358428376, 2798531.3743090290]
  y1 = [8484052.6373285130, 8598003.0927982368]

  xx = x1
  yy = y1
  zz = 0
  point_count = size(xx)
  point_offset = 1

  !print*, xx, yy, zz, point_count, point_offset
  
  !status = pj_transform_f(srcdefn%proj, dstdefn%proj, &
  !                        int(point_count, C_LONG), point_offset,&
  !                        xx, yy, zz)

  x_ptr = c_loc(xx(1))
  y_ptr = c_loc(yy(1))
  z_ptr = c_loc(zz(1))

  stat = pj_transform_f(srcdefn, dstdefn, &
                        point_count, point_offset,&
                        x_ptr, y_ptr, z_ptr)

  lam1 = xx
  phi1 = yy
  !write(*,*) x1,y1,lam1,phi1

end program testproj
