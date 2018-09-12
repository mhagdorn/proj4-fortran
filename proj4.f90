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

module proj4

  use iso_c_binding

  real(kind=8), private, parameter :: PI = 4._8 * datan(1._8) 
  real(kind=8), parameter :: RAD2DEG = 180._8 / PI
  real(kind=8), parameter :: DEG2RAD = PI / 180._8

  type projPJ_f
     private
     type(c_ptr) :: proj
  end type projPJ_f

  interface
      function pj_init_plus_f(params, prjdefn)&
                              bind(c, name='cfort_pj_init_plus')&
                              result(stat)
          use iso_c_binding
          character(C_CHAR)          :: params(*)
          type(c_ptr), intent(inout) :: prjdefn
          integer(C_INT)             :: stat
      end function pj_init_plus_f
  end interface

  interface
      function pj_transform_f(srcdefn, dstdefn, point_count, point_offset,&
                              x, y, z) bind(c, name='cfort_pj_transform_2')&
                              result(stat)
          use iso_c_binding
          type(c_ptr), intent(in)            :: srcdefn, dstdefn
          integer(C_LONG), value, intent(in) :: point_count
          integer(C_INT), value, intent(in)  :: point_offset
          real(C_DOUBLE), intent(inout)      :: x, y, z
          integer(C_INT)                     :: stat
      end function pj_transform_f
  end interface

!  include 'proj4.inc'
!
!  integer, parameter :: PRJ90_NOERR = PRJF_NOERR
!
!  type prj90_projection
!     private
!     integer(kind=8) :: prj
!  end type prj90_projection
!
!
!  interface prj90_fwd
!     module procedure prj90_fwd_pt, prj90_fwd_array
!  end interface
!
!  interface prj90_inv
!     module procedure prj90_inv_pt, prj90_inv_array
!  end interface
!
!contains
!  function prj90_strerrno(prj_errno)
!    implicit none
!    character(len=80) :: prj90_strerrno
!    integer, intent(in) :: prj_errno
!
!    prj90_strerrno = prjf_strerrno(prj_errno)
!  end function prj90_strerrno
!
!  function prj90_init(prj,args)
!    implicit none
!    integer :: prj90_init
!    type(prj90_projection), intent(inout) :: prj
!    character(len=*) :: args
!
!    prj90_init = prjf_init(prj%prj,args)
!  end function prj90_init
!
!  function prj90_free(prj)
!    implicit none
!    integer :: prj90_free
!    type(prj90_projection), intent(inout) :: prj
!
!    prj90_free =  prjf_free(prj%prj)
!  end function prj90_free
!
!  function prj90_fwd_pt(prj_out,lam,phi,x,y)
!    implicit none
!    integer :: prj90_fwd_pt
!    real(kind=8), intent(in) :: lam, phi
!    real(kind=8), intent(out) :: x,y
!    type(prj90_projection), intent(inout) :: prj_out
!    type(prj90_projection) :: prj_in
!    integer stat
!
!    stat=prj90_init(prj_in, "+proj=latlong +ellps=WGS84 +datum=WGS84")
!    if (stat.ne.PRJ90_NOERR) then
!       write(*,*) prj90_strerrno(stat)
!       stop
!    end if
!    
!    x = lam*DEG2RAD
!    y = phi*DEG2RAD
!    prj90_fwd_pt = prjf_transform(prj_in%prj,prj_out%prj,&
!                                  x,y,1)
!    
!    stat = prj90_free(prj_in)
!    if (stat.ne.PRJ90_NOERR) then
!       write(*,*) prj90_strerrno(stat)
!       stop
!    end if
!  end function prj90_fwd_pt
!    
!  function prj90_inv_pt(prj_in,x,y,lam,phi)
!    implicit none
!    integer :: prj90_inv_pt
!    real(kind=8), intent(in) :: x,y
!    real(kind=8), intent(out) :: lam, phi
!    type(prj90_projection), intent(inout) :: prj_in
!    type(prj90_projection) :: prj_out
!    integer stat
!
!    stat=prj90_init(prj_out, "+proj=latlong +ellps=WGS84 +datum=WGS84")
!    if (stat.ne.PRJ90_NOERR) then
!       write(*,*) prj90_strerrno(stat)
!       stop
!    end if
!    
!    lam = x
!    phi = y
!    prj90_inv_pt = prjf_transform(prj_in%prj,prj_out%prj,&
!                                  lam,phi,1)
!    lam = lam * RAD2DEG
!    phi = phi * RAD2DEG
!    
!    stat = prj90_free(prj_out)
!    if (stat.ne.PRJ90_NOERR) then
!       write(*,*) prj90_strerrno(stat)
!       stop
!    end if
!  end function prj90_inv_pt  
!
!  function prj90_fwd_array(prj_out,lam,phi,x,y)
!    implicit none
!    integer :: prj90_fwd_array
!    real(kind=8), dimension(:), intent(in) :: lam, phi
!    real(kind=8), dimension(:), intent(out) :: x,y
!    type(prj90_projection), intent(inout) :: prj_out
!    type(prj90_projection) :: prj_in
!    integer stat
!
!    stat=prj90_init(prj_in, "+proj=latlong +ellps=WGS84 +datum=WGS84")
!    if (stat.ne.PRJ90_NOERR) then
!       write(*,*) prj90_strerrno(stat)
!       stop
!    end if
!    
!    x = lam*DEG2RAD
!    y = phi*DEG2RAD
!    prj90_fwd_array = prjf_transform(prj_in%prj,prj_out%prj,&
!                                     x,y,size(x))
!    
!    stat = prj90_free(prj_in)
!    if (stat.ne.PRJ90_NOERR) then
!       write(*,*) prj90_strerrno(stat)
!       stop
!    end if
!  end function prj90_fwd_array
! 
!  function prj90_inv_array(prj_in,x,y,lam,phi)
!    implicit none
!    integer :: prj90_inv_array
!    real(kind=8), dimension(:), intent(in) :: x,y
!    real(kind=8), dimension(:), intent(out) :: lam, phi
!    type(prj90_projection), intent(inout) :: prj_in
!    type(prj90_projection) :: prj_out
!    integer stat
!
!    stat=prj90_init(prj_out, "+proj=latlong +ellps=WGS84 +datum=WGS84")
!    if (stat.ne.PRJ90_NOERR) then
!       write(*,*) prj90_strerrno(stat)
!       stop
!    end if
!    
!    lam = x
!    phi = y
!    prj90_inv_array = prjf_transform(prj_in%prj,prj_out%prj,&
!                                     lam,phi,size(lam))
!    lam = lam * RAD2DEG
!    phi = phi * RAD2DEG
!    
!    stat = prj90_free(prj_out)
!    if (stat.ne.PRJ90_NOERR) then
!       write(*,*) prj90_strerrno(stat)
!       stop
!    end if
!  end function prj90_inv_array

end module proj4
