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

    integer, parameter :: PRJ90_NOERR = 0

    type prj90_projection
        private
        type(c_ptr) :: prj
    end type prj90_projection

    real(kind=8), private, parameter :: PI = 4._8 * datan(1._8) 
    real(kind=8), parameter :: RAD2DEG = 180._8 / PI
    real(kind=8), parameter :: DEG2RAD = PI / 180._8

    interface prj90_transform
        module procedure prj90_transform_pt,&
                         prj90_transform_array,&
                         prj90_transform_array_2d
    end interface

    interface prj90_fwd
        module procedure prj90_fwd_pt,&
                         prj90_fwd_array,&
                         prj90_fwd_array_2d
    end interface

    interface prj90_inv
        module procedure prj90_inv_pt,&
                         prj90_inv_array,&
                         prj90_inv_array_2d
    end interface

    interface
        function pj_init_plus_f(prjdefn, params)&
                                bind(c, name='cfort_pj_init_plus')&
                                result(stat)
            use iso_c_binding
            type(c_ptr), intent(out) :: prjdefn
            character(C_CHAR)          :: params(*)
            integer(C_INT)             :: stat
        end function pj_init_plus_f
    end interface

    interface
        function pj_free_f(prjdefn)&
                                bind(c, name='cfort_pj_free')&
                                result(stat)
            use iso_c_binding
            type(c_ptr), intent(in) :: prjdefn
            integer(C_INT)             :: stat
        end function pj_free_f
    end interface

    interface
        function pj_strerrno_f(err, err_msg)&
                                bind(c, name='cfort_pj_strerrno')&
                                result(stat)
            use iso_c_binding
            integer(C_INT), value, intent(in) :: err
            character(C_CHAR), intent(inout)  :: err_msg(*)
            integer(C_INT)                    :: stat
        end function pj_strerrno_f
    end interface

    interface
        function pj_transform_f(srcdefn, dstdefn, point_count, point_offset,&
                                x, y, z) bind(c, name='cfort_pj_transform')&
                                result(stat)
            use iso_c_binding
            type(c_ptr), intent(in)            :: srcdefn, dstdefn
            integer(C_LONG), value, intent(in) :: point_count
            integer(C_INT), value, intent(in)  :: point_offset
            real(C_DOUBLE), intent(inout)      :: x, y, z
            integer(C_INT)                     :: stat
        end function pj_transform_f
    end interface


contains

    function prj90_strerrno(prj_errno)
        implicit none
        character(len=80)   :: prj90_strerrno
        integer, intent(in) :: prj_errno
        integer             :: stat

        prj90_strerrno = ""
        stat = pj_strerrno_f(prj_errno, prj90_strerrno)
    end function prj90_strerrno

    function prj90_init(prj,args)
        implicit none
        integer                               :: prj90_init
        type(prj90_projection), intent(out) :: prj
        character(len=*)                      :: args

        prj90_init = pj_init_plus_f(prj%prj, trim(args)//C_NULL_CHAR)
    end function prj90_init

    function prj90_free(prj)
        implicit none
        integer                               :: prj90_free
        type(prj90_projection), intent(in) :: prj

        prj90_free =  pj_free_f(prj%prj)
    end function prj90_free

    function prj90_fwd_pt(dstdefn, lam, phi, x, y) result(stat)
        implicit none
        integer                               :: stat
        real(C_DOUBLE), intent(in)            :: lam, phi
        real(C_DOUBLE), target, intent(out)   :: x, y
        type(prj90_projection), intent(in) :: dstdefn

        type(prj90_projection)          :: srcdefn

        stat = prj90_init(srcdefn, "+proj=latlong "//&
                                   "+ellps=WGS84 "//&
                                   "+datum=WGS84")
        if (stat .ne. PRJ90_NOERR) return
        stat = prj90_transform(srcdefn, dstdefn,&
                               lam * DEG2RAD, &
                               phi * DEG2RAD,&
                               x, y)
        if (stat .ne. PRJ90_NOERR) return
        stat = prj90_free(srcdefn)
    end function prj90_fwd_pt

    function prj90_fwd_array(dstdefn, lam, phi, x, y) result(stat)
        implicit none
        integer                               :: stat
        real(C_DOUBLE), intent(in)            :: lam(:), phi(:)
        real(C_DOUBLE), target, intent(out)   :: x(:), y(:)
        type(prj90_projection), intent(in) :: dstdefn

        type(prj90_projection)  :: srcdefn

        stat = prj90_init(srcdefn, "+proj=latlong "//&
                                   "+ellps=WGS84 "//&
                                   "+datum=WGS84")
        if (stat .ne. PRJ90_NOERR) return
        stat = prj90_transform(srcdefn, dstdefn,&
                               lam * DEG2RAD,&
                               phi * DEG2RAD,&
                               x, y)
        if (stat .ne. PRJ90_NOERR) return
        stat = prj90_free(srcdefn)
    end function prj90_fwd_array

    function prj90_fwd_array_2d(dstdefn, lam, phi, x, y) result(stat)
        implicit none
        integer                               :: stat
        real(C_DOUBLE), intent(in)            :: lam(:,:), phi(:,:)
        real(C_DOUBLE), target, intent(out)   :: x(:,:), y(:,:)
        type(prj90_projection), intent(in) :: dstdefn

        type(prj90_projection)  :: srcdefn

        stat = prj90_init(srcdefn, "+proj=latlong "//&
                                   "+ellps=WGS84 "//&
                                   "+datum=WGS84")
        if (stat .ne. PRJ90_NOERR) return
        stat = prj90_transform(srcdefn, dstdefn,&
                               lam * DEG2RAD,&
                               phi * DEG2RAD,&
                               x, y)
        if (stat .ne. PRJ90_NOERR) return
        stat = prj90_free(srcdefn)
    end function prj90_fwd_array_2d
 
    function prj90_inv_pt(srcdefn, x, y, lam, phi) result(stat)
        implicit none
        integer                               :: stat
        real(C_DOUBLE), intent(in)            :: x, y
        real(C_DOUBLE), target, intent(out)   :: lam, phi
        type(prj90_projection), intent(in) :: srcdefn

        type(prj90_projection)  :: dstdefn

        stat = prj90_init(dstdefn, "+proj=latlong "//&
                                   "+ellps=WGS84 "//&
                                   "+datum=WGS84")
        if (stat .ne. PRJ90_NOERR) return
        stat = prj90_transform(srcdefn,dstdefn,&
                               x, y, lam, phi)
        if (stat .ne. PRJ90_NOERR) return
        lam = lam * RAD2DEG
        phi = phi * RAD2DEG
        stat = prj90_free(dstdefn)
    end function prj90_inv_pt   

    function prj90_inv_array(srcdefn, x, y, lam, phi) result(stat)
        implicit none
        integer                               :: stat
        real(C_DOUBLE), intent(in)            :: x(:), y(:)
        real(C_DOUBLE), target, intent(out)   :: lam(:), phi(:)
        type(prj90_projection), intent(in) :: srcdefn

        type(prj90_projection)  :: dstdefn

        stat = prj90_init(dstdefn, "+proj=latlong "//&
                                   "+ellps=WGS84 "//&
                                   "+datum=WGS84")
        if (stat .ne. PRJ90_NOERR) return
        stat = prj90_transform(srcdefn,dstdefn,&
                               x, y, lam, phi)
        if (stat .ne. PRJ90_NOERR) return
        lam = lam * RAD2DEG
        phi = phi * RAD2DEG
        stat = prj90_free(dstdefn)
    end function prj90_inv_array   

    function prj90_inv_array_2d(srcdefn, x, y, lam, phi) result(stat)
        implicit none
        integer                               :: stat
        real(C_DOUBLE), intent(in)            :: x(:,:), y(:,:)
        real(C_DOUBLE), target, intent(out)   :: lam(:,:), phi(:,:)
        type(prj90_projection), intent(in)    :: srcdefn

        type(prj90_projection)                :: dstdefn

        stat = prj90_init(dstdefn, "+proj=latlong "//&
                                   "+ellps=WGS84 "//&
                                   "+datum=WGS84")
        if (stat .ne. PRJ90_NOERR) return
        stat = prj90_transform(srcdefn,dstdefn,&
                               x, y, lam, phi)
        if (stat .ne. PRJ90_NOERR) return
        lam = lam * RAD2DEG
        phi = phi * RAD2DEG
        stat = prj90_free(dstdefn)
    end function prj90_inv_array_2d

    function prj90_transform_pt(srcdefn, dstdefn, x, y, lam, phi) result(stat)
        implicit none
        integer                             :: stat
        real(C_DOUBLE), intent(in)          :: x, y
        real(C_DOUBLE), target, intent(out) :: lam, phi
        type(prj90_projection), intent(in)  :: srcdefn
        type(prj90_projection), intent(in)  :: dstdefn

        real(C_DOUBLE), target  :: z
        real(C_DOUBLE), pointer :: x_ptr, y_ptr, z_ptr
        integer(C_LONG)         :: point_count
        integer(C_INT)          :: point_offset
 
        lam = x 
        phi = y 
        z = 0
        x_ptr => lam
        y_ptr => phi
        z_ptr => z
        point_count = 1
        point_offset = 1
        stat = pj_transform_f(srcdefn%prj,dstdefn%prj,&
                              point_count, point_offset,&
                              x_ptr, y_ptr, z_ptr)
    end function prj90_transform_pt

    function prj90_transform_array(srcdefn, dstdefn, x, y, lam, phi) result(stat)
        implicit none
        integer                               :: stat
        real(C_DOUBLE), intent(in)            :: x(:), y(:)
        real(C_DOUBLE), target, intent(out)   :: lam(:), phi(:)
        type(prj90_projection), intent(in) :: srcdefn
        type(prj90_projection), intent(in)  :: dstdefn

        real(C_DOUBLE), target  :: z(size(x))
        real(C_DOUBLE), pointer :: x_ptr, y_ptr, z_ptr
        integer(C_LONG)         :: point_count
        integer(C_INT)          :: point_offset
 
        lam = x 
        phi = y 
        z = 0
        x_ptr => lam(1)
        y_ptr => phi(1)
        z_ptr => z(1)
        point_count = size(x)
        point_offset = 1
        stat = pj_transform_f(srcdefn%prj,dstdefn%prj,&
                              point_count, point_offset,&
                              x_ptr, y_ptr, z_ptr)
    end function prj90_transform_array

    function prj90_transform_array_2d(srcdefn, dstdefn, x, y, lam, phi) result(stat)
        implicit none
        integer                               :: stat
        real(C_DOUBLE), intent(in)            :: x(:,:), y(:,:)
        real(C_DOUBLE), target, intent(out)   :: lam(:,:), phi(:,:)
        type(prj90_projection), intent(in) :: srcdefn
        type(prj90_projection), intent(in)  :: dstdefn

        real(C_DOUBLE), target  :: z(size(x,1), size(x,2))
        real(C_DOUBLE), pointer :: x_ptr, y_ptr, z_ptr
        integer(C_LONG)         :: point_count
        integer(C_INT)          :: point_offset
 
        lam = x 
        phi = y 
        z = 0
        x_ptr => lam(1,1)
        y_ptr => phi(1,1)
        z_ptr => z(1,1)
        point_count = size(x)
        point_offset = 1
        stat = pj_transform_f(srcdefn%prj,dstdefn%prj,&
                              point_count, point_offset,&
                              x_ptr, y_ptr, z_ptr)
    end function prj90_transform_array_2d
 
end module proj4
