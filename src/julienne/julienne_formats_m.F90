! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_formats_m
  !! Useful strings for formatting `print` and `write` statements
  implicit none

  character(len=*), parameter :: csv = "(*(G0,:,','))" !! comma-separated values
  character(len=*), parameter :: cscv = "(*('(',G0,',',G0,')',:,',')))" !! comma-separated complex values

#ifndef _CRAYFTN

  interface

    pure module function separated_values(separator, mold) result(format_string)
      character(len=*), intent(in) :: separator 
      class(*), intent(in) :: mold(..)
      character(len=:), allocatable :: format_string
    end function

  end interface

#else

  interface separated_values

    pure module function separated_values_1D(separator, mold) result(format_string)
      character(len=*), intent(in) :: separator 
      class(*), intent(in) :: mold(:)
      character(len=:), allocatable :: format_string
    end function

  end interface

#endif


contains

#ifndef _CRAYFTN

  module procedure separated_values
    character(len=*), parameter :: prefix = "(*(G0,:,'"
    character(len=*), parameter :: suffix =           "'))"
    character(len=*), parameter :: complex_prefix = "(*('(',G0,',',G0,')',:,'" 

    select rank(mold)
      rank(1)
        select type(mold)
          type is(complex)
            format_string = complex_prefix // separator // suffix
          type is(double precision)
            format_string = prefix // separator // suffix
          type is(real)
            format_string = prefix // separator // suffix
          type is(integer)
            format_string = prefix // separator // suffix
          type is(character(len=*))
            format_string = prefix // separator // suffix
          class default
             error stop "format_s separated_values: unsupported type"
        end select
      rank default
        error stop "formats_s separated_values: unsupported rank"
    end select
  end procedure

#else

  module procedure separated_values_1D
    character(len=*), parameter :: prefix = "(*(G0,:,'"
    character(len=*), parameter :: suffix =           "'))"
    character(len=*), parameter :: complex_prefix = "(*('(',G0,',',G0,')',:,'" 

    select type(mold)
      type is(complex)
        format_string = complex_prefix // separator // suffix
      type is(double precision)
        format_string = prefix // separator // suffix
      type is(real)
        format_string = prefix // separator // suffix
      type is(integer)
        format_string = prefix // separator // suffix
      type is(character(len=*))
        format_string = prefix // separator // suffix
      class default
         error stop "format_s separated_values_1D: unsupported type"
    end select
  end procedure

#endif

end module julienne_formats_m
