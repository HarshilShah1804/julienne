! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
#include "assert_macros.h"

module julienne_file_m
  !! A representation of a file as an object
  use julienne_string_m, only : string_t
  use iso_fortran_env, only : iostat_end, iostat_eor, output_unit
  use assert_m

  private
  public :: file_t

  type file_t
    private
    type(string_t), allocatable :: lines_(:)
  contains
    procedure :: lines
    generic :: write_lines => write_to_output_unit, write_to_character_file_name, write_to_string_file_name
    procedure, private ::     write_to_output_unit, write_to_character_file_name, write_to_string_file_name
  end type

  interface file_t

    module function from_file_with_string_name(file_name) result(file_object)
      implicit none
      type(string_t), intent(in) :: file_name
      type(file_t) file_object
    end function

    module function from_file_with_character_name(file_name) result(file_object)
      implicit none
      character(len=*), intent(in) :: file_name
      type(file_t) file_object
    end function

    pure module function from_lines(lines) result(file_object)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(file_t) file_object
    end function

  end interface

  interface

    pure module function lines(self)  result(my_lines)
      implicit none
      class(file_t), intent(in) :: self
      type(string_t), allocatable :: my_lines(:)
    end function

    module subroutine write_to_output_unit(self)
      implicit none
      class(file_t), intent(in) :: self
    end subroutine

    impure elemental module subroutine write_to_string_file_name(self, file_name)
      implicit none
      class(file_t), intent(in) :: self
      type(string_t), intent(in) :: file_name
    end subroutine

    impure elemental module subroutine write_to_character_file_name(self, file_name)
      implicit none
      class(file_t), intent(in) :: self
      character(len=*), intent(in) :: file_name
    end subroutine


  end interface

contains

  module procedure lines
    my_lines = self%lines_
  end procedure

  module procedure write_to_output_unit
    integer l

    call_assert(allocated(self%lines_))

    do l = 1, size(self%lines_)
      write(output_unit, '(a)') self%lines_(l)%string()
    end do
  end procedure

  module procedure write_to_character_file_name
    integer file_unit, l

    call_assert(allocated(self%lines_))

    open(newunit=file_unit, file=file_name, form='formatted', status='unknown', action='write')

    do l = 1, size(self%lines_)
      write(file_unit, '(a)') self%lines_(l)%string()
    end do
  end procedure
  
  module procedure write_to_string_file_name
    call self%write_to_character_file_name(file_name%string())
  end procedure

  module procedure from_lines
    file_object%lines_ = lines
  end procedure

  module procedure from_file_with_character_name
    file_object = from_file_with_string_name(string_t(file_name))
  end procedure

  module procedure from_file_with_string_name

    integer io_status, file_unit, line_num
    character(len=:), allocatable :: line
    integer, parameter :: max_message_length=128
    character(len=max_message_length) error_message
    integer, allocatable :: lengths(:)

    open(newunit=file_unit, file=file_name%string(), form='formatted', status='old')

    lengths = line_lengths(file_unit)

    associate(num_lines => size(lengths))

      allocate(file_object%lines_(num_lines))
  
      do line_num = 1, num_lines
        allocate(character(len=lengths(line_num)) :: line)
        read(file_unit, '(a)') line
        file_object%lines_(line_num) = string_t(line)
        deallocate(line)
      end do

    end associate

    close(file_unit)

  contains
   
    function line_count(file_unit) result(num_lines)
      integer, intent(in) :: file_unit
      integer num_lines
    
      rewind(file_unit)
      num_lines = 0 
      do  
        read(file_unit, *, iostat=io_status)
        if (io_status==iostat_end) exit
        num_lines = num_lines + 1 
      end do
      rewind(file_unit)
    end function

    function line_lengths(file_unit) result(lengths)
      integer, intent(in) :: file_unit
      integer, allocatable ::  lengths(:)
      integer io_status, l
      character(len=1) c

      associate(num_lines => line_count(file_unit))

        allocate(lengths(num_lines), source = 0)
        rewind(file_unit)

        do l = 1, num_lines
          do
            read(file_unit, '(a)', advance='no', iostat=io_status, iomsg=error_message) c
            associate(eliminate_unused_variable_warning => c) ! eliminate NAG compiler "variable c set but never referenced" warning
            end associate
            if (io_status==iostat_eor .or. io_status==iostat_end) exit
            lengths(l) = lengths(l) + 1
          end do
        end do

        rewind(file_unit)
  
      end associate
    end function

  end procedure

end module julienne_file_m
