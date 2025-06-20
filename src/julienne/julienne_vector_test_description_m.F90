! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
#include "assert_macros.h"
module julienne_vector_test_description_m
  use assert_m
  !! Define an abstraction for describing test intentions and array-valued test functions
  use julienne_string_m, only : string_t
  use julienne_test_result_m, only : test_result_t
  use julienne_test_diagnosis_m, only : test_diagnosis_t
  implicit none

  private
  public :: vector_test_description_t
  public :: vector_diagnosis_function_i
#ifdef __GFORTRAN__
  public :: run
#endif

  abstract interface
    function vector_diagnosis_function_i() result(diagnoses)
      import test_diagnosis_t
      implicit none
      type(test_diagnosis_t), allocatable :: diagnoses(:)
    end function
  end interface

  type vector_test_description_t
    private
    type(string_t), allocatable :: descriptions_(:)
    procedure(vector_diagnosis_function_i), pointer, nopass :: vector_diagnosis_function_ => null()
  contains
    procedure run
    generic :: contains_text => contains_string_t, contains_characters
    procedure, private::        contains_string_t, contains_characters
  end type

  interface vector_test_description_t

    module function construct_from_strings(descriptions, vector_diagnosis_function) result(vector_test_description)
     !! The result is a vector_test_description_t object with the components defined by the dummy arguments
      implicit none
      type(string_t), intent(in) :: descriptions(:)
      procedure(vector_diagnosis_function_i), intent(in), pointer, optional :: vector_diagnosis_function
      type(vector_test_description_t) vector_test_description
    end function

  end interface

  interface

    impure module function run(self) result(test_results)
      !! The result encapsulates the test description and test outcome
      implicit none
      class(vector_test_description_t), intent(in) :: self
      type(test_result_t), allocatable :: test_results(:)
    end function

    module function contains_characters(self, substring) result(match_vector)
      !! The result is .true. if the test description includes the value of substring 
      implicit none
      class(vector_test_description_t), intent(in) :: self
      character(len=*), intent(in) :: substring
      logical, allocatable :: match_vector(:)
    end function

    module function contains_string_t(self, substring) result(match_vector)
      !! The result is .true. if the test description includes the value of substring%string()
      implicit none
      class(vector_test_description_t), intent(in) :: self
      type(string_t), intent(in) :: substring
      logical, allocatable :: match_vector(:)
    end function

  end interface


contains

  module procedure contains_characters
    integer i
    call_assert(allocated(self%descriptions_))
    match_vector = [(index(self%descriptions_(i)%string(), substring) /= 0, i = 1, size(self%descriptions_))]
  end procedure

  module procedure contains_string_t
    match_vector = self%contains_characters(substring%string())
  end procedure

#ifndef __GFORTRAN__

  module procedure construct_from_strings
    vector_test_description%descriptions_ = descriptions
    vector_test_description%vector_diagnosis_function_ => vector_diagnosis_function
  end procedure

#else

  module function construct_from_strings(descriptions, vector_diagnosis_function) result(vector_test_description)
    type(string_t), intent(in) :: descriptions(:)
    procedure(vector_diagnosis_function_i), intent(in), pointer, optional :: vector_diagnosis_function
    type(vector_test_description_t) vector_test_description
    vector_test_description%descriptions_ = descriptions
    if (present(vector_diagnosis_function)) vector_test_description%vector_diagnosis_function_ => vector_diagnosis_function
  end function

#endif

  module procedure run
    if (.not. associated(self%vector_diagnosis_function_)) then
      test_results = test_result_t(self%descriptions_)
    else
      associate(diagnoses => self%vector_diagnosis_function_())
#if defined(ASSERTIONS)
        associate(num_descriptions => size(self%descriptions_), num_results => size(diagnoses))
          call_assert(num_descriptions == num_results)
        end associate
#endif
        test_results = test_result_t(self%descriptions_, diagnoses)
      end associate
    end if
  end procedure

end module julienne_vector_test_description_m
