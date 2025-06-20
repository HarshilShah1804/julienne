! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
#include "assert_macros.h"

module julienne_test_diagnosis_m
  !! Define abstractions, defined operations, and procedures for writing correctness checks in
  !! the form of assertions and tests.
  use assert_m
  use julienne_string_m, only : operator(.csv.), operator(.cat.)
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: test_diagnosis_t
  public :: call_julienne_assert_
  public :: julienne_assert
  public :: operator(.all.)
  public :: operator(.and.)
  public :: operator(.approximates.)
  public :: operator(.within.)
  public :: operator(.withinFraction.)
  public :: operator(.withinPercentage.)
  public :: operator(.equalsExpected.)
  public :: operator(.lessThan.)
  public :: operator(.lessThanOrEqualTo.)
  public :: operator(.greaterThan.)
  public :: operator(.greaterThanOrEqualTo.)

  type test_diagnosis_t
    !! Encapsulate test outcome and diagnostic information
    private
    logical test_passed_
    character(len=:), allocatable :: diagnostics_string_
  contains
    procedure test_passed
    procedure diagnostics_string
  end type

  integer, parameter :: default_real = kind(1.), double_precision = kind(1D0)

#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
  type operands_t(k)
    integer, kind :: k = default_real
    real(k) actual, expected 
  end type
#else
  type operands_t
    real actual, expected 
  end type

  type double_precision_operands_t
    double precision actual, expected 
  end type
#endif

  interface call_julienne_assert_

    pure module subroutine julienne_assert(test_diagnosis, file, line)
      !! Use cases:
      !!   1. When invoked via the generic interface, the preprocessor passes the 'file' and 'line' dummy arguments automatically.
      !!   2. When invoked directly, there is 1 argument: an expression containing defined operations such as 1 .equalsExpected. 1
      implicit none
      type(test_diagnosis_t), intent(in) :: test_diagnosis
      character(len=*), intent(in), optional :: file
      integer, intent(in), optional :: line
    end subroutine

  end interface

  interface operator(.all.)
     
#ifndef __GFORTRAN__

    pure module function aggregate_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(..)
      type(test_diagnosis_t) diagnosis
    end function

#else

    pure module function aggregate_scalar_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_vector_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:)
      type(test_diagnosis_t) diagnosis
    end function

#endif
  end interface

  interface operator(.and.)
     
    elemental module function and(lhs, rhs) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: lhs, rhs
      type(test_diagnosis_t) diagnosis
    end function

  end interface

  interface operator(.approximates.)

    elemental module function approximates_real(actual, expected) result(operands)
      implicit none
      real, intent(in) :: actual, expected
      type(operands_t) operands
    end function

    elemental module function approximates_double_precision(actual, expected) result(operands)
      implicit none
      double precision, intent(in) :: actual, expected
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
      type(operands_t(double_precision)) operands
#else
      type(double_precision_operands_t) operands
#endif
    end function

  end interface

  interface operator(.equalsExpected.)

    elemental module function equals_expected_integer(actual, expected) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.lessThan.)

    elemental module function less_than_real(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      real, intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function less_than_double(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      double precision, intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function less_than_integer(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.lessThanOrEqualTo.)

    elemental module function less_than_or_equal_to_integer(actual, expected_max) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_max
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.greaterThanOrEqualTo.)

    elemental module function greater_than_or_equal_to_integer(actual, expected_min) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_min
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.greaterThan.)

    elemental module function greater_than_real(actual, expected_floor) result(test_diagnosis)
      implicit none
      real, intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function greater_than_double(actual, expected_floor) result(test_diagnosis)
      implicit none
      double precision, intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function greater_than_integer(actual, expected_floor) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.within.)

    elemental module function within_real(operands, tolerance) result(test_diagnosis)
      implicit none
      type(operands_t), intent(in) :: operands
      real, intent(in) :: tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
    elemental module function within_double_precision(operands, tolerance) result(test_diagnosis)
      implicit none
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
      type(operands_t(double_precision)), intent(in) :: operands
#else
      type(double_precision_operands_t), intent(in) :: operands
#endif
      double precision, intent(in) :: tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
  end interface

  interface operator(.withinFraction.)

    elemental module function within_real_fraction(operands, fractional_tolerance) result(test_diagnosis)
      implicit none
      type(operands_t), intent(in) :: operands
      real, intent(in) :: fractional_tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
    elemental module function within_double_precision_fraction(operands, fractional_tolerance) result(test_diagnosis)
      implicit none
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
      type(operands_t(double_precision)), intent(in) :: operands
#else
      type(double_precision_operands_t), intent(in) :: operands
#endif
      double precision, intent(in) :: fractional_tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
  end interface

  interface operator(.withinPercentage.)

    elemental module function within_real_percentage(operands, percentage_tolerance) result(test_diagnosis)
      implicit none
      type(operands_t), intent(in) :: operands
      real, intent(in) :: percentage_tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
    elemental module function within_double_precision_percentage(operands, percentage_tolerance) result(test_diagnosis)
      implicit none
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
      type(operands_t(double_precision)), intent(in) :: operands
#else
      type(double_precision_operands_t), intent(in) :: operands
#endif
      double precision, intent(in) :: percentage_tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
  end interface

  interface test_diagnosis_t

    elemental module function construct_from_string_t(test_passed, diagnostics_string) result(test_diagnosis)
      !! The result is a test_diagnosis_t object with the components defined by the dummy arguments
      implicit none
      logical, intent(in) :: test_passed
      type(string_t), intent(in) :: diagnostics_string
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function construct_from_character(test_passed, diagnostics_string) result(test_diagnosis)
      !! The result is a test_diagnosis_t object with the components defined by the dummy arguments
      implicit none
      logical, intent(in) :: test_passed
      character(len=*), intent(in) :: diagnostics_string
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface

    elemental module function test_passed(self) result(passed)
      !! The result is .true. if the test passed and false otherwise
      implicit none
      class(test_diagnosis_t), intent(in) :: self
      logical passed
    end function

    elemental module function diagnostics_string(self) result(string_)
      !! The result is a string describing the condition(s) that caused a test failure
      implicit none
      class(test_diagnosis_t), intent(in) :: self
      type(string_t) string_
    end function

  end interface

contains

  module procedure and
     diagnosis = .all. ([lhs,rhs])
  end procedure 

#ifndef __GFORTRAN__

  module procedure aggregate_diagnosis
    character(len=*), parameter :: new_line_indent = new_line('') // "        "
    integer i
    type(string_t), allocatable :: empty(:)

    select rank(diagnoses)
      rank(0)
        diagnosis = diagnoses
      rank(1)
        diagnosis = test_diagnosis_t( &
           test_passed = all(diagnoses%test_passed_) &
          ,diagnostics_string = .cat. pack( &
             array = [(string_t(new_line_indent // diagnoses(i)%diagnostics_string_), i=1,size(diagnoses))] &
            ,mask  = .not. diagnoses%test_passed_ &
        ) )
      rank default 
        associate(diagnoses_rank => string_t(rank(diagnoses)))
          error stop "aggregate_diagnosis (julienne_test_diagnosis_s): rank " // diagnoses_rank%string() // " unspported"
        end associate
    end select
  end procedure

#else

  module procedure aggregate_scalar_diagnosis
    diagnosis = diagnoses
  end procedure
   
  module procedure aggregate_vector_diagnosis
    character(len=*), parameter :: new_line_indent = new_line('') // "        "
    type(string_t), allocatable :: array(:)
    integer i
    allocate(array(size(diagnoses)))
    do i = 1, size(diagnoses)
      array(i) = string_t(new_line_indent // diagnoses(i)%diagnostics_string_)
    end do
    diagnosis = test_diagnosis_t( &
       test_passed = all(diagnoses%test_passed_) &
      ,diagnostics_string = .cat. pack( &
         array = array &
        ,mask  = .not. diagnoses%test_passed_ &
    ) ) 
  end procedure
#endif

  module procedure approximates_real
    operands = operands_t(actual, expected) 
  end procedure

  module procedure approximates_double_precision
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
    operands = operands_t(double_precision)(actual, expected) 
#else
    operands = double_precision_operands_t(actual, expected) 
#endif
  end procedure

  module procedure equals_expected_integer

    if (actual == expected) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "expected " // string_t(expected) // "; actual value is " // string_t(actual) &
      )
    end if

  end procedure

  module procedure less_than_real

    if (actual < expected_ceiling) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than " // string_t(expected_ceiling) &
      )
    end if

  end procedure

  module procedure less_than_double

    if (actual < expected_ceiling) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than " // string_t(expected_ceiling) &
      )
    end if

  end procedure

  module procedure less_than_integer

    if (actual < expected_ceiling) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than " // string_t(expected_ceiling) &
      )
    end if

  end procedure

  module procedure less_than_or_equal_to_integer

    if (actual <= expected_max) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than or equal to " // string_t(expected_max) &
      )
    end if

  end procedure

  module procedure greater_than_or_equal_to_integer

    if (actual >= expected_min) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than or equal to " // string_t(expected_min) &
      )
    end if

  end procedure

  module procedure greater_than_real

    if (actual > expected_floor) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than " // string_t(expected_floor) &
      )
    end if

  end procedure

  module procedure greater_than_double

    if (actual > expected_floor) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than " // string_t(expected_floor) &
      )
    end if

  end procedure

  module procedure greater_than_integer

    if (actual > expected_floor) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than " // string_t(expected_floor) &
      )
    end if

  end procedure

  module procedure within_real

    if (abs(operands%actual - operands%expected) <= tolerance) then
      ! We use <= to allow for tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // " within a tolerance of " // string_t(tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_real_fraction

    if (abs(operands%actual - operands%expected) <= abs(fractional_tolerance*operands%expected)) then
      ! We use <= to allow for fractional_tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // " within a fractional tolerance of " // string_t(fractional_tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_real_percentage

    if (abs(operands%actual - operands%expected) <= abs(operands%expected*percentage_tolerance/1D02)) then
      ! We use <= to allow for fractional_tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected " // string_t(operands%expected) &
                           // " within a tolerance of " // string_t(percentage_tolerance) // " percent;" &
                           // " actual value is " // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_double_precision

    if (abs(operands%actual - operands%expected) <= tolerance) then
      ! We use <= to allow for tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // " within a tolerance of " // string_t(tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_double_precision_fraction

    if (abs(operands%actual - operands%expected) <= abs(fractional_tolerance*operands%expected)) then
      ! We use <= to allow for tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // " within a fractional tolerance of " // string_t(fractional_tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_double_precision_percentage

    if (abs((operands%actual - operands%expected)) <= abs(operands%expected*percentage_tolerance/1D02)) then
      ! We use <= to allow for tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "               // string_t(operands%expected) &
                           // " within a tolerance of " // string_t(percentage_tolerance) // " percent;" &
                           // " actual value is "       // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure construct_from_string_t
    test_diagnosis%test_passed_ = test_passed
    test_diagnosis%diagnostics_string_ = diagnostics_string
  end procedure

  module procedure construct_from_character
    test_diagnosis%test_passed_ = test_passed
    test_diagnosis%diagnostics_string_ = diagnostics_string
  end procedure

  module procedure test_passed
    passed = self%test_passed_
  end procedure

  module procedure diagnostics_string
    call_assert(allocated(self%diagnostics_string_))
    string_ = string_t(self%diagnostics_string_)
  end procedure

  module procedure julienne_assert
    character(len=:), allocatable :: diagnostics_string
    diagnostics_string =  test_diagnosis%diagnostics_string_
    if (present(file)) diagnostics_string = diagnostics_string // " in file " // file
    if (present(line)) diagnostics_string = diagnostics_string // " at line " // string_t(line)
    call assert_always(test_diagnosis%test_passed_, diagnostics_string)
  end procedure


end module julienne_test_diagnosis_m
