! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module julienne_test_m
  !! Define an abstract test_t type with deferred bindings ("subject" and "results")
  !! used by a type-bound procedure ("report") for reporting test results.  The "report"
  !! procedure thus represents an implementation of the Template Method pattern.
  use julienne_test_result_m, only : test_result_t
  use julienne_user_defined_collectives_m, only : co_all
  use julienne_command_line_m, only : command_line_t

  implicit none

  private
  public :: test_t, test_description_substring

  character(len=:), allocatable :: test_description_substring

  type, abstract :: test_t
    !! Facilitate testing and test reporting
  contains
    procedure(subject_interface), nopass, deferred :: subject 
    procedure(results_interface), nopass, deferred :: results
    procedure :: report
  end type

  abstract interface

    pure function subject_interface() result(specimen_description)
      !! The result is the name of the test specimen (the subject of testing)
      character(len=:), allocatable :: specimen_description
    end function

    function results_interface() result(test_results)
      !! The result is an array of test results for subsequent reporting in the "report" type-bound procedure
      import test_result_t
      type(test_result_t), allocatable :: test_results(:)
    end function

  end interface

  interface

    module subroutine report(test, passes, tests, skips)
      !! Print the test results and increment the tallies of passing tests, total tests, and skipped tests.
      implicit none
      class(test_t), intent(in) :: test
      integer, intent(inout) :: passes, tests, skips
    end subroutine

  end interface

contains

contains

  module procedure report

#if HAVE_MULTI_IMAGE_SUPPORT
    associate(me => this_image())
#else
    integer me
    me = 1
#endif

      if (me==1) then

        first_report: &
        if (.not. allocated(test_description_substring)) then
          block
            type(command_line_t) command_line
            test_description_substring = command_line%flag_value("--contains")
          end block
          print *
          if (len(test_description_substring)==0) then
            print '(a)',"Running all tests."
            print '(a)',"(Add '-- --contains <string>' to run only tests with subjects or descriptions containing the specified string.)"
          else
            print '(*(a))',"Running only tests with subjects or descriptions containing '", test_description_substring,"'."
          end if
        end if first_report

        print '(*(a))', new_line('a'), test%subject()

      end if

#if HAVE_MULTI_IMAGE_SUPPORT
      call co_broadcast(test_description_substring, source_image=1)
#endif

#ifndef _CRAYFTN
      associate(test_results => test%results())
        associate(num_tests => size(test_results))
          tests = tests + num_tests
          if (me==1) then
            block
              integer i
              do i=1,num_tests
                if (me==1) print '(3x,a)', test_results(i)%characterize()
              end do
            end block
          end if
          block
            logical, allocatable :: passing_tests(:), skipped_tests(:)

            passing_tests = test_results%passed()
            skipped_tests = test_results%skipped()

            call co_all(passing_tests)
            call co_all(skipped_tests)

            associate(num_passes => count(passing_tests), num_skipped => count(skipped_tests))
              if (me==1) print '(a,3(i0,a))'," ",num_passes," of ", num_tests," tests pass.  ", num_skipped, " tests were skipped."
              passes = passes + num_passes
              skips  = skips  + num_skipped
            end associate
          end block
        end associate
#if HAVE_MULTI_IMAGE_SUPPORT
      end associate
#endif

#else
      block
        logical, allocatable :: passing_tests(:)
        type(test_result_t), allocatable :: test_results(:)
        integer i

        test_results = test%results()
        associate(num_tests => size(test_results))
          tests = tests + num_tests
          if (me==1) then
            do i=1,num_tests
              if (me==1) print '(3x,a)', test_results(i)%characterize()
            end do
          end if
          passing_tests = test_results%passed()
          call co_all(passing_tests)
          associate(num_passes => count(passing_tests))
            if (me==1) print '(a,2(i0,a))'," ",num_passes," of ", num_tests," tests pass."
            passes = passes + num_passes
          end associate
        end associate
      end block
#endif

    end associate

  end procedure
end module julienne_test_m
