! RUN: %B/test/Semantics/test_errors.sh %s %flang %t
!Testing data constraints : C876, C877
module m
  integer :: first
  contains
    subroutine h
      integer a,b
      !C876
      !ERROR: Data object must not be accessed by host association
      DATA first /1/
      common /c/ a,b
      !C876
      !ERROR: Data object part 'a' must not be in a named COMMON block outside a BLOCK DATA program unit
      DATA a /1/
    end subroutine

    function g(i)
      type newType
        sequence
        integer number
      end type
      type(newType) num
      common /c/ num
      !C876
      !ERROR: Data object part 'num' must not be in a named COMMON block outside a BLOCK DATA program unit
      DATA num%number /1/
      integer ::i
      g = i *1024
    end

    function f(i)
      integer ::i
      integer ::result
      integer, allocatable :: a
      integer :: b(i)
      !C876
      !ERROR: Data object must not be a dummy argument
      DATA i /1/
      !C876
      !ERROR: Data object must not be a function result
      DATA f /1/
      !C876
      !ERROR: Data object must not be a function name
      DATA g /1/
      !C876
      !ERROR: Data object part 'a' must not be an allocatable object
      DATA a /1/
      !C876
      !ERROR: Data object part 'b' must not be an automatic array
      DATA b(0) /1/
      f = i *1024
    end

    subroutine CheckObject(i)
      type specialNumbers
        integer one
        integer numbers(5)
        type(specialNumbers), pointer :: headOfTheList
      end type
      type large
        integer, allocatable :: elt(:)
        integer val
        type(specialNumbers) numsArray(5)
      end type
      type(large) largeNumber
      type(large), allocatable :: allocatableLarge
      type(large) :: largeNumberArray(i)
      !C877
      !OK: Correct use
      DATA(largeNumber % numsArray(j) % headOfTheList, j = 1, 10) / 10 * NULL() /
      !C877
      !ERROR: Only right-most part of data object can be a pointer
      DATA(largeNumber % numsArray(j) % headOfTheList % one, j = 1, 10) / 10 * NULL() /
      !C876
      !ERROR: Data object part 'elt' must not be an allocatable object
      DATA(largeNumber % elt(j) , j = 1, 10) / 10 * 1/
      !C876
      !ERROR: Data object part 'largenumberarray' must not be an automatic array
      DATA(largeNumberArray(j) % val, j = 1, 10) / 10 * NULL() /
      !C876
      !ERROR: Data object part 'allocatablelarge' must not be an allocatable object
      DATA allocatableLarge % val / 1 /
    end
  end

  block data foo
          integer :: a,b
          common /c/ a,b
          !C876
          !OK: Correct use
          DATA a /1/
  end block data

  module m2
    integer m2_i
    type newType
      integer number
    end type
    type(newType) m2_number1
    contains

    subroutine checkDerivedType(m2_number)
      type(newType) m2_number
      type(newType) m2_number3
      !C876
      !ERROR: Data object must not be a dummy argument
      DATA m2_number%number /1/
      !C876
      !ERROR: Data object must not be accessed by host association
      DATA m2_number1%number /1/
      !C876
      !OK: m2_number3 is not associated through use association
      DATA m2_number3%number /1/
    end
  end

  program new
    use m2
    integer a
    real    b,c
    COMMON b,a,c
    type(newType) m2_number2
    !C876
    !ERROR: Data object part 'b' must not be in blank COMMON
    DATA b /1/
    !C876
    !ERROR: Data object must not be accessed by use association
    DATA m2_i /1/
    !C876
    !ERROR: Data object must not be accessed by use association
    DATA m2_number1%number /1/
    !C876
    !OK: m2_number2 is not associated through use association
    DATA m2_number2%number /1/
  end program
