!>@date 16.03.2022
!>@brief A stopwatch module used for execution time evaluation.

module timeit_module

    use, intrinsic :: iso_fortran_env, only: output_unit, int16
    use stdlib_kinds, only: dp
    use stdlib_stats, only: mean, var

    implicit none

    private

    integer, parameter :: STDREPS = 1000

    type, public :: stopwatch
        integer(int16) :: outunit
        real(dp) :: t1 = 0_dp
        real(dp) :: t2 = 0_dp
        real(dp), allocatable :: runtimes(:)
    contains
        procedure, private :: Init
        procedure :: Start
        procedure :: Stop
        procedure :: Reset
        final :: Destroy
    end type stopwatch

    interface stopwatch
        module procedure GetInstance, GetPtrInstance
    end interface stopwatch

    contains

    function GetInstance(runs, outunit) result(self)
        class(stopwatch), allocatable :: self
        integer(int16), intent(in), optional :: runs
        integer(int16), intent(in), optional :: outunit

        allocate(self)
        call self%Init(runs, outunit)
    end function GetInstance

    function GetPtrInstance(runs, outunit) result(self)
        class(stopwatch), pointer :: self
        integer(int16), intent(in), optional :: runs
        integer(int16), intent(in), optional :: outunit

        allocate(self)
        call self%Init(runs, outunit)
    end function GetPtrInstance

    subroutine Init(self, runs, outunit)
        class(stopwatch), intent(inout) :: self
        integer(int16), intent(in), optional :: runs
        integer(int16), intent(in), optional :: outunit

        self%t1 = 0_dp; self%t2 = 0_dp

        deallocate(self%runtimes)
        
        if (present(runs)) then
            allocate(self%runtimes(runs))
        else
            allocate(self%runtimes(STDREPS))
        end if
        self%runtimes = 0_dp

        if (present(outunit)) then
            self%outunit = outunit
        else
            self%outunit = output_unit
        end if
    end subroutine Init

    subroutine Start(self)
        class(stopwatch), intent(inout) :: self

        call cpu_time(self%t1)
    end subroutine

    subroutine Stop(self)
        class(stopwatch), intent(inout) :: self

        call cpu_time(self%t2)
    end subroutine

    subroutine Reset(self, runs, outunit)
        class(stopwatch), intent(inout) :: self
        integer(int16), intent(in), optional :: runs
        integer(int16), intent(in), optional :: outunit

        call self%Init(runs, outunit)
    end subroutine

    subroutine Destroy(self)
        type(stopwatch) :: self

        if (allocated(self%runtimes)) deallocate(self%runtimes)
    end subroutine Destroy

end module timeit_module