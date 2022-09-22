!>@date 28.3.2022
!>@brief A program for testing macros.

#include <macros.h>

program test_macros

    implicit none

    real, allocatable :: arr(:)
    real, pointer :: ptr(:)

    EWRITE("Testing error write.")
    WWRITE("Testing warning write.")

    DWRITE("Testing debug write.")
    VWRITE("Testing verbose write.")

    open(unit=LOG_UNIT, file="log_test.txt", status='replace')

    DWRITE("Testing debug write.")
    VWRITE("Testing verbose write.")

    VWRITE("Testing deallocation.")
    PTR_DEALLOC(ptr)
    ALL_DEALLOC(arr)
    allocate(arr(10), ptr(10))
    PTR_DEALLOC(ptr)
    ALL_DEALLOC(arr)

    PCONT
    
    VWRITE("Tests complete.")
    
    close(LOG_UNIT)

    PEXIT

end program test_macros