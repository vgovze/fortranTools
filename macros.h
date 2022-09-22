/* Date 28.3.2022. */
/* Auxilary preprocessor functions to simplify code. */

/* Array and pointer deallocation. */
#define PTR_DEALLOC(var) if (associated(var)) deallocate(var)
#define ALL_DEALLOC(var) if (allocated(var)) deallocate(var)

#ifndef LOG_UNIT
#define LOG_UNIT 7
#endif

#ifndef DEBUG
#define DEBUG 2
#endif

#ifndef VERBOSE
#define VERBOSE 2
#endif

/* Press any key to... */
#define PEXIT write(*,*) "Press any key to exit..."; read(*,*); stop
#define PCONT write(*,*) "Press any key to continue..."; read(*,*)

/* Print statemets. */
#define WWRITE(msg) write(*,*) __FILE__, __LINE__, "WARNING! ", msg
#define EWRITE(msg) write(*,*) __FILE__, __LINE__, "ERROR! ", msg

#if (DEBUG == 1)
#define DWRITE(msg) write(*,*) __FILE__, __LINE__, msg
#elif (DEBUG == 2)
#define DWRITE(msg) write(LOG_UNIT,*) __FILE__, __LINE__, msg
#else
#define DWRITE(msg)
#endif

#if (VERBOSE == 1)
#define VWRITE(msg) write(*,*) msg
#elif (VERBOSE == 2)
#define VWRITE(msg) write(LOG_UNIT,*) msg
#else
#define VWRITE(msg)
#endif