system.exceptions.raise_handler
    get %255, rJ       ;  where we came from
system.exceptions.fail_handler
    set %4, %255
    setl %0, 57344     ;  load handler from 16#FFFF_E000#
    inch %0, 65535     ;  high word
    ld %1, %0, 0       ;  could save an instruction with ld %1, %0, 3800 but that is less clear I think
    bz %1, 3f          ;  no exception handlers, leave immediately
1   ld %2, %1, 0       ;  offset 0 -> base of handled range
    sub %3, %4, %2     ;  check that our origin is greater than the base
    bn %3, 2f          ;  2f goes to the next handler
    ld %2, %1, 1       ;  offset 1 -> bound of handled range
    sub %3, %2, %4     ;  we must be less than this
    bnp %3, 2f         ;  next handler
    ld %2, %1, 2       ;  we are in range, so we can call the handler (offset 2)
    put rJ, %2         ;  put handler into rJ so we can pop back to it
    pop 0, 0           ;  pop

2   ld %1, %1, 3       ;  offset 3 -> next handler record
    bnz %1, 1b         ;  non-zero, we have a next

3   set %255, %254     ;  exception was unhandled, so just halt
    trap 0, 0, 0

