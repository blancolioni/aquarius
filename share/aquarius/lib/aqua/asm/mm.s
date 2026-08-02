    data

;   heap+0  next   the next free address
;   heap+1  limit  one past the end of the block 'next' points into
;
;   Both zero before the first allocation, which makes the available-space
;   test below fail and so asks the OS for the first block.  There is no
;   special case for it.

heap
    word 0, 0

    code

mm.allocate
    get %5, rJ
    geta %4, heap_addr
    ld %4, %4, 0
    ld %1, %4, 0            ; next
    ld %2, %4, 1            ; limit
    sub %3, %2, %1          ; available = limit - next
    sub %3, %3, %0          ; available - requested
    bnn %3, 2f              ; enough room left in this block
    setl %255, 64           ; no: ask the OS for another 64K
    trap 4, 0, 0
    bz %255, 3f             ; zero means no more memory for us :-(
    set %1, %255            ; next = base of the new block
    seth %2, 1              ; 65536: seth sets the high half word
    add %2, %1, %2          ; limit = base + 64K
    st %1, %4, 0
    st %2, %4, 1
    sub %3, %2, %1          ; re-test: a request bigger than a whole block
    sub %3, %3, %0          ; can never be satisfied, and must not be
    bn %3, 3f               ; handed out anyway
2   add %2, %1, %0          ; carve the request off the front
    st %2, %4, 0            ; next += size
    set %0, %1              ; and the old next is the result
    put rJ, %5
    pop 1, 0
3   trap 0, 0, 0            ; we ded

heap_addr
    word heap
