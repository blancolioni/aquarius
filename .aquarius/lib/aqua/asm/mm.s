    data

heap
    word 0, 0

    code

mm.allocate
    get %5, rJ
    geta %4, heap_addr
    ld %4, %4, 0
    ld %1, %4, 1            ; heap_last has the last available address
    bz %1, 1f               ; if it's zero, we haven't allocated anything yet
    ld %2, %4, 0            ; see if we have enough room for the request
    sub %3, %1, %2          ; available = heap_last - heap_top
    sub %3, %3, %0          ; available < size?
    bnn %3, 2f              ; we have enough, so allocate it
1   setl %255, 64           ; we will allocate 64K at a time
    trap 4, 0, 0            ; ask the OS
    bz %255, 3f             ; zero means no more memory for us :-(
    st %255, %4, 0          ; store new memory in heap_top
    st %255, %4, 1          ; and heap last
2   ld %1, %4, 1            ; get the current end of the heap
    add %2, %1, %0          ; reserve the requested number of bytes
    set %0, %1              ; end of heap is also return value
    st %2, %4, 1            ; update heap last
    put rJ, %5
    pop 1, 0                
3   trap 0, 0, 0            ; we ded

heap_addr
    word heap


