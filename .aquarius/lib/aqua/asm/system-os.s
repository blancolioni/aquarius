system.os.halt
    set %255, %0
    trap 0, 0, 0
    
system.os.set_exit_status
    put rE, %0
    pop 1, 0
