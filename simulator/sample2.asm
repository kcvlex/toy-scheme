        ADD     a0,zero,zero     # sum
        ADDI    t2,zero,100
        ADD     t3,sp,zero       # A
        ADD     t1,zero,zero     # initialize the array
        ADD     t4,t3,zero
LOOP1:  
        SW      t1,0(t4)
        ADDI    t4,t4,-4
        ADDI    t1,t1,1
        BNE     t1,t2,LOOP1
        ADDI    t1,zero,1         # compute
        ADD     t4,t3,zero
LOOP2:  
        LW      a1,0(t4)
        LW      a2,-4(t4)
        ADD     t5,a1,a2
        SW      t5,-4(t4)
        ADDI    t4,t4,-4
        ADDI    t1,t1,1
        BNE     t1,t2,LOOP2
        ADD     t1,zero,zero     # obtain the sum
        ADD     t4,t3,zero
LOOP3:  
        LW      t5,0(t4)
        ADD     a0,a0,t5
        ADDI    t4,t4,-4
        ADDI    t1,t1,1
        BNE     t1,t2,LOOP3
        JAL     zero,exit
