# Haskell-Little-Man-Computer
A simple implementation of Little Man Computer in Haskell(Denoted as LMC in later docs)

Detailed discription of LMC: http://en.wikipedia.org/wiki/Little_man_computer


###LMC Instruction Code Reference 
- INP: ask user to type in a number then store it in the accumulator 
- STA: get the value in the accumulator then store it in the mailbox of the given label 
- LDA: get the value in the mailbox of the given label then store it in the accumulator 
- ADD: get the value in the mailbox of the given label, add it with the accumulator and store 
- SUB: subtract the value in mailbox of the given label from the value in the accumulator 
- OUT: print out the value in the accumulator 
- BRA: jump to the instruction with the given label 
- BRZ: check the value in the accumulator. if it is zero jump to the instruction with the given label 
- BRP: check the value in the accumulator. if it is positive, jump to the instruction with the given label 
- HLT: terminate the program 
- DAT: declare a variable with an initial value. The initial value is zero by default


###Rules:

1.Every line must contain an instruction

2.Each line starts with either a label or a space and/or end with a comment.

3.DAT instructions will appear only at the bottom of a program


###Sample Code:

####A)
    INP
    STA FIRST
    INP
    STA SECOND
    LDA FIRST
    SUB SECOND
    OUT
    HLT
    FIRST DAT
    SECOND DAT

####B)
         INP
    LOOP SUB ONE  // Label this memory address as LOOP, The instruction will then subtract the value stored at address ONE from the accumulator
         OUT
         BRZ QUIT // If the accumulator value is 0, jump to the memory address labeled QUIT
         BRA LOOP // If the accumulator value is not 0, jump to the memory address labeled LOOP
    QUIT HLT      // Label this memory address as QUIT
    ONE  DAT 1    // Store the value 1 in this memory address, and label it ONE (variable declaration)
