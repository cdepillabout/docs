
# Switch to asm layout.
# This makes the top portion of the screen show the assembly code,
# and the bottom portion of the screen your command line.
layout asm

# step through one asm instruction
stepi

# after each time the program stops, display the current
# asm instruction
display/i $pc

# this also does something similar and gives you more disassembly context
set disassemble-next-line on

# just print the current asm instruction
x/i $pc
