// This file is part of the materials accompanying the book 
// "The Elements of Computing Systems" by Nisan and Schocken, 
// MIT Press. Book site: www.idc.ac.il/tecs
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[3], respectively.)

//initialize the sum
@sum
M = 0

//jump to the end if the multiplicand is 0
@R0
D = M

@END_LOOP
D;JEQ

//jump to the end if the multiplier is 0
@R1
D = M

@END_LOOP
D;JEQ

//add the multiplicand to the sum (first iteration)
@R0
D = M

@sum
M = D

(LOOP)
	
	//decrement the multiplier
	@R1
	D = M-1
	M = D

	//terminate the loop if the multiplier is 0
	@END_LOOP
	D;JLE

	//add the multiplicand to the sum
	@R0
	D = M

	@sum
	M = D+M

	//continue to the next iteration
	@LOOP
	0;JMP

(END_LOOP)
	//store the result in R2
	@sum
	D = M

	@R2
	M = D
(END)
	@END
	0;JMP