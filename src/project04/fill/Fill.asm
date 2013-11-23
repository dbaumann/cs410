// This file is part of the materials accompanying the book 
// "The Elements of Computing Systems" by Nisan and Schocken, 
// MIT Press. Book site: www.idc.ac.il/tecs
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, 
// the screen should be cleared.


(INITIALIZE)
	@SCREEN
	D = A
	
	@pos
	M = D

(LOOP)
	//increment pos
	@pos
	D = M+1
	M = D

	@KBD
	D = A-D

	//re-initialize if pos = SCREEN
	@INITIALIZE
	D;JLT

	//load the pressed key
	@KBD
	D = M

	@PRESSED
	D;JNE
	//key NOT pressed
	@pos
	A = M-1 //fill the pre-incremented address
	M = 0

	@LOOP
	0;JMP
	
	(PRESSED)
		//key pressed
		@pos
		A = M-1 //fill the pre-incremented address
		M = -1

		@LOOP
		0;JMP