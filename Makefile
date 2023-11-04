build:
	gfortran TicTacToe.f90 -o TicTacToe

clean:
	rm TicTacToe

rebuild:
	make clean
	make build

