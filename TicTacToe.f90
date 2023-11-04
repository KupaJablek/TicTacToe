module helper_functions
contains
    ! will return a board populated with 'empty' squares 
    function initialize_board(c)
        implicit none

        character, dimension(3, 3) :: c_b = " ", initialize_board
        character :: c
        integer :: i, j

        do i = 1, 3
            do j = 1, 3
            c_b(i, j) = c
            end do
        end do 

        initialize_board = c_b
    end function initialize_board

    ! run a game and return the winner -> 0:tie, 1:X, 2:O
    function play_game(p1_name, p2_name)
        implicit none

        character, dimension(3, 3) :: c_board = "*"
        CHARACTER (LEN = 15) :: p1_name, p2_name
        CHARACTER :: fill_board
        LOGICAL :: game_over = .False., current_player 
        integer :: play_game, win_condition !0:tie, 1:p1, 2:p2

        ! create new gameboard
        fill_board = " "
        c_board = initialize_board(fill_board)

        ! set first player
        current_player = .True. ! p1 is the starting player 

        ! main gameplay loop
        do while(game_over .eqv. .False.)
            call clear_terminal()
            call print_board(c_board) 

            if (current_player) then
                print *, "Player 1's turn"
            else
                print *, "Player 2's turn"
            end if
            ! player turn
            call take_turn(c_board, current_player)

            current_player = .not. current_player

            ! check for win or tie
            win_condition = check_win_condition(c_board)
            if (win_condition /= -1) then
                ! GAMEOVER
                print *, 'GAMEOVER'
                call clear_terminal()

                call print_board(c_board)

                game_over = .True.
            end if
        end do

        ! return winner
        play_game = win_condition 
    end function play_game

    subroutine take_turn(board, c_player)
        implicit none

        character, dimension(3, 3) :: board 
        logical :: c_player, valid_input 
        integer :: xInput, yInput

        ! reset valid_input to false.
        valid_input = .false.

        do while(.not. valid_input)
            ! get user input
            print *, 'Enter x position (1-3):'
            read *, xInput

            print *, 'Enter y position (1-3):'
            read *, yInput

            ! check if input valid
            if (3 < xInput .or. xInput < 1 .or. 3 < yInput .or. yInput < 1) then
                print *, 'Enter a position between 1 and 3'
                continue
            end if

            ! check if space is taken
            if (board(xInput, yInput) /= "O" .and. board(xInput, yInput) /= "X") then
                valid_input = .true.
            else
                print *, "Space is already taken"
            end if
        end do

        ! fill empty space with correct sign
        if (c_player) then
           board(xInput, yInput) = "X" 
        else
            board(xInput, yInput) = "O"
        end if
    end subroutine

    function check_win_condition (board) ! -1:no win, 0: tie, 1:X win, 2:O win
        integer :: win_cond = -1, check_win_condition, i
        CHARACTER, dimension(3,3) :: board

        ! check for horizontal win
        do i = 1, 3
            if (board(i, 1) == 'X' .and. board(i, 2) == 'X' .and. board(i,3) == 'X') then
                win_cond = 1
            else if (board(i, 1) == 'O' .and. board(i, 2) == 'O' .and. board(i,3) == 'O') then
                win_cond = 2
            end if 
        end do

        ! check for vertical win
        do i = 1, 3
            if (board(1, i) == 'X' .and. board(2, i) == 'X' .and. board(3,i) == 'X') then
                win_cond = 1
            else if (board(1, i) == 'O' .and. board(2, i) == 'O' .and. board(3,i) == 'O') then
                win_cond = 2
            end if 
        end do

        ! check diagonal
        if (board(1, 1) == 'X' .and. board(2, 2) == 'X' .and. board(3,3) == 'X') then
            win_cond = 1
        else if (board(3, 1) == 'X' .and. board(2, 2) == 'X' .and. board(1,3) == 'X') then
            win_cond = 1
        end if

        if (board(1, 1) == 'O' .and. board(2, 2) == 'O' .and. board(3,3) == 'O') then
            win_cond = 2
        else if (board(3, 1) == 'O' .and. board(2, 2) == 'O' .and. board(1,3) == 'O') then
            win_cond = 2
        end if

        ! check tie
        if (.not. any(board == ' ')) then
            win_cond = 0
        end if

        check_win_condition = win_cond
    end function check_win_condition

    ! takes in character(3,3) array and prints to screen
    subroutine print_board (b)
        implicit none

        CHARACTER, dimension(3,3) :: b
        INTEGER :: i, j

        print *, "  1 2 3"
        print *, " +-+-+-+"
        do i = 1, 3
            write (*, 100) i, (" |"//b(i,1)//"|"//b(i,2)//"|"//b(i,3)//"| "), i
            100 format (I1,A,I1)
            print *, "  +-+-+-+"
        end do
        print *, ""
    end subroutine print_board

    subroutine clear_terminal()
        print *, achar(27)//"[2J"
    end subroutine clear_terminal

end module helper_functions

! main program
program tictactoe
    use helper_functions 
    implicit none

    logical :: keep_playing = .True. 
    integer :: p1_wins = 0, p2_wins = 0, winner 
    CHARACTER (LEN = 15) :: p1_name, p2_name

    ! clear terminal
    call clear_terminal()

    ! get player names
    print *, 'Enter player 1 name:'
    read *, p1_name

    print *, 'Enter player 2 name:'
    read *, p2_name

    winner = play_game(p1_name, p2_name)

    if (winner == 0) then
        print *, "TIE GAME"
    else if (winner == 1) then
        print *, "Winner is ", p1_name
    else if (winner == 2) then
        print *, "Winner is ", p2_name
    end if

end program tictactoe
