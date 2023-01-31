subroutine show_board(board, row, column)
  implicit none
  integer, intent(in) :: row, column
  integer, dimension(row, column), intent(in) :: board
  integer :: i, j, k

  do k=0, column-1
    print'(a,i1,a,$)', "  ", k+1, "  "
  end do
  print*," "
  do i=0, row-1
    !print'(i1,$)', j+1
    do j=0, column-1
      !print'(i1,$)', j+1
      if (board(i,j)==1) then
        print'(a,$)', " [o] "
      else if (board(i, j)==2)then
        print'(a,$)', " [*] "
      else if (board(i,j)==3)then
        print'(a,$)', " [c] "
      else
        print'(a,$)', " [ ] "
      end if

    end do
    print*, " "
  end do

end subroutine show_board

subroutine player_turn(board, row, column, player, winner, computer)
  implicit none
  integer, intent(in) :: row, column, computer
  integer, dimension(row, column), intent(inout) :: board
  integer, intent(inout) :: winner, player
  integer :: temp, col_num, i, temp2=0
  !There is note just because the use of temp2 is because it gives me warning if I put board(zero, anyThing) the row position it gives me Warning
  !I don't even know why is that

  winner=0

  do while (winner==0)
    !call check_player(board, row, column, winner)
    call show_board(board, row, column)
    ! call the_board_values(board, row, column)
    !Will set very time the column to -1
    col_num=-1
    !if ((player==1) .or. (player==2))then
      do while(col_num<0)
        if ((player==1) .or. (player==2))then
          ! The player 1 or 2
          if (player==1)then
            print'(a,$)',  "o : Drop where? (0 = Exit) "
          else
            print'(a,$)',  "* : Drop where? (0 = Exit) "
          end if
        
          read*, temp
          !col_num=temp
        else
          call computer_player(board, row, column, temp)
          col_num=temp
        endif
        
	    if (temp==0)then
          winner=-1
          print*, "Game Over"
          exit
        
        else if(temp==100) then
          call the_board_values(board, row, column)
        
        else if (temp>column) then
          print*, "Out of range"
        else if (board(temp2, temp-1)/=0) then
          print*, "It's full"
        !If the below condtion is true then will start to loop over 
        else if ((temp>=1) .and. (temp<=column)) then
          do i=row-1, 0, -1
            if (board(i, temp-1)==0)then
              board(i, temp-1)=player
              col_num=temp
              !print*, board(i, temp-1)
              exit
            end if
          end do
          exit
	    else
           print*, "Enter Correct Value"
        end if
      end do
    call check_player(board, row, column, winner)
    temp=winner
    winner=temp

    !swapping between players
    if (computer==0)then
      if ((player==1) )then
        player=2
        !print*, "Changing to player 2"
      else
	    player=1
        !print*, "Changing to player 1"
      end if
    else
      if ((player==1) )then
        player=3
        !print*, "Changing to player 2"
      else
	    player=1
        !print*, "Changing to player 1"
      end if
    end if
    !call check_player(board, row, column, winner)
    !temp=winner
    !winner=temp
  end do
end subroutine player_turn

subroutine check_player(board, row, column, winner)
  implicit none
  integer, intent(in) :: row, column
  integer, dimension(row, column), intent(in) :: board
  integer :: i, j
  integer, intent(inout) :: winner

  !Checking the vertical
  do i=0, row-4
    do j=0, column-1
      if ((board(i, j)/=0) .and. (board(i, j)==board(i+1, j))) then
        if ((board(i, j)==board(i+2, j)) .and. (board(i, j)==board(i+3, j))) then
          winner=board(i,j)
          call show_board(board, row, column)
          if (winner==1)then
            print*, "o Wins"
          else if (winner==2)then
            print*, "* Wins"
          else
            print*, "c wins"
          end if
          exit
         end if
      end if
    end do
    if (winner/=0)then
      exit
    end if
  end do

  !Checking the horizontal 
  do i=0, row-1
    do j=0, column-4
      if ((board(i, j)/=0) .and. (board(i, j)==board(i, j+1))) then
        if ((board(i, j)==board(i, j+2)) .and. (board(i, j)==board(i, j+3)))then
          winner=board(i,j)
          call show_board(board, row, column)
          !print*, winner, "Wins"
          if (winner==1)then
            print*, "o Wins"
          else if (winner==2)then
            print*, "* Wins"
          else
            print*, "c wins"
          end if
          exit
	  end if
      end if
    end do

    if (winner/=0)then
      exit
    end if
  end do
 
   !Checking right-down
  do i=0, row-4
    do j=0, column-4
      if ((board(i, j)/=0) .and. (board(i, j)==board(i+1, j+1))) then
        if ((board(i, j)==board(i+2, j+2)) .and. (board(i, j)==board(i+3, j+3)))then
          winner=board(i,j)
          call show_board(board, row, column)
          if (winner==1)then
            print*, "o Wins"
          else
            print*, "* Wins"
          end if
          !print*, winner, "Wins"
          exit
	end if
      end if
    end do
  end do

  !Checking right - up
  do i=row-3, row-1
    do j=0, column-4
      if ((board(i, j)/=0) .and. (board(i, j)==board(i-1, j+1))) then
        if ((board(i, j)==board(i-2, j+2)) .and. (board(i, j)==board(i-3, j+3)))then
          winner=board(i,j)
          call show_board(board, row, column)
          if (winner==1)then
            print*, "o Wins"
          else
            print*, "* Wins"
          end if
          !print*, winner, "Wins"
          exit
	end if
      end if
    end do
  end do

end subroutine check_player

subroutine computer_player(board, row, column, col_num)
  implicit none
  integer, dimension(row, column), intent(inout):: board
  integer, intent(in) :: row, column
  integer, intent(out) :: col_num
  integer :: i, j, temp1, temp2, temp3=0

  temp3=0
  col_num=-1
  
  !Checking the vertical
  do i=0, row-3
    do j=0, column-1
        if ((board(i, j)==1) .and. (board(temp3, j)==0) .and. (board(i, j)==board(i+1, j))) then
            if ((board(i, j)==board(i+2, j)) .and. (board(i+3, j) == 0) .and. (board(i+4, j)==0)) then
              col_num=j+1
              print*,"          Check point vertical"
              print'(a, i1)',"      board(i+3, j)= ", board(i+2, j)
              print'(a, i1)',"      board(temp3, col_num)= ", board(temp3, col_num)
              return
              exit
            end if
        end if
    end do
  end do

    !Checking the horizontal 
    do i=0, row-1
      do j=0, column-3
        if ( (board(i, j)==1) .and. (board(i, j)==board(i, j+1)) ) then
          if ( (board(i, j)==board(i, j+2)) .and. ((board(i, j+3)==0) .and. (j+3/=column)) ) then
            if (j+4<column-1)then
              col_num=j+4
            else
              col_num=j-3
            end if
            print*,"       Check point horizontal"
            return
            exit
          end if
        end if
      end do
    end do

  print*, "         Check point three...    "
  print*, board(temp3, col_num-1)
  if (board(temp3, temp3)==0)then
    col_num=1
   else if (board(temp3, 2)==0)then
    col_num=3
  else if (board(temp3, 4)==0)then
    col_num=5
  
  else if (board(temp3, 3)==0)then
    col_num=4
  else if (board(temp3, 1)==0)then
    col_num=2
  else if (board(temp3, 5)==0)then
    col_num=6
  else
    col_num=7
  end if
  return
end subroutine computer_player


!This subroutine for showing the values of array and to check whether it's working well or not
subroutine the_board_values(board, row, column)
  implicit none
  integer, dimension(row, column), intent(in):: board
  integer, intent(in) :: row, column
  integer :: i, j

  do i=0, row-1
    do j=0, column-1
      print'(i1,$)', board(i, j)
    end do
    print*, " "
  end do
end subroutine the_board_values

program main
  implicit none
  integer, dimension(:,:), allocatable :: board
  integer :: row, column, winner, player, computer, p
  
  
      print'(a,$)', "Enter 1 if one player 2 if two player."
      read*, p
      if (p==1)then
        computer=1
      else if (p==2) then
        computer=0
      else
        print*, "Out of range"
      end if
  row=6
  column=7
  winner=0
  player=1
  allocate(board(row, column))
  board(:,:)=0
  board(5,0)=0
  !board(0,9)=1
  !call show_board(board, row, column)
  !print*, board(:,:)
  !print*, board(5,0)
  call player_turn(board, row, column, player, winner, computer)
  !call check_player(board)

end program main
