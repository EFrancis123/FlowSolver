      subroutine check_grid


      use common_block


      implicit none

! Local stuff
      integer :: i, j, test
      real :: xSum, ySum,small

! Check your grid and areas for both the "bump" and the "bend"
! test data.

! First check that all areas are positive (by program or writing out)

! INSERT your code here
	do i=1,(ni-1)
		do j=1,(nj-1)
			if (area(i,j) < 0) then
				write(6,*) ' AREA LESS THAN 0 '
			end if
		end do
	end do
	
	

! Next check that the sum of the length vectors of the 4 faces
! of every element is very nearly zero in each coordinate direction.
! It is absolutely essential that this is correct !
! If not go back and check your subroutine "generate_grid".

! Careful with a test of the form
!          if( a == 0.0 ) then .....
! This will probably never be true.  Computers work to a finite number of
! Significant figures and "a" will probably be +0.0000001 or -0.0000001.
! Test for something like
!          if( abs(a) <= small_number ) then ...

! Insert your code here
	small = 0.000001*dmin
	xsum = 0.0
	ysum = 0.0
	do i=1,(ni-1)
	   do j=1,(nj-1)
		ysum = - dlix(i,j) + dljx(i,j+1) + dlix(i+1,j) - dljx(i,j)
		xsum = - dliy(i,j) + dljy(i,j+1) + dliy(i+1,j) - dljy(i,j)			
			if (abs(xsum) .GT. small) then
				write(6,*) dliy(i,j), dljy(i,j+1), dliy(i+1,j), dljy(i,j)
				write(6,*) xsum
				write(6,*) 'X-Grid Non Zero'
				!stop
			end if
			if (abs(ysum) .GT. small) then
				write(6,*) i, j
				write(6,*) dlix(i,j), dljx(i,j+1), dlix(i+1,j), dljx(i,j)
				write(6,*) ysum
				write(6,*) 'Y-Grid Non Zero'
				!stop
			endif
		end do
	end do	

	
! Any other tests that you can think of. For example you could plot
! contours of  "area(i,j)" by using  --  call output_hg(area) .

      end
