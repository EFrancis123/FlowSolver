      subroutine generate_grid

      use common_block


      implicit none

! Local variables
      integer :: i, j
      real :: a1, b1, a2, b2, z

! Calculate x and y values  x(i,j),y(i,j) of the grid nodes.

! For each value of "i" the i-grid line joins (xlow(i),ylow(i)) to
! (xhigh(i),yhigh(i)). for each value of "i" grid points (nodes) should be
! linearly interpolated between these values for j between 1 and nj.
! i.e.  x(i,1) should be xlow(i), x(i,nj) should be xhigh(i), etc.

! INSERT your code here
	do i=1,ni
		do j=1,nj
			x(i,j) = (xhigh(i) - xlow(i)) * ((j - 1.0) / (nj - 1.0)) + xlow(i)
			y(i,j) = (yhigh(i) - ylow(i)) * ((j - 1.0) / (nj - 1.0)) + ylow(i)
		end do
	end do

! Calculate the areas of the cells area(i,j)
! (N.B. there are (ni-1) x (nj-1) cells.

! The area of a quadrilateral (regular or irregular) can be shown to be
! half of the cross product of the vectors forming the diagonals.
! see Hirsch volume 1, section 6.2.1. (or lecture).
! Make sure that the area comes out positive!

! INSERT your code here
	
	do i=1,(ni-1)
		do j=1,(nj-1)
			a1 = x(i,j+1) - x(i+1,j)
			b1 = y(i,j+1) - y(i+1,j)
			a2 = x(i+1,j+1) - x(i,j)
			b2 = y(i+1,j+1) - y(i,j)
			area(i,j) = 0.5 * (a2 * b1 - a1 * b2)
		end do
	end do

! Calculate the x and y components of the length vector of the i-faces
! (i.e. those corresponding to i = constant).
! The length vector of a face is a vector normal to the face wi
! magnitude equal to the length of the face.
! It is positive in the direction of an inward normal to the cell i,j .
! Call these lengths dlix(i,j) and dliy(i,j)

! INSERT your code here
	dmin = 10000.0
	do i=1,ni
		do j=1,(nj-1)
			dlix(i,j) = y(i,j+1) - y(i,j)
			dliy(i,j) = -(x(i,j+1) - x(i,j))
			dli(i,j) = sqrt(dlix(i,j)**2 + dliy(i,j)**2)
			if (dli(i,j) < dmin) then
				dmin = dli(i,j)
			end if
			
		end do
	end do



! Now calculcate the x and y components of the length vector of the j-faces. (i.e. those corresponding to j = constant)
! Call these lengths dljx(i,j) and dljy(i,j)

! INSERT your code here
!	do i=1,(ni-1)
!		do j=1,(nj-1)
!			dljx(i,j) = y(i,j) - y(i+1,j)
!			dljy(i,j) = x(i+1,j) - x(i,j)
!		end do
!	end do

! Now find "dmin" the minimum length scale of any element. This is
! defined as the length of the shortest side of the element.
! Call this minimum "dmin". it is used to set the time step from the cfl no.

! Insert your code here (or in the do loops above).

	do i=1,(ni-1)
		do j=1,nj
			dljx(i,j) = -(y(i+1,j) - y(i,j))
			dljy(i,j) = x(i+1,j) - x(i,j)
			dlj(i,j) = sqrt(dljx(i,j)**2 + dljy(i,j)**2)
			if (dlj(i,j) < dmin) then
				dmin = dlj(i,j)
			end if
		end do
	end do


      write(6,*)  ' overall minimum element size = ', dmin

      end
