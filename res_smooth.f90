      subroutine res_smooth(del, sf, maxi, maxj)


      use common_block


      implicit none

      real, dimension(i_max,j_max) ::  del

      real  ::  sf, sfm1, avg, avg1, avgmaxj
      integer  ::  i, j, ip1, im1, maxi, maxj
      real, dimension(i_max,j_max) ::  store

      sfm1 = 1 - sf

      do i=1,maxi
        ip1 = i+1
         if( i==maxi ) ip1 = maxi
        im1 = i-1
         if( i==1  ) im1 = 1

         do j=2,maxj-1
           avg = 0.25*(del(ip1,j)+del(im1,j)+del(i,j-1)+del(i,j+1))

! INSERT your code here
     	   store(i,j) = sfm1*del(i,j) + sf*avg

         enddo

! On the surfaces j=1 and j=nj take the average as shown below.

         avg1  = (del(im1,1)+del(ip1,1)+2.*del(i,2)-del(i,3))/3.0
         avgmaxj = (del(im1,maxj) + del(ip1,maxj) + 2.*del(i,maxj-1)    &
              -  del(i,maxj-2))/3.0

! INSERT your code here to smooth the surface values
	 store(i,1) = sfm1*del(i,1) + sf*avg1
	 store(i,maxj) = sfm1*del(i,maxj) + sf*avgmaxj

      enddo

! Reset the smoothed value to "prop" before returning to the main program.

      do i=1,maxi
        do j=1,maxj
          del(i,j) = store(i,j)
        end do
      end do

      end
