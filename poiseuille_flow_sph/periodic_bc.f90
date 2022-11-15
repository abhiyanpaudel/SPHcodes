subroutine periodic_bc(x,y,ntotal,xl)

!  this subroutine adjusts the particle coordinates for periodic boundary conditions
!   according to the minimum image criterion.

implicit none 
include  "parameters.inc.txt" 

double precision,intent(inout):: x(maxn),y(maxn)
integer,intent(in):: ntotal
double precision,intent(in):: xl
integer:: i 
do i = 1,ntotal
    if (x(i) < 0.d0) then
    	x(i) = x(i) + xl
		y(i) = y(i)+0.0
    else if (x(i) > xl) then
        x(i) = x(i) - xl
		y(i) = y(i)+0.0
    end if
end do        

end 