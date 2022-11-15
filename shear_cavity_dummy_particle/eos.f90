subroutine pressure(itimestep,ntotal,rho, p, c)

!   artificial equation of state for the artificial compressibility 

implicit none
include  "parameters.inc.txt" 
integer,intent(in) :: ntotal,itimestep 
double precision,intent(in) :: rho(maxn)
double precision,intent(out) ::p(maxn),c(maxn) 
integer :: i
do i = 1,ntotal
c(i) = 0.01
p(i) = c(i)**2*rho(i)
end do 
end

