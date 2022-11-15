program sph

implicit none
include  "parameters.inc.txt"
integer :: ntotal, itype(maxn), maxtimestep,itimestep    
double precision :: x(maxn),y(maxn), vx(maxn),vy(maxn), mass(maxn),rho(maxn),&
          p(maxn),dt,hsml,c(maxn),vtot(maxn)

dt = 5.e-5

call input(x, y, vx, vy,vtot,itype, mass, rho,p, hsml, ntotal)
write(*,*)'***************************************************' 
write(*,*)'    enter the maximum time step     '
write(*,*)'***************************************************'
read(*,*) maxtimestep  

call time_integration(x,y, vx,vy,vtot, mass, rho, p,c,itype, & 
        hsml, ntotal, maxtimestep, dt )

call output(itimestep,x,y,vx,vy,vtot,mass, rho, p,c,itype, hsml, ntotal) 

end 