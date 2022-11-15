subroutine single_step(itimestep, dt, ntotal, hsml, mass, x,y, vx,vy,&  
                       rho, p,dx,dy,dvx,dvy,itype, avx,avy,xl,drhodt) 
					   
    implicit none
     include  "parameters.inc.txt" 					   
		
!     dx       :  dx = vx = dx/dt
!     dy       :  dy = vy = dy/dt                                  
!     dvx      :  dvx = dvx/dt, force per unit mass along x-direction 
!     dvy      :  dvy = dvy/dt ,force per unit mass along y-direction              		
double precision,intent(inout):: rho(maxn)
integer,intent(in)::itimestep,ntotal,itype(maxn)
double precision,intent(in)::hsml,dt,mass(maxn),x(maxn),y(maxn),vx(maxn),vy(maxn)
double precision,intent(out)::p(maxn),dx(maxn),dy(maxn),dvx(maxn),dvy(maxn),avx(maxn),avy(maxn),xl,drhodt(maxn) 
double precision:: w(max_interaction), dwdx(max_interaction),dwdy(max_interaction),&  
                   indvxdt(maxn),indvydt(maxn),c(maxn),fx	
integer :: nbound,niac,pair_i(max_interaction),pair_j(max_interaction),i,nghost 				   
fx = 1.e-04				   
do  i=1,ntotal
          indvxdt(i) = 0.
		  indvydt(i) = 0.
        
        
end do 

nbound = 0.
nghost=0.

! positions of boundary particles 
call boundary_part(itimestep,ntotal,nbound,hsml,mass,itype,x,y,vx,vy,rho,p,xl)


! positions and properties of ghost particles
call ghost_bc(itimestep,ntotal,itype,nbound,mass,x,y,hsml,vx,vy,rho,p,nghost,xl)

! interaction parameters, calculating neighboring particles
call nnps(itimestep,xl,itype,ntotal+nbound+nghost,hsml,x,y,niac,pair_i,pair_j,w,dwdx,dwdy)

! density approximation
call continuity_density(ntotal+nbound+nghost,mass,niac,pair_i,pair_j,dwdx,dwdy,vx,vy,itype,x,y,rho, drhodt)

! particle pressure 
call pressure(itimestep,ntotal+nbound+nghost,rho, p, c)

! internal forces:
call navier_stoke(itimestep,dt,ntotal+nbound+nghost,hsml,mass,vx,vy,niac,rho,&
							pair_i,pair_j,dwdx,dwdy,itype,x,y,c,p,indvxdt,indvydt) 
							


! calculating average velocity of each partile for avoiding penetration
call avg_vel(ntotal,mass,niac,pair_i,pair_j,w, vx,vy, rho, avx,avy )

do  i=1,ntotal
    dvx(i) = indvxdt(i)+fx 
	dvy(i) = indvydt(i)  
    
end do 

end 



