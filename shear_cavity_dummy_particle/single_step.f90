subroutine single_step(itimestep, dt, ntotal, hsml, mass, x,y, vx,vy,&  
                       rho, p,dx,dy,dvx,dvy,itype, avx,avy) 
					   
    implicit none
     include  "parameters.inc.txt" 					   
		
!     dx       :  dx = vx = dx/dt
!     dy       :  dy = vy = dy/dt                                  
!     dvx      :  dvx = dvx/dt, force per unit mass along x-direction 
!     dvy      :  dvy = dvy/dt ,force per unit mass along y-direction              		
double precision,intent(inout):: rho(maxn)
integer,intent(in)::itimestep,ntotal,itype(maxn)
double precision,intent(in)::hsml,dt,mass(maxn),x(maxn),y(maxn),vx(maxn),vy(maxn)
double precision,intent(out)::p(maxn),dx(maxn),dy(maxn),dvx(maxn),dvy(maxn),avx(maxn),avy(maxn)
double precision:: w(max_interaction), dwdx(max_interaction),dwdy(max_interaction),&  
                   indvxdt(maxn),indvydt(maxn),exdvxdt(maxn),exdvydt(maxn),c(maxn)
integer :: nbound,niac,pair_i(max_interaction),pair_j(max_interaction),i,ndummy 				   
				   
do  i=1,ntotal
          indvxdt(i) = 0.
		  indvydt(i) = 0.
          
        
end do 

nbound = 0.
ndummy = 0.

! positions of boundary particles 
call boundary_part(itimestep,ntotal,nbound,hsml,mass,itype,x,y,vx,vy,rho,p,itype)

! positions of dummy particles 
call dummy_part(itimestep,ntotal,nbound,ndummy,hsml,mass,itype,x,y,vx,vy,rho,p)

! interaction parameters, calculating neighboring particles
call nnps(itimestep,ntotal+nbound+ndummy,hsml,x,y,niac,pair_i,pair_j,w,dwdx,dwdy)

! density approximation
call summation_density(ntotal+nbound+ndummy,hsml,mass,niac,pair_i,pair_j,w,itype,rho)

! particle pressure 
call pressure(itimestep,ntotal+nbound+ndummy,rho, p, c)

!mapping to dummy particles
call dummy(ntotal+nbound+ndummy,mass,x,y,niac,pair_i,pair_j,itype,hsml,rho,p)

! internal forces:
call navier_stoke(itimestep,dt,ntotal+nbound+ndummy,hsml,mass,vx,vy,niac,rho,&
							pair_i,pair_j,dwdx,dwdy,itype,x,y,c,p,indvxdt,indvydt) 
							



! calculating average velocity of each partile for avoiding penetration
call avg_vel(ntotal,mass,niac,pair_i,pair_j,w, vx,vy, rho, avx,avy )

do  i=1,ntotal
    dvx(i) = indvxdt(i) 
	dvy(i) = indvydt(i)  
    
end do 

end 



