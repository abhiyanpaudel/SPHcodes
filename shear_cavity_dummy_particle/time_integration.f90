subroutine time_integration(x,y, vx,vy,vtot, mass, rho, p,c,itype, & 
        hsml, ntotal, maxtimestep, dt )
implicit none
include  "parameters.inc.txt" 
double precision,intent(inout) :: vx(maxn),vy(maxn),x(maxn),y(maxn), &
                    rho(maxn),p(maxn)
integer,intent(in) :: itype(maxn),ntotal ,maxtimestep 
double precision,intent(in) ::  mass(maxn),hsml ,dt 
double precision,intent(out) :: c(maxn),vtot(maxn) 
                             						 
integer :: i, itimestep,save_step,j
double precision ::dx(maxn),dy(maxn), dvx(maxn),dvy(maxn),avx(maxn),avy(maxn)         
save_step = 100




    do itimestep = 1,maxtimestep
		call single_step(itimestep, dt, ntotal, hsml, mass, x,y, vx,vy,&  
                       rho, p,dx,dy,dvx,dvy,itype, avx,avy) 
					   
		do i=1,ntotal  
            vx(i) = vx(i) + dt * dvx(i) + avx(i)
			vy(i) = vy(i) + dt * dvy(i) + avy(i)
			vtot(i) = sqrt(vx(i)**2+vy(i)**2)
            x(i) = x(i) + dt * vx(i) 	
			y(i) = y(i) + dt * vy(i)
		end do 		 
		if (mod(itimestep,save_step)==0) then
		
		     
			call output(itimestep,x,y,vx,vy,vtot,mass, rho, p,c,itype, hsml, ntotal)   
		
		end if   
		
		 
		
		
		
		
		
			
    
	end do
	
	
   
end     
