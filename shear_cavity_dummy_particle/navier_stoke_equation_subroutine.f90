    subroutine navier_stoke(itimestep,dt,ntotal,hsml,mass,vx,vy,niac,rho,&
							pair_i,pair_j,dwdx,dwdy,itype,x,y,c,p,indvxdt,indvydt) 

	
!--------------------------------------------------------------------------------------------------------
! subroutine to find the right handside of navier-stokes equation which includes
! the pressure gradient and the gradient of the viscous stress tensor
!--------------------------------------------------------------------------------------------------------   
!                                                                       
!     ntotal    : number of particles                                  
!     hsml      : smoothing length                                     
!     mass      : particle masses                                      
!     vx        : velocity with respect to x
!     vy        : velocity with respect to y                         
!     niac      : number of interaction pairs                          
!     rho       : density                                              
!     mu        : dynamic viscosity                                    
!     pair_i    : list of first partner of interaction pair            
!     pair_j    : list of second partner of interaction pair           
!     dwdx      : derivative of kernel with respect to x
!     dwdy      : derivative of kernel with respect to y  
!     itype     : particle type 
!     x         : particles  in x - direction 
!     y         : particles  in y - direction  
!     c         : particle speed of sound                                                                                                                                        
!     p         : particle pressure    
!     dt        : time step
!     itimestep : current timestep number                              
!     indvxdt     : acceleration with respect to x
!     indvydt     : acceleration with respect to y
!---------------------------------------------------------------------------------------------------------                     
	
	
	
	implicit none
    include  "parameters.inc.txt" 
    double precision,intent(in) :: dwdx(max_interaction), &
	          dwdy(max_interaction),rho(maxn),mass(maxn), &
			  hsml,dt,vx(maxn),vy(maxn), & 
			  x(maxn),y(maxn)
	                               
    integer,intent(in) :: itimestep,ntotal,niac,pair_i(max_interaction),pair_j(max_interaction),itype(maxn)
    double precision,intent(out) :: indvxdt(maxn),indvydt(maxn),c(maxn),p(maxn) 
    integer :: i,k,j,d   
    double precision ::hx,hy,txx(maxn),tyy(maxn), &
					   txy(maxn),mu(maxn),hxx,hyy,hxy,dvx,dvy
					   
    
!   initialization of shear tensor, velocity divergence,acceleration 

	do i = 1,ntotal
		txx(i) = 0.0
		tyy(i) = 0.0
		txy(i) = 0.0
		indvxdt(i)  = 0.0
		indvydt(i)  = 0.0
		mu(i)  = 1.0e-3
	end do 	
		
		
!   calculate sph sum of strain rate tensor tab = va,b + vb,a -2/3 delta_ab vc,c

    do k = 1,niac
		i = pair_i(k)
		j = pair_j(k)
		dvx = vx(j)-vx(i)
		dvy = vy(j)-vy(i) 
        hxx = 2.e0*dvx*dwdx(k) -  dvy*dwdy(k) 
        hxy = dvx*dwdy(k) + dvy*dwdx(k)
        hyy = 2.e0*dvy*dwdy(k) - dvx*dwdx(k)
		
		
		hxx = 2.0/3.0 * hxx
		hxy = 2.0/3.0 * hxy
		hyy = 2.0/3.0 * hyy
		
          
        txx(i) = txx(i) + mass(j)*hxx/rho(j)
        txx(j) = txx(j) + mass(i)*hxx/rho(i)   
        txy(i) = txy(i) + mass(j)*hxy/rho(j)
        txy(j) = txy(j) + mass(i)*hxy/rho(i)            
        tyy(i) = tyy(i) + mass(j)*hyy/rho(j)
        tyy(j) = tyy(j) + mass(i)*hyy/rho(i)  
			
	
! 	pressure part 

	      hx = -(p(i)/rho(i)**2 + p(j)/rho(j)**2)*dwdx(k) 
		  hy = -(p(i)/rho(i)**2 + p(j)/rho(j)**2)*dwdy(k) 
		  


            
!   x-coordinate of acceleration			
			
              
			   hx = hx + (mu(i)*txx(i)/rho(i)**2 + mu(j)*txx(j)/rho(j)**2)*dwdx(k)+ &
			        (mu(i)*txy(i)/rho(i)**2 + mu(j)*txy(j)/rho(j)**2)*dwdy(k)



!     y-coordinate of acceleration	

				hy = hy + (mu(i)*txy(i)/rho(i)**2 + mu(j)*txy(j)/rho(j)**2)*dwdx(k)   &
                    + (mu(i)*tyy(i)/rho(i)**2 + mu(j)*tyy(j)/rho(j)**2)*dwdy(k)
		
	
			    
!       acceleration 		   
		indvxdt(i) = indvxdt(i)+mass(j)*hx
        indvxdt(j) = indvxdt(j)-mass(i)*hx
        indvydt(i) = indvydt(i)+mass(j)*hy
        indvydt(j) = indvydt(j)-mass(i)*hy
			
			

    end do 
	
	
	
	end subroutine 






























	
	
     
