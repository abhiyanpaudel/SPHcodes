subroutine dummy_part(itimestep,ntotal,nbound,ndummy,hsml,mass,itype,x,y,vx,vy,rho,p)

! this subroutine is to create boundary particles

implicit none 
include  "parameters.inc.txt" 

integer,intent(in) :: ntotal,itimestep,nbound 
integer,intent(out) :: ndummy 
integer,intent(inout) :: itype(maxn)
double precision,intent(in):: hsml 
double precision,intent(inout) :: mass(maxn),x(maxn),y(maxn),vx(maxn),vy(maxn),rho(maxn),p(maxn)
double precision :: v_upper,dx,dy,xl,yl
integer :: m,mp,n,np,i,k,j


    m = 41
      n = 41
      mp = m-1
      np = n-1
      xl = 1.e-3
      yl = 1.e-3
      dx = xl/mp
      dy = yl/np
	   v_upper = 1.e-3
      
 
  k = ntotal+nbound 		
		   ndummy = 0
		
!    dummy particles on the upper side 
		
		 do i = 1,2* mp+13
			do j = 1,6 
		    ndummy = ndummy + 1 
		   x(k+ndummy) = (i-1)*dx/2
		   y(k+ndummy) = yl+7*dy/2+(j-1)*dy/2
		   vx(k+ndummy) = v_upper 
	        vy(k+ndummy) = 0.
			end do
		end do 
		
!    dummy particles on the lower side 

		do i = 1,2*mp+13
			do j = 1,6
		    ndummy = ndummy + 1 
		   x(k+ndummy) = (i-1)*dx/2
		   y(k+ndummy) = (j-1)*dy/2
		   vx(k+ndummy) = 0.
	       vy(k+ndummy) = 0.
			end do
		end do 
		
!    dummy particles on the left side 


		do i = 1, 6
			do j = 1,2*np+1 
				ndummy = ndummy+1
				x(k+ndummy) = (i-1)*dx/2
				y(k+ndummy) = 6*dy/2+(j-1)*dy/2
				vx(k+ndummy) = 0.
	            vy(k+ndummy) = 0.	
				
			end do
		end do 	
!    dummy particles on the right side 	

		do i = 1,6 
			do j = 1,2*np+1 
			ndummy = ndummy+1 
				x(k+ndummy) = xl+7*dx/2+(i-1)*dx/2 
				y(k+ndummy) = 6*dy/2+(j-1)*dy/2  
				vx(k+ndummy) = 0.
	            vy(k+ndummy) = 0.
				
			end do 
		end do 	
		
!    initialization of dummy particles 		
		
		do i = 1, ndummy
			rho (k + i) = 1000.0
			mass(k + i) = rho (k + i) * dx * dy
			p(k + i) = 0.0
			itype(k+i) = -2
			
       end do 	


	end subroutine 	