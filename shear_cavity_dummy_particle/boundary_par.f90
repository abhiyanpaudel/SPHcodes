subroutine boundary_part(itimestep,ntotal,nbound,hsml,mass,itype,x,y,vx,vy,rho,p)

! this subroutine is to create boundary particles

implicit none 
include  "parameters.inc.txt" 

integer,intent(in) :: ntotal,itimestep 
integer,intent(out) :: nbound
integer,intent(inout) :: itype(maxn)
double precision,intent(in):: hsml 
double precision,intent(inout) :: mass(maxn),x(maxn),y(maxn),vx(maxn),vy(maxn),rho(maxn),p(maxn)
double precision :: v_upper,dx,dy,xl,yl
integer :: m,mp,n,np,i

nbound = 0.
    m = 41
      n = 41
      mp = m-1
      np = n-1
      xl = 1.e-3
      yl = 1.e-3
      dx = xl/mp
      dy = yl/np
      v_upper = 1.e-3
 
 !    boundary particle on the upper side

          
		do i = 1, 2*mp+1
			nbound = nbound + 1
			x(ntotal + nbound) = 6*dx/2+(i-1)*dx/2
			y( ntotal + nbound) = yl+6*dy/2 
			vx(ntotal + nbound) = v_upper 
	        vy(ntotal + nbound) = 0.
         
        end do 

!     boundary particle on the lower side

        do i = 1, 2*mp+1
			nbound = nbound + 1
			x( ntotal + nbound) = 6*dx/2+(i-1)*dx/2 
			y( ntotal + nbound) = 6*dy/2 
			vx(ntotal + nbound) = 0.
	        vy(ntotal + nbound) = 0.
          
        end do 

!     boundary particle on the left side

        do i = 1, 2*mp-1
			nbound = nbound + 1
			x( ntotal + nbound) = 6*dx/2 
			y( ntotal + nbound) = 6*dy/2+i*dy/2
			vx(ntotal + nbound) = 0.
	        vy(ntotal + nbound) = 0.
          
        end do 

!     boundary virtual particle on the right side

        do i = 1, 2*mp-1
			nbound = nbound + 1
			x( ntotal + nbound) = xl+6*dx/2  
			y(ntotal + nbound) = 6*dy/2+i*dy/2
			vx(ntotal + nbound) = 0.
	        vy(ntotal + nbound) = 0.
		end do   

!    initialization of boundary particle 		
		
		do i = 1, nbound
			rho (ntotal + i) = 1000.0
			mass(ntotal + i) = rho (ntotal + i) * dx * dy
			p(ntotal + i) = 0.0
			itype(ntotal+i) = -1
			
       end do 
	!hsml = dx 
	
	    open(1,file='xv_vp.dat')
        open(2,file='state_vp.dat')
               
        !write(1,*) nbound 
        do i = ntotal + 1, ntotal + nbound         
          write(1,*) i, x(i),y(i),vx(i),vy(i)              
          write(2,*) i, mass(i), rho(i), p(i)
                                       
        end do      

        close(1)
        close(2) 
     
	 end 