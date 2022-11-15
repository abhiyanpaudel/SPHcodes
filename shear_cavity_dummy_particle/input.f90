subroutine input(x, y, vx, vy,vtot,itype, mass, rho,p, hsml, ntotal)

! subroutine to generate initial particle information

	implicit none
    include  "parameters.inc.txt" 
    double precision,intent(out) :: mass(maxn),rho(maxn),hsml,x(maxn),y(maxn),vx(maxn),vy(maxn),p(maxn),vtot(maxn)
    integer,intent(out) :: ntotal,itype(maxn)
	integer :: i 
        
	open(1,file='ini_xv.dat')
    open(2,file='ini_state.dat')
    open(3,file='ini_other.dat')
	call shear_cavity(x, y, vx, vy,vtot,itype, mass, rho,p, hsml, ntotal)
	  do i = 1, ntotal 
          write(1,*) x(i),y(i),vx(i),vy(i),vtot(i) 
          write(2,*) mass(i), rho(i), p(i)       
          
	  end do 

        write(*,*)'  **************************************************'
        write(*,*)'      initial particle configuration generated   '       
        write(*,*)'      total number of particles   ', ntotal    	
        write(*,*)'  **************************************************' 

    close(1)
	close(2)

	end subroutine 


!-----------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------	
	
subroutine shear_cavity(x, y, vx, vy,vtot,itype, mass, rho,p, hsml, ntotal)
    
    ! this subroutine is to create the points and assign the masses on the points
    
    implicit none
    include  "parameters.inc.txt" 
    double precision,intent(out) :: mass(maxn),rho(maxn),hsml,x(maxn),y(maxn),vx(maxn),vy(maxn),p(maxn),vtot(maxn)
    integer,intent(out) :: ntotal,itype(maxn)
    integer :: i,j,m,n,mp,np,k
   
    double precision :: xl,yl,dx,dy
   
    m = 41
      n = 41
      mp = m-1
      np = n-1
      ntotal = mp * np
      xl = 1.e-3
      yl = 1.e-3
      dx = xl/mp
      dy = yl/np

      
       k = 0
       do i = 1,mp
            do j = 1,np 
                k = k+1 
                x(k) = (i-1)*dx + 7*dx/2
                y(k) = (j-1)*dy + 7*dy/2 
				
            end do
        end do
    

                
         do i = 1, ntotal     
            rho (i) = 1000.0   
            mass(i) = dx*dy*rho(i) 
            vx(i) = 0.0
            vy(i) = 0.0  
			vtot(i) = 0.0	
            p(i) = 0.0   
			itype(i) = 1
           
        end do
       hsml = dx
    

        
    end subroutine  
      
      
