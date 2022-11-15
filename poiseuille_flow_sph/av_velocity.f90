	subroutine avg_vel(ntotal,mass,niac,pair_i,pair_j,w, vx,vy, rho, avx,avy )

!----------------------------------------------------------------------
!     subroutine to calculate the average velocity to correct velocity
!     for preventing penetration 

		implicit none 
		include  "parameters.inc.txt" 
	
	  integer,intent(in) :: ntotal,niac,pair_i(max_interaction),pair_j(max_interaction)
	  double precision,intent(in):: mass(maxn),w(max_interaction),vx(maxn),vy(maxn),rho(maxn)
	  double precision,intent(out):: avx(maxn),avy(maxn)
      double precision :: dvx,dvy,e
	  integer :: i,j,k
	
		e = 0.3
      
	  do i = 1, ntotal
        
          avx(i) = 0.
		  avy(i) = 0.
      
      end do
     
      do k=1,niac       
        i = pair_i(k)
        j = pair_j(k)       
        
          dvx = vx(i) - vx(j)
          dvy = vy(i) - vy(j)		  
          avx(i) = avx(i) - 2*mass(j)*dvx/(rho(i)+rho(j))*w(k)
          avx(j) = avx(j) + 2*mass(i)*dvx/(rho(i)+rho(j))*w(k)   
		  avy(i) = avy(i) - 2*mass(j)*dvy/(rho(i)+rho(j))*w(k)
          avy(j) = avy(j) + 2*mass(i)*dvy/(rho(i)+rho(j))*w(k)  
                      
      end do  
        
		 do i = 1, ntotal
          avx(i) = e * avx(i)
		   avy(i) = e * avy(i) 
         end do  
                  

    end subroutine 