    subroutine summation_density(ntotal,hsml,mass,niac,pair_i,pair_j,w,itype,rho)
      implicit none 
      include "parameters.inc.txt"
      
      integer,intent(in) ::  ntotal, niac, pair_i(max_interaction),pair_j(max_interaction),itype(maxn) 
      double precision,intent(in) :: hsml,mass(maxn), w(max_interaction)
	  double precision,intent(out) :: rho(maxn)
      integer :: i, j, k     
      double precision :: selfdens,r,wi(maxn),hvx,hvy 
!     wi(maxn)---integration of the kernel itself
      hvx = 0.e0
	  hvy = 0.e0

!     self density of each particle: wii (kernel for distance 0)    and take contribution of particle itself:
    r = 0.
    
	
     
      
!     firstly calculate the integration of the kernel over the space
     
      do i=1,ntotal
        call kernel(r,hvx,hvy,hsml,selfdens,hvx,hvy)    
        wi(i)=selfdens*mass(i)/rho(i)
      end do
      
      do k=1,niac
        i = pair_i(k)
        j = pair_j(k)
        wi(i) = wi(i) + mass(j)/rho(j)*w(k)
        wi(j) = wi(j) + mass(i)/rho(i)*w(k)
		
      end do

!    secondly calculate the rho integration over the space

      do i=1,ntotal
        call kernel(r,hvx,hvy,hsml,selfdens,hvx,hvy) 
        rho(i) = selfdens*mass(i)
      end do

!     calculating sph sum for rho:
      do k=1,niac
        i = pair_i(k)
        j = pair_j(k)
        rho(i) = rho(i) + mass(j)*w(k)
        rho(j) = rho(j) + mass(i)*w(k)
      end do

!    thirdly, calculate the normalized rho, rho=sum(rho)/sum(w)
     
       do i=1, ntotal
          rho(i)=rho(i)/wi(i)
      
        end do
   end subroutine  
      
 
!!------------------------------------------------------------------------------------!! 
 
 subroutine continuity_density(ntotal,mass,niac,pair_i,pair_j,dwdx,dwdy,vx,vy,itype,x,y,rho, drhodt)
 
! subroutine to calculate the density with sph continuiity approach.
 
implicit none 
include "parameters.inc.txt"  

integer,intent(in)::ntotal,niac,pair_i(max_interaction),pair_j(max_interaction),itype(maxn)
double precision,intent(in)::mass(maxn),dwdx(max_interaction),dwdy(max_interaction),vx(maxn),vy(maxn),&
                             x(maxn),y(maxn),rho(maxn)
double precision,intent(out)::drhodt(maxn)							 
integer :: i,j,k ,d   
double precision:: vcc, dvx,dvy 

do i = 1, ntotal
 drhodt(i) = 0.
end do 
     
do k=1,niac      
        i = pair_i(k)
        j = pair_j(k)
       dvx = vx(i) - vx(j)
       dvy = vy(i)-vy(j)
   
	   vcc = dvx*dwdx(k) + dvy*dwdx(k)
	
       drhodt(i) = drhodt(i) + mass(j)*vcc
       drhodt(j) = drhodt(j) + mass(i)*vcc       
end do   
	 
end 	 
	 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 