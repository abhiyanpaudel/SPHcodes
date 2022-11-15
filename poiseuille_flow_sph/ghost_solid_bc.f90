subroutine ghost_bc(itimestep,ntotal,itype,nbound,mass,x,y,hsml,vx,vy,rho,p,nghost,xl)

!--------------------------------------------------------------------------     
!!!!!..subroutine to create ghost particles..!!!!!!!
 implicit none
 include  "parameters.inc.txt"    
integer,intent(in)::ntotal,nbound,itimestep
integer,intent(out):: nghost
integer,intent(inout)::itype(maxn)
double precision,intent(in)::hsml
double precision,intent(inout) :: mass(maxn),x(maxn),y(maxn),vx(maxn),vy(maxn),rho(maxn),p(maxn)
double precision,intent(out) :: xl 
integer :: i,j,k,pair_i(max_interaction),pair_j(max_interaction),scale_k,niac,m,n,t,q,r,interaction(max_interaction),s,a

 
double precision::dxiac,driac,dyiac,dx,dy,mhsml

 if (skf==1) then 
        scale_k =2
     else if (skf==2) then 
        scale_k = 3 
     else if (skf==3) then 
        scale_k = 2 
	 else if (skf==4) then 
        scale_k = 3
     else
     print*,'<<error>>'	 
     end if 

dx = 0.000025
dy = 0.000025

nghost = 0.

niac = 0.
q=ntotal+nbound
	do i=ntotal+1,q     
        do j = 1, ntotal
			
				dxiac = x(i) - x(j)
				dyiac = y(i) - y(j)
				driac = dxiac*dxiac + dyiac*dyiac		  
				mhsml = hsml
				
				if (sqrt(driac)<scale_k*mhsml) then
				if (niac.lt.max_interaction) then 
				    niac = niac+1
					if(niac==1) then
					 niac =niac+1
					end if  
					pair_i(niac) = i 
                    pair_j(niac) = j
					
					
				
				  			
							  
				end if
				end if 
		
		end do	
	end do




pair_i(1)=0.
pair_j(1)=0.			
			
	do k =2,niac
	    i = pair_i(k)
		j = pair_j(k)
	
		if (i==ntotal+1 .or. i==ntotal+33 .or. i==ntotal+34 .or. i==q) then 
		 if (x(i)/=x(j)) then
		    nghost=nghost+1
			x(q+nghost)=2*x(i)-x(j)
			y(q+nghost)=2*y(i)-y(j)	
			vx(q+nghost)=-vx(j)
			vy(q+nghost)=-vy(j)
			rho(q+nghost)=rho(j)
			mass(q+nghost) = rho (q+nghost) * dx * dy
			p(q+nghost)= p(j)
			itype(q+nghost)=-2
		 end if 
		end if 
	end do 	




	do k = 2,niac
		i = pair_i(k)
        j = pair_j(k)	
	       do  t = 2,k
          	r=pair_j(t-1)  
			if (j==r) exit
		    end do	
		  if(j==r) cycle 
			n=j
			nghost = nghost+1
				
				x(q+nghost)=x(n)
				y(q+nghost)=2*y(i)-y(n)	
				vx(q+nghost)=-vx(n)
				vy(q+nghost)=-vy(n)
				rho(q+nghost)=rho(n)
				mass(q+nghost) = rho (q+nghost) * dx * dy
				p(q+nghost)= p(n)
			    itype(q+nghost)=-2
			
		
				
				
				
	end do 	

  
				
!print*,nghost 
open(5,file='ghost_par.dat')	
do i = 1,nghost

write(5,*) x(q+i),y(q+i)
end do 
close(5)
 	
end subroutine 
	
	