      subroutine dummy(ntotal,mass,x,y,niac,pair_i,pair_j,itype,hsml,rho,p)

!--------------------------------------------------------------------------     
       

		implicit none 
       include  "parameters.inc.txt" 
      
      integer,intent(in):: ntotal,pair_i(max_interaction),pair_j(max_interaction),niac,itype(maxn)
	  double precision,intent(in):: mass(maxn),x(maxn),hsml,y(maxn)
	  double precision,intent(inout):: rho(maxn),p(maxn)
      integer :: i, j, k
      double precision :: dx,dy, rr, f, rr0, dd, p1, p2     

        
!    

      
    do  k=1,niac
        i = pair_i(k)
        j = pair_j(k)  
        if(itype(i)==-1 .and. itype(j)==-2) then 
		    if (x(i)==x(j) .or. y(i)==y(j)) then 
				rho(j) = rho(i)
				p(j) = p(i)
			
			else
                rho(j) = rho(i)
                  p(j) = p(i)
            end if 				  
		end if 
	end do	
	
	end subroutine          