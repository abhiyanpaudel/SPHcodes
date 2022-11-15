
    subroutine nnps(itimestep,ntotal,hsml,x,y,niac,pair_i,pair_j,w,dwdx,dwdy)

! this subroutine is to find the distance between the interaction points.

!niac   : number of interaction pairs   
!pair_i : list of first partner of interaction pair            
!pair_j : list of second partner of interaction pair 
!countiac  : number of neighboring particles       
implicit none 
include  "parameters.inc.txt" 
integer,intent (in) :: ntotal,itimestep(maxn)
double precision,intent (in):: hsml,x(maxn),y(maxn)
integer,intent (out) :: niac,pair_i(max_interaction), pair_j(max_interaction)
double precision,intent (out) :: w(max_interaction),dwdx(max_interaction),dwdy(max_interaction) 
integer :: scale_k ,i,j
double precision :: dxiac,dyiac, driac, r,mhsml,tdwdx ,tdwdy,w1 


    
 
    if (skf==1) then 
        scale_k =3
     else if (skf==2) then 
        scale_k = 3 
     else if (skf==3) then 
        scale_k = 2 
     end if 

   
      
      niac = 0
     
	
    
    do i=1,ntotal-1     
        do j = i+1, ntotal
          
          dxiac = x(i) - x(j)
          dyiac = y(i) - y(j)
          driac    = dxiac*dxiac + dyiac*dyiac
          mhsml = hsml
          if (sqrt(driac).lt.scale_k*mhsml) then
           if (niac.lt.max_interaction) then 
              
            
!     neighboring pair list, and totalinteraction number and
!     the interaction number for each particle 

              niac = niac + 1
              pair_i(niac) = i
              pair_j(niac) = j
              r = sqrt(driac)
		    !     kernel and derivations of kernel
			  call kernel(r,dxiac,dyiac,hsml,w1,tdwdx,tdwdy) 
				dwdx(niac) =    tdwdx
				dwdy(niac) =    tdwdy
		        w(niac)    =    w1 
				
               
            end if
          end if             
         
        end do
    end do    

    end subroutine

    