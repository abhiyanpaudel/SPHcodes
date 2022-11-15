subroutine kernel(r,dxx,dyy,hsml,w,dwdx,dwdy)   

!----------------------------------------------------------------------
!    subroutine to calculate the smoothing kernel wij and its 
!   derivatives dwdxij.
!     if skf = 1 refers to bell-shaped function
!             = 2 refers to gaussian kernel 
!             = 3 refers to cubic spline function

!     r    : distance between particles i and j                     
!     dx   : x-, y- and z-distance between i and j                   
!     hsml : smoothing length                                       
!     w    : kernel for all interaction pairs                      
!     dwdx : derivative of kernel with respect to x, y and z       
!     ri   : distance at the i particle itself
    
      
      implicit none
     include  "parameters.inc.txt" 
      
      double precision,intent(in) :: r, hsml
      double precision,intent(in) :: dxx,dyy
      
      double precision,intent(out):: w,dwdx,dwdy 
     
      double precision :: q,factor 
      
  
      
    
      q = r/hsml 
      w = 0.e0 
	  dwdx = 0.e0
	  dwdy = 0.e0 
    if (skf==1) then
          factor = 5.e0/(pi*hsml*hsml)
        
        if (q<=1 .and. q>=0) then
            w = factor*(1.+3.*q)*(1.-q)*(1.-q)*(1.-q)
            dwdx =   (factor*6.*(1.-q)**2*(1.+q)*dxx)/(q*hsml*hsml)
            dwdy =   (factor*6.*(1.-q)**2*(1.+q)*dyy)/(q*hsml*hsml)
                        
        else 
            w = 0.e0 
            dwdx =   0.e0
            dwdy = 0.e0              
        end if
        
    
    else if (skf==2) then 
    

         factor = 1./(pi*hsml*hsml)
         w = factor*exp(-q*q)
        dwdx = factor*exp(-q*q) *  (-2.* dxx/hsml**2)
        dwdy = factor*exp(-q*q) *  (-2.* dyy/hsml**2)
        
        
    else if (skf==3) then

          factor = 15.e0/(7.e0*pi*hsml*hsml)
        if (q>=1 .and. q<=2) then
            w = factor *(2-q)*(2-q)*(2-q)/6
            dwdx =factor * 1.e0/6.e0 * 3.*(2.-q)**2/hsml * (dxx/r)
            dwdx =factor * 1.e0/6.e0 * 3.*(2.-q)**2/hsml * (dyy/r)    
        
        else if (q>=0 .and. q<=1) then 
            w = (factor*2/3)-(factor*q*q)+(factor*0.5*q*q*q) 
            dwdx = factor * (-2.+3./2.*q)/hsml**2 * dxx 
            dwdy = factor * (-2.+3./2.*q)/hsml**2 * dyy 


        else
            w = 0
            dwdx = 0
            dwdy = 0
        end if
     
	 else if (skf==4) then

          factor = 7/(478*pi*hsml*hsml)
		  
		  if (q>=0 .and. q<=1) then
            w = factor *((3-q)**5-6*(2-q)**5+15*(1-q)**5)
            dwdx =factor * (5*(3-q)**4-30*(2-q)**4+75*(1-q)**4)/hsml * (dxx/r)
            dwdx =factor * (5*(3-q)**4-30*(2-q)**4+75*(1-q)**4)/hsml * (dyy/r)    
        
        else if (q>=1 .and. q<=2) then 
            w = factor*((3-q)**5-6*(2-q)**5)
            dwdx = factor * (5*(3-q)**4-30*(2-q)**4)/hsml * (dxx/r)
            dwdy = factor * (5*(3-q)**4-30*(2-q)**4)/hsml * (dyy/r)

        else if (q>=2 .and. q<=3) then 
            w = factor*(3-q)**5
            dwdx = factor * (5*(3-q)**4)/hsml * (dxx/r)
            dwdy = factor * (5*(3-q)**4)/hsml * (dyy/r)
        else
            w = 0
            dwdx = 0
            dwdy = 0
        end if
		
    else
    print *,' >>> error <<< : wrong kernel function: skf =',skf
         stop
     
     end if 
     
	 end subroutine 
             
