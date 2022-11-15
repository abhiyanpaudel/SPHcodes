 subroutine output(itimestep,x,y,vx,vy,vtot,mass, rho, p,c,itype, hsml, ntotal)   
      
!----------------------------------------------------------------------           
!    subroutine for saving particle information to external disk file



     implicit none
    include  "parameters.inc.txt" 
      
      integer,intent(in) :: itype(maxn), ntotal,itimestep 
	  double precision,intent(in) :: x(maxn),y(maxn),vx(maxn),vy(maxn),mass(maxn), &
            rho(maxn),p(maxn), hsml(maxn),c(maxn),vtot(maxn)
     
      integer :: i,save_step ,outunit,filenum ,j         
	  character(len=70) :: fn
	  save_step = 100
      
      outunit = 44
	  
     
           j = itimestep/save_step
		
		    
				write(fn,fmt='(i0,a)') j, '.dat'
				open(unit=j+outunit,file=fn, form='formatted')
				
			
		
			
            
				
		
			do i = 1,ntotal
				write(j+outunit,*) x(i),y(i),vx(i),vy(i),vtot(i)
			end do
		
				
			close(j+outunit)	
		
		
   end         