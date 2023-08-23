FUNCTION ANN_x2_daily(X2_prv, DO_prv, currDay,currMonth,currYear)  RESULT (outputX2) 
   
    USE, INTRINSIC :: ISO_C_BINDING
    
    !DEC$ ATTRIBUTES DLLEXPORT :: ANN_x2_daily
    
    use SFtideModule
    use ann
    
    Real, dimension(148), intent(inout) :: X2_prv
    Real, dimension(148), intent(inout) :: DO_prv
    Real, dimension(148):: currSFtide

    integer(C_LONG), INTENT(IN) :: currDay, currMonth, currYear
      
    REAL    :: outputX2 
    
    logical :: debugCode = .false.
  
    SFtideEndIndex= getSFtideArrayEndIndex(currday,currMonth,currYear)
    SFtideStartIndex= SFtideEndIndex - 148 + 1
    currSFtide(1:148)=SFtide(SFtideStartIndex:SFtideEndIndex) 
    
    outputX2 =  calcx2daily(X2_prv(31:148),DO_prv(31:148),currSFtide(31:148),13) 

    if(debugCode == .TRUE.) then
        print*,"Information X2 function:"
        print*,"month, water year:",currMonth,currYear
        print*,"X2(t-1): ",X2_prv0,X2_prv1,X2_prv2,X2_prv3,X2_prv4             
        print*,"DO: ",DO_prv0,DO_prv1,DO_prv2,DO_prv3,DO_prv4    
        print*,"***calculated X2: ", outputX2
    end if

    END function ANN_x2_daily

    
FUNCTION getNDO_x2_daily( X2, X2_prv, X2_last, DO_prv, currDay, currMonth, currYear)  RESULT (requiredNDO) 
    !** returning the required current monthly NDO to obtain a desired X2.
    !DO: delta outflow
    
    USE, INTRINSIC :: ISO_C_BINDING
    
    !DEC$ ATTRIBUTES DLLEXPORT :: getNDO_x2_daily
    
    use SFtideModule
    use ann
    
    REAL,INTENT(IN)    ::  X2
    REAL,INTENT(IN)    ::  X2_last
    Real, dimension(147), intent(inout) :: X2_prv
    Real, dimension(147), intent(inout) :: DO_prv
    integer(C_LONG), INTENT(IN) :: currDay, currMonth, currYear
    
    Real, dimension(148):: X2_prv_temp
    Real, dimension(148):: DO_prv_temp
    Real, dimension(148):: currSFtide
    REAL   :: requiredNDO
    
    !logical debugCode = .true.
    
    real InitialDO,tempDO, DO_lo, DO_high, DIFF, X2_TOL, DO_TOL, tempX2, BigDOstep  
    
    InitialDO=20000.0 
    tempDO=0.0 
    DO_lo=0.0
    DO_high=0.0
    DIFF = 0.0
    X2_TOL = 0.1
    DO_TOL= 0.5
    tempX2=0.
    BigDOstep = 10000.0 
    
    do i=1, 147
        X2_prv_temp(i)=X2_prv(i)
    end do
    X2_prv_temp(148)=X2_last
    
    do i=1, 147
        DO_prv_temp(i)=DO_prv(i)
    end do
    DO_prv_temp(148)=InitialDO
    
    SFtideEndIndex= getSFtideArrayEndIndex(currday,currMonth,currYear)
    SFtideStartIndex= SFtideEndIndex - 148 + 1
    currSFtide(1:148)=SFtide(SFtideStartIndex:SFtideEndIndex)  
    
    tempX2=calcx2daily(X2_prv_temp(31:148),DO_prv_temp(31:148),currSFtide(31:148),13)
    
    !find DO range to have desired X2
    
    tempDO=initialDO
    
    !use big step to search first 
    if(tempX2 > X2) then
        do while (tempX2 > X2) 
            DO_lo= tempDO
!            tempDO = DO_lo+BigDOstep
            tempDO = tempDO+BigDOstep
            DO_high= tempDO
     
           if (DO_high> 99999.9) then  !shengjun add 10/19/07: 99999.9 is the maximum required DO can go
                requiredNDO =  99999.9
                return
           end if
                       
           DO_prv_temp(148)=tempDO
           tempX2=calcx2daily(X2_prv_temp(31:148),DO_prv_temp(31:148),currSFtide(31:148),13)                            
        end do
    else
        do while (tempX2 < X2) 
        
            DO_high= tempDO                        
!            tempDO = DO_high-BigDOstep!shengjun revise 10/19/07
            tempDO = tempDO-BigDOstep!shengjun revise 10/19/07            
            DO_lo= tempDO            

            if (DO_lo < 0.0) then !4/20/08
                tempDO =  0.0
            end if            
            
            DO_prv_temp(148)=tempDO
            tempX2=calcx2daily(X2_prv_temp(31:148),DO_prv_temp(31:148),currSFtide(31:148),13)                                    
                            
            if(tempX2 < X2 .and. tempDO<0.1) then !4/20/08
              requiredNDO = 0.
              return
            end if
           
                            
        end do
    end if
    
    !now try to find out correct DO
    DIFF=ABS(X2-tempX2)    
       
    do while(DIFF > X2_TOL .and. (DO_high-DO_lo) > DO_TOL) !shengjun10/19/07: may need to consider no DO can be found for certain X2
        
        tempDO=0.5*(DO_lo+DO_high)
        DO_prv_temp(148)=tempDO
        tempX2=calcx2daily(X2_prv_temp(31:148),DO_prv_temp(31:148),currSFtide(31:148),13)                  
                                       	    	
    	if(tempX2 > X2) then
	        DO_lo= tempDO
	    else
	        DO_high= tempDO
	    end if
		
        DIFF=ABS(X2-tempX2)
    end do
    
    !print *, "DO diff", DO_high-DO_lo 
      
    requiredNDO =  tempDO
   
END function getNDO_x2_daily
!end of add 


