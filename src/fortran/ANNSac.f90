FUNCTION ANNSAC( &
     Qsac_prv0,Qsac_prv1,Qsac_prv2,Qsac_prv3,&
     Qexp_prv0,Qexp_prv1,Qexp_prv2,Qexp_prv3,Qexp_prv4,&
     Qsjr_prv0,Qsjr_prv1,Qsjr_prv2,Qsjr_prv3,Qsjr_prv4,&
     DXC_prv0,DXC_prv1,DXC_prv2,DXC_prv3,DXC_prv4,&     
     DICU_prv0,DICU_prv1,DICU_prv2,DICU_prv3,DICU_prv4,&  !shengjun add 2/24/2005          
     Qsac_oth_prv0,Qsac_oth_prv1,Qsac_oth_prv2,Qsac_oth_prv3,Qsac_oth_prv4,&
     Qexp_oth_prv0,Qexp_oth_prv1,Qexp_oth_prv2,Qexp_oth_prv3,Qexp_oth_prv4,&
     VernEC_prv0,VernEC_prv1,VernEC_prv2,VernEC_prv3,VernEC_prv4,&
     ECTARGET, &
     mon0,mon1,mon2,mon3,mon4,location,ave_type,currMonth,currYear,BeginDay, EndDay)  RESULT (outputSac) !shengjun 2/14/2005
  !DEC$ ATTRIBUTES DLLEXPORT :: annsac
  !use ann_ext
       
  use SFtideModule !shengjun 2/14/2005
  USE, INTRINSIC :: ISO_C_BINDING

  IMPLICIT NONE
  
    REAL ::  ANN_Month
  integer :: initall
  ! input and output of the function
  ! DXC=0,CLOSED  DXC=1,OPEN
  REAL,INTENT(IN)    :: &
       Qsac_prv0,Qsac_prv1,Qsac_prv2,Qsac_prv3,&
       Qexp_prv0,Qexp_prv1,Qexp_prv2,Qexp_prv3,Qexp_prv4,&
       Qsjr_prv0,Qsjr_prv1,Qsjr_prv2,Qsjr_prv3,Qsjr_prv4,&
       DXC_prv0,DXC_prv1, DXC_prv2, DXC_prv3,DXC_prv4,&
       Qsac_oth_prv0,Qsac_oth_prv1,Qsac_oth_prv2,Qsac_oth_prv3,Qsac_oth_prv4,&
       Qexp_oth_prv0,Qexp_oth_prv1,Qexp_oth_prv2,Qexp_oth_prv3,Qexp_oth_prv4, &
       DICU_prv0,DICU_prv1,DICU_prv2,DICU_prv3,DICU_prv4, &  !shengjun add 2/24/2005 
       VernEC_prv0,VernEC_prv1,VernEC_prv2,VernEC_prv3,VernEC_prv4, &    !Hao add 9/2008
       ECTarget
  
  integer(C_LONG), INTENT(IN) :: currMonth, currYear !shengjun 2/14/2005
  integer(C_LONG), optional, intent(in) :: BeginDay, EndDay !only used in ave_type ==10
!!beginchange
  logical,dimension(0:4) :: VernEC_prv_BW =.false.
  real   ,dimension(0:4) :: VernEC_prv_V=0
  integer,dimension(210) :: T=0
  integer,dimension(0:4) :: tim_beg, tim_end
  integer :: counter=0, end_time, j
!!endchange
  INTEGER(C_LONG) :: location,mon0,mon1,mon2,mon3,mon4,ave_type
  REAL    :: outputSac, outputEC, QsacUB, QsacLB, QsacMid
  integer :: tim0,tim1,tim2,tim3,tim4,tim5
  REAL,DIMENSION(148) :: Qsac,Qexp,Qsjr,DXC,Qsac_oth,Qexp_oth,currSFtide,DICU, VernEC !2/14/2005 and 2/24/2005 and 9/2008
  INTEGER             :: i,SFtideStartIndex,SFtideEndIndex !2/14/2005  
  integer :: getSFtideArrayEndIndex !2/14/2005
  real, allocatable :: smoothedFlow(:) !4/1/2005  
  integer debugFlag ; debugFlag =0

  i = initall()

  tim5=148          ! last day of current month
  tim4=tim5-mon4+1  ! first day of current month
  tim3=tim4-mon3    ! first day of 1 month ago
  tim2=tim3-mon2    ! first day of 2 months ago
  tim1=tim2-mon1    ! first day of 3 months ago
  tim0=1            ! first day of 118 days before the last day of the current month

  allocate (smoothedFlow(mon0+mon1+mon2+mon3+mon4))

  ! 118 values daily are needed to estimate one daily value of EC
  ! this function builds the first set of input arrays representing
  ! dxc position and river inflow.
  !
  ! The arrays have the following structure
  ! tim0         tim1         tim2         tim3           tim4         tim5
  ! --------------------------------------------------------------------
  ! |this month-4|this month-3|this month-2| this month-1 | this month |
  ! --------------------------------------------------------------------

  Qsac(tim0:(tim1-1))=Qsac_prv0 !shengjun comment to make smooth input 4/4/2005
  Qsac(tim1:(tim2-1))=Qsac_prv1
  Qsac(tim2:(tim3-1))=Qsac_prv2
  Qsac(tim3:(tim4-1))=Qsac_prv3
  !Qsac(tim4:tim5)=Qsac_prv4    

!  smoothedFlow = ConservativeSpline(Qsac_prv0,Qsac_prv1,Qsac_prv2,Qsac_prv3,Qsac_prv4, mon0,mon1,mon2,mon3,mon4)                          
 ! Qsac(1:tim5)= smoothedFlow(mon0+mon1+mon2+mon3+mon4-tim5+1:mon0+mon1+mon2+mon3+mon4)  
  
  Qexp(tim0:(tim1-1))=Qexp_prv0
  Qexp(tim1:(tim2-1))=Qexp_prv1
  Qexp(tim2:(tim3-1))=Qexp_prv2
  Qexp(tim3:(tim4-1))=Qexp_prv3
  Qexp(tim4:tim5)=Qexp_prv4

  Qsjr(tim0:(tim1-1))=Qsjr_prv0 !shengjun comment to make smooth input 4/4/2005
  Qsjr(tim1:(tim2-1))=Qsjr_prv1
  Qsjr(tim2:(tim3-1))=Qsjr_prv2
  Qsjr(tim3:(tim4-1))=Qsjr_prv3
  Qsjr(tim4:tim5)=Qsjr_prv4

  !smoothedFlow = ConservativeSpline(Qsjr_prv0,Qsjr_prv1,Qsjr_prv2,Qsjr_prv3,Qsjr_prv4, mon0,mon1,mon2,mon3,mon4)
  !Qsjr(1:tim5)= smoothedFlow(mon0+mon1+mon2+mon3+mon4-tim5+1:mon0+mon1+mon2+mon3+mon4)

  Qsac_oth(tim0:(tim1-1))=Qsac_oth_prv0
  Qsac_oth(tim1:(tim2-1))=Qsac_oth_prv1
  Qsac_oth(tim2:(tim3-1))=Qsac_oth_prv2
  Qsac_oth(tim3:(tim4-1))=Qsac_oth_prv3
  Qsac_oth(tim4:tim5)=Qsac_oth_prv4

  Qexp_oth(tim0:(tim1-1))=Qexp_oth_prv0
  Qexp_oth(tim1:(tim2-1))=Qexp_oth_prv1
  Qexp_oth(tim2:(tim3-1))=Qexp_oth_prv2
  Qexp_oth(tim3:(tim4-1))=Qexp_oth_prv3
  Qexp_oth(tim4:tim5)=Qexp_oth_prv4
  
  !start of add by shengjun 2/24/2005
  DICU(tim0:(tim1-1))=DICU_prv0
  DICU(tim1:(tim2-1))=DICU_prv1
  DICU(tim2:(tim3-1))=DICU_prv2
  DICU(tim3:(tim4-1))=DICU_prv3
  DICU(tim4:tim5)=DICU_prv4
  !end of add
  
  !start of add by shengjun 2/15/2005
  SFtideEndIndex= getSFtideArrayEndIndex(mon4,currMonth,currYear)
  SFtideStartIndex= SFtideEndIndex - tim5 + 1
  currSFtide(tim0:tim5)=SFtide(SFtideStartIndex:SFtideEndIndex)  
  !end of add
  
  deallocate (smoothedFlow) !4/1/2005
    
  do i=tim0,(tim1-1)              ! last portion of 4th previous month
    if ( i < (dxc_prv0-(mon0-tim1))) then
      dxc(i) = 1.0 !shengjun changed to make 1 as DXC open 8/2/2004
    else
      dxc(i) = 0.0 !shengjun changed to make 0 as DXC close 8/2/2004
    end if
  end do
  DO i=tim1,(tim2-1)              ! 3rd previous month
    if ( i-tim1 < dxc_prv1) then
      dxc(i) = 1.0 !shengjun changed to make 1 as DXC open 8/2/2004
    else
      dxc(i) = 0.0 !shengjun changed to make 0 as DXC close 8/2/2004
    end if
  END DO
  DO i=tim2,(tim3-1)              ! 2nd previous month
    if ( i-tim2 < dxc_prv2) then
      dxc(i) = 1.0 !shengjun changed to make 1 as DXC open 8/2/2004
    else
      dxc(i) = 0.0 !shengjun changed to make 0 as DXC close 8/2/2004
    end if
  END DO
  DO i=tim3,(tim4-1)              ! previous month
    if ( i-tim3 < dxc_prv3) then
      dxc(i) = 1.0 !shengjun changed to make 1 as DXC open 8/2/2004
    else
      dxc(i) = 0.0 !shengjun changed to make 0 as DXC close 8/2/2004
    end if
  END DO
  DO i=tim4,tim5                  ! current month
    if ( i-tim4 < dxc_prv4) then
      dxc(i) = 1.0 !shengjun changed to make 1 as DXC open 8/2/2004
    else
      dxc(i) = 0.0 !shengjun changed to make 0 as DXC close 8/2/2004
    end if
  END DO
  
  !start of add by Hao 9/2008
!  VernEC(tim0:(tim1-1))=VernEC_prv0
!  VernEC(tim1:(tim2-1))=VernEC_prv1
!  VernEC(tim2:(tim3-1))=VernEC_prv2
!  VernEC(tim3:(tim4-1))=VernEC_prv3
!  VernEC(tim4:tim5)=VernEC_prv4

!! beginchange

  T(1:210)=0
  end_time = tim5
  do j = 1, 30
   do i = 1, 7
    if ( MOD(j,2)== 0 )  then 
        T(i+(j-1)*7)=1 
    endif
   enddo
  enddo
  VernEC_prv_V(0) = VernEC_prv0
  VernEC_prv_V(1) = VernEC_prv1
  VernEC_prv_V(2) = VernEC_prv2
  VernEC_prv_V(3) = VernEC_prv3
  VernEC_prv_V(4) = VernEC_prv4
  tim_beg = (/tim0,    tim1,    tim2,    tim3,    tim4/)
  tim_end = (/tim1-1,  tim2-1,  tim3-1,  tim4-1,  tim5/)
  end_time = tim5
  
  do i=0,4
    VernEC_prv_BW(i) =  ( 0.4 < VernEC_prv_V(i) .and. VernEC_prv_V(i) < 0.6 )
  enddo

  counter=0
    
  do i=0,4
    if (.not. VernEC_prv_BW(i)) then
      counter = 0
      VernEC(tim_beg(i):tim_end(i))=VernEC_prv_V(i) 
    else if (counter < 0.5) then
      counter = 1
      VernEC(tim_beg(i):end_time)=T(1:end_time-tim_beg(i)+1)
    end if  
  enddo
!! endchange
  
  
  !end of add
  
  QsacUB=184862.0
  QsacLB=0.0
  
  Qsac(tim4:tim5)=QsacUB
  outputEC=ANN_Month(Qsac+Qsac_oth,Qexp+Qexp_oth,Qsjr,currSFtide,DICU,DXC,VernEC,location,mon4,ave_type,BeginDay, EndDay)    
  if (outputEC>ECTarget) then
    outputSac=999999.0
    return
  endif
   
  Qsac(tim4:tim5)=QsacLB
  outputEC=ANN_Month(Qsac+Qsac_oth,Qexp+Qexp_oth,Qsjr,currSFtide,DICU,DXC,VernEC,location,mon4,ave_type,BeginDay, EndDay)
  if (outputEC<ECTarget) then
    outputSac=0.0
    return
  endif
  
  QsacMid=(QsacUB+QsacLB)/2.0
  do while (abs(QsacUB-QsacLB)>0.5)
    Qsac(tim4:tim5)=QsacMid
    outputEC=ANN_Month(Qsac+Qsac_oth,Qexp+Qexp_oth,Qsjr,currSFtide,DICU,DXC,VernEC,location,mon4,ave_type,BeginDay, EndDay) 
    
    if (outputEC>ECTarget) then
        QsacLB=QsacMid
    else if (outputEC<ECTarget) then
        QsacUB=QsacMid
    else
        outputSac=QsacMid
        return
    endif    
        
    QsacMid=(QsacUB+QsacLB)/2.0
    
    if(debugFlag == 1 .and. location==2 .and. ave_type==1) then  
        print*,"Information in ANNEC():"
        print*,"month, water year:",currMonth,currYear            
        print*,"Qsac_smooth=",Qsac(1:tim5)  !4/13/2005
        print*,"Qsjr_smooth=",Qsjr(1:tim5)  !4/13/2005
        print*,"dxc=",dxc(1:tim5)  !4/21/2005
        print*,"Qsac_oth_prv0,Qsac_oth_prv1,Qsac_oth_prv2,Qsac_oth_prv3,Qsac_oth_prv4:",Qsac_oth_prv0,Qsac_oth_prv1,Qsac_oth_prv2,Qsac_oth_prv3,Qsac_oth_prv4
        print*,"Qsac0,Qsac1,Qsac2,Qsac3,Qsac4:",Qsac_prv0,Qsac_prv1,Qsac_prv2,Qsac_prv3,QsacMid    
        print*,"Qexp_oth_prv0,Qexp_oth_prv1,Qexp_oth_prv2,Qexp_oth_prv3,Qexp_oth_prv4:",Qexp_oth_prv0,Qexp_oth_prv1,Qexp_oth_prv2,Qexp_oth_prv3,Qexp_oth_prv4
        print*,"Qexp0,Qexp1,Qexp2,Qexp3,Qexp4:",Qexp_prv0,Qexp_prv1,Qexp_prv2,Qexp_prv3,Qexp_prv4
        print*,"SFtideStartIndex,SFtideEndIndex:",SFtideStartIndex,SFtideEndIndex
        print*,"SFtide=",currSFtide(1:tim5)  !4/13/2005   
        print*,"DICU_prv0,DICU_prv1,DICU_prv2,DICU_prv3,DICU_prv4:",DICU_prv0,DICU_prv1,DICU_prv2,DICU_prv3,DICU_prv4
        print*,"VernEC_prv0,VernEC_prv1,VernEC_prv2,VernEC_prv3,VernEC_prv4:",VernEC_prv0,VernEC_prv1,VernEC_prv2,VernEC_prv3,VernEC_prv4
        print*,"outputEC at ORRSL: ",outputEC
    end if
  enddo
  
  outputSac=(QsacUB+QsacLB)/2.0
  
END function ANNSAC