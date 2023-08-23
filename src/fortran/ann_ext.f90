! Last change: RW Feb 8,2003
function initall() result(outval)
  use ann
  use fnet_orrsl
  use fnet_jp
  use fnet_emm
  use fnet_antioch
  use fnet_CO
  use fnet_mallard
  use fnet_LosVaqueros !shengjun 4/26/2005
  
  use fnet_CCFB !swu 7/1/07
  use fnet_CCFB_intake 
  use fnet_MidR_intake
  use fnet_Victoria_intake
  use fnet_CVP_intake
  use fnet_X2
  use fnet_beldan  ! NJ 8/09/2020  
  use fnet_MTZ  ! NJ 8/16/2020  
  
  integer :: outval
  !dll_export initall
  outval = 1
  if ( init_called ) return
  !allocate(inarray(108))!here need to be changed for different size of ANN shengjun comment out to use fixed array 8/23/2005
  !allocate(outarray(1)) !shengjun use fixed array 8/23/2005 
  !write(*,*) 'Allocated in and out arrays'
  call fnet_jp_initall()
  call fnet_emm_initall()
  call fnet_orrsl_initall()
  call fnet_antioch_initall()
  call fnet_CO_initall()
  call fnet_mallard_initall()
  call fnet_LosVaqueros_initall()
  call fnet_CCFB_initall !swu 7/1/07
  call fnet_CCFB_intake_initall
  call fnet_MidR_intake_initall
  call fnet_Victoria_intake_initall
  call fnet_CVP_intake_initall
  call fnet_X2_initall
  call fnet_beldan_initall()  ! NJ 9/08/2020
  call fnet_MTZ_initall()  ! NJ 9/16/2020
  
  init_called = .true.
  outval=0
end function initall

function ANN_Month(Qsac,Qexp,Qsjr,currSFtide,DICU,DXC,VERNEC,location,days,ave_type,BeginDay, EndDay) result(outval)
!
! ANN_Month provides interaction between the monthly data and the daily data
! through various averaging techniques (monthly averag, 14-day average, daily max, etc.).
! ave_type = 1: monthly average
!            2: first day of month value
!            3: last day of month value
!            4: maximum daily value
!            5: minimum daily vlaue
!            6: maximum 14-day value
!            7: average for first 15 days
!            8: average for last 15 days
!           10: average between specified range (BeginDay- EndDay, counting from the beginning of the month) 7/11/2007!
!           99: only print out for last cycle
! BeginDay, EndDay: Day ID counting from the beginning of the month
!
  use ann
  implicit none
  !dll_export ANN_Month
  real, dimension(148), intent(inout) :: Qsac, Qexp, Qsjr, DXC,currSFtide, DICU, VERNEC   !9/2008
  integer, intent(inout)  :: location,days,ave_type
  real :: sum,num
  real, dimension(31) :: day
  real :: outval
  integer :: i,base, offset, j 
  integer, optional, intent(in) :: BeginDay, EndDay !only used in ave_type ==10    
  
  real ecday
  !open (unit = 1, file = 'jp.dat') MW 1/9/23 comment start
  !open (unit = 2, file = 'rs.dat')
  !open (unit = 3, file = 'em.dat')
  !open (unit = 5, file = 'co.dat')
  !open (unit = 7, file = 'bl.dat')  
  !open (unit = 8, file = 'mtz.dat') MW 1/9/23 comment end
  
! also to adjust for the number of days in a month
  base = 148-days

!  to adjust for the number of days in a month
  offset = 31-days   

! attempting to pass into calcDaily 118 previous values from each day in the month.

  sum = 0.0
  num = 0.0
  
  !shengjun comment 4/22/2005
!  do i=1+offset,days+offset      
 !   sum = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),location)!shengjun 12/16/2004
  !end do
  
  !3/23/2006:initialize to record montly input range status
  currMonthInputStatus=inputStatus(.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.)  !9/2008  
  
   if(ave_type == 99) then
	  open (unit = 1, file = 'jp.dat')  ! MW 1/9/23 moved these opening lines from LN76 to inside this conditional statemet 
      open (unit = 2, file = 'rs.dat')
      open (unit = 3, file = 'em.dat')
      open (unit = 5, file = 'co.dat')
      open (unit = 7, file = 'bl.dat')  
      open (unit = 8, file = 'mtz.dat') ! MW 1/9/23 end
       ! monthly average
    sum = 0.
    do i=1+offset,days+offset
      !sum = sum
      sum = sum + calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)!shengjun 12/16/2004 & Hao 9/2008

      ecday = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)
      if (location .eq. 1) then
           write(1,*) ecday
       else if (location .eq. 2) then   
           write(2,*) ecday   
       else if (location .eq. 3) then   
           write(3,*) ecday   
       else if (location .eq. 5) then   
           write(5,*) ecday   
       else if (location .eq. 20) then   
           write(7,*) ecday   
       else if (location .eq. 21) then   
           write(8,*) ecday  
       end if     
                   
!      if(location .eq. 2) then for debuging
 !       write(*,*) 'day ID and EC',i, calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),location)
  !    end if
      
    end do
	close (Unit = 1) ! MW 1/9/23 start closing files
	close (unit = 2)
	close (unit = 3)
	close (unit = 5)
	close (unit = 7)
	close (unit = 8) ! MW 1/9/23 end closing files
    sum = sum/real(days)   

       
  else if(ave_type == 1) then
    ! monthly average
    sum = 0.
    do i=1+offset,days+offset
      !sum = sum
      sum = sum + calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)!shengjun 12/16/2004 & Hao 9/2008
!      ecday = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)
!      if (location .eq. 1) then
!           write(1,*) ecday
!       else if (location .eq. 2) then   
!           write(2,*) ecday   
!       else if (location .eq. 3) then   
!           write(3,*) ecday   
!       else if (location .eq. 5) then   
!           write(5,*) ecday   
!       end if     
                   
!      if(location .eq. 2) then for debuging
 !       write(*,*) 'day ID and EC',i, calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),location)
  !    end if
      
    end do
    sum = sum/real(days)
  
  else if(ave_type == 2) then
    ! first day value
    sum = 0.
    sum = calcDaily(Qsac(1+offset:1+base),Qsjr(1+offset:1+base),Qexp(1+offset:1+base),currSFtide(1+offset:1+base),DICU(1+offset:1+base),DXC(1+offset:1+base),VERNEC(1+offset:1+base),location) !12/15/2004 & Hao 9/2008
  
  else if(ave_type == 3) then
    ! last day value
    sum = 0.
    sum = calcDaily(Qsac(31:148),Qsjr(31:148),Qexp(31:148),currSFtide(31:148),DICU(31:148),DXC(31:148),VERNEC(31:148),location)!shengjun 12/16/2004 & Hao 9/2008
  
  else if(ave_type == 33) then
    ! last 3 day average
    sum = 0.
    sum = ( calcDaily(Qsac(31:148),Qsjr(31:148),Qexp(31:148),currSFtide(31:148),DICU(31:148),DXC(31:148),VERNEC(31:148),location) &
          + calcDaily(Qsac(30:147),Qsjr(30:147),Qexp(30:147),currSFtide(30:147),DICU(30:147),DXC(30:147),VERNEC(30:147),location) &
          + calcDaily(Qsac(29:146),Qsjr(29:146),Qexp(29:146),currSFtide(29:146),DICU(29:146),DXC(29:146),VERNEC(29:146),location) &
          )/3      
    
  else if(ave_type == 4) then
    ! maximum daily value   
    sum = 0.
    do i=1+offset,days+offset
      !lkfjgldjflsakjfllk
      num = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)  ! Hao 9/2008
      sum = max(sum,num)
    end do
  
  else if(ave_type == 5) then
    ! minimum daily value
    sum = 99999.9
    do i=1+offset,days+offset
      !lkfjgldjflsakjfllk
      num = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)!shengjun 12/16/2004 & Hao 9/2008
      sum = min(sum,num)
    end do
  
  else if(ave_type == 6) then
    ! max 14-day average
    sum = 0.
    j = 0
    do i=1+offset,days+offset
      j = j + 1
      day(j) = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)!shengjun 12/16/2004 & Hao 9/2008
    end do
    do i=1,j-13
      num = (day(i)+day(i+1)+day(i+2)+day(i+3)+day(i+4)+day(i+5)+day(i+6)+&
             day(i+7)+day(i+8)+day(i+9)+day(i+10)+day(i+11)+day(i+12)+day(i+13))/14.
      sum = max(sum,num)
    end do
  
  else if(ave_type == 7) then
    ! average for first 15 days
    sum = 0.
    j = 0
    do i=1+offset,15+offset
      !sum = sum
      j = j+1
      num = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)!shengjun 12/16/2004 & Hao 9/2008
      sum = sum + num
      !WRITE(*,*) 'j=',j,' day=',num
    end do
    sum = sum/15.0
    !WRITE(*,*) 'sum=',sum
  
  else if(ave_type == 8) then
    ! average for last 15 days
    sum = 0.
    j = 0
    do i=days-14+offset,days+offset
      !sum = sum
      j = j + 1
      num = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)!shengjun 12/16/2004 & Hao 9/2008
      sum = sum + num
      !WRITE(*,*) 'j=',j,' day=',num
    end do
    sum = sum/15.0
    !WRITE(*,*) 'sum=',sum
	
  else if(ave_type == 36) then
    ! average for last 6 days
    sum = 0.
    j = 0
    do i=days-5+offset,days+offset
      !sum = sum
      j = j + 1
      num = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)!shengjun 12/16/2004 & Hao 9/2008
      sum = sum + num
      !WRITE(*,*) 'j=',j,' day=',num
    end do
    sum = sum/6.0
    
  else if(ave_type == 37) then
    ! average for last 7 days
    sum = 0.
    j = 0
    do i=days-6+offset,days+offset
      !sum = sum
      j = j + 1
      num = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)!shengjun 12/16/2004 & Hao 9/2008
      sum = sum + num
      !WRITE(*,*) 'j=',j,' day=',num
    end do
    sum = sum/7.0
    
  else if(ave_type == 10) then !shengjun 7/11/07
    ! average between BeginDay, EndDay
    if (BeginDay<1 .or. BeginDay >31 .or. BeginDay> EndDay .or. EndDay > 31 .or. Endday <1) then
        print *, "wrong begining and ending days in the month are specified. BeginDay and EndDay:", BeginDay,Endday
        stop
    end if    
      
    sum = 0.
    j = 0
    do i=BeginDay+offset,EndDay+offset      
        
      !sum = sum
      j = j + 1
      num = calcDaily(Qsac(i:i+base-offset),Qsjr(i:i+base-offset),Qexp(i:i+base-offset),currSFtide(i:i+base-offset),DICU(i:i+base-offset),DXC(i:i+base-offset),VERNEC(i:i+base-offset),location)!shengjun 12/16/2004 & Hao 9/2008
      
      !print *, "day#,x2:", j, num !4/27/08
      sum = sum + num
      !WRITE(*,*) 'j=',j,' day=',num
    end do
    
    sum = sum/(EndDay-BeginDay+1)
    !WRITE(*,*) 'sum=',sum
    !print *, "average x2 between defined period:", sum !4/27/08
  
  end if
  
  outval = sum
      
!  if(currMonthInputStatus.OutOfBound) then
 !   open(17,file="ANN_Info.txt",status="unknown",position="append") !3/23/2006
  !  write(17,'(1x,a,2i6)') "input range out of ANN training range at location and days in a month:",location, days
    
!    write(17,'(1x,6(a,a6))')  "Range violation at DXC:",merge(" True", "False", currMonthInputStatus.DXCscaled), &
 !                             ",Export:",merge(" True", "False", currMonthInputStatus.ExportScaled), &
  !                            ",Northern flow:",merge(" True", "False", currMonthInputStatus.NorthScaled), &
   !                           ",SJR:",merge(" True", "False", currMonthInputStatus.SouthScaled), &
    !                          ",SF tide:",merge(" True", "False", currMonthInputStatus.SFtideScaled), &
     !                         ",DICU:",merge(" True", "False", currMonthInputStatus.DICUscaled), &
      !                        ",Vernalis EC:",merge(" True", "False", currMonthInputStatus.VernECscaled)
!    close(17)
 ! end if
  
end function ANN_Month

!start of add by shengjun 2/15/2005
integer function getSFtideArrayEndIndex(daysIncurrMonth,currMonth,currYear) result(outval)
    !currMonth and currYear is in water year. therefore, 1= October, January =3
  
    integer, INTENT(IN):: daysIncurrMonth,currMonth,currYear
    integer CalenderMonth, CalenderYear       
    
    !convert from water year to calender year    
    if (currMonth .gt. 3) then
      CalenderMonth= currMonth - 3; CalenderYear = currYear
    else
      CalenderMonth= currMonth + 9 ; CalenderYear = currYear-1
    end if
    
 !   if(CalenderYear > 2026) then
     if(currYear > 2025) then
      write(*,*) 'This ANN can not used after Year 2025'
      stop
    end if
          
    !write (*,*) 'calender year, month:', CalenderYear,CalenderMonth
    
    outval = IYMDJL (CalenderYear,CalenderMonth,daysIncurrMonth) - IYMDJL (1921,1,1) +1
    
end function getSFtideArrayEndIndex  
!end of add

function ANN_Daily(Qsac,Qexp,Qsjr,currSFtide,DICU,DXC,VERNEC,location) result(outval)
!
! ANN_Month provides interaction between the monthly data and the daily data
! through various averaging techniques (monthly averag, 14-day average, daily max, etc.).
! ave_type = 1: monthly average
!            2: first day of month value
!            3: last day of month value
!            4: maximum daily value
!            5: minimum daily vlaue
!            6: maximum 14-day value
!            7: average for first 15 days
!            8: average for last 15 days
!           10: average between specified range (BeginDay- EndDay, counting from the beginning of the month) 7/11/2007!
! BeginDay, EndDay: Day ID counting from the beginning of the month
!
  use ann
  implicit none
  !dll_export ANN_Month
  real, dimension(148), intent(inout) :: Qsac, Qexp, Qsjr, DXC,currSFtide, DICU, VERNEC   !9/2008
  integer, intent(inout)  :: location
  real :: outval
  
  outval = calcDaily(Qsac(31:148),Qsjr(31:148),Qexp(31:148),currSFtide(31:148),DICU(31:148),DXC(31:148),VERNEC(31:148),location)
  
end function ANN_Daily