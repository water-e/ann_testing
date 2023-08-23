real function ANN_X2ARRAY(Parameters, LastElementIndex) 
!DEC$ ATTRIBUTES DLLEXPORT :: ANN_X2Array
implicit none

real :: ANN_X2

double precision, intent(in) :: Parameters(20)
integer, intent(in) :: LastElementIndex
real :: outputANN_X2

outputANN_X2 = ANN_X2(real(Parameters(1)),real(Parameters(2)),real(Parameters(3)),real(Parameters(4)),real(Parameters(5)),real(Parameters(6)),real(Parameters(7)),real(Parameters(8)),real(Parameters(9)),real(Parameters(10)),int(Parameters(11)),int(Parameters(12)),int(Parameters(13)),int(Parameters(14)),int(Parameters(15)),int(Parameters(16)),int(Parameters(17)),int(Parameters(18)))

ANN_X2ARRAY = outputANN_X2
end function ANN_X2ARRAY


real function GetNDO_X2ARRAY(Parameters, LastElementIndex) 
!DEC$ ATTRIBUTES DLLEXPORT :: GetNDO_X2Array
implicit none

real :: getNDO_X2

double precision, intent(in) :: Parameters(20)
integer, intent(in) :: LastElementIndex
real :: outputGetNDO_X2

outputGetNDO_X2 = getNDO_X2(real(Parameters(1)),real(Parameters(2)),real(Parameters(3)),real(Parameters(4)),real(Parameters(5)),real(Parameters(6)),real(Parameters(7)),real(Parameters(8)),real(Parameters(9)),real(Parameters(10)),int(Parameters(11)),int(Parameters(12)),int(Parameters(13)),int(Parameters(14)),int(Parameters(15)),int(Parameters(16)),int(Parameters(17)),int(Parameters(18)),int(Parameters(19)),int(Parameters(20)))

GetNDO_X2ARRAY = outputGetNDO_X2
end function GetNDO_X2ARRAY
    

real function ANN_x2_curMonInpSplitARRAY(Parameters, LastElementIndex) 
!DEC$ ATTRIBUTES DLLEXPORT :: ANN_x2_curMonInpSplitARRAY
implicit none

real :: ANN_x2_curMonInpSplit

double precision, intent(in) :: Parameters(24)
integer, intent(in) :: LastElementIndex
real :: outputANN_x2_curMonInpSplit

outputANN_x2_curMonInpSplit = ANN_x2_curMonInpSplit(real(Parameters(1)),real(Parameters(2)),real(Parameters(3)),real(Parameters(4)),real(Parameters(5)),real(Parameters(6)),real(Parameters(7)),real(Parameters(8)),real(Parameters(9)),real(Parameters(10)),real(Parameters(11)),real(Parameters(12)),int(Parameters(13)),int(Parameters(14)),int(Parameters(15)),int(Parameters(16)),int(Parameters(17)),int(Parameters(18)),int(Parameters(19)),int(Parameters(20)),int(Parameters(21)),int(Parameters(22)),int(Parameters(23)),int(Parameters(24)))

ANN_x2_curMonInpSplitARRAY = outputANN_x2_curMonInpSplit
end function ANN_x2_curMonInpSplitARRAY     
    
    
real function GetNDO_x2_curMonNDOSplitARRAY(Parameters, LastElementIndex) 
!DEC$ ATTRIBUTES DLLEXPORT :: GetNDO_x2_curMonNDOSplitARRAY
implicit none

real :: GetNDO_x2_curMonNDOSplit

double precision, intent(in) :: Parameters(24)
integer, intent(in) :: LastElementIndex
real :: outputGetNDO_x2_curMonNDOSplit

outputGetNDO_x2_curMonNDOSplit = GetNDO_x2_curMonNDOSplit(real(Parameters(1)),real(Parameters(2)),real(Parameters(3)),real(Parameters(4)),real(Parameters(5)),real(Parameters(6)),real(Parameters(7)),real(Parameters(8)),real(Parameters(9)),real(Parameters(10)),real(Parameters(11)),real(Parameters(12)),int(Parameters(13)),int(Parameters(14)),int(Parameters(15)),int(Parameters(16)),int(Parameters(17)),int(Parameters(18)),int(Parameters(19)),int(Parameters(20)),int(Parameters(21)),int(Parameters(22)),int(Parameters(23)),int(Parameters(24)))

GetNDO_x2_curMonNDOSplitARRAY = outputGetNDO_x2_curMonNDOSplit
end function GetNDO_x2_curMonNDOSplitARRAY    
    
    