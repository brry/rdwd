! FORTRAN subroutine for converting Dataset of 2 Byte raw data to integer
! Originally written by Henning Rust <henning.rust@met.fu-berlin.de> and Christoph Ritschel (2016)
! Added to rdwd in May 2019 for readRadarFile used in readDWD.binary.

! Parameter: dims --> Dimension of 2 Byte Dataset
!            raw --> 2 Byte Dataset (as vector)
!            numeric --> integer Dataset for output

! INFO: Bit 13 --> Flag for interpolated
!       Bit 14 --> Flag for missing
!       Bit 15 --> Flag for negative
!       Bit 16 --> Flag for clutter
!      Bits 1-12 --> Data Value

subroutine binary_to_num(raw,dims,numeric,fNAval,fCLUTTERval)
  implicit none
  integer:: i
  integer,INTENT(in):: fNAval,fCLUTTERval
  integer,DIMENSION(2),INTENT(in)::dims
  integer(KIND=2),DIMENSION(dims(1)*dims(2)),INTENT(in)::raw
  integer,DIMENSION(dims(1)*dims(2)),INTENT(out)::numeric
  logical::interp,negative,missing,clutter
  
  ! do cycle over all entries 
  do i=1,dims(1)*dims(2) 

     interp=.FALSE. ! set interp false before checking
     missing=.FALSE. ! set missing false before checking
     negative=.FALSE. ! set negative false before checking
     clutter=.FALSE. ! set clutter false before checking

     numeric(i)=IBITS(raw(i),0,12)
     if(IBITS(raw(i),12,1)==1) THEN ! check for interpolated
        interp=.TRUE.
     end if
     if(IBITS(raw(i),13,1)==1) THEN ! check for missing 
        missing=.TRUE.
        numeric(i)=fNAval
     end if
     if(IBITS(raw(i),14,1)==1) THEN ! check for negative
        negative=.TRUE.
        numeric(i)=numeric(i)*(-1)
     end if
     if(IBITS(raw(i),15,1)==1) THEN ! check for clutter
        clutter=.TRUE.
        numeric(i)=fCLUTTERval
     end if
     
  end do

end subroutine binary_to_num

! INFO: RX only one Byte 
! NA-Value: 250
! Clutter-Value: 249

subroutine binary_to_num_RX(raw,dims,numeric,fNAval,fCLUTTERval)
  implicit none
  integer:: i
  integer,INTENT(in):: fNAval,fCLUTTERval
  integer,DIMENSION(2),INTENT(in)::dims
  integer(KIND=1),DIMENSION(dims(1)*dims(2)),INTENT(in)::raw
  integer,DIMENSION(dims(1)*dims(2)),INTENT(out)::numeric
  logical::interp,negative,missing,clutter
  
  ! do cycle over all entries 
  do i=1,dims(1)*dims(2) 

     numeric(i)=IBITS(raw(i),0,7)
     if(numeric(i)==250) THEN
        numeric(i)=fNAval
     end if
     if(numeric(i)==249) THEN
        numeric(i)=fCLUTTERval
     end if
     
  end do

end subroutine binary_to_num_RX









