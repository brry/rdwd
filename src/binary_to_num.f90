!-------------------------------------------------------------------------------
! FORTRAN subroutine for converting Dataset of 2 Byte raw data to integer
! Originally written by Henning Rust <henning.rust@met.fu-berlin.de>
! and Christoph Ritschel (2016). Added to rdwd in May 2019 for readRadarFile
! used in readDWD.binary. Updated and tweaked by Avraham Adler 2019-05-27
!-------------------------------------------------------------------------------

module B2N
  use, intrinsic :: iso_c_binding
  implicit none
  public :: binary_to_num, binary_to_num_rx
  
contains

!-------------------------------------------------------------------------------
! Parameter: dims --> Dimension of 2 Byte Dataset
!            raw --> 2 Byte Dataset (as vector)
!            numeric --> integer Dataset for output

! INFO: Bit 13 --> Flag for interpolated, ignored here
!       Bit 14 --> Flag for missing
!       Bit 15 --> Flag for negative
!       Bit 16 --> Flag for clutter
!       Bits 1-12 --> Data Value
!-------------------------------------------------------------------------------

  subroutine binary_to_num(raw, Flength, numeric, Fna, Fclutter) &
                           bind(C, name = "binary_to_num_")
    integer(kind = c_int), intent(in)                   ::Flength, Fna, Fclutter
    integer(kind = c_int), intent(in), dimension(Flength) ::raw
    integer(kind = c_int), intent(out), dimension(Flength)::numeric
    integer                                               ::i
    
    ! do cycle over all entries 
    do i = 1, Flength
       numeric(i) = IBITS(raw(i), 0, 12)
       if (IBITS(raw(i), 13, 1) == 1) then ! check for missing 
          numeric(i) = Fna
       end if
       if (IBITS(raw(i), 14, 1) == 1) THEN ! check for negative
          numeric(i) = -numeric(i)
       end if
       if (IBITS(raw(i), 15, 1) == 1) THEN ! check for clutter
          numeric(i) = Fclutter
       end if
    end do
  
  end subroutine binary_to_num

!-------------------------------------------------------------------------------
! INFO: RX only one Byte 
! NA-Value: 250
! Clutter-Value: 249
!-------------------------------------------------------------------------------

  subroutine binary_to_num_rx(raw, Flength, numeric, Fna, Fclutter) &
                           bind(C, name = "binary_to_num_rx_")
    
    integer(kind = c_int), intent(in)                   ::Flength, Fna, Fclutter
    integer(kind = c_int), intent(in), dimension(Flength) ::raw
    integer(kind = c_int), intent(out), dimension(Flength)::numeric
    integer                                               ::i
 
    ! do cycle over all entries 
    do i=1, Flength
       numeric(i) = IBITS(raw(i), 0, 7)
       if (numeric(i) == 250_c_int) then ! check for missing 
          numeric(i) = Fna
       end if
       if (numeric(i) == 249_c_int) then ! check for clutter 
          numeric(i) = Fclutter
       end if
    end do
  
  end subroutine binary_to_num_rx
  
end module B2N
