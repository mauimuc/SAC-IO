module sac_io
  implicit none

  type :: sac_typ
    real :: DELTA    ,DEPMIN   ,DEPMAX   ,SCALE  ,ODELTA
    real :: B        ,E        ,O        ,A
    real :: T0       ,T1       ,T2       ,T3     ,T4
    real :: T5       ,T6       ,T7       ,T8     ,T9
    real :: F        ,RESP0    ,RESP1    ,RESP2  ,RESP3
    real :: RESP4    ,RESP5    ,RESP6    ,RESP7  ,RESP8
    real :: RESP9    ,STLA     ,STLO     ,STEL   ,STDP
    real :: EVLA     ,EVLO     ,EVEL     ,EVDP   ,MAG
    real :: USER0    ,USER1    ,USER2    ,USER3  ,USER4
    real :: USER5    ,USER6    ,USER7    ,USER8  ,USER9
    real :: DIST     ,AZ       ,BAZ      ,GCARC
    real :: DEPMEN   ,CMPAZ    ,CMPINC ,XMINIMUM
    real :: XMAXIMUM ,YMINIMUM ,YMAXIMUM
    integer :: NZYEAR ,   NZJDAY , NZHOUR , NZMIN ,  NZSEC
    integer :: NZMSEC ,   NVHDR  , NORID  , NEVID ,  NPTS
    integer ::  NWFID ,  NXSIZE  , NYSIZE
    integer :: IFTYPE ,   IDEP   , IZTYPE ,  IINST
    integer :: ISTREG ,   IEVREG , IEVTYP , IQUAL ,  ISYNTH
    integer :: IMAGTYP,   IMAGSRC
    logical :: LEVEN,   LPSPOL,  LOVROK,  LCALDA
    character(8) :: KSTNM
    character(16) :: KEVNM
    character(8) :: KHOLE,  KO,  KA
    character(8) :: KT0,    KT1,     KT2
    character(8) :: KT3,    KT4,     KT5
    character(8) :: KT6,    KT7,     KT8
    character(8) :: KT9,    KF,  KUSER0
    character(8) :: KUSER1,  KUSER2,  KCMPNM
    character(8) :: KNETWK,  KDATRD,  KINST
    real(4), allocatable :: dependent(:), independent(:)
  contains
    procedure :: has_independent_data
    procedure, pass(DTV) :: read_alphanumeric
    procedure, pass(DTV) :: write_alphanumeric
    procedure, pass(DTV) :: read_binary
    GENERIC :: READ(FORMATTED) => read_alphanumeric
    generic :: WRITE(FORMATTED) => write_alphanumeric
    GENERIC :: READ(UNFORMATTED) => read_binary
  end type

contains

  elemental function has_independent_data(this)
    class(sac_typ), intent(in) :: this
    logical :: has_independent_data
    has_independent_data = this%leven.neqv..TRUE.
  end function


  subroutine write_alphanumeric(DTV, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)
    implicit none
    CLASS(sac_typ), INTENT(IN)    :: DTV
    INTEGER,        INTENT(IN)    :: UNIT
    CHARACTER(*),   INTENT(IN)    :: IOTYPE
    INTEGER,        INTENT(IN)    :: V_LIST(:)
    INTEGER,        INTENT(OUT)   :: IOSTAT
    CHARACTER(*),   INTENT(INOUT) :: IOMSG
    real, parameter :: rinternal = -12345.00, runused = -12345.00
    integer, parameter :: iinternal = -12345, iunused = -12345
    INTEGER :: LEVEN, LPSPOL, LOVROK, LCALDA
    character(*), parameter :: fmt='(14(5G15.7/),8(5I10/),(A8,A16/),7(3A8/))'

! logical to integer
        LEVEN  = 0!dtv%LEVEN .eqv..TRUE.
        LPSPOL = 0!dtv%LPSPOL.eqv..TRUE.
        LOVROK = 0!dtv%LOVROK.eqv..TRUE.
        LCALDA = 0!dtv%LCALDA.eqv..TRUE.

    WRITE(UNIT=unit, FMT=fmt) &
! real
        dtv%DELTA, dtv%DEPMIN, dtv%DEPMAX, dtv%SCALE, dtv%ODELTA, &
        dtv%B, dtv%E, dtv%O, dtv%A, rinternal, &
        dtv%T0, dtv%T1, dtv%T2, dtv%T3, dtv%T4, &
        dtv%T5, dtv%T6, dtv%T7, dtv%T8, dtv%T9, &
        dtv%F, dtv%RESP0, dtv%RESP1, dtv%RESP2, dtv%RESP3, &
        dtv%RESP4, dtv%RESP5, dtv%RESP6, dtv%RESP7, dtv%RESP8, &
        dtv%RESP9, dtv%STLA, dtv%STLO, dtv%STEL, dtv%STDP, &
        dtv%EVLA, dtv%EVLO, dtv%EVEL, dtv%EVDP, dtv%MAG, &
        dtv%USER0, dtv%USER1, dtv%USER2, dtv%USER3, dtv%USER4, &
        dtv%USER5, dtv%USER6, dtv%USER7, dtv%USER8, dtv%USER9, &
        dtv%DIST, dtv%AZ, dtv%BAZ, dtv%GCARC, rinternal, &
        rinternal, dtv%DEPMEN, dtv%CMPAZ, dtv%CMPINC, dtv%XMINIMUM, &
        dtv%XMAXIMUM, dtv%YMINIMUM, dtv%YMAXIMUM, runused, runused, &
        runused, runused, runused, runused, runused, &
! integer
        dtv%NZYEAR, dtv%NZJDAY, dtv%NZHOUR, dtv%NZMIN, dtv%NZSEC, &
        dtv%NZMSEC, dtv%NVHDR, dtv%NORID, dtv%NEVID, dtv%NPTS, &
        iinternal, dtv%NWFID, dtv%NXSIZE, dtv%NYSIZE, iunused, &
        dtv%IFTYPE, dtv%IDEP, dtv%IZTYPE, iunused, dtv%IINST, &
        dtv%ISTREG, dtv%IEVREG, dtv%IEVTYP, dtv%IQUAL, dtv%ISYNTH, &
        dtv%IMAGTYP, dtv%IMAGSRC, iunused, iunused, iunused, &
        iunused, iunused, iunused, iunused, iunused, &
! logical
        LEVEN, LPSPOL, LOVROK, LCALDA, iunused, &
! character
        dtv%KSTNM, dtv%KEVNM, &
        dtv%KHOLE, dtv%KO, dtv%KA, &
        dtv%KT0, dtv%KT1, dtv%KT2, &
        dtv%KT3, dtv%KT4, dtv%KT5, &
        dtv%KT6, dtv%KT7, dtv%KT8, &
        dtv%KT9, dtv%KF, dtv%KUSER0, &
        dtv%KUSER1, dtv%KUSER2, dtv%KCMPNM, &
        dtv%KNETWK, dtv%KDATRD, dtv%KINST

        write(unit, FMT='(5G15.7)') dtv%dependent
  end subroutine

  subroutine read_binary(DTV, UNIT, IOSTAT, IOMSG)
    CLASS(sac_typ),    INTENT(INOUT)  :: DTV
    INTEGER,           INTENT(IN)     :: UNIT
    INTEGER,           INTENT(OUT)    :: IOSTAT
    CHARACTER (LEN=*), INTENT(INOUT)  :: IOMSG
    INTEGER :: iinternal, iunused
    real :: runused, rinternal

    read(UNIT=unit) &
! real
        dtv%DELTA,    dtv%DEPMIN, dtv%DEPMAX, dtv%SCALE, dtv%ODELTA, &
        dtv%B,        dtv%E, dtv%O, dtv%A, rinternal, &
        dtv%T0,       dtv%T1, dtv%T2, dtv%T3, dtv%T4, &
        dtv%T5,       dtv%T6, dtv%T7, dtv%T8, dtv%T9, &
        dtv%F,        dtv%RESP0, dtv%RESP1, dtv%RESP2, dtv%RESP3, &
        dtv%RESP4,    dtv%RESP5, dtv%RESP6, dtv%RESP7, dtv%RESP8, &
        dtv%RESP9,    dtv%STLA, dtv%STLO, dtv%STEL, dtv%STDP, &
        dtv%EVLA,     dtv%EVLO, dtv%EVEL, dtv%EVDP, dtv%MAG, &
        dtv%USER0,    dtv%USER1, dtv%USER2, dtv%USER3, dtv%USER4, &
        dtv%USER5,    dtv%USER6, dtv%USER7, dtv%USER8, dtv%USER9, &
        dtv%DIST,     dtv%AZ, dtv%BAZ, dtv%GCARC, rinternal, &
        rinternal,    dtv%DEPMEN, dtv%CMPAZ, dtv%CMPINC, dtv%XMINIMUM, &
        dtv%XMAXIMUM, dtv%YMINIMUM, dtv%YMAXIMUM, runused, runused, &
        runused,      runused, runused, runused, runused, &
! integer
        dtv%NZYEAR,   dtv%NZJDAY,  dtv%NZHOUR, dtv%NZMIN, dtv%NZSEC, &
        dtv%NZMSEC,   dtv%NVHDR,   dtv%NORID, dtv%NEVID, dtv%NPTS, &
        iinternal,    dtv%NWFID,   dtv%NXSIZE, dtv%NYSIZE, iunused, &
        dtv%IFTYPE,   dtv%IDEP,    dtv%IZTYPE, iunused, dtv%IINST, &
        dtv%ISTREG,   dtv%IEVREG,  dtv%IEVTYP, dtv%IQUAL, dtv%ISYNTH, &
        dtv%IMAGTYP,  dtv%IMAGSRC, iunused, iunused, iunused, &
        iunused,      iunused,     iunused, iunused, iunused, &
! logical
        dtv%LEVEN,  dtv%LPSPOL, dtv%LOVROK, dtv%LCALDA, iunused, &
! character
        dtv%KSTNM,  dtv%KEVNM, &
        dtv%KHOLE,  dtv%KO, dtv%KA, &
        dtv%KT0,    dtv%KT1, dtv%KT2, &
        dtv%KT3,    dtv%KT4, dtv%KT5, &
        dtv%KT6,    dtv%KT7, dtv%KT8, &
        dtv%KT9,    dtv%KF, dtv%KUSER0, &
        dtv%KUSER1, dtv%KUSER2, dtv%KCMPNM, &
        dtv%KNETWK, dtv%KDATRD, dtv%KINST

        allocate(dtv%dependent(dtv%npts))
        read(unit) dtv%dependent
  end subroutine

  subroutine read_alphanumeric(DTV, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)
    implicit none
    CLASS(sac_typ), INTENT(INOUT) :: DTV
    INTEGER,        INTENT(IN)    :: UNIT
    CHARACTER(*),   INTENT(IN)    :: IOTYPE
    INTEGER,        INTENT(IN)    :: V_LIST(:)
    INTEGER,        INTENT(OUT)   :: IOSTAT
    CHARACTER(*),   INTENT(INOUT) :: IOMSG
    INTEGER :: LEVEN, LPSPOL, LOVROK, LCALDA, iinternal, iunused
    real :: runused, rinternal

    read(UNIT=unit, FMT=*) &
! real
        dtv%DELTA, dtv%DEPMIN, dtv%DEPMAX, dtv%SCALE, dtv%ODELTA, &
        dtv%B, dtv%E, dtv%O, dtv%A, rinternal, &
        dtv%T0, dtv%T1, dtv%T2, dtv%T3, dtv%T4, &
        dtv%T5, dtv%T6, dtv%T7, dtv%T8, dtv%T9, &
        dtv%F, dtv%RESP0, dtv%RESP1, dtv%RESP2, dtv%RESP3, &
        dtv%RESP4, dtv%RESP5, dtv%RESP6, dtv%RESP7, dtv%RESP8, &
        dtv%RESP9, dtv%STLA, dtv%STLO, dtv%STEL, dtv%STDP, &
        dtv%EVLA, dtv%EVLO, dtv%EVEL, dtv%EVDP, dtv%MAG, &
        dtv%USER0, dtv%USER1, dtv%USER2, dtv%USER3, dtv%USER4, &
        dtv%USER5, dtv%USER6, dtv%USER7, dtv%USER8, dtv%USER9, &
        dtv%DIST, dtv%AZ, dtv%BAZ, dtv%GCARC, rinternal, &
        rinternal, dtv%DEPMEN, dtv%CMPAZ, dtv%CMPINC, dtv%XMINIMUM, &
        dtv%XMAXIMUM, dtv%YMINIMUM, dtv%YMAXIMUM, runused, runused, &
        runused, runused, runused, runused, runused, &
! integer
        dtv%NZYEAR, dtv%NZJDAY, dtv%NZHOUR, dtv%NZMIN, dtv%NZSEC, &
        dtv%NZMSEC, dtv%NVHDR, dtv%NORID, dtv%NEVID, dtv%NPTS, &
        iinternal, dtv%NWFID, dtv%NXSIZE, dtv%NYSIZE, iunused, &
        dtv%IFTYPE, dtv%IDEP, dtv%IZTYPE, iunused, dtv%IINST, &
        dtv%ISTREG, dtv%IEVREG, dtv%IEVTYP, dtv%IQUAL, dtv%ISYNTH, &
        dtv%IMAGTYP, dtv%IMAGSRC, iunused, iunused, iunused, &
        iunused, iunused, iunused, iunused, iunused, &
! logical
        LEVEN, LPSPOL, LOVROK, LCALDA, iunused, &
! character
        dtv%KSTNM, dtv%KEVNM, &
        dtv%KHOLE, dtv%KO, dtv%KA, &
        dtv%KT0, dtv%KT1, dtv%KT2, &
        dtv%KT3, dtv%KT4, dtv%KT5, &
        dtv%KT6, dtv%KT7, dtv%KT8, &
        dtv%KT9, dtv%KF, dtv%KUSER0, &
        dtv%KUSER1, dtv%KUSER2, dtv%KCMPNM, &
        dtv%KNETWK, dtv%KDATRD, dtv%KINST

! Integer to logical
        dtv%LEVEN  = LEVEN.ne.0
        dtv%LPSPOL = LPSPOL.ne.0
        dtv%LOVROK = LOVROK.ne.0
        dtv%LCALDA = LCALDA.ne.0
        allocate(dtv%dependent(dtv%npts))
        read(unit,FMT='(5G15.7)') dtv%dependent
  end subroutine

end module sac_io

