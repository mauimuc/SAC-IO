MODULE sac_io
  IMPLICIT NONE

  TYPE :: sac_typ
    REAL :: DELTA    ,DEPMIN   ,DEPMAX   ,SCALE  ,ODELTA
    REAL :: B        ,E        ,O        ,A
    REAL :: T0       ,T1       ,T2       ,T3     ,T4
    REAL :: T5       ,T6       ,T7       ,T8     ,T9
    REAL :: F        ,RESP0    ,RESP1    ,RESP2  ,RESP3
    REAL :: RESP4    ,RESP5    ,RESP6    ,RESP7  ,RESP8
    REAL :: RESP9    ,STLA     ,STLO     ,STEL   ,STDP
    REAL :: EVLA     ,EVLO     ,EVEL     ,EVDP   ,MAG
    REAL :: USER0    ,USER1    ,USER2    ,USER3  ,USER4
    REAL :: USER5    ,USER6    ,USER7    ,USER8  ,USER9
    REAL :: DIST     ,AZ       ,BAZ      ,GCARC
    REAL :: DEPMEN   ,CMPAZ    ,CMPINC ,XMINIMUM
    REAL :: XMAXIMUM ,YMINIMUM ,YMAXIMUM
    INTEGER :: NZYEAR ,   NZJDAY , NZHOUR , NZMIN ,  NZSEC
    INTEGER :: NZMSEC ,   NVHDR  , NORID  , NEVID ,  NPTS
    INTEGER ::  NWFID ,  NXSIZE  , NYSIZE
    INTEGER :: IFTYPE ,   IDEP   , IZTYPE ,  IINST
    INTEGER :: ISTREG ,   IEVREG , IEVTYP , IQUAL ,  ISYNTH
    INTEGER :: IMAGTYP,   IMAGSRC
    LOGICAL :: LEVEN,   LPSPOL,  LOVROK,  LCALDA
    CHARACTER(LEN=8) :: KSTNM
    CHARACTER(LEN=16) :: KEVNM
    CHARACTER(LEN=8) :: KHOLE,  KO,  KA
    CHARACTER(LEN=8) :: KT0,    KT1,     KT2
    CHARACTER(LEN=8) :: KT3,    KT4,     KT5
    CHARACTER(LEN=8) :: KT6,    KT7,     KT8
    CHARACTER(LEN=8) :: KT9,    KF,  KUSER0
    CHARACTER(LEN=8) :: KUSER1,  KUSER2,  KCMPNM
    CHARACTER(LEN=8) :: KNETWK,  KDATRD,  KINST
    REAL(4), ALLOCATABLE :: depENDent(:), indepENDent(:)
  CONTAINS
    PROCEDURE :: has_indepENDent_data
    PROCEDURE, PASS(DTV) :: READ_alphanumeric
    PROCEDURE, PASS(DTV) :: write_alphanumeric
    PROCEDURE, PASS(DTV) :: READ_binary
    GENERIC :: READ(FORMATTED) => READ_alphanumeric
    GENERIC :: WRITE(FORMATTED) => write_alphanumeric
    GENERIC :: READ(UNFORMATTED) => READ_binary
  END TYPE

  ENUM, BIND(C)
    ENUMERATOR :: itime   =01
    ENUMERATOR :: irlim   =02
    ENUMERATOR :: iamph   =03
    ENUMERATOR :: ixy     =04
    ENUMERATOR :: iunkn   =05
    ENUMERATOR :: idisp   =06
    ENUMERATOR :: ivel    =07
    ENUMERATOR :: iacc    =08
    ENUMERATOR :: ib      =09
    ENUMERATOR :: iday    =10
    ENUMERATOR :: io      =11
    ENUMERATOR :: ia      =12
    ENUMERATOR :: it0     =13
    ENUMERATOR :: it1     =14
    ENUMERATOR :: it2     =15
    ENUMERATOR :: it3     =16
    ENUMERATOR :: it4     =17
    ENUMERATOR :: it5     =18
    ENUMERATOR :: it6     =19
    ENUMERATOR :: it7     =20
    ENUMERATOR :: it8     =21
    ENUMERATOR :: it9     =22
    ENUMERATOR :: iradnv  =23
    ENUMERATOR :: itannv  =24
    ENUMERATOR :: iradev  =25
    ENUMERATOR :: itanev  =26
    ENUMERATOR :: inorth  =27
    ENUMERATOR :: ieast   =28
    ENUMERATOR :: ihorza  =29
    ENUMERATOR :: idown   =30
    ENUMERATOR :: iup     =31
    ENUMERATOR :: illlbb  =32
    ENUMERATOR :: iwwsn1  =33
    ENUMERATOR :: iwwsn2  =34
    ENUMERATOR :: ihglp   =35
    ENUMERATOR :: isro    =36
    ENUMERATOR :: inucl   =37
    ENUMERATOR :: ipren   =38
    ENUMERATOR :: ipostn  =39
    ENUMERATOR :: iquake  =40
    ENUMERATOR :: ipreq   =41
    ENUMERATOR :: ipostq  =42
    ENUMERATOR :: ichem   =43
    ENUMERATOR :: iother  =44
    ENUMERATOR :: igood   =45
    ENUMERATOR :: iglch   =46
    ENUMERATOR :: idrop   =47
    ENUMERATOR :: ilowsn  =48
    ENUMERATOR :: irldta  =49
    ENUMERATOR :: ivolts  =50
    ENUMERATOR :: imb     =52
    ENUMERATOR :: ims     =53
    ENUMERATOR :: iml     =54
    ENUMERATOR :: imw     =55
    ENUMERATOR :: imd     =56
    ENUMERATOR :: imx     =57
    ENUMERATOR :: ineic   =58
    ENUMERATOR :: ipdeq   =59
    ENUMERATOR :: ipdew   =60
    ENUMERATOR :: ipde    =61
    ENUMERATOR :: iisc    =62
    ENUMERATOR :: ireb    =63
    ENUMERATOR :: iusgs   =64
    ENUMERATOR :: ibrk    =65
    ENUMERATOR :: icaltech=66
    ENUMERATOR :: illnl   =67
    ENUMERATOR :: ievloc  =68
    ENUMERATOR :: ijsop   =69
    ENUMERATOR :: iuser   =70
    ENUMERATOR :: iunknown=71
    ENUMERATOR :: iqb     =72
    ENUMERATOR :: iqb1    =73
    ENUMERATOR :: iqb2    =74
    ENUMERATOR :: iqbx    =75
    ENUMERATOR :: iqmt    =76
    ENUMERATOR :: ieq     =77
    ENUMERATOR :: ieq1    =78
    ENUMERATOR :: ieq2    =79
    ENUMERATOR :: ime     =80
    ENUMERATOR :: iex     =81
    ENUMERATOR :: inu     =82
    ENUMERATOR :: inc     =83
    ENUMERATOR :: io_     =84
    ENUMERATOR :: il      =85
    ENUMERATOR :: ir      =86
    ENUMERATOR :: it      =87
    ENUMERATOR :: iu      =88
    ENUMERATOR :: ieq3    =89
    ENUMERATOR :: ieq0    =90
    ENUMERATOR :: iex0    =91
    ENUMERATOR :: iqc     =92
    ENUMERATOR :: iqb0    =93
    ENUMERATOR :: igey    =94
    ENUMERATOR :: ilit    =95
    ENUMERATOR :: imet    =96
    ENUMERATOR :: iodor   =97
    ENUMERATOR :: ios     =103
  END ENUM

CONTAINS

  ELEMENTAL FUNCTION has_indepENDent_data(this)
    CLASS(sac_typ), INTENT(in) :: this
    LOGICAL :: has_indepENDent_data
    has_indepENDent_data = this%leven.NEQV..TRUE.
  END FUNCTION


  SUBROUTINE write_alphanumeric(DTV, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)
    IMPLICIT NONE
    CLASS(sac_typ), INTENT(IN)    :: DTV
    INTEGER,        INTENT(IN)    :: UNIT
    CHARACTER(*),   INTENT(IN)    :: IOTYPE
    INTEGER,        INTENT(IN)    :: V_LIST(:)
    INTEGER,        INTENT(OUT)   :: IOSTAT
    CHARACTER(*),   INTENT(INOUT) :: IOMSG
    REAL, PARAMETER :: rinternal = -12345.00, runused = -12345.00
    INTEGER, PARAMETER :: iinternal = -12345, iunused = -12345
    INTEGER :: LEVEN, LPSPOL, LOVROK, LCALDA
    CHARACTER(LEN=*), PARAMETER :: fmt='(14(5G15.7/),8(5I10/),(A8,A16/),7(3A8/))'

! LOGICAL to INTEGER
        LEVEN  = 0!dtv%LEVEN .eqv..TRUE.
        LPSPOL = 0!dtv%LPSPOL.eqv..TRUE.
        LOVROK = 0!dtv%LOVROK.eqv..TRUE.
        LCALDA = 0!dtv%LCALDA.eqv..TRUE.

    WRITE(UNIT=unit, FMT=fmt) &
! Real
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
! Integer
        dtv%NZYEAR, dtv%NZJDAY, dtv%NZHOUR, dtv%NZMIN, dtv%NZSEC, &
        dtv%NZMSEC, dtv%NVHDR, dtv%NORID, dtv%NEVID, dtv%NPTS, &
        iinternal, dtv%NWFID, dtv%NXSIZE, dtv%NYSIZE, iunused, &
        dtv%IFTYPE, dtv%IDEP, dtv%IZTYPE, iunused, dtv%IINST, &
        dtv%ISTREG, dtv%IEVREG, dtv%IEVTYP, dtv%IQUAL, dtv%ISYNTH, &
        dtv%IMAGTYP, dtv%IMAGSRC, iunused, iunused, iunused, &
        iunused, iunused, iunused, iunused, iunused, &
! LOGICAL
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

        WRITE(UNIT=unit, FMT='(5G15.7)') dtv%depENDent
  END SUBROUTINE

  SUBROUTINE READ_binary(DTV, UNIT, IOSTAT, IOMSG)
    CLASS(sac_typ),    INTENT(INOUT)  :: DTV
    INTEGER,           INTENT(IN)     :: UNIT
    INTEGER,           INTENT(OUT)    :: IOSTAT
    CHARACTER (LEN=*), INTENT(INOUT)  :: IOMSG
    INTEGER :: iinternal, iunused
    REAL :: runused, rinternal

    READ(UNIT=unit) &
! REAL
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
! Integer
        dtv%NZYEAR,   dtv%NZJDAY,  dtv%NZHOUR, dtv%NZMIN, dtv%NZSEC, &
        dtv%NZMSEC,   dtv%NVHDR,   dtv%NORID, dtv%NEVID, dtv%NPTS, &
        iinternal,    dtv%NWFID,   dtv%NXSIZE, dtv%NYSIZE, iunused, &
        dtv%IFTYPE,   dtv%IDEP,    dtv%IZTYPE, iunused, dtv%IINST, &
        dtv%ISTREG,   dtv%IEVREG,  dtv%IEVTYP, dtv%IQUAL, dtv%ISYNTH, &
        dtv%IMAGTYP,  dtv%IMAGSRC, iunused, iunused, iunused, &
        iunused,      iunused,     iunused, iunused, iunused, &
! LOGICAL
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

        ALLOCATE(dtv%depENDent(dtv%npts))
        READ(UNIT=unit) dtv%depENDent
  END SUBROUTINE

  SUBROUTINE READ_alphanumeric(DTV, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)
    IMPLICIT NONE
    CLASS(sac_typ), INTENT(INOUT) :: DTV
    INTEGER,        INTENT(IN)    :: UNIT
    CHARACTER(*),   INTENT(IN)    :: IOTYPE
    INTEGER,        INTENT(IN)    :: V_LIST(:)
    INTEGER,        INTENT(OUT)   :: IOSTAT
    CHARACTER(*),   INTENT(INOUT) :: IOMSG
    INTEGER :: LEVEN, LPSPOL, LOVROK, LCALDA, iinternal, iunused
    REAL :: runused, rinternal

    READ(UNIT=unit, FMT=*) &
! Real
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
! Integer
        dtv%NZYEAR, dtv%NZJDAY, dtv%NZHOUR, dtv%NZMIN, dtv%NZSEC, &
        dtv%NZMSEC, dtv%NVHDR, dtv%NORID, dtv%NEVID, dtv%NPTS, &
        iinternal, dtv%NWFID, dtv%NXSIZE, dtv%NYSIZE, iunused, &
        dtv%IFTYPE, dtv%IDEP, dtv%IZTYPE, iunused, dtv%IINST, &
        dtv%ISTREG, dtv%IEVREG, dtv%IEVTYP, dtv%IQUAL, dtv%ISYNTH, &
        dtv%IMAGTYP, dtv%IMAGSRC, iunused, iunused, iunused, &
        iunused, iunused, iunused, iunused, iunused, &
! Logical
        LEVEN, LPSPOL, LOVROK, LCALDA, iunused, &
! Character
        dtv%KSTNM, dtv%KEVNM, &
        dtv%KHOLE, dtv%KO, dtv%KA, &
        dtv%KT0, dtv%KT1, dtv%KT2, &
        dtv%KT3, dtv%KT4, dtv%KT5, &
        dtv%KT6, dtv%KT7, dtv%KT8, &
        dtv%KT9, dtv%KF, dtv%KUSER0, &
        dtv%KUSER1, dtv%KUSER2, dtv%KCMPNM, &
        dtv%KNETWK, dtv%KDATRD, dtv%KINST

! Integer to Logical
        dtv%LEVEN  = LEVEN.NE.0
        dtv%LPSPOL = LPSPOL.NE.0
        dtv%LOVROK = LOVROK.NE.0
        dtv%LCALDA = LCALDA.NE.0
        ALLOCATE(dtv%depENDent(dtv%npts))
        READ(UNIT=unit,FMT='(5G15.7)') dtv%depENDent
  END SUBROUTINE

END MODULE sac_io

