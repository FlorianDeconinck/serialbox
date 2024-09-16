!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license.
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------
!
!+ This module contains the interface for serializing idx_d3-blocked fields
!
!------------------------------------------------------------------------------

MODULE utils_ppser_buffered

!------------------------------------------------------------------------------
!
! Description:
!
!   This module contains subroutines which allow to serialize idx_d3-blocked
!   fields using internal buffering of the data before flushing them
!   off to serialbox. It uses buffering of the fields and fields are
!   automatically flushed once all data has been written.
!
! Current Code Owner: Vulcan Inc, Oliver Fuhrer
!  email:  oliverf@vulcan.com
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!============================================================================

USE iso_c_binding
USE m_serialize
USE utils_ppser
      
IMPLICIT NONE

PUBLIC :: &
  fs_write_buffered, finalize_buffered

PRIVATE

  TYPE buffer_type
    LOGICAL :: in_use = .FALSE.                   ! is this buffers in use?
    TYPE(C_PTR) :: serializer                     ! serializer object associated with buffers
    CHARACTER(LEN=256) :: savepoint_name
    CHARACTER(LEN=256) :: fieldname
    INTEGER :: D1 = 0, D2 = 0, D3 = 0, D4 = 0     ! dimensions of 3d-field to be serialized
    INTEGER :: call_index = 0                     ! track multiple kbuffers for the same savepoint name 
                                                  ! and field being filled in parallel  
    LOGICAL :: has_minushalos, has_plushalos
    INTEGER :: minushalos(3), plushalos(3)
    INTEGER :: field_type = 0                     ! 0 = not used, 1 = int, 2 = r4, 3 = r8
    INTEGER, ALLOCATABLE :: buffer_i4(:,:,:,:)
    REAL(KIND=C_FLOAT), ALLOCATABLE :: buffer_r4(:,:,:,:)
    REAL(KIND=C_DOUBLE), ALLOCATABLE :: buffer_r8(:,:,:,:)
    LOGICAL, ALLOCATABLE :: ok(:,:,:,:)           ! has this index been written?
  END TYPE buffer_type

  INTEGER, PARAMETER   :: max_buffer = 65535      ! increase in case you get errors
  TYPE(buffer_type) :: buffers(max_buffer)     ! array containing buffers

  ! overload interface for different types and dimensions
  INTERFACE fs_write_buffered
      MODULE PROCEDURE fs_write_buffered_i4
      MODULE PROCEDURE fs_write_buffered_r4
      MODULE PROCEDURE fs_write_buffered_r8
  END INTERFACE

  LOGICAL :: first_call = .TRUE.                  ! used for initialization

  LOGICAL, PARAMETER :: debug = .FALSE.           ! get verbose messaging

CONTAINS

!============================================================================

! initialize buffering: makes sure all buffers are set to not in use
SUBROUTINE init_buffered()
  IMPLICIT NONE

  INTEGER :: idx

  IF (debug) THEN
    WRITE(0,*) 'DEBUG init_buffered'
  END IF

  first_call = .FALSE.

  DO idx = 1, max_buffer
    buffers(idx)%in_use = .FALSE.
    buffers(idx)%fieldname = ""
    buffers(idx)%savepoint_name = ""
    buffers(idx)%call_index = 0
  END DO

END SUBROUTINE init_buffered

!============================================================================

! Flush all buffers, irrigardless or their write status
SUBROUTINE flush_all_buffers()
  IMPLICIT NONE

  INTEGER :: idx
  DO idx = 1, max_buffer
    buffers(idx)%ok(:,:,:,:) = .TRUE.

    ! SELECT CASE (field_type)
    !   CASE(1)
    !     ALLOCATE(buffers(buffer_id)%buffer_i4(D1, D2, D3, D4))
    !   CASE(2)
    !     ALLOCATE(buffers(buffer_id)%buffer_r4(D1, D2, D3, D4))
    !   CASE(3)
    !     ALLOCATE(buffers(buffer_id)%buffer_r8(D1, D2, D3, D4))
    !   CASE DEFAULT
    !     WRITE(0,*) 'ERROR in utils_ppser_buffered: unsupported field_type encountered'
    ! END SELECT

  END DO
END SUBROUTINE

!============================================================================

! finalize buffering: should be called once all buffers have been flushed
SUBROUTINE finalize_buffered()
  IMPLICIT NONE

  INTEGER :: idx

  IF (debug) THEN
    WRITE(0,*) 'DEBUG finalize_buffered'
  END IF

  DO idx = 1, max_buffer
    IF (buffers(idx)%in_use) THEN
      WRITE(0,*) 'ERROR in utils_ppser_buffered: finalize called before all buffers have been flushed'
      WRITE(0,*) 'savepoint_name: ', TRIM(buffers(idx)%savepoint_name)
      WRITE(0,*) 'name of partially empty field: ', TRIM(buffers(idx)%fieldname)
      WRITE(0,*) 'Indices filled boolean: ', buffers(idx)%ok(:,:,:,:)
      STOP
    END IF
    buffers(idx)%fieldname = ""
    buffers(idx)%savepoint_name = ""
  END DO

  first_call = .TRUE.

END SUBROUTINE finalize_buffered

!============================================================================

! overloads fs_write_buffered: version for r8 floats and 3d fields
SUBROUTINE fs_write_buffered_r8(serializer, savepoint, fieldname, scalar, &
                                idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4, &
                                mode, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: scalar
  INTEGER, INTENT(IN)                     :: idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4
  INTEGER, INTENT(IN)                     :: mode
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! local vars
  INTEGER :: buffer_id = 0
  INTEGER :: field_type = 3

  ! do nothing in case serialization is switched off
  IF (.NOT. (fs_is_serialization_on())) THEN
    RETURN
  ENDIF

  ! find buffer_id and check if a buffers slot was found
  call setup_buffer(buffer_id, serializer, savepoint, fieldname, field_type, &
                    idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4, &
                    mode, minushalos, plushalos)

  ! store data
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_buffered_r8: store data'
  END IF
  buffers(buffer_id)%buffer_r8(idx_d1,idx_d2,idx_d3,idx_d4) = scalar
  buffers(buffer_id)%ok(idx_d1,idx_d2,idx_d3,idx_d4) = .TRUE.

  ! write if we are complete
  IF (ALL(buffers(buffer_id)%ok(:,:,:,:))) THEN
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_buffered_r8: flush data'
    END IF
    IF (buffers(buffer_id)%has_minushalos) THEN
      IF (buffers(buffer_id)%has_plushalos) THEN
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,:,:,1), &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,:,1,1), &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,1,1,1), &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8, &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        END IF
      ELSE
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,:,:,1), &
            minushalos=buffers(buffer_id)%minushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,:,1,1), &
            minushalos=buffers(buffer_id)%minushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,1,1,1), &
            minushalos=buffers(buffer_id)%minushalos)
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8, &
          minushalos=buffers(buffer_id)%minushalos)
        END IF
      END IF
    ELSE
      IF (buffers(buffer_id)%has_plushalos) THEN
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,:,:,1), &
            plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,:,1,1), &
            plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,1,1,1), &
            plushalos=buffers(buffer_id)%plushalos)
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8, &
            plushalos=buffers(buffer_id)%plushalos)
        END IF
      ELSE
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,:,:,1))
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,:,1,1))
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8(:,1,1,1))
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r8)
        END IF
      END IF
    END IF

    CALL destroy_buffered(buffer_id)
  END IF

END SUBROUTINE fs_write_buffered_r8

!============================================================================

! overloads fs_write_buffered: version for r4 floats and 3d fields
SUBROUTINE fs_write_buffered_r4(serializer, savepoint, fieldname, scalar, &
                                  idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4, &
                                  mode, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET  :: scalar
  INTEGER, INTENT(IN)                     :: idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4
  INTEGER, INTENT(IN)                     :: mode
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! local vars
  INTEGER :: buffer_id = 0
  INTEGER :: field_type = 2

  ! do nothing in case serialization is switched off
  IF (.NOT. (fs_is_serialization_on())) THEN
    RETURN
  ENDIF

  ! find buffer_id and check if a buffers slot was found
  call setup_buffer(buffer_id, serializer, savepoint, fieldname, field_type, &
                    idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4, &
                    mode, minushalos, plushalos)

  ! store data
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_buffered_r4: store data'
  END IF
  buffers(buffer_id)%buffer_r4(idx_d1,idx_d2,idx_d3,idx_d4) = scalar
  buffers(buffer_id)%ok(idx_d1,idx_d2,idx_d3,idx_d4) = .TRUE.

  ! write if we are complete
  IF (ALL(buffers(buffer_id)%ok(:,:,:,:))) THEN
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_buffered_r4: flush data'
    END IF
    IF (buffers(buffer_id)%has_minushalos) THEN
      IF (buffers(buffer_id)%has_plushalos) THEN
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,:,:,1), &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,:,1,1), &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,1,1,1), &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4, &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        END IF
      ELSE
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,:,:,1), &
            minushalos=buffers(buffer_id)%minushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,:,1,1), &
            minushalos=buffers(buffer_id)%minushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,1,1,1), &
            minushalos=buffers(buffer_id)%minushalos)
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4, &
          minushalos=buffers(buffer_id)%minushalos)
        END IF
      END IF
    ELSE
      IF (buffers(buffer_id)%has_plushalos) THEN
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,:,:,1), &
            plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,:,1,1), &
            plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,1,1,1), &
            plushalos=buffers(buffer_id)%plushalos)
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4, &
            plushalos=buffers(buffer_id)%plushalos)
        END IF
      ELSE
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,:,:,1))
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,:,1,1))
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4(:,1,1,1))
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_r4)
        END IF
      END IF
    END IF

    CALL destroy_buffered(buffer_id)
  END IF

END SUBROUTINE fs_write_buffered_r4

!============================================================================

! overloads fs_write_buffered: version for i4 integers and 3d fields
SUBROUTINE fs_write_buffered_i4(serializer, savepoint, fieldname, scalar, &
                                  idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4, &
                                  mode, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: scalar
  INTEGER, INTENT(IN)                     :: idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4, mode
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! local vars
  INTEGER :: buffer_id = 0
  INTEGER :: field_type = 1

  ! do nothing in case serialization is switched off
  IF (.NOT. (fs_is_serialization_on())) THEN
    RETURN
  ENDIF

  ! find buffer_id and check if a buffers slot was found
  call setup_buffer(buffer_id, serializer, savepoint, fieldname, field_type, &
                    idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4, &
                    mode, minushalos, plushalos)

  ! store data
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_buffered_i4: store data'
  END IF
  buffers(buffer_id)%buffer_i4(idx_d1,idx_d2,idx_d3,idx_d4) = scalar
  buffers(buffer_id)%ok(idx_d1,idx_d2,idx_d3,idx_d4) = .TRUE.

  ! write if we are complete
  IF (ALL(buffers(buffer_id)%ok(:,:,:,:))) THEN
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_buff_3d_i4ers: flush data'
    END IF
    IF (buffers(buffer_id)%has_minushalos) THEN
      IF (buffers(buffer_id)%has_plushalos) THEN
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,:,:,1), &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,:,1,1), &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,1,1,1), &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4, &
            minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
        END IF
      ELSE
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,:,:,1), &
            minushalos=buffers(buffer_id)%minushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,:,1,1), &
            minushalos=buffers(buffer_id)%minushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,1,1,1), &
            minushalos=buffers(buffer_id)%minushalos)
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4, &
          minushalos=buffers(buffer_id)%minushalos)
        END IF
      END IF
    ELSE
      IF (buffers(buffer_id)%has_plushalos) THEN
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,:,:,1), &
            plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,:,1,1), &
            plushalos=buffers(buffer_id)%plushalos)
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,1,1,1), &
            plushalos=buffers(buffer_id)%plushalos)
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4, &
            plushalos=buffers(buffer_id)%plushalos)
        END IF
      ELSE
        IF (D4==1 .AND. D3 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,:,:,1))
        ELSE IF(D4==1 .AND. D3==1 .AND. D2 > 1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,:,1,1))
        ELSE IF(D4==1 .AND. D3==1 .AND. D2==1) THEN
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4(:,1,1,1))
        ELSE
          CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buffer_i4)
        END IF
      END IF
    END IF

    CALL destroy_buffered(buffer_id)
  END IF

END SUBROUTINE fs_write_buffered_i4

!============================================================================

! checks if a buffers exists for this fields and if yes, checks consistency with
! current request. if not, it creates a new buffers.
SUBROUTINE setup_buffer(buffer_id, serializer, savepoint, fieldname, field_type, &
                        idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4,&
                        mode, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: idx_d1, D1, idx_d2, D2, idx_d3, D3, idx_d4, D4
  INTEGER, INTENT(IN)                     :: mode, field_type
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)
  INTEGER, INTENT(OUT)                    :: buffer_id

  ! local vars
  INTEGER :: call_index = 0
      
  IF (debug) THEN
    WRITE(0,*) 'DEBUG setup_buffer: savepoint=', TRIM(savepoint%savepoint_name)
    WRITE(0,*) 'DEBUG setup_buffer: fieldname=', TRIM(fieldname)
    WRITE(0,*) 'DEBUG setup_buffer: idx_d1=', idx_d1, '(', D1, ')'
    WRITE(0,*) 'DEBUG setup_buffer: idx_d2=', idx_d2, '(', D2, ')'
    WRITE(0,*) 'DEBUG setup_buffer: idx_d3=', idx_d3, '(', D3, ')'
    WRITE(0,*) 'DEBUG setup_buffer: idx_d3=', idx_d4, '(', D4, ')'
  END IF

  ! ppser mode numbers do not align with m_serialize constants....
  IF ( mode /= PPSER_MODE_WRITE ) THEN
    WRITE(0,*) 'ERROR, can only use kbuffer in write mode'
    STOP
  END IF

  ! initialize if this is the first call
  IF ( first_call ) THEN
    CALL init_buffered()
  END IF

  ! find ID if it already exists
  CALL find_buffered_id(fieldname, savepoint, idx_d1, idx_d2, idx_d3, idx_d4, buffer_id, call_index)
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_buffered_r8: find buffer_id=', buffer_id
  END IF

  ! check if a buffers slot was found
  IF ( buffer_id == 0 ) THEN
    ! no, so create a new buffers
    CALL get_free_buffered_id(buffer_id)
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_buffered: buffer_id=', buffer_id
    END IF
    CALL create_buffered(buffer_id, serializer, savepoint, fieldname, field_type, &
                        D1, D2, D3, D4, call_index, minushalos, plushalos)
  ELSE
    ! yes, so check for consistency of current request with stored metadata
    CALL check_buffered(buffer_id, serializer, savepoint, fieldname, field_type, &
                        D1, D2, D3, D4, idx_d1, idx_d2, idx_d3, idx_d4, minushalos, plushalos)
  END IF

END SUBROUTINE setup_buffer

!============================================================================


! create a new buffers (allocate memory, store metadata)
SUBROUTINE create_buffered(buffer_id, serializer, savepoint, fieldname, field_type, &
                          D1, D2, D3, D4, call_index, minushalos, plushalos)
  IMPLICIT NONE

  INTEGER, INTENT(IN)                     :: buffer_id
  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: field_type
  INTEGER, INTENT(IN)                     :: D1, D2, D3, D4
  INTEGER, INTENT(IN)                     :: call_index
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! debug information
  IF (debug) THEN
    WRITE(0,*) 'DEBUG create_buffered: buffer_id=', buffer_id
    WRITE(0,*) 'DEBUG create_buffered: savepoint=', TRIM(savepoint%savepoint_name)
    WRITE(0,*) 'DEBUG create_buffered: fieldname=', TRIM(fieldname)
    WRITE(0,*) 'DEBUG create_buffered: field_type=', field_type
    WRITE(0,*) 'DEBUG create_buffered: D1,D2,D3,D4=', D1, D2, D3, D4
  END IF

  ! security check
  IF (buffer_id < 1 .OR. buffer_id > max_buffer) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: illegal buffer_id encountered'
    STOP
  END IF
  IF (buffers(buffer_id)%in_use) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: create called for buffers already in use'
    STOP
  END IF

  ! store metadata
  buffers(buffer_id)%in_use = .TRUE.
  buffers(buffer_id)%serializer = serializer%serializer_ptr
  buffers(buffer_id)%call_index = call_index
  buffers(buffer_id)%savepoint_name = TRIM(savepoint%savepoint_name)
  buffers(buffer_id)%fieldname = TRIM(fieldname)
  buffers(buffer_id)%D1 = D1
  buffers(buffer_id)%D2 = D2
  buffers(buffer_id)%D3 = D3
  buffers(buffer_id)%D4 = D4
  IF (PRESENT(minushalos)) THEN
    buffers(buffer_id)%minushalos = minushalos
    buffers(buffer_id)%has_minushalos = .TRUE.
  ELSE
    buffers(buffer_id)%has_minushalos = .FALSE.
  ENDIF
  IF (PRESENT(plushalos)) THEN
    buffers(buffer_id)%plushalos = plushalos
    buffers(buffer_id)%has_plushalos = .TRUE.
  ELSE
    buffers(buffer_id)%has_plushalos = .FALSE.
  ENDIF
  buffers(buffer_id)%field_type = field_type

  ! allocate memory
  SELECT CASE (field_type)
    CASE(1)
      ALLOCATE(buffers(buffer_id)%buffer_i4(D1, D2, D3, D4))
    CASE(2)
      ALLOCATE(buffers(buffer_id)%buffer_r4(D1, D2, D3, D4))
    CASE(3)
      ALLOCATE(buffers(buffer_id)%buffer_r8(D1, D2, D3, D4))
    CASE DEFAULT
      WRITE(0,*) 'ERROR in utils_ppser_buffered: unsupported field_type encountered'
  END SELECT
  ALLOCATE(buffers(buffer_id)%ok(D1, D2, D3, D4))

  ! make sure all grid points as unwritten
  buffers(buffer_id)%ok(:,:,:,:) = .FALSE.

END SUBROUTINE create_buffered

!============================================================================

! release a buffers (release memory, reset metadata)
SUBROUTINE destroy_buffered(buffer_id)
  IMPLICIT NONE

  INTEGER, INTENT(IN)                     :: buffer_id
  INTEGER                                 :: idx
  ! debug information
  IF (debug) THEN
    WRITE(0,*) 'DEBUG destroy_buffered: buffer_id=', buffer_id
    WRITE(0,*) 'DEBUG destroy_buffered: savepoint=', TRIM(buffers(buffer_id)%savepoint_name)
    WRITE(0,*) 'DEBUG destroy_buffered: fieldname=', TRIM(buffers(buffer_id)%fieldname)
  END IF

  ! security check
  IF (buffer_id < 1 .OR. buffer_id > max_buffer) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: illegal buffer_id encountered'
    STOP
  END IF
  IF (.NOT. buffers(buffer_id)%in_use) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: destroy called for buffers not in use'
    STOP
  END IF

  ! update the call_index of the rest of the related kbuffers     
  DO idx = 1, max_buffer
    IF (idx /= buffer_id .and. buffers(idx)%in_use) THEN 
      IF (TRIM(buffers(buffer_id)%fieldname) == TRIM(buffers(idx)%fieldname)) THEN
        IF (TRIM( buffers(buffer_id)%savepoint_name) == TRIM(buffers(idx)%savepoint_name)) THEN
          ! This should not be needed, calls should stay in order...
          IF (buffers(idx)%call_index >  buffers(buffer_id)%call_index) THEN 
            buffers(idx)%call_index = buffers(idx)%call_index - 1
          END IF
        END IF
      END IF
    END IF
  END DO

  ! release memory
  SELECT CASE (buffers(buffer_id)%field_type)
    CASE(1)
      DEALLOCATE(buffers(buffer_id)%buffer_i4)
    CASE(2)
      DEALLOCATE(buffers(buffer_id)%buffer_r4)
    CASE(3)
      DEALLOCATE(buffers(buffer_id)%buffer_r8)
    CASE DEFAULT
      WRITE(0,*) 'ERROR in utils_ppser_buffered: unsupported field_type encountered in destroy'
  END SELECT
  DEALLOCATE(buffers(buffer_id)%ok)

  ! reset metadata
  buffers(buffer_id)%in_use = .FALSE.
  buffers(buffer_id)%serializer = C_NULL_PTR
  buffers(buffer_id)%savepoint_name = ""
  buffers(buffer_id)%call_index = 0
  buffers(buffer_id)%fieldname = ""
  buffers(buffer_id)%field_type = 0
  buffers(buffer_id)%D1 = 0
  buffers(buffer_id)%D2 = 0
  buffers(buffer_id)%D3 = 0
  buffers(buffer_id)%D4 = 0
  IF (buffers(buffer_id)%has_minushalos) THEN
    buffers(buffer_id)%minushalos = (/0, 0, 0/)
  ENDIF
  buffers(buffer_id)%has_minushalos = .FALSE.
  IF (buffers(buffer_id)%has_plushalos) THEN
    buffers(buffer_id)%plushalos = (/0, 0, 0/)
  ENDIF
  buffers(buffer_id)%has_plushalos = .FALSE.

END SUBROUTINE destroy_buffered

!============================================================================

! check consistency of current request with metadata stored in buffers
SUBROUTINE check_buffered(buffer_id, serializer, savepoint, fieldname, field_type, &
                         D1, D2, D3, D4, idx_d1, idx_d2, idx_d3, idx_d4, minushalos, plushalos)
  IMPLICIT NONE

  INTEGER, INTENT(IN)                     :: buffer_id
  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: field_type
  INTEGER, INTENT(IN)                     :: D1, D2, D3, D4
  INTEGER, INTENT(IN)                     :: idx_d1, idx_d2, idx_d3, idx_d4
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! debug information
  IF (debug) THEN
    WRITE(0,*) 'DEBUG check_buffered: buffer_id=', buffer_id
    WRITE(0,*) 'DEBUG check_buffered: savepoint=', TRIM(savepoint%savepoint_name)
    WRITE(0,*) 'DEBUG check_buffered: fieldname=', TRIM(fieldname)
    WRITE(0,*) 'DEBUG check_buffered: field_type=', field_type
    WRITE(0,*) 'DEBUG check_buffered: D1,D2,D3,D4=', D1, D2, D3, D4
    WRITE(0,*) 'DEBUG check_buffered: idx_d1=', idx_d1
    WRITE(0,*) 'DEBUG check_buffered: idx_d2=', idx_d2
    WRITE(0,*) 'DEBUG check_buffered: idx_d3=', idx_d3
    WRITE(0,*) 'DEBUG check_buffered: idx_d4=', idx_d4
  END IF

  ! security check
  IF (buffer_id < 1 .OR. buffer_id > max_buffer) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: illegal buffer_id encountered'
    STOP
  END IF
  IF (.NOT. buffers(buffer_id)%in_use) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: check called for buffers not in use'
    STOP
  END IF

  ! check consistency
  IF (.NOT. (TRIM(buffers(buffer_id)%fieldname) == TRIM(fieldname))) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: inconsistent name encountered'
    STOP
  END IF
  IF (.NOT. (C_ASSOCIATED(buffers(buffer_id)%serializer, serializer%serializer_ptr))) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: write called for same field but different serializer'
    STOP
  END IF
  
  IF (.NOT. (TRIM(buffers(buffer_id)%savepoint_name) == TRIM(savepoint%savepoint_name))) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: write called for same field but different savepoint'
    STOP
  END IF
  IF (ANY( (/buffers(buffer_id)%D1, buffers(buffer_id)%D2, buffers(buffer_id)%D3, buffers(buffer_id)%D4/) /= (/D1, D2, D3, D4/) )) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: write called with inconsistent dimensions'
    STOP
  END IF
  IF ((idx_d3 < 1) .OR. (idx_d3 > D3)) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: out of bound idx_d3-index encountered:', idx_d3
    STOP
  END IF
  IF ((idx_d4 < 1) .OR. (idx_d4 > D4)) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: out of bound idx_d3-index encountered', idx_d4
    STOP
  END IF
  IF (buffers(buffer_id)%has_minushalos .AND. PRESENT(minushalos)) THEN
    IF (ANY(buffers(buffer_id)%minushalos /= minushalos)) THEN
      WRITE(0,*) 'ERROR in utils_ppser_buffered: inconsistent minushalos encountered'
      STOP
    END IF
  END IF
  IF (buffers(buffer_id)%has_plushalos .AND. PRESENT(plushalos)) THEN
    IF (ANY(buffers(buffer_id)%plushalos /= plushalos)) THEN
      WRITE(0,*) 'ERROR in utils_ppser_buffered: inconsistent plushalos encountered'
      STOP
    END IF
  END IF
  IF (buffers(buffer_id)%field_type /= field_type) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: write with inconsistent field_type encountered'
    STOP
  END IF
  ! Should be redundant, but doesn't hurt to recheck
  IF (buffers(buffer_id)%ok(idx_d1,idx_d2,idx_d3,idx_d4)) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: index already written'
    STOP
  END IF

END SUBROUTINE check_buffered

!============================================================================


! find the ID of a buffers given name of field and savepoint
SUBROUTINE find_buffered_id(fieldname, savepoint, idx_d1, idx_d2, idx_d3, idx_d4, buffer_id, call_index)
  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  INTEGER, INTENT(IN)                     :: idx_d1, idx_d2, idx_d3, idx_d4
  INTEGER, INTENT(OUT)                    :: buffer_id, call_index

  ! local vars
  INTEGER :: idx
  
  buffer_id = 0
  call_index = 0
  IF (debug) THEN
    WRITE(0,*) 'DEBUG find_buffered_id: fieldname=', TRIM(fieldname), ' savepoint=', TRIM(savepoint%savepoint_name)
  END IF

  DO idx = 1, max_buffer
    IF (buffers(idx)%in_use) THEN 
      IF (TRIM(fieldname) == TRIM(buffers(idx)%fieldname)) THEN
        IF (TRIM( savepoint%savepoint_name) == TRIM(buffers(idx)%savepoint_name)) THEN
          IF (debug) THEN
            WRITE(0, *) 'DEBUG found name match at ', idx, 'idx_d1,idx_d2,idx_d3,idx_d4=', idx_d1,',',idx_d2,',',idx_d3,',',idx_d4, buffers(idx)%ok(idx_d1,idx_d2,idx_d3,idx_d4)
          END IF
          IF (buffers(idx)%ok(idx_d1,idx_d2,idx_d3,idx_d4)) THEN
            ! The idx_d3 for this buffers has already been filled, keep looking for another with the same name
            call_index = call_index + 1
          ELSE
            IF (debug) THEN
              WRITE(0, *) 'DEBUG found buffer_id', idx, ', counted similar buffers: ', call_index
            END IF
            buffer_id = idx
            EXIT
          END IF
        END IF     
      END IF
    END IF
  END DO

  IF (debug) THEN
    IF (buffer_id == 0) THEN
      WRITE(0,*) 'DEBUG find_buffered_id: no buffers found, call_index=', call_index
    ELSE
      WRITE(0,*) 'DEBUG find_buffered_id: found buffer_id=', buffer_id, ' call_index=', call_index
    END IF
  END IF

END SUBROUTINE find_buffered_id

!============================================================================

! find a free buffers ID
SUBROUTINE get_free_buffered_id(buffer_id)
  IMPLICIT NONE

  INTEGER, INTENT(OUT)         :: buffer_id

  ! local vars
  INTEGER :: idx

  buffer_id = 0

  ! find a free index
  DO idx = 1, max_buffer
    IF (.NOT. buffers(idx)%in_use) THEN
      buffer_id = idx
      EXIT
    END IF
  END DO

  ! abort if no free index has been found
  IF (idx > max_buffer) THEN
    WRITE(0,*) 'ERROR in utils_ppser_buffered: no more free buffers (increase max_buffer)'
    STOP
  END IF

END SUBROUTINE get_free_buffered_id

!============================================================================

END MODULE utils_ppser_buffered
