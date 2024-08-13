!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license.
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------
!
!+ This module contains the interface for serializing k-blocked fields
!
!------------------------------------------------------------------------------

MODULE utils_ppser_ijkbuff

!------------------------------------------------------------------------------
!
! Description:
!
!   This module contains subroutines which allow to serialize k-blocked
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
  fs_write_ijkbuff, finalize_ijkbuff

PRIVATE

  TYPE ijkbuff_type
    LOGICAL :: in_use = .FALSE.                   ! is this buffers in use?
    TYPE(C_PTR) :: serializer                     ! serializer object associated with buffers
    CHARACTER(LEN=256) :: savepoint_name
    CHARACTER(LEN=256) :: fieldname
    INTEGER :: dim_i = 0, dim_j = 0, dim_k = 0    ! dimensions of 3d-field to be serialized
    INTEGER :: call_index = 0                     ! track multiple kbuffers for the same savepoint name 
                                                  ! and field being filled in parallel  
    LOGICAL :: has_minushalos, has_plushalos
    INTEGER :: minushalos(3), plushalos(3)
    INTEGER :: field_type = 0                     ! 0 = not used, 1 = int, 2 = r4, 3 = r8
    INTEGER, ALLOCATABLE :: buff_3d_i4(:,:,:)
    REAL(KIND=C_FLOAT), ALLOCATABLE :: buff_3d_r4(:,:,:)
    REAL(KIND=C_DOUBLE), ALLOCATABLE :: buff_3d_r8(:,:,:)
    LOGICAL, ALLOCATABLE :: ok(:,:,:)             ! has this ijk-level been written?
  END TYPE ijkbuff_type

  INTEGER, PARAMETER :: max_buffer = 9999         ! increase in case you get errors
  TYPE(ijkbuff_type) :: buffers(max_buffer)         ! array containing buffers

  ! overload interface for different types and dimensions
  INTERFACE fs_write_ijkbuff
      MODULE PROCEDURE fs_write_ijkbuff_3d_i4
      MODULE PROCEDURE fs_write_ijkbuff_3d_r4
      MODULE PROCEDURE fs_write_ijkbuff_3d_r8
  END INTERFACE

  LOGICAL :: first_call = .TRUE.                  ! used for initialization

  LOGICAL, PARAMETER :: debug = .FALSE.           ! get verbose messaging

CONTAINS

!============================================================================

! initialize buffering: makes sure all buffers are set to not in use
SUBROUTINE init_ijkbuff()
  IMPLICIT NONE

  INTEGER :: idx

  IF (debug) THEN
    WRITE(0,*) 'DEBUG init_ijkbuff'
  END IF

  first_call = .FALSE.

  DO idx = 1, max_buffer
    buffers(idx)%in_use = .FALSE.
    buffers(idx)%fieldname = ""
    buffers(idx)%savepoint_name = ""
    buffers(idx)%call_index = 0
  END DO

END SUBROUTINE init_ijkbuff

!============================================================================

! finalize buffering: should be called once all buffers have been flushed
SUBROUTINE finalize_ijkbuff()
  IMPLICIT NONE

  INTEGER :: idx

  IF (debug) THEN
    WRITE(0,*) 'DEBUG finalize_ijkbuff'
  END IF

  DO idx = 1, max_buffer
    IF (buffers(idx)%in_use) THEN
      WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: finalize called before all buffers have been flushed'
      WRITE(0,*) 'savepoint_name: ', TRIM(buffers(idx)%savepoint_name)
      WRITE(0,*) 'name of partially empty field: ', TRIM(buffers(idx)%fieldname)
      WRITE(0,*) 'Indices filled boolean: ', buffers(idx)%ok(:,:,:)
      STOP
    END IF
    buffers(idx)%fieldname = ""
    buffers(idx)%savepoint_name = ""
  END DO

  first_call = .TRUE.

END SUBROUTINE finalize_ijkbuff

!============================================================================

! overloads fs_write_ijkbuff: version for r8 floats and 3d fields
SUBROUTINE fs_write_ijkbuff_3d_r8(serializer, savepoint, fieldname, scalar, &
                                  i, i_size, j, j_size, k, k_size, &
                                  mode, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: scalar
  INTEGER, INTENT(IN)                     :: i, i_size, j, j_size, k, k_size
  INTEGER, INTENT(IN)                     :: mode
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! local vars
  INTEGER :: buff_id = 0
  INTEGER :: field_type = 3

  ! do nothing in case serialization is switched off
  IF (.NOT. (fs_is_serialization_on())) THEN
    RETURN
  ENDIF

  ! find buff_id and check if a buffers slot was found
  call setup_buffer(buff_id, serializer, savepoint, fieldname, field_type, &
                    i, i_size, j, j_size, k_size, k, &
                    mode, minushalos, plushalos)

  ! store data
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_ijkbuff_3d_r8: store data'
  END IF
  buffers(buff_id)%buff_3d_r8(i,j,k) = scalar
  buffers(buff_id)%ok(i,j,k) = .TRUE.

  ! write if we are complete
  IF (ALL(buffers(buff_id)%ok(:,:,:))) THEN
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_ijkbuff_3d_r8: flush data'
    END IF
    IF (buffers(buff_id)%has_minushalos) THEN
      IF (buffers(buff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buff_id)%buff_3d_r8, &
          minushalos=buffers(buff_id)%minushalos, plushalos=buffers(buff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buff_id)%buff_3d_r8, &
          minushalos=buffers(buff_id)%minushalos)
      END IF
    ELSE
      IF (buffers(buff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buff_id)%buff_3d_r8, &
          plushalos=buffers(buff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buff_id)%buff_3d_r8)
      END IF
    END IF

    CALL destroy_ijbuffer(buff_id)
  END IF

END SUBROUTINE fs_write_ijkbuff_3d_r8

!============================================================================

! overloads fs_write_ijkbuff: version for r4 floats and 3d fields
SUBROUTINE fs_write_ijkbuff_3d_r4(serializer, savepoint, fieldname, scalar, &
                                  i, i_size, j, j_size, k, k_size, &
                                  mode, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET  :: scalar
  INTEGER, INTENT(IN)                     :: i, i_size, j, j_size, k, k_size
  INTEGER, INTENT(IN)                     :: mode
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! local vars
  INTEGER :: buff_id = 0
  INTEGER :: field_type = 2

  ! do nothing in case serialization is switched off
  IF (.NOT. (fs_is_serialization_on())) THEN
    RETURN
  ENDIF

  ! find buff_id and check if a buffers slot was found
  call setup_buffer(buff_id, serializer, savepoint, fieldname, field_type, &
                    i, i_size, j, j_size, k_size, k, &
                    mode, minushalos, plushalos)

  ! store data
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_ijkbuff_3d_r4: store data'
  END IF
  buffers(buff_id)%buff_3d_r4(i,j,k) = scalar
  buffers(buff_id)%ok(i,j,k) = .TRUE.

  ! write if we are complete
  IF (ALL(buffers(buff_id)%ok(:,:,:))) THEN
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_ijkbuff_3d_r4: flush data'
    END IF
    IF (buffers(buff_id)%has_minushalos) THEN
      IF (buffers(buff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buff_id)%buff_3d_r4, &
          minushalos=buffers(buff_id)%minushalos, plushalos=buffers(buff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buff_id)%buff_3d_r4, &
          minushalos=buffers(buff_id)%minushalos)
      END IF
    ELSE
      IF (buffers(buff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buff_id)%buff_3d_r4, &
          plushalos=buffers(buff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buff_id)%buff_3d_r4)
      END IF
    END IF

    CALL destroy_ijbuffer(buff_id)
  END IF

END SUBROUTINE fs_write_ijkbuff_3d_r4

!============================================================================

! overloads fs_write_ijkbuff: version for i4 integers and 3d fields
SUBROUTINE fs_write_ijkbuff_3d_i4(serializer, savepoint, fieldname, scalar, &
                                  i, i_size, j, j_size, k, k_size, &
                                  mode, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: scalar
  INTEGER, INTENT(IN)                     :: i, i_size, j, j_size, k, k_size, mode
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
                    i, i_size, j, j_size, k_size, k, mode, minushalos, plushalos)

  ! store data
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_ijkbuff_3d_i4: store data'
  END IF
  buffers(buffer_id)%buff_3d_i4(i,j,k) = scalar
  buffers(buffer_id)%ok(i,j,k) = .TRUE.

  ! write if we are complete
  IF (ALL(buffers(buffer_id)%ok(:,:,:))) THEN
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_buff_3d_i4ers: flush data'
    END IF
    IF (buffers(buffer_id)%has_minushalos) THEN
      IF (buffers(buffer_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buff_3d_i4, &
          minushalos=buffers(buffer_id)%minushalos, plushalos=buffers(buffer_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buff_3d_i4, &
          minushalos=buffers(buffer_id)%minushalos)
      END IF
    ELSE
      IF (buffers(buffer_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buff_3d_i4, &
          plushalos=buffers(buffer_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, buffers(buffer_id)%buff_3d_i4)
      END IF
    END IF

    CALL destroy_ijkbuff(buffer_id)
  END IF

END SUBROUTINE fs_write_ijkbuff_3d_i4

!============================================================================

! checks if a buffers exists for this fields and if yes, checks consistency with
! current request. if not, it creates a new buffers.
SUBROUTINE setup_buffer(buffer_id, serializer, savepoint, fieldname, field_type, &
                        i, i_size, j, j_size, k_size, k, &
                        mode, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: i, i_size, j, j_size, k, k_size
  INTEGER, INTENT(IN)                     :: mode, field_type
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)
  INTEGER, INTENT(OUT)                    :: buffer_id

  ! local vars
  INTEGER :: call_index = 0
      
  IF (debug) THEN
    WRITE(0,*) 'DEBUG setup_buffer: savepoint=', TRIM(savepoint%savepoint_name)
    WRITE(0,*) 'DEBUG setup_buffer: fieldname=', TRIM(fieldname)
    WRITE(0,*) 'DEBUG setup_buffer: i=', i, '(', i_size, ')'
    WRITE(0,*) 'DEBUG setup_buffer: j=', j, '(', j_size, ')'
    WRITE(0,*) 'DEBUG setup_buffer: k=', k, '(', k_size, ')'
  END IF

  ! ppser mode numbers do not align with m_serialize constants....
  IF ( mode /= PPSER_MODE_WRITE ) THEN
    WRITE(0,*) 'ERROR, can only use kbuffer in write mode'
    STOP
  END IF
 
  ! initialize if this is the first call
  IF ( first_call ) THEN
    CALL init_ijkbuff()
  END IF

  ! find ID if it already exists
  CALL find_ijkbuffer_id(fieldname, savepoint, i, j, k, buffer_id, call_index)
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_ijkbuff_3d_r8: find buffer_id=', buffer_id
  END IF

  ! check if a buffers slot was found
  IF ( buffer_id == 0 ) THEN
    ! no, so create a new buffers
    CALL get_free_ijkbuffer_id(buffer_id)
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_ijkbuff_3d: buffer_id=', buffer_id
    END IF
    CALL create_ijkbuff(buffer_id, serializer, savepoint, fieldname, field_type, &
                        i_size, j_size, k_size, call_index, minushalos, plushalos)
  ELSE
    ! yes, so check for consistency of current request with stored metadata
    CALL check_ijkbuff(buffer_id, serializer, savepoint, fieldname, field_type, &
                       i_size, j_size, k_size, i, j, k, minushalos, plushalos)
  END IF

 END SUBROUTINE setup_buffer

!============================================================================


! create a new buffers (allocate memory, store metadata)
SUBROUTINE create_ijkbuff(buff_id, serializer, savepoint, fieldname, field_type, &
                          dim_i, dim_j, dim_k, call_index, minushalos, plushalos)
  IMPLICIT NONE

  INTEGER, INTENT(IN)                     :: buff_id
  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: field_type
  INTEGER, INTENT(IN)                     :: dim_i, dim_j, dim_k
  INTEGER, INTENT(IN)                     :: call_index
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! debug information
  IF (debug) THEN
    WRITE(0,*) 'DEBUG create_ijkbuff: buff_id=', buff_id
    WRITE(0,*) 'DEBUG create_ijkbuff: savepoint=', TRIM(savepoint%savepoint_name)
    WRITE(0,*) 'DEBUG create_ijkbuff: fieldname=', TRIM(fieldname)
    WRITE(0,*) 'DEBUG create_ijkbuff: field_type=', field_type
    WRITE(0,*) 'DEBUG create_ijkbuff: dim_i,dim_j,dim_k=', dim_i, dim_j, dim_k
  END IF

  ! security check
  IF (buff_id < 1 .OR. buff_id > max_buffer) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: illegal buff_id encountered'
    STOP
  END IF
  IF (buffers(buff_id)%in_use) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: create called for buffers already in use'
    STOP
  END IF

  ! store metadata
  buffers(buff_id)%in_use = .TRUE.
  buffers(buff_id)%serializer = serializer%serializer_ptr
  buffers(buff_id)%call_index = call_index
  buffers(buff_id)%savepoint_name = TRIM(savepoint%savepoint_name)
  buffers(buff_id)%fieldname = TRIM(fieldname)
  buffers(buff_id)%dim_i = dim_i
  buffers(buff_id)%dim_j = dim_j
  buffers(buff_id)%dim_k = dim_k
  IF (PRESENT(minushalos)) THEN
    buffers(buff_id)%minushalos = minushalos
    buffers(buff_id)%has_minushalos = .TRUE.
  ELSE
    buffers(buff_id)%has_minushalos = .FALSE.
  ENDIF
  IF (PRESENT(plushalos)) THEN
    buffers(buff_id)%plushalos = plushalos
    buffers(buff_id)%has_plushalos = .TRUE.
  ELSE
    buffers(buff_id)%has_plushalos = .FALSE.
  ENDIF
  buffers(buff_id)%field_type = field_type

  ! allocate memory
  SELECT CASE (field_type)
    CASE(1)
      ALLOCATE(buffers(buff_id)%buff_3d_i4(dim_i, dim_j, dim_k))
    CASE(2)
      ALLOCATE(buffers(buff_id)%buff_3d_r4(dim_i, dim_j, dim_k))
    CASE(3)
      ALLOCATE(buffers(buff_id)%buff_3d_r8(dim_i, dim_j, dim_k))
    CASE DEFAULT
      WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: unsupported field_type encountered'
  END SELECT
  ALLOCATE(buffers(buff_id)%ok(dim_i, dim_j, dim_k))

  ! make sure all grid points as unwritten
  buffers(buff_id)%ok(:,:,:) = .FALSE.

END SUBROUTINE create_ijkbuff

!============================================================================

! release a buffers (release memory, reset metadata)
SUBROUTINE destroy_ijkbuff(buffer_id)
  IMPLICIT NONE

  INTEGER, INTENT(IN)                     :: buffer_id
  INTEGER                                 :: idx
  ! debug information
  IF (debug) THEN
    WRITE(0,*) 'DEBUG destroy_ijkbuff: buffer_id=', buffer_id
    WRITE(0,*) 'DEBUG destroy_ijkbuff: savepoint=', TRIM(buffers(buffer_id)%savepoint_name)
    WRITE(0,*) 'DEBUG destroy_ijkbuff: fieldname=', TRIM(buffers(buffer_id)%fieldname)
  END IF

  ! security check
  IF (buffer_id < 1 .OR. buffer_id > max_buffer) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: illegal buffer_id encountered'
    STOP
  END IF
  IF (.NOT. buffers(buffer_id)%in_use) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: destroy called for buffers not in use'
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
      DEALLOCATE(buffers(buffer_id)%buff_3d_i4)
    CASE(2)
      DEALLOCATE(buffers(buffer_id)%buff_3d_r4)
    CASE(3)
      DEALLOCATE(buffers(buffer_id)%buff_3d_r8)
    CASE DEFAULT
      WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: unsupported field_type encountered in destroy'
  END SELECT
  DEALLOCATE(buffers(buffer_id)%ok)

  ! reset metadata
  buffers(buffer_id)%in_use = .FALSE.
  buffers(buffer_id)%serializer = C_NULL_PTR
  buffers(buffer_id)%savepoint_name = ""
  buffers(buffer_id)%call_index = 0
  buffers(buffer_id)%fieldname = ""
  buffers(buffer_id)%field_type = 0
  buffers(buffer_id)%dim_i = 0
  buffers(buffer_id)%dim_j = 0
  buffers(buffer_id)%dim_k = 0
  IF (buffers(buffer_id)%has_minushalos) THEN
    buffers(buffer_id)%minushalos = (/0, 0, 0/)
  ENDIF
  buffers(buffer_id)%has_minushalos = .FALSE.
  IF (buffers(buffer_id)%has_plushalos) THEN
    buffers(buffer_id)%plushalos = (/0, 0, 0/)
  ENDIF
  buffers(buffer_id)%has_plushalos = .FALSE.

END SUBROUTINE destroy_ijkbuff

!============================================================================

! check consistency of current request with metadata stored in buffers
SUBROUTINE check_ijkbuff(buffer_id, serializer, savepoint, fieldname, field_type, &
                         dim_i, dim_j, dim_k, i, j, k, minushalos, plushalos)
  IMPLICIT NONE

  INTEGER, INTENT(IN)                     :: buffer_id
  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: field_type
  INTEGER, INTENT(IN)                     :: dim_i, dim_j, dim_k
  INTEGER, INTENT(IN)                     :: i, j, k
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! debug information
  IF (debug) THEN
    WRITE(0,*) 'DEBUG check_ijkbuff: buffer_id=', buffer_id
    WRITE(0,*) 'DEBUG check_ijkbuff: savepoint=', TRIM(savepoint%savepoint_name)
    WRITE(0,*) 'DEBUG check_ijkbuff: fieldname=', TRIM(fieldname)
    WRITE(0,*) 'DEBUG check_ijkbuff: field_type=', field_type
    WRITE(0,*) 'DEBUG check_ijkbuff: dim_i,dim_j,dim_k=', dim_i, dim_j, dim_k
    WRITE(0,*) 'DEBUG check_ijkbuff: i=', i
    WRITE(0,*) 'DEBUG check_ijkbuff: j=', j
    WRITE(0,*) 'DEBUG check_ijkbuff: k=', k
  END IF

  ! security check
  IF (buffer_id < 1 .OR. buffer_id > max_buffer) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: illegal buffer_id encountered'
    STOP
  END IF
  IF (.NOT. buffers(buffer_id)%in_use) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: check called for buffers not in use'
    STOP
  END IF

  ! check consistency
  IF (.NOT. (TRIM(buffers(buffer_id)%fieldname) == TRIM(fieldname))) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: inconsistent name encountered'
    STOP
  END IF
  IF (.NOT. (C_ASSOCIATED(buffers(buffer_id)%serializer, serializer%serializer_ptr))) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: write called for same field but different serializer'
    STOP
  END IF
  
  IF (.NOT. (TRIM(buffers(buffer_id)%savepoint_name) == TRIM(savepoint%savepoint_name))) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: write called for same field but different savepoint'
    STOP
  END IF
  IF (ANY( (/buffers(buffer_id)%dim_i, buffers(buffer_id)%dim_j, buffers(buffer_id)%dim_k/) /= (/dim_i, dim_j, dim_k/) )) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: write called with inconsistent dimensions'
    STOP
  END IF
  IF ((k < 1) .OR. (k > dim_k)) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: out of bound k-index encountered'
    STOP
  END IF
  IF (buffers(buffer_id)%has_minushalos .AND. PRESENT(minushalos)) THEN
    IF (ANY(buffers(buffer_id)%minushalos /= minushalos)) THEN
      WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: inconsistent minushalos encountered'
      STOP
    END IF
  END IF
  IF (buffers(buffer_id)%has_plushalos .AND. PRESENT(plushalos)) THEN
    IF (ANY(buffers(buffer_id)%plushalos /= plushalos)) THEN
      WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: inconsistent plushalos encountered'
      STOP
    END IF
  END IF
  IF (buffers(buffer_id)%field_type /= field_type) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: write with inconsistent field_type encountered'
    STOP
  END IF
  ! Should be redundant, but doesn't hurt to recheck
  IF (buffers(buffer_id)%ok(i,j,k)) THEN
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: k-index already written'
    STOP
  END IF

END SUBROUTINE check_ijkbuff

!============================================================================


! find the ID of a buffers given name of field and savepoint
SUBROUTINE find_ijkbuffer_id(fieldname, savepoint, i, j, k, buffer_id, call_index)
  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  INTEGER, INTENT(IN)                     :: i, j, k    
  INTEGER, INTENT(OUT)                    :: buffer_id, call_index

  ! local vars
  INTEGER :: idx
  
  buffer_id = 0
  call_index = 0
  IF (debug) THEN
    WRITE(0,*) 'DEBUG find_ijkbuffer_id: fieldname=', TRIM(fieldname), ' savepoint=', TRIM(savepoint%savepoint_name)
  END IF

  DO idx = 1, max_buffer
    IF (buffers(idx)%in_use) THEN 
      IF (TRIM(fieldname) == TRIM(buffers(idx)%fieldname)) THEN
        IF (TRIM( savepoint%savepoint_name) == TRIM(buffers(idx)%savepoint_name)) THEN
          IF (debug) THEN
            WRITE(0, *) 'DEBUG found name match at ', idx, 'i,j,k=', i,',',j,',',k, buffers(idx)%ok(i,j,k)
          END IF
          IF (buffers(idx)%ok(i,j,k)) THEN
            ! The k for this buffers has already been filled, keep looking for another with the same name
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
      WRITE(0,*) 'DEBUG find_ijkbuffer_id: no buffers found, call_index=', call_index
    ELSE
      WRITE(0,*) 'DEBUG find_ijkbuffer_id: found buffer_id=', buffer_id, ' call_index=', call_index
    END IF
  END IF

END SUBROUTINE find_ijkbuffer_id

!============================================================================

! find a free buffers ID
SUBROUTINE get_free_ijkbuffer_id(buffer_id)
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
    WRITE(0,*) 'ERROR in utils_ppser_ijkbuff: no more free buffers (increase max_buffer)'
    STOP
  END IF

END SUBROUTINE get_free_ijkbuffer_id

!============================================================================

END MODULE utils_ppser_ijkbuff
