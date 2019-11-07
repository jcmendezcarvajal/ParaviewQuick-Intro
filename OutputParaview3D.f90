Module ParaviewVariables
    implicit none
    real, dimension(:,:), allocatable :: Coord, VectorParaview
    real, dimension(:), allocatable :: ScalarParaviewRho, ScalarParaviewPress, ScalarParaviewTemp
    integer,dimension(:),allocatable :: Connectivity
    integer :: NumberNodes, NumberElements, m, ijk
End Module ParaviewVariables

Subroutine OutputParaview3D()
    use ParaviewVariables
    use Variables   ! This module contains the basics definition for geometric variables
    Implicit None
    character (len=80) :: Geometry = 'Geo.geo'
    character (len=80) :: VectorField = 'VF_Velocity.vec'
    character (len=80) :: ScalarField_UVel = 'SF_UVelocity.scl'
    character (len=80) :: ScalarField_VVel = 'SF_VVelocity.scl'
    character (len=80) :: ScalarField_WVel = 'SF_WVelocity.scl'
    character (len=80) :: ScalarField_Rho = 'SF_Density.scl'
    character (len=80) :: ScalarField_Temp = 'SF_Temperature.scl'
    character (len=80) :: ScalarField_Pre = 'SF_Pressure.scl'
    character (len=80) :: CaseFile = 'Output.case'
    character (len=80) :: writer


    NumberNodes = Imax*Jmax*Kmax
    NumberElements = (Imax-1)*(Jmax-1)*(Kmax-1)

    allocate(Coord(NumberNodes,3))
    allocate(VectorParaview(NumberNodes,3))
    allocate(ScalarParaviewRho(NumberNodes))
    allocate(ScalarParaviewTemp(NumberNodes))
    allocate(ScalarParaviewPress(NumberNodes))
    allocate(Connectivity(NumberNodes))

    Call Coordinates()
    Call ConnectivitySub()
    Call ScalarVectorField()

    ! ===============================================================
    ! This Part takes care of the geometry for Paraview
    !================================================================

    open(9,file = trim(Geometry), form = 'Unformatted',access = 'stream', status = 'replace')

    writer = 'Fortran Binary'
    write (9) writer
    writer = 'description line 1'
    write (9) writer
    writer = 'description line 2'
    write (9) writer
    writer = 'node id given'
    write (9) writer
    writer = 'element id given'
    write (9) writer
    writer = 'part'
    write (9) writer
    write (9) 1
    writer = 'description line'
    write (9) writer

    writer = 'coordinates'
    write (9) writer
    write (9) NumberNodes
    write (9) (i, i=1,NumberNodes)
    write (9) ((Coord(i,j),i=1,NumberNodes), j = 1,3)

    writer = 'hexa8'
    write (9) writer
    write (9) NumberElements
    write (9) (i, i=1,NumberElements)

    Do k = 1, Kmax-1
        Do j = 1, Jmax-1
             Do i = 1, Imax -1
                 m = Imax*Jmax*(k-1) + Imax*(j-1) + i
                 write (9) Connectivity(m)
                 write (9) Connectivity(m) + 1
                 write (9) Connectivity(m) + Imax+ 1
                 write (9) Connectivity(m) + Imax
                 write (9) Connectivity(m) + (Imax*Jmax)
                 write (9) Connectivity(m) + (Imax*Jmax) + 1
                 write (9) Connectivity(m) + (Imax*Jmax) + Imax + 1
                 write (9) Connectivity(m) + (Imax*Jmax) + Imax
             End Do
        End Do
    End Do

    close(9)

    ! ========================================================================
    ! This Part takes care of the scalar field (Density) per node for Paraview
    !=========================================================================

    open(9,file = trim(ScalarField_Rho),form = 'Unformatted',access = 'stream', status = 'replace')

    writer = 'scalar file'
    write(9) writer
    writer = 'part'
    write(9) writer
    write(9) 1
    writer = 'coordinates'
    write(9) writer
    write(9) (ScalarParaviewRho(i),i=1,NumberNodes)

    close(9)


    ! =============================================================================
    ! This Part takes care of the scalar field (Temperature) per node for Paraview
    !==============================================================================

    open(9,file = trim(ScalarField_Temp),form = 'Unformatted',access = 'stream', status = 'replace')

    writer = 'scalar file'
    write(9) writer
    writer = 'part'
    write(9) writer
    write(9) 1
    writer = 'coordinates'
    write(9) writer
    write(9) (ScalarParaviewTemp(i),i=1,NumberNodes)

    close(9)


    ! =============================================================================
    ! This Part takes care of the scalar field (Pressure) per node for Paraview
    !==============================================================================

    open(9,file = trim(ScalarField_Pre),form = 'Unformatted',access = 'stream', status = 'replace')

    writer = 'scalar file'
    write(9) writer
    writer = 'part'
    write(9) writer
    write(9) 1
    writer = 'coordinates'
    write(9) writer
    write(9) (ScalarParaviewPress(i),i=1,NumberNodes)

    close(9)

    ! =============================================================================
    ! This Part takes care of the scalar field (U Velocity) per node for Paraview
    !==============================================================================

    open(9,file = trim(ScalarField_UVel),form = 'Unformatted',access = 'stream', status = 'replace')

    writer = 'scalar file'
    write(9) writer
    writer = 'part'
    write(9) writer
    write(9) 1
    writer = 'coordinates'
    write(9) writer
    write(9) ((VectorParaview(i,j),i=1,NumberNodes), j = 1)

    close(9)

    ! =============================================================================
    ! This Part takes care of the scalar field (V Velocity) per node for Paraview
    !==============================================================================

    open(9,file = trim(ScalarField_VVel),form = 'Unformatted',access = 'stream', status = 'replace')

    writer = 'scalar file'
    write(9) writer
    writer = 'part'
    write(9) writer
    write(9) 1
    writer = 'coordinates'
    write(9) writer
    write(9) ((VectorParaview(i,j),i=1,NumberNodes), j = 2)

    close(9)

    ! =============================================================================
    ! This Part takes care of the scalar field (W Velocity) per node for Paraview
    !==============================================================================

    open(9,file = trim(ScalarField_WVel),form = 'Unformatted',access = 'stream', status = 'replace')

    writer = 'scalar file'
    write(9) writer
    writer = 'part'
    write(9) writer
    write(9) 1
    writer = 'coordinates'
    write(9) writer
    write(9) ((VectorParaview(i,j),i=1,NumberNodes), j = 3)

    close(9)


    !==================================================================
    ! This part takes care of the vector field per node for Paraview
    !==================================================================

    open(9,file = trim(VectorField),form = 'Unformatted',access = 'stream', status = 'replace')

    writer = 'vector file'
    write(9) writer
    writer = 'part'
    write(9) writer
    write(9) 1
    writer = 'coordinates'
    write(9) writer
    write(9) ((VectorParaview(i,j),i=1,NumberNodes), j = 1,3)

    close(9)

    !========================================================================
    !This part builds up the Case file for Paraview
    !========================================================================

    open(9,file = trim(CaseFile), status = 'replace')

    write(9,'(A)') 'FORMAT'
    write(9,'(A)') 'type: ensight gold'
    write(9,'(A)') 'GEOMETRY'
    write(9,'(A)') 'model: Geo.geo'
    write(9,'(A)') 'VARIABLE'
    write(9,'(A)') 'scalar per node:   Density       SF_Density.scl'
    write(9,'(A)') 'scalar per node:   Temperature   SF_Temperature.scl'
    write(9,'(A)') 'scalar per node:   Pressure      SF_Pressure.scl'
    write(9,'(A)') 'scalar per node:   U Velocity    SF_UVelocity.scl'
    write(9,'(A)') 'scalar per node:   V Velocity    SF_VVelocity.scl'
    write(9,'(A)') 'scalar per node:   W Velocity    SF_WVelocity.scl'
    write(9,'(A)') 'vector per node:   Velocity      VF_Velocity.vec'

    close(9)


    EndSubroutine OutputParaview3D

    !=========================== Subroutines ====================================

    Subroutine Coordinates()
    use ParaviewVariables
    use Variables
    integer :: xi, yj
    real    :: dxx, dyy, dzz

    ! Coordiantes for the "x" direction
    dxx = 0.
    xi = 0
    Do i = 1, NumberNodes
        Coord(i,1) = dxx
        dxx = dxx + dx
        xi = xi + 1
        If (xi == Imax) then
            dxx = 0.
            xi = 0
        End if
    End do

    ! Coordiantes for the "y" direction
    dyy = 0.
    xi = 0
    yj = 1
    Do i = 1, NumberNodes
        Coord(i,2) = dyy
        xi = xi + 1
        If (xi == Imax) then
            dyy = dyy + dy
            xi = 0
            yj = yj + 1
           If (yj > Jmax) then
               dyy = 0.
               yj = 1
           End if
       End if
    End do

    ! Coordiantes for the "z" direction
    xi = 0
    dzz = 0.
    Do i = 1, NumberNodes
        Coord(i,3) = dzz
        xi = xi + 1
            If (xi == Imax*Jmax) then
                xi = 0
                dzz = dzz + dz
            End if
    End do

    End Subroutine Coordinates

    Subroutine ConnectivitySub()
    use ParaviewVariables
    use Variables

    Do k = 1, Kmax
        Do j = 1, Jmax
            Do i = 1, Imax
                    Connectivity(Imax*Jmax*(k-1) + Imax*(j-1) + i) = Imax*Jmax*(k-1) + Imax*(j-1) + i
            End Do
        End Do
    End Do

    End Subroutine ConnectivitySub


    Subroutine ScalarVectorField()
    use Variables
    use ParaviewVariables

    ijk = 0
    Do k = 1, Kmax
        Do j = 1, Jmax
            Do i = 1, Imax
                ijk = ijk + 1
                ScalarParaviewRho(ijk)  = r_old(i,j,k)
                ScalarParaviewTemp(ijk) = t_old(i,j,k)
                ScalarParaviewPress(ijk)= t_old(i,j,k)*r_old(i,j,k)
                VectorParaview(ijk,1) = u_old(i,j,k)
                VectorParaview(ijk,2) = v_old(i,j,k)
                VectorParaview(ijk,3) = w_old(i,j,k)
            End Do
        End Do
    End Do

    End Subroutine ScalarVectorField
