# ParaviewQuick-Intro

The subroutine OutputParaview3D.f90 is written in FORTRAN 90 and it prints the 3D steady state solution in Paraview for cartesian coordinates; either unstructured or structured mesh. 
It prints: Pressure, Temperature and Density as scalar functions, and Velocity (U,V and W) as vectorial functions. Thus, the Velocity vector is read by Paraview as a vector. 
This Subroutine creates a set of files that Paraview requires. The output contains information about the geometry, the case and the CFD solution (only primitive variables for 2D). 
The extension of the file is EnsightGold and the type of file is Binary. 
This Subroutine contains its own module (Module ParaviewVariables) and it requires the module with all the variables (geometric variables) from the CFD driver. In this subroutine the module that contains all the geometric information is called VARIABLES. 
This Subroutine reads the variables from the driver, the variables are called: 
Density = r_old
Temperature = t_old
Pressure = t_old*r_old
Streamwise velocity = u_old
Vertical velocity = v_old
Spanwise velocity = w_old

This was developed with the help of my colleague David Doodoamoo.
This was created back in 2016 so there are details I may not remember (11/3/2016)

