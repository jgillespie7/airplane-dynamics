airplane-dynamics
=================

6 DOF numeric simulation of the dynamics of an airplane

An improved, expanded, and completely rewritten version of https://github.com/Jochi-Labs/J3CA-main/tree/master/2Dsim

The numeric solver, dynamics.f90, calls euler_rotate.f90 to perform transformations between body-centered and inertial coordinate systems.

The actual dynamics of the airplane are contained in airplane.f90, allowing easy adaptations to other dynamic simulations.
