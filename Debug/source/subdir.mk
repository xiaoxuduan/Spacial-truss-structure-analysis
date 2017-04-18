################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../source/CONKB.f90 \
../source/DATAIN.f90 \
../source/DATAOUT.f90 \
../source/DISPLS.f90 \
../source/FKE.f90 \
../source/FLMT.f90 \
../source/FMAXA.f90 \
../source/FT.f90 \
../source/LDLT.f90 \
../source/MAT.f90 \
../source/MKFORCE.f90 \
../source/OPENF.f90 \
../source/RESOLVE.f90 \
../source/TRUSS.f90 

OBJS += \
./source/CONKB.o \
./source/DATAIN.o \
./source/DATAOUT.o \
./source/DISPLS.o \
./source/FKE.o \
./source/FLMT.o \
./source/FMAXA.o \
./source/FT.o \
./source/LDLT.o \
./source/MAT.o \
./source/MKFORCE.o \
./source/OPENF.o \
./source/RESOLVE.o \
./source/TRUSS.o 


# Each subdirectory must supply rules for building sources it contributes
source/%.o: ../source/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

source/CONKB.o: ../source/CONKB.f90

source/DATAIN.o: ../source/DATAIN.f90

source/DATAOUT.o: ../source/DATAOUT.f90

source/DISPLS.o: ../source/DISPLS.f90

source/FKE.o: ../source/FKE.f90

source/FLMT.o: ../source/FLMT.f90

source/FMAXA.o: ../source/FMAXA.f90

source/FT.o: ../source/FT.f90

source/LDLT.o: ../source/LDLT.f90

source/MAT.o: ../source/MAT.f90

source/MKFORCE.o: ../source/MKFORCE.f90

source/OPENF.o: ../source/OPENF.f90

source/RESOLVE.o: ../source/RESOLVE.f90

source/TRUSS.o: ../source/TRUSS.f90


