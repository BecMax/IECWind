#================================================================================#
# This makefile created by B. Jonkman on 6-Apr-2015,                             #
# adapted from FAST v8.10.00.                                                    #
# (c) 2015 National Renewable Energy Laboratory                                  #
#                                                                                #
# This makefile has been tested on Windows 7 with gfortran.                      #
# This makefile works with mingw32-make.exe.                                     #
#                                                                                #
# It was designed to be used with:                                               #
#     TurbSim (v2.00.01)                                                         #
#                                                                                #
# Older versions of the source code may not work with this makefile.             #
#================================================================================#
# Adapted for IECWind by M. Becker of WindGuard Certification on 19-Feb-2019 

   # 32-bit or 64-bit?
#BITS = 32
BITS = 64


   # Location of source files for TurbSim and its dependencies.

IECWind_DIR  = ../Source

BIN_DIR      = ../bin

   # Name of compiler to use and flags to use.
FC      = gfortran

FFLAGS  = -O2 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input
LDFLAGS = -O2 -m$(BITS) -fbacktrace

#FFLAGS  = -O2 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input
#LDFLAGS = -O2 -m$(BITS) -fbacktrace -Wl,--large-address-aware

#FFLAGS  = -O0 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input -g -pg
#LDFLAGS = -O0 -m$(BITS) -fbacktrace -Wl,--stack=999999999,--large-address-aware -g -pg

#-DDOUBLE_PRECISION

# -mthreads
# some useful gfortran options:
#  -DFPE_TRAP_ENABLED
#  -Wl,--large-address-aware   # not necessary when $(BITS) is 64 (64-bit target)
#  -Wl,--stack=999999999       # not necessary when $(BITS) is 64
#  -Wconversion-extra -Wconversion
#  -fdefault-real-8 -fcheck=bounds,do,mem,pointer -std=f2003 -O0 -v -Wall
#  -pg                          # generate debugging info for debugger
# http://gcc.gnu.org/onlinedocs/gfortran/Option-Index.html#Option-Index
#
# makefile:
# --warn-undefined-variables



   # Destination and RootName for executable

OUTPUT_NAME = IECWind
DEST_DIR    = $(BIN_DIR)

   #==========================================================#
   # You should not need to change anything beyond this point #
   #==========================================================#

   # System-specific settings.

ifeq ($(OS),Windows_NT)
      # Windows
   DEL_CMD   = del
   EXE_EXT   = _gwin$(BITS).exe
   INTER_DIR = Obj_win$(BITS)
   MD_CMD    = @mkdir
   OBJ_EXT   = .obj
   PATH_SEP  = \\
   SYS_FILE  = SysGnuWin
else
      # Linux
   DEL_CMD   = rm -f
   EXE_EXT   = _glin$(BITS)
   INTER_DIR = Obj_lin$(BITS)
   MD_CMD    = @mkdir -p
   OBJ_EXT   = .o
   PATH_SEP  = /
   SYS_FILE  = SysGnuLinux
endif


   # Source files (by module)


IECWind_SOURCES =        \
	IECwind.f90     


vpath %.f90 $(IECWind_DIR) 
vpath %.mod $(INTER_DIR)
vpath %.obj $(INTER_DIR)


ALL_SOURCES = $(IECWind_SOURCES) 
tmp_objs1   = $(ALL_SOURCES:.f90=.obj)
tmp_objs2   = $(tmp_objs1:.F90=.obj)       #note the upper case here (from IceFloe)
ALL_OBJS    = $(tmp_objs2:.f=.obj)


   # Rule to do everything.

all:     default
# use this for "all" for debugging: print variables:
#all:     ; $(info $$IceF_SOURCES is [${IceF_SOURCES}]) echo debugging
#
default:  $(INTER_DIR) $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT)


   # General rules for compliling the files.

%.obj: %.f90
	$(FC) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)

%.obj: %.F90
	$(FC) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)

%.obj: %.f
	$(F77) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)


   # Make sure the destination directory for the intermediate files exist.

$(INTER_DIR):
	$(MD_CMD) $(INTER_DIR)


   # For linking main program.

$(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT): $(ALL_OBJS) | $(INTER_DIR)
	$(FC) $(LDFLAGS) -I $(INTER_DIR) -o $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT) \
	$(foreach src, $(ALL_OBJS), $(addprefix $(INTER_DIR)/,$(src))) 

   # Cleanup afterwards.

clean:
	$(DEL_CMD) $(INTER_DIR)$(PATH_SEP)*.mod $(INTER_DIR)$(PATH_SEP)*.obj

