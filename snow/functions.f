c----------------------------------------------------------------------
       
      module snow_model
          
          implicit none
          
          type :: snow_model_type
              
              integer:: ICL,IOPEN,SIXHRS,N_TIME_STEPS
              real   :: PADJ
              reaL,dimension(2)   :: TSFC
              real   :: PCPN
              REAL   :: OLD,CD
              REAL   :: NEW,NEWP
              
!              REAL,allocatable:: TAIR(:), PREC(:), SNOD(:), SDEN(:)
              
              real:: t2_last, t, dt, t2_current
              
              integer:: n_x, n_y, id
              real:: dx, dy
              
              
       
          end type snow_model_type
              
c----------------------------------------------------------------------
      contains
      
      SUBROUTINE update(model)
      
      type (snow_model_type) :: model
      
      ! update ONE time step:
           
      MODEL%TSFC(1) = MODEL%t2_last
      MODEL%TSFC(2) = MODEL%t2_current
                
      call NEW_BRUCE(MODEL)
      
!      MODEL%SNOD(model%t) = model%NEW
!      MODEL%SDEN(model%t) = model%NEWP
      
      ! replace previous variables by current ones:
            
      MODEL%t2_last  = MODEL%TSFC(2) 
      
      MODEL%OLD = model%new
      MODEL%CD  = model%newp
      
      ! Read new variables from files: Temperature and Precipitation.
      
      read(201, *)  MODEL%t2_current
      read(202, *)  MODEL%PCPN
      
      ! update time:
      model%t = model%t + model%dt

      END SUBROUTINE update
      
!================================================
      
      SUBROUTINE initialize(model, config_file)
      
      character (len=*), intent (in) :: config_file
      type (snow_model_type), intent (out) :: model
      integer::i
      
      integer:: status
      
      character path_separator
      
      character*64:: dummystd,c_tair_filename,c_prec_filename
      character*64:: c_path
      
      if (sizeof(trim(adjustl(config_file))) .eq.0) then
      
      print *, 'Please set configuration file'
      print *, 'e.g., ./run_bmisnow_model ~/Documents/test.cfg'
      
      stop
      
      endif
      
      ! guess the path of cfg file:
      status = index(config_file, '/', back=.true.)
      
      print *, status
            
      open(101, file = config_file, status = 'unknown')
      
      READ(101,'(A)') dummystd
      READ(101,*)     MODEL%ICL

      READ(101,'(A)') dummystd
      READ(101,*)     MODEL%IOPEN

      READ(101,'(A)') dummystd
      READ(101,*)     MODEL%PADJ

      READ(101,'(A)') dummystd
      READ(101,*)     MODEL%OLD
      
      READ(101,'(A)') dummystd
      READ(101,*)     MODEL%CD
      
      READ(101,'(A)') dummystd
      READ(101,*)     MODEL%N_TIME_STEPS
      
!      allocate(MODEL%TAIR(MODEL%N_TIME_STEPS))
!      allocate(MODEL%PREC(MODEL%N_TIME_STEPS))
!      allocate(MODEL%SNOD(MODEL%N_TIME_STEPS))
!      allocate(MODEL%SDEN(MODEL%N_TIME_STEPS))

      READ(101,'(A)') dummystd
      READ(101,'(A)') c_path
      
      if (sizeof(trim(adjustl(c_path))) .eq. 0) then
      print *, 'No available path, will try to find inputs'
      print *, 'from the same folder of this configuration'
      path_separator = '/'
      
      endif
            
      READ(101,'(A)') dummystd
      READ(101,'(A)') c_tair_filename
      open(201, file = trim(adjustl(c_path))//c_tair_filename)
!      READ(101,*)     (MODEL%TAIR(i), i=1, MODEL%N_TIME_STEPS)  
      
      read(201, *) MODEL%t2_last
      
      rewind(201)

      READ(101,'(A)') dummystd
      READ(101,'(A)') c_prec_filename
      OPEN(202, file = trim(adjustl(c_path))//c_prec_filename)
                  
      model%t  = 0.
      model%dt = 1.
      
      model%n_x = 1
      model%n_y = 1
      
      model%dx  = 1
      model%dy  = 1
      
      model%id   = 0
      
      MODEL%SIXHRS  =24
      
      MODEL%t2_current = MODEL%t2_last
      MODEL%PCPN       = 0
      
      close(101)
                  
      END SUBROUTINE initialize

!================================================

      subroutine print_info(model)
      type (snow_model_type), intent (in) :: model

      write(*,"(a10, i8)") "n_x:", model%n_x
      write(*,"(a10, i8)") "n_y:", model%n_y
      write(*,"(a10, f8.2)") "dx:", model%dx
      write(*,"(a10, f8.2)") "dy:", model%dy
      write(*,"(a10, f8.2)") "dt:", model%dt
      write(*,"(a10, f8.2)") "t:", model%t
      write(*,"(a10, f8.2)") "t_end:", model%N_TIME_STEPS
      end subroutine print_info
        
!================================================

      SUBROUTINE finalize(model)
      type (snow_model_type) :: model
      
!      deallocate(MODEL%TAIR)
      
      close(201);close(202)
                
      END subroutine finalize

!================================================
      
!      SUBROUTINE RUN_SNOW_MODEL(N_TIME, N_DAYS_IN_TIMESTEP 
!     * , TAIR, PRE ,ICL, IOPEN, PADJ, SNOD, SDEN)
! 
!* 
!* AUTHOR  - Kang Wang January 2019
!*
!* PURPOSE - Use a simple function to apply Bruce subroutine to a time series. 
!*           only for a single time series input
!* 
!* ARGUMENTS-
!*    I    - N_TIME             - How many time step (months or daily) 
!*    I    - N_DAYS_IN_TIMESTEP - How days in a time step
!*                                N_TIME = Daily, N_DAYS_IN_TIMESTEP= an array of ONE
!*                                N_TIME = Monthly, N_DAYS_IN_TIMESTEP = days in months
!
!*    I    - TAIR               - Array of air temperature (Length = N_TIME) (Degree C)
!*    I    - PRE                - Array of precipitation   (Length = N_TIME) (m)
!
!*    I    - ICL                - SNOW CLIMATE CLASS, STURM ET AL 1995 (CODE FROM 0-7)
!*    I    - IOPEN              - FORESTED/OPEN FLAG FOR VALIDATING RESULTS IN BOREAL
!*                                FOREST ZONE (MOST SNOW DEPTHS MEASURED AT OPEN SITES)
!*                                IOPEN=1 IN OPEN, O IN FOREST
!*
!c...Sturm Snow Classification:
!c
!c  		water = 0            RHOMIN
!c  		tundra snow = 1      200 
!c  		taiga snow = 2       160 
!c  		maritime snow = 3    160 
!c  		ephemeral snow = 4   180 
!c  		prairie snow = 5     140 
!c  		alpine snow = 6      120 
!c  		ice = 7 (ice caps)   (same as Tundra)
!c
!
!      IMPLICIT NONE
!
!      INTEGER, intent(in):: N_TIME, ICL,IOPEN
!      
!      INTEGER, intent(in):: N_DAYS_IN_TIMESTEP(N_TIME)
!      REAL, intent(in)::    PADJ
!      REAL, intent(in)::    TAIR(N_TIME), PRE(N_TIME)
!      real, intent(out)::   SNOD(N_TIME), SDEN(N_TIME)
!     
!      REAL save_OLD, save_CD, TSFC(2), OLD, CD
!      REAL t2_air, t2_last, pcpn
!      real NEW,NEWP
!
!      INTEGER SIXHRS, i
!          
!      save_old = 0
!      save_cd  = 0
!      
!      do i = 1,N_TIME
!      	  
!          SIXHRS = N_DAYS_IN_TIMESTEP(i) * 24
!          
!          t2_air = TAIR(i)
!          
!          if (i ==1) then 
!          t2_last = TAIR(i)
!          else
!          t2_last = TAIR(i-1)
!          endif
!          
!          pcpn = PRE(i)
!          
!          TSFC(1) = t2_last
!          TSFC(2) = t2_air
!          
!          OLD = save_old
!          CD  = save_cd         
!          
!          call NEW_BRUCE(OLD,NEW,NEWP,TSFC,PCPN,CD,ICL,IOPEN,Padj
!     *  ,SIXHRS)      
!     
!     	  save_old = new
!     	  save_cd  = newp
!     	  
!     	  SNOD(i) = new
!     	  SDEN(i) = newp
!      
!      enddo
!      
!      RETURN
!      
!      END SUBROUTINE RUN_SNOW_MODEL

c----------------------------------------------------------------------
      SUBROUTINE NEW_BRUCE(model)
          
      IMPLICIT NONE 
      
      type (snow_model_type):: model

* 
*AUTHOR  - B. BRASNETT  FEBRUARY 1997
*
*PURPOSE - EMPIRICAL ALGORITHM TO MELT SNOW ACCORDING TO THE 
*          SURFACE TEMPERATURE AND INCREASE SNOW DEPTH ACCORDING
*          TO THE PRECIPITATION THAT HAS FALLEN SINCE THE LAST 
*          ANALYSIS TIME
* 
*ARGUMENTS- 
*   I/O   - OLD    - ORIGINAL SNOW DEPTH FIELD (cm)
*    O    - NEW    - MODIFIED SNOW DEPTH FIELD (cm)
*    I    - TSFC   - SURFACE TEMPERATURE FIELDS (NORMALLY THE 6 HOUR
*                    TEMPERATURES FROM THE START TO THE END OF THE INTERVAL - deg.C)
*    I    - PCPN   - PRECIPITATION FIELD (m)
*   I/O   - CD     - INITIAL MEAN DENSITY OF SNOW PACK IN KG/M3
*    O    - NEWP   - FINAL MEAN DENSITY OF SNOW PACK IN KG/M3
*    I    - ICL    - SNOW CLIMATE CLASS, STURM ET AL 1995 (CODE FROM 0-7)
*    I    - IOPEN  - FORESTED/OPEN FLAG FOR VALIDATING RESULTS IN BOREAL
*                    FOREST ZONE (MOST SNOW DEPTHS MEASURED AT OPEN SITES)
*                    IOPEN=1 IN OPEN, O IN FOREST
*
c...Sturm Snow Classification:
c
c  		water = 0            RHOMIN
c  		tundra snow = 1      200 
c  		taiga snow = 2       160 
c  		maritime snow = 3    160 
c  		ephemeral snow = 4   180 
c  		prairie snow = 5     140 
c  		alpine snow = 6      120 
c  		ice = 7 (ice caps)   (same as Tundra)
c
*------------------------------------------------------------------------ 
* 
*IMPLICITS:
      INTEGER SIXHRS
C     PARAMETER (SIXHRS = 6)
c
      REAL    NEW, NEWDENS
      REAL    OLD,TSFC(2),Tmelt,TDD,Padj
      real    pcpn
      REAL    CD,NEWP,dd,den_gcm,del_den,C2
      real    Lf,Cw,rhow,rhoice,phase,rain,snow,rmelt,Prain
      INTEGER TIME,ICL,IOPEN
      REAL    TS1,TS2,RHOMIN(7),RHOMAX,GAMMA,DGAIN
      REAL    H2O,DENSITY,SIXTH,TS,FRESH
      REAL    sdepcm, denmax, delt, rhonew, den_diff
      REAL    timfac, sfall, rhosfall
*     UNITS: RHOMIN, RHOMAX KG/M3
      data   rhomin / 200., 160., 160., 180., 140., 120., 200./
      DATA   RHOMAX / 600./
      
      OLD    = MODEL%OLD
      TSFC   = MODEL%TSFC
      PCPN   = MODEL%PCPN *0.001
      CD     = MODEL%CD
      ICL    = MODEL%ICL
      IOPEN  = MODEL%IOPEN
      Padj   = MODEL%PADJ
      SIXHRS = MODEL%SIXHRS
      NEW    = 0
      NEWP   = 0
            
cf2py intent(out) NEW, NEWP      
      
*
*_____GAMMA IS THE MELTING RATE, DGAIN IS THE DENSIFICATION RATE
*
      DATA rhow, rhoice, Cw, Lf /1000., 917., 4.18e3, 0.334e6/
* 
      Tmelt = -1.  !melt threshold temp
      SIXTH = 1./float(SIXHRS)
      DELT = 3600. !1-hour time step (s)
c
c	write(66,*)' In NEW_BRUCE: OLD DENSITY = ',CD,' TSFC = ',tsfc,
c     *             ' PCPN= ',pcpn
c
*_____SCREEN OUT GRID POINTS WITH NO SNOW AND NO POSSIBLITY OF GETTING
*     SNOW OVER THE PERIOD
*
      IF (.NOT. (OLD .LE. 0. .AND. TSFC(1) .GT. 2.
     *                         .AND. TSFC(2) .GT. 2.)) THEN
*
*_____CONVERT SNOW VOLUME TO WATER EQUIVALENT (MM)
*
          DENSITY = CD
          DENSITY = AMAX1(RHOMIN(icl),AMIN1(RHOMAX,DENSITY))
          H2O = OLD*DENSITY*0.01 !snow water equiv (mm)
c
c...Determine melt factor (gamma) as a function of snow density (age)
c   and vegetation type following Kuusisto (1980)
c
       if(icl.eq.2 .and. iopen.eq.0) then !boreal forest
c	   dd = 0.0104 * density - 0.70
       dd = 0.0104/2. * density - 0.70 !reduce melt index by half
c	   if(dd.lt.1.4) dd = 1.4
	   if(dd.lt.0.1) dd = 0.1
	   if(dd.gt.3.5) dd = 3.5
	   PCPN = 0.8*PCPN !reduce precip 20% for canopy interception/sublimation
	else
c	   dd = 0.0196 * density - 2.39	!open areas
	   dd = 0.0196/2. * density - 2.39	!open areas
c	   if(dd.lt.1.5) dd = 1.5
	   if(dd.lt.0.1) dd = 0.1
	   if(dd.gt.5.5) dd = 5.5
	endif
c
	GAMMA = dd/24. !convert from mm/day to mm/hr
c
c...Reduce Tundra and Prairie snowpack precip 20% for blowing snow
c   sublimation loss (low vegetation, exposed to wind)
c
	if(icl.eq.1 .or. icl.eq.5)then
	   PCPN = Padj * PCPN
	endif
*
*_______FOR EACH TIMESTEP, MELTING ALGORITHM FIRST, THEN
*       DENSIFICATION DUE TO MELTING, THEN ADD SNOWFALL ACCUMULATION
*       ADJUST DENSITY FOR NEW SNOW AFTER THE FINAL TIMESTEP
*
          TS1 = TSFC(1)
          TS2 = TSFC(2)
          NEW = H2O
c
c...Compute new snowfall density as function of air temperature following
c   Hedstrom and Pomeroy (1998).  For temperatures greater than 0C assume 
c   a linear relationship between snow density and temp following
c   Pomeroy and Gray (1995) Fig. 1 to max of 200 kg.m-3. (RB 30/09/98)
c
	  if(ts1.le.0) then
	     rhosfall = 67.9 + 51.3*exp(ts1/2.6)
	  else
	     rhosfall = 119.2 + 20.*ts1
	     if(rhosfall.gt.200.) rhosfall = 200.
	  endif
c
*_______PRECIPITATION HAS UNITS OF METRES
*       CONVERT METRES TO MILLIMETERS (I.E. KG/M2) BEFORE USING THE PCPN
*
c
c...Determine precip phase.  Precip is the accumulated total (m) over 6 hrs.
c   Do not treat liquid water storage/refreeze in snowpack.
c
	  SNOW = 0.
	  RAIN = 0.
	  if(PCPN.gt.0.) then
	     if(TS1.gt.0.) then
		phase = 0.
	     else
		phase = 1.
	     endif
c
c...Evaluate the inclusion of mixed precip
c
c	     if(TS1.ge.2.) phase = 0.
c	     if(TS1.le.0.) phase = 1.
c	     if(TS1.gt.0. .and. TS1.lt.2.) then
c	        phase = 1.0 - TS1*0.5
c	     endif
c 
            SNOW = PCPN*phase
            RAIN = PCPN*(1.-phase)
	  endif
c
          IF (SNOW .GT. 0.0) THEN
	    sfall = 0.5*SIXTH*1000.*AMAX1(0.0,SNOW)
	    if(sfall.gt.0.) then
             NEW = NEW + sfall !accumulate new snow water equiv
             FRESH = AMAX1(0.0,sfall)
             NEWDENS = (rhosfall*FRESH + DENSITY*(NEW - FRESH))/NEW !adjust density for new snow
	     DENSITY = AMAX1(NEWDENS,RHOMIN(icl))
	    endif
          ENDIF
c
c...Take any rain melt into effect
c
	  if (rain.gt.0. .and. TS1.gt.0.) then
	     Prain = RAIN*1000./(6.*3600.) !convert rain back to Kg/m2/s
	     rmelt = (rhow*Cw*Prain*TS1)/(Lf*rhoice)  ! Kg/m2/s (mm/s)
	     rmelt = rmelt/1000. * (6.*3600.) ! m
	     NEW = AMAX1(0.0,NEW - 0.5*SIXTH*1000.*rmelt)
	  endif
c
c_________MELT AT TEMPERATURE TS1 (weighted by 0.5)
c
	  TDD = TS1 - Tmelt
          IF (TDD .GT. 0.0) THEN
	     NEW = AMAX1(0.0, NEW - 0.5*GAMMA*TDD)
	  ENDIF
c
c...AGE SNOW
c
c..Warm wet snow - use higher upper limit for settling. Value of 700 selected
c  based on performance at Col de Porte.
c
          IF(TS1.ge.Tmelt .and. NEW.GT.0.) then
	    sdepcm = NEW/DENSITY * 100.
	    denmax = 700. - (20470./sdepcm)*(1.-exp(-sdepcm/67.3))
c	    write(66,*) density,denmax
	    den_diff = denmax - density
            if(den_diff.gt.0.1) THEN
             TIMFAC=EXP(LOG((denmax-DENSITY)/200.0)-2.778E-6*(DELT/2.)) !delt divided by 2 for first and last time steps
             RHONEW=denmax-200.0*TIMFAC
             DENSITY=AMIN1(RHOMAX,RHONEW)
            else
	     density = AMIN1(RHOMAX,density)
	    endif
 	  ENDIF
c
c...Cold snow aging: 
c
	  if(TS1.lt.Tmelt .and. NEW.GT.0.) then
c
	     if(icl.eq.2 .or. icl.eq.6) then !cold snowpacks (depth hoar)
	        C2 = -28.
	     else
	        C2 = -21.
	     endif
c
c...Snow settling as a function of SWE and temperature following
c   Anderson (1978). Bulk SWE is multiplied by factor of 0.6 to approximate
c   computing over entire snow column.  Different values of C2 used (28 vs 21)
c   for deep snowpacks.
c
	    den_gcm = density/1000.
	    del_den = 0.02*exp(-0.08*(Tmelt-TS1))*
     *                0.6*NEW/10.*exp(C2*den_gcm)
	    dgain = del_den*1000. !convert to kg.m-3
	    RHONEW = density + dgain/2.
            DENSITY=AMIN1(RHOMAX,RHONEW)
	  endif
c
c	  write(66,*)' Density after TS1 of ',ts1,' = ',density
c
c-------------------------------------------------------------------------
c
          DO 10 TIME=1, SIXHRS-1
            TS = ((TSFC(2) - TSFC(1))*
     *             FLOAT(TIME)*SIXTH + TSFC(1))
c
c...Compute new snowfall density as function of air temperature following
c   Hedstrom and Pomeroy (1998)
c
	    if(ts.le.0) then
	      rhosfall = 67.9 + 51.3*exp(ts/2.6)
	    else
	      rhosfall = 119.2 + 20.*ts
	      if(rhosfall.gt.200.) rhosfall = 200.
	    endif
c
c...Determine precip phase, add new snowfall and adjust density
c
	    SNOW = 0.
	    RAIN = 0.
	    if(PCPN.gt.0.) then

	      if(TS.gt.0.) then
		phase = 0.
	      else
		phase = 1.
	      endif
c
c...Evaluate the inclusion of mixed precip
c
c	     if(TS.ge.2.) phase = 0.
c	     if(TS.le.0.) phase = 1.
c	     if(TS.gt.0. .and. TS.lt.2.) then
c	        phase = 1.0 - TS*0.5
c	     endif

              SNOW = PCPN*phase
	      RAIN = PCPN*(1.-phase)
	    endif
c
            IF(SNOW .GT. 0.0) THEN
	     sfall = SIXTH*1000.*AMAX1(0.0,SNOW)
	     if(sfall.gt.0.) then
               NEW = NEW + sfall !accumulate new snow water equiv
               FRESH = AMAX1(0.0,sfall)
               NEWDENS = (rhosfall*FRESH + DENSITY*(NEW - FRESH))/NEW !adjust density for new snow
	       DENSITY = AMAX1(NEWDENS,RHOMIN(icl))
	     endif
            ENDIF
c
c...Take any rain melt into effect.
c
	    if (rain.gt.0. .and. TS.gt.0.) then
	      Prain = RAIN*1000./(6.*3600.) !convert rain back to Kg/m2/s
	      rmelt = (rhow*Cw*Prain*TS)/(Lf*rhoice)  ! Kg/m2/s (mm/s)
	      rmelt = rmelt/1000. * (6.*3600.) ! m
	      NEW = AMAX1(0.0,NEW - SIXTH*1000.*rmelt)
	    endif
c
c___________MELT AT TEMPERATURE TS
c
	    TDD = TS - Tmelt
            IF (TDD .GT. 0.0) then
	       NEW = AMAX1(0.0, NEW - GAMMA*TDD)
	    endif
*
*_________DENSIFICATION DUE TO MELT-FREEZE METAMORPHISM
*
c
c..Warm wet snow - use higher upper limit for settling. Value of 700 selected
c  based on performance at Col de Porte.
c
          IF(TS.ge.Tmelt .and. NEW.GT.0.) then
	   sdepcm = NEW/DENSITY * 100.
	   denmax = 700. - (20470./sdepcm)*(1.-exp(-sdepcm/67.3))
c	   write(66,*) density,denmax
	   den_diff = denmax - density
           if(den_diff.gt.0.1) THEN
             TIMFAC=EXP(LOG((denmax-DENSITY)/200.0)-2.778E-6*DELT)                
             RHONEW=denmax-200.0*TIMFAC
             DENSITY=AMIN1(RHOMAX,RHONEW)
           else
	     density = AMIN1(RHOMAX,density)
           endif
 	  ENDIF
c
c...Cold snow aging:
c
	  if(TS.lt.Tmelt .and. NEW.GT.0.) then
c
	     if(icl.eq.2 .or. icl.eq.6) then !deep cold snowpacks
	        C2 = -28.
	     else
	        C2 = -21.
	     endif

c...Snow settling as a function of SWE and temperature following
c   Anderson (1978). Bulk SWE is multiplied by factor of 0.6 to approximate
c   computing over entire snow column. Different values of C2 used (28 vs 21)
c   for deep snowpacks.
c
	    den_gcm = density/1000.
	    del_den = 0.02*exp(-0.08*(Tmelt-TS))*
     *                0.6*NEW/10.*exp(C2*den_gcm)
	    dgain = del_den*1000. !convert to kg.m-3
	    RHONEW = density + dgain
            DENSITY=AMIN1(RHOMAX,RHONEW)
	  endif
c
c	  write(66,*)' Density after TS of ',ts,' = ',density
c
 10      CONTINUE
c
c-----------------------------------------------------------------------------------
c_________Conditions for TS2 (weighted by 0.5)
c
c...Compute new snowfall density as function of air temperature following
c   Hedstrom and Pomeroy (1998)
c
	  if(ts2.le.0) then
	     rhosfall = 67.9 + 51.3*exp(ts2/2.6)
	  else
	     rhosfall = 119.2 + 20.*ts2
	     if(rhosfall.gt.200.) rhosfall = 200.
	  endif	  
c
c...Determine precip phase, add new snowfall and adjust density
c
	    SNOW = 0.
	    RAIN = 0.
	    if(PCPN.gt.0.) then
	      if(TS2.gt.0.) then
		phase = 0.
	      else
		phase = 1.
	      endif
c
c...Evaluate the inclusion of mixed precip
c
c	     if(TS2.ge.2.) phase = 0.
c	     if(TS2.le.0.) phase = 1.
c	     if(TS2.gt.0. .and. TS2.lt.2.) then
c	        phase = 1.0 - TS2*0.5
c	     endif

              SNOW = PCPN*phase
	      RAIN = PCPN*(1.-phase)
	    endif
c
          IF (SNOW .GT. 0.0) THEN
	    sfall = 0.5*SIXTH*1000.*AMAX1(0.0,SNOW)
	    if(sfall.gt.0.) then
             NEW = NEW + sfall !accumulate new snow water equiv
             FRESH = AMAX1(0.0,sfall)
             NEWDENS = (rhosfall*FRESH + DENSITY*(NEW - FRESH))/NEW !adjust density for new snow
	     DENSITY = AMAX1(NEWDENS,RHOMIN(icl))
	    endif
          ENDIF
c
c..Take any rain melt into account
c
	  if (rain.gt.0.) then
	     Prain = RAIN*1000./(6.*3600.) !convert rain back to Kg/m2/s
	     rmelt = (rhow*Cw*Prain*TS2)/(Lf*rhoice)  ! Kg/m2/s (mm/s)
	     rmelt = rmelt/1000. * (6.*3600.) ! m
	     NEW = AMAX1(0.0,NEW - 0.5*SIXTH*1000.*rmelt)
	  endif
c
c...MELT at TSFC(2)
c
	  TDD = TS2 - Tmelt
          IF (TDD .GT. 0.0) then
	      NEW = AMAX1(0.0, NEW - 0.5*GAMMA*TDD)
	  ENDIF
c
c...Warm snow aging
c
          IF(TS2.ge.Tmelt .and. NEW.GT.0.) then
	   sdepcm = NEW/DENSITY * 100.
	   denmax = 700. - (20470./sdepcm)*(1.-exp(-sdepcm/67.3))
c	    write(66,*) density,denmax
	   den_diff = denmax - density
           if(den_diff.gt.0.1) THEN
             TIMFAC=EXP(LOG((denmax-DENSITY)/200.0)-2.778E-6*(DELT/2.))
             RHONEW=denmax-200.0*TIMFAC
             DENSITY=AMIN1(RHOMAX,RHONEW)
	   else
	     density = AMIN1(RHOMAX,density)
           endif
 	  ENDIF
c
c...Cold snow aging
c
	  if(TS2.lt.Tmelt .and. NEW.GT.0.) then
c
	     if(icl.eq.2 .or. icl.eq.6) then !deep cold snowpacks
	        C2 = -28.
	     else
	        C2 = -21.
	     endif
c
c...Snow settling as a function of SWE and temperature following
c   Anderson (1978). Bulk SWE is multiplied by factor of 0.6 to approximate
c   computing over entire snow column.  Different values of C2 used (28 vs 21)
c   for deep snowpacks.
c
	    den_gcm = density/1000.
	    del_den = 0.02*exp(-0.08*(Tmelt-TS2))*
     *                0.6*NEW/10.*exp(C2*den_gcm)
	    dgain = del_den*1000. !convert to kg.m-3
	    RHONEW = density + dgain/2.
            DENSITY=AMIN1(RHOMAX,RHONEW)
	  endif
c
c	  write(66,*)' Density after TS2 of ',ts2,' = ',density
*
*_______CONVERT FROM WATER EQUIVALENT TO DEPTH
*
          NEW  = 100.0*NEW/DENSITY
          NEWP = DENSITY
          NEW  = AMIN1(NEW, 600.) !600 cm max
          CD   = NEWP
c
c	  write(66,*)' Final density = ',NEWP
c
          OLD  = NEW
 20       CONTINUE
        ELSE
          NEW  = 0.0
          NEWP = RHOMIN(icl)
        ENDIF
 30   CONTINUE
 
      MODEL%NEW  = NEW
      MODEL%NEWP = NEWP
      
* 
      RETURN
      END SUBROUTINE NEW_BRUCE
      
      end module snow_model