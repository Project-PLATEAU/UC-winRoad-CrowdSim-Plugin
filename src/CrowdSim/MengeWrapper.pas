unit MengeWrapper;

interface

uses
    Windows,
    SysUtils,
    Classes,
    SyncObjs;

const
    {$IFDEF DEBUG}
        DLL_NAME = 'MengeCore_d.dll';
    {$ELSE}
        DLL_NAME = 'MengeCore.dll';
    {$ENDIF}

type    

//	/*!
//	 *	@brief		Initializes a simulator.  The simulator uses the given pedestrian @p model type
//	 *				and is initialized using the given behavior file and scene file.
//	 *
//	 *	@param		behaveFile		Path to the behavior file.
//	 *	@param		sceneFile		Path to the scene specification file.
//	 *	@param		model			Name of the model type to use.
//	 *	@param		pluginPath		Optional path to location of plugin directories.  If not
//	 *								provided, plugins will *not* be loaded.
//	 *	@returns	True if initialization was successful.
//	 */
//function    InitSimulator(behaveFile: PAnsiChar; sceneFile: PAnsiChar; model: PAnsiChar; pluginPath: PAnsiChar): Boolean; cdecl; external DLL_NAME;
    TInitSimulator = function(behaveFile: PAnsiChar; sceneFile: PAnsiChar; model: PAnsiChar; pluginPath: PAnsiChar): Boolean cdecl;

//	/*!
//	 *	@brief		Sets the time step for the simulator.
//	 */
//procedure   SetTimeStep(timestep: Single); cdecl; external DLL_NAME;
    TSetTimeStep = procedure(timestep: Single) cdecl;

//	/*!
//	 *	@brief		Advances the state of the simulator one time step.
//	 *
//	 *	@returns	True if the simulation can keep running.
//	 */
//function    DoStep(): Boolean; cdecl; external DLL_NAME;
    TDoStep = function(): Boolean cdecl;

///*!
// @brief    Reports the name of the state with the given id.
//
// @param    state_id   The id of the desired state.
// @returns  A pointer to the c-string of the state's name. Nullptr if state_id does not refer to a
//           valid state.
// */
//function    GetStateName(state_id: NativeUInt): PAnsiChar; cdecl; external DLL_NAME;
    TGetStateName = function(state_id: NativeUInt): PAnsiChar cdecl;

///*!
// @brief    Reports the number of states in the BFSM.
// */
    TStateCount = function(): NativeUInt cdecl;


//	/*!
//	 *	@brief		Reports the number of agents in the simulation.
//	 *
//	 *	@returns	The agent count.
//	 */
//function    AgentCount(): NativeUInt; cdecl; external DLL_NAME;
    TAgentCount = function(): NativeUInt cdecl;

//	/*!
//	 *	@brief		Reports the 3D position of the indicated agent.
//	 *
//	 *	@param		i		The index of the desired agent.
//	 *	@param[out]	x		The position's x-component.
//	 *	@param[out]	y		The position's y-component.
//	 *	@param[out]	z		The position's z-component.
//	 *	@returns			True if the values were successfully set.
//	 */
//function    GetAgentPosition(i: NativeUInt; var x, y, z: Single): Boolean; cdecl; external DLL_NAME;
    TGetAgentPosition = function(i: NativeUInt; var x, y, z: Single): Boolean cdecl;

//	/*!
//	 *	@brief		Reports the 3D velocity of the indicated agent.
//	 *
//	 *	@param		i		The index of the desired agent.
//	 *	@param[out]	x		The velocity's x-component.
//	 *	@param[out]	y		The velocity's y-component.
//	 *	@param[out]	z		The velocity's z-component.
//	 *	@returns			True if the values were successfully set.
//	 */
//function    GetAgentVelocity(i: NativeUInt; var x, y, z: Single): Boolean; cdecl; external DLL_NAME;
    TGetAgentVelocity = function(i: NativeUInt; var x, y, z: Single): Boolean cdecl;

//    /*!
//     @brief   Reports the 2D preferred velocity of the indicated agent.
//
//     @param[in]   i    The index of the desired agent.
//     @param[out]  x    The preferred velocity's x-component.
//     @param[out]  y    The preferred velocity's y-component.
//     @returns     True if the values were successfully set.
//     */
//function    GetAgentPrefVelocity(i: NativeUInt; var x, y: Single): Boolean; cdecl; external DLL_NAME;
    TGetAgentPrefVelocity = function(i: NativeUInt; var x, y: Single): Boolean cdecl;

//    /*!
//     @brief   Reports the id of the state the indicated agent is currently in.
//
//     @param[in]   i         The index of the desired agent.
//     @param[out]  state_id  The id of the state the agent is currently in.
//     @returns     True if the values were successfully set.
//     */
//function    GetAgentState(i: NativeUInt; var state_id: NativeUInt): Boolean; cdecl; external DLL_NAME;
    TGetAgentState = function(i: NativeUInt; var state_id: NativeUInt): Boolean cdecl;


//	/*!
//	 *	@brief		Reports the 2D orientation of the indicated agent.  It is the facing direction
//	 *				of the agent, projected onto the xz plane.
//	 *
//	 *	@param		i		The index of the desired agent.
//	 *	@param[out]	x		The orient's x-component.
//	 *	@param[out]	y		The orient's y-component.
//	 *	@returns			True if the values were successfully set.
//	 */
//function GetAgentOrient(i: NativeUInt; var x, y: Single): Boolean; cdecl; external DLL_NAME;
    TGetAgentOrient = function(i: NativeUInt; var x, y: Single): Boolean cdecl;

//	/*!
//	 *	@brief		Reports the agent class for this particular class.
//	 *
//	 *	@param		i		The index of the desired agent.
//	 *	@returns			The agent's class (-1 if it can't be found).
//	 */
//function    GetAgentClass(i: NativeUInt): Integer; cdecl; external DLL_NAME;
    TGetAgentClass = function(i: NativeUInt): Integer cdecl;

//	/*!
//	 *	@brief		Reports the radius of the given agent.
//	 *
//	 *	@param		i		The index of the desired agent.
//	 *	@returns			The agent's radius (negative for errors).
//	 */
//function    GetAgentRadius(i: NativeUInt): Single; cdecl; external DLL_NAME;
    TGetAgentRadius = function(i: NativeUInt): Single cdecl;

//	/*!
//	 *	@brief		Reports the number of external triggers exposed in the simulator.
//	 *	@returns	The number of external triggers available.
//	 */
//function    ExternalTriggerCount(): Integer; cdecl; external DLL_NAME;
    TExternalTriggerCount = function(): Integer cdecl;

//	/*!
//	 *	@brief		The name of the i_th external trigger. If `i` is _not_ a valid trigger index,
//	 *				`nullptr` is returned.
//	 *	@param		i		The index of the desired external trigger. Must be <= the value
//	 *						returned by ExternalTriggerCount().
//	 *	@returns	The name of the i_th trigger, or null if `i` is invalid.
//	 */
//function    ExternalTriggerName(i: Integer): PAnsiChar; cdecl; external DLL_NAME;
    TExternalTriggerName = function(i: Integer): PAnsiChar cdecl;

//	/*!
//	 *	@brief		Fires the trigger of the given name.
//	 *	If the name does not refer to a valid external trigger, nothing happens.
//	 *	@param	triggerName		The name of the trigger to fire.
//	 */
//procedure   FireExternalTrigger(lpString: PAnsiChar); cdecl; external DLL_NAME;
    TFireExternalTrigger = procedure(lpString: PAnsiChar) cdecl;

///*!
// @brief   Report the total number of obstacles in the simulation.
//
// @returns  The total number of obstacles.
// */
//function    ObstacleCount(): NativeUInt; cdecl; external DLL_NAME;
    TObstacleCount = function(): NativeUInt cdecl;

// /*!
//  @brief   Given the index of one obstacle, reports the index of the next obstacle in the loop.
//
//  @param   i   The index of the query obstacle.
//  @returns The index of the obstacle's whose point p0 is the same as this obstacles p1.
//  */
//function    GetNextObstacle(i: NativeUInt): NativeUInt; cdecl; external DLL_NAME;
    TGetNextObstacle = function(i: NativeUInt): NativeUInt cdecl;
    
///*!
// @brief   Given the index of an obstacle, returns both endpoints, p0 and p1, that make up the
//          obstacle.
//
// @param    i     The index of the query obstacle
// @param    x0    The x-position of end point p0, set by this function.
// @param    y0    The y-position of end point p0, set by this function.
// @param    z0    The z-position of end point p0, set by this function.
// @param    x1    The x-position of end point p1, set by this function.
// @param    y1    The y-position of end point p1, set by this function.
// @param    z1    The z-position of end point p1, set by this function.
// @returns  True if the values have been properly set.
// */
//function    GetObstacleEndPoints(i: NativeUInt; var x0, y0, z0, x1, y1, z1: Single): Boolean; cdecl; external DLL_NAME;
    TGetObstacleEndPoints = function(i: NativeUInt; var x0, y0, z0, x1, y1, z1: Single): Boolean cdecl;
    
///*!
// @brief   Given the index of an obstacle, returns its first endpoint, p0.
//
// @param    i     The index of the query obstacle
// @param    x0    The x-position of end point p0, set by this function.
// @param    y0    The y-position of end point p0, set by this function.
// @param    z0    The z-position of end point p0, set by this function.
// @returns  True if the values have been properly set.
// */
//function    GetObstacleP0(i: NativeUInt; var x0, y0, z0: Single): Boolean; cdecl; external DLL_NAME;
    TGetObstacleP0 = function(i: NativeUInt; var x0, y0, z0: Single): Boolean cdecl;

// /*!
//  @brief   Given the index of an obstacle, returns its second endpoints, p1.
// 
//  @param    i     The index of the query obstacle
//  @param    x1    The x-position of end point p1, set by this function.
//  @param    y1    The y-position of end point p1, set by this function.
//  @param    z1    The z-position of end point p1, set by this function.
//  @returns  True if the values have been properly set.
//  */
//function    GetObstacleP1(i: NativeUInt; var x1, y1, z1: Single): Boolean; cdecl; external DLL_NAME;
    TGetObstacleP1 = function(i: NativeUInt; var x1, y1, z1: Single): Boolean cdecl;

    /// <summary>
    ///    Menge関連のDLLを管理するクラス
    /// </summary>
    TMengeWrapper = class
        private
            p_handle    : THandle;
            FInitSimulator  : TInitSimulator;
            FSetTimeStep    : TSetTimeStep;
            FDoStep         : TDoStep;
            FGetStateName   : TGetStateName;
            FStateCount     : TStateCount;
            FAgentCount     : TAgentCount;
            FGetAgentPosition   : TGetAgentPosition;
            FGetAgentVelocity   : TGetAgentVelocity;
            FGetAgentPrefVelocity   : TGetAgentPrefVelocity;
            FGetAgentState          : TGetAgentState;
            FGetAgentOrient         : TGetAgentOrient;
            FGetAgentClass          : TGetAgentClass;
            FGetAgentRadius         : TGetAgentRadius;
            FExternalTriggerCount   : TExternalTriggerCount;
            FExternalTriggerName    : TExternalTriggerName;
            FFireExternalTrigger    : TFireExternalTrigger;
            FObstacleCount          : TObstacleCount;
            FGetNextObstacle        : TGetNextObstacle;
            FGetObstacleEndPoints   : TGetObstacleEndPoints;
            FGetObstacleP0          : TGetObstacleP0;
            FGetObstacleP1          : TGetObstacleP1;

            procedure   LoadMengeDLL;
            procedure   UnloadMengeDLL;
            function    GetProcAddressOrRaise(const ProcName: string): Pointer;

            procedure   WaitTheForceUnloadDLL;

        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;

            procedure   BeforeCalculation;
            procedure   AfterCalculation;

            function    InitSimulator(behaveFile: PAnsiChar; sceneFile: PAnsiChar; model: PAnsiChar; pluginPath: PAnsiChar): Boolean;
            procedure   SetTimeStep(timestep: Single);
            function    DoStep(): Boolean;
            function    GetStateName(state_id: NativeUInt): PAnsiChar;
            function    StateCount(): NativeUInt;
            function    AgentCount(): NativeUInt;
            function    GetAgentPosition(i: NativeUInt; var x, y, z: Single): Boolean;
            function    GetAgentVelocity(i: NativeUInt; var x, y, z: Single): Boolean;
            function    GetAgentPrefVelocity(i: NativeUInt; var x, y: Single): Boolean;
            function    GetAgentState(i: NativeUInt; var state_id: NativeUInt): Boolean;
            function    GetAgentOrient(i: NativeUInt; var x, y: Single): Boolean;
            function    GetAgentClass(i: NativeUInt): Integer;
            function    GetAgentRadius(i: NativeUInt): Single;
            function    ExternalTriggerCount(): Integer;
            function    ExternalTriggerName(i: Integer): PAnsiChar;
            procedure   FireExternalTrigger(lpString: PAnsiChar);
            function    ObstacleCount(): NativeUInt;
            function    GetNextObstacle(i: NativeUInt): NativeUInt;
            function    GetObstacleEndPoints(i: NativeUInt; var x0, y0, z0, x1, y1, z1: Single): Boolean;
            function    GetObstacleP0(i: NativeUInt; var x0, y0, z0: Single): Boolean;
            function    GetObstacleP1(i: NativeUInt; var x1, y1, z1: Single): Boolean;
            
        end;

implementation




{ TMengeWrapper }

procedure TMengeWrapper.AfterCalculation;
    begin
    UnloadMengeDLL;
    end;

procedure TMengeWrapper.AfterConstruction;
    begin
    inherited;
    p_handle := 0;
    end;

procedure TMengeWrapper.BeforeCalculation;
    begin
    LoadMengeDLL;
    end;

procedure TMengeWrapper.BeforeDestruction;
    begin
    inherited;
    UnloadMengeDLL;
    end;

procedure TMengeWrapper.LoadMengeDLL;
    begin
    if p_handle > 0 then
        UnloadMengeDLL;
    p_handle := LoadLibrary(DLL_NAME);

    @FInitSimulator         := GetProcAddressOrRaise('InitSimulator');
    @FSetTimeStep           := GetProcAddressOrRaise('SetTimeStep');
    @FDoStep                := GetProcAddressOrRaise('DoStep');
    @FGetStateName          := GetProcAddressOrRaise('GetStateName');
    @FStateCount            := GetProcAddressOrRaise('StateCount');
    @FAgentCount            := GetProcAddressOrRaise('AgentCount');
    @FGetAgentPosition      := GetProcAddressOrRaise('GetAgentPosition');
    @FGetAgentVelocity      := GetProcAddressOrRaise('GetAgentVelocity');
    @FGetAgentPrefVelocity  := GetProcAddressOrRaise('GetAgentPrefVelocity');
    @FGetAgentState         := GetProcAddressOrRaise('GetAgentState');
    @FGetAgentOrient        := GetProcAddressOrRaise('GetAgentOrient');
    @FGetAgentClass         := GetProcAddressOrRaise('GetAgentClass');
    @FGetAgentRadius        := GetProcAddressOrRaise('GetAgentRadius');
    @FExternalTriggerCount  := GetProcAddressOrRaise('ExternalTriggerCount');
    @FExternalTriggerName   := GetProcAddressOrRaise('ExternalTriggerName');
    @FFireExternalTrigger   := GetProcAddressOrRaise('FireExternalTrigger');
    @FObstacleCount         := GetProcAddressOrRaise('ObstacleCount');
    @FGetNextObstacle       := GetProcAddressOrRaise('GetNextObstacle');
    @FGetObstacleEndPoints  := GetProcAddressOrRaise('GetObstacleEndPoints');
    @FGetObstacleP0         := GetProcAddressOrRaise('GetObstacleP0');
    @FGetObstacleP1         := GetProcAddressOrRaise('GetObstacleP1');
    end;

procedure TMengeWrapper.UnloadMengeDLL;
    begin
    if p_handle > 0 then
        if not FreeLibrary(p_handle) then
            WaitTheForceUnloadDLL;

    p_handle := 0;

    @FInitSimulator  := nil;
    @FSetTimeStep    := nil;
    @FDoStep         := nil;
    @FGetStateName   := nil;
    @FStateCount     := nil;
    @FAgentCount     := nil;
    @FGetAgentPosition   := nil;
    @FGetAgentVelocity   := nil;
    @FGetAgentPrefVelocity   := nil;
    @FGetAgentState          := nil;
    @FGetAgentOrient         := nil;
    @FGetAgentClass          := nil;
    @FGetAgentRadius         := nil;
    @FExternalTriggerCount   := nil;
    @FExternalTriggerName    := nil;
    @FFireExternalTrigger    := nil;
    @FObstacleCount          := nil;
    @FGetNextObstacle        := nil;
    @FGetObstacleEndPoints   := nil;
    @FGetObstacleP0          := nil;
    @FGetObstacleP1          := nil;
    end;

procedure TMengeWrapper.WaitTheForceUnloadDLL;
    var
        finishedEvent   : TEvent;
    begin
    finishedEvent := TEvent.Create(nil, True, False, '');
    try
        TThread.CreateAnonymousThread(
            procedure
                begin
                try
                    FreeLibraryAndExitThread(p_handle, 0);
                finally
                    finishedEvent.SetEvent;
                    end;
                end
        ).Start;

        if finishedEvent.WaitFor(INFINITE) = wrSignaled then
            begin
            {$IFDEF DEBUG}
            OutputDebugString(PChar('DLLを解放できなかったので、強制解放しました。'));
            {$ENDIF}
            end;

    finally
        FreeAndNil(finishedEvent);
        end;
    end;

function TMengeWrapper.GetProcAddressOrRaise(const ProcName: string): Pointer;
    begin
    Result := GetProcAddress(p_handle, PChar(ProcName));
    if not Assigned(Result) then
        raise Exception.CreateFmt('Function %s not found in DLL: %s', [ProcName, DLL_NAME]);
    end;

function TMengeWrapper.AgentCount: NativeUInt;
    begin
    if not Assigned(FAgentCount) then
        raise Exception.Create('DLL function AgentCount is not loaded.');
    Result := FAgentCount;
    end;

function TMengeWrapper.DoStep: Boolean;
    begin
    if not Assigned(FDoStep) then
        raise Exception.Create('DLL function DoStep is not loaded.');
    Result := FDoStep;
    end;

function TMengeWrapper.ExternalTriggerCount: Integer;
    begin
    if not Assigned(FExternalTriggerCount) then
        raise Exception.Create('DLL function ExternalTriggerCount is not loaded.');
    Result := FExternalTriggerCount;
    end;

function TMengeWrapper.ExternalTriggerName(i: Integer): PAnsiChar;
    begin
    if not Assigned(FExternalTriggerName) then
        raise Exception.Create('DLL function ExternalTriggerName is not loaded.');
    Result := FExternalTriggerName(i);
    end;

procedure TMengeWrapper.FireExternalTrigger(lpString: PAnsiChar);
    begin
    if not Assigned(FFireExternalTrigger) then
        raise Exception.Create('DLL function FireExternalTrigger is not loaded.');
    FFireExternalTrigger(lpString);
    end;

function TMengeWrapper.ObstacleCount: NativeUInt;
    begin
    if not Assigned(FObstacleCount) then
        raise Exception.Create('DLL function ObstacleCount is not loaded.');
    Result := FObstacleCount;
    end;

procedure TMengeWrapper.SetTimeStep(timestep: Single);
    begin
    if not Assigned(FSetTimeStep) then
        raise Exception.Create('DLL function SetTimeStep is not loaded.');
    FSetTimeStep(timestep);
    end;

function TMengeWrapper.StateCount: NativeUInt;
    begin
    if not Assigned(FStateCount) then
        raise Exception.Create('DLL function StateCount is not loaded.');
    Result := FStateCount;
    end;

function TMengeWrapper.GetAgentClass(i: NativeUInt): Integer;
    begin
    if not Assigned(FGetAgentClass) then
        raise Exception.Create('DLL function GetAgentClass is not loaded.');
    Result := FGetAgentClass(i);
    end;

function TMengeWrapper.GetAgentOrient(i: NativeUInt; var x, y: Single): Boolean;
    begin
    if not Assigned(FGetAgentOrient) then
        raise Exception.Create('DLL function GetAgentOrient is not loaded.');
    Result := FGetAgentOrient(i, x, y);
    end;

function TMengeWrapper.GetAgentPosition(i: NativeUInt; var x, y, z: Single): Boolean;
    begin
    if not Assigned(FGetAgentPosition) then
        raise Exception.Create('DLL function GetAgentPosition is not loaded.');
    Result := FGetAgentPosition(i, x, y, z);
    end;

function TMengeWrapper.GetAgentPrefVelocity(i: NativeUInt; var x, y: Single): Boolean;
    begin
    if not Assigned(FGetAgentPrefVelocity) then
        raise Exception.Create('DLL function GetAgentPrefVelocity is not loaded.');
    Result := FGetAgentPrefVelocity(i, x, y);
    end;

function TMengeWrapper.GetAgentRadius(i: NativeUInt): Single;
    begin
    if not Assigned(FGetAgentRadius) then
        raise Exception.Create('DLL function GetAgentRadius is not loaded.');
    Result := FGetAgentRadius(i);
    end;

function TMengeWrapper.GetAgentState(i: NativeUInt; var state_id: NativeUInt): Boolean;
    begin
    if not Assigned(FGetAgentState) then
        raise Exception.Create('DLL function GetAgentState is not loaded.');
    Result := FGetAgentState(i, state_id);
    end;

function TMengeWrapper.GetAgentVelocity(i: NativeUInt; var x, y, z: Single): Boolean;
    begin
    if not Assigned(FGetAgentVelocity) then
        raise Exception.Create('DLL function GetAgentVelocity is not loaded.');
    Result := FGetAgentVelocity(i, x, y, z);
    end;

function TMengeWrapper.GetNextObstacle(i: NativeUInt): NativeUInt;
    begin
    if not Assigned(FGetNextObstacle) then
        raise Exception.Create('DLL function GetNextObstacle is not loaded.');
    Result := FGetNextObstacle(i);
    end;

function TMengeWrapper.GetObstacleEndPoints(i: NativeUInt; var x0, y0, z0, x1, y1, z1: Single): Boolean;
    begin
    if not Assigned(FGetObstacleEndPoints) then
        raise Exception.Create('DLL function GetObstacleEndPoints is not loaded.');
    Result := FGetObstacleEndPoints(i, x0, y0, z0, x1, y1, z1);
    end;

function TMengeWrapper.GetObstacleP0(i: NativeUInt; var x0, y0, z0: Single): Boolean;
    begin
    if not Assigned(FGetObstacleP0) then
        raise Exception.Create('DLL function GetObstacleP0 is not loaded.');
    Result := FGetObstacleP0(i, x0, y0, z0);
    end;

function TMengeWrapper.GetObstacleP1(i: NativeUInt; var x1, y1, z1: Single): Boolean;
    begin
    if not Assigned(FGetObstacleP1) then
        raise Exception.Create('DLL function GetObstacleP1 is not loaded.');
    Result := FGetObstacleP1(i, x1, y1, z1);
    end;

function TMengeWrapper.GetStateName(state_id: NativeUInt): PAnsiChar;
    begin
    if not Assigned(FGetStateName) then
        raise Exception.Create('DLL function GetStateName is not loaded.');
    Result := FGetStateName(state_id);
    end;

function TMengeWrapper.InitSimulator(behaveFile, sceneFile, model, pluginPath: PAnsiChar): Boolean;
    begin
    if not Assigned(FInitSimulator) then
        raise Exception.Create('DLL function InitSimulator is not loaded.');
    Result := FInitSimulator(behaveFile, sceneFile, model, pluginPath);
    end;

end.
