unit MengeAgent;

interface

uses
    System.Generics.Collections,
    XML.XMlIntf,
    F8Utils,
    PedestrianMap,
    PedestrianCell,
    PedestrianUtil,
    MovingFeature,AgentSettings,
    MengeGoalDefinitions,MengeEventDefinitions,MengeStateDefinitions,MengeActionDefinitions,MengeVelComponentDefinitions,
    MengeAgentDefinitions,MengeExperimentDefinitions;


type
    /// <summary>
    ///    Agentとそのパラメータを管理するクラス
    ///    Agentに関するシミュレーション計算や出力を行う
    /// </summary>
    MengeAgentClass = class
        private
            p_MovingFeature : MovingFeatureClass;
            p_ClassIndex : Integer;
            p_StartTime : integer;

            p_profile : TAgentProfile;//現所不変,将来的にRadius等が可変
            p_Group : TAgentGroup;//GeneratePosが可変
            p_Goals : TGoalSet;//不変
            p_States : TList<TState>;//不変
            p_Transitions : TList<TTransition>;//WaitのTimerが可変
            p_IsExport : Boolean;

            function GetIsExport : Boolean;
        public
            constructor Create(mf : MovingFeatureClass; ClassIndex : integer);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  Clone(index,dTime : Integer) : MengeAgentClass;

            procedure Exportprofile(parentNode : IXMLNode);
            procedure ExportGroup(parentNode : IXMLNode);
            procedure ExportGoals(parentNode : IXMLNode);
            procedure ExportStates(parentNode : IXMLNode);
            procedure ExportTransitions(parentNode: IXMLNode);

            procedure Update(Elapsed,SignalTime : Integer;const currentState : String);
            procedure UpdateStateOnMenge(name : String);
            procedure ConstituteTransitions;
            procedure ConstituteGroup;

            procedure ChangeClassIndex(index : Integer);

            function SummarizeBehavior(startTime, endTime: TDateTime;
                                       setting: TAgentSettingsType;
                                       map: TPedestrianMap): Boolean;
            
            property  IsExport      : Boolean            read GetIsExport;
            property  MovingFeature : MovingFeatureClass read p_MovingFeature;
        end;

    /// <summary>
    ///    MengeAgentClassのリスト
    /// </summary>
    MengeAgentList = Class
        private
            p_Agents   : TList<MengeAgentClass>;
            function GetNumberOfAgent: Integer;
            function GetAgent(const index: Integer): MengeAgentClass;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure CreateAgents(startTime,EndTime : TDateTime; simPer : Integer; setting: TAgentSettingsType);
            procedure CreateMagnificateFeature(magnification : integer);
            procedure Clear;
            function  Update(Elapsed,SignalTime : Integer) : Boolean;
            procedure UpdateStatesOnMenge;

            property Agent[const index : Integer] : MengeAgentClass read GetAgent;
            property NumberOfAgent : Integer read GetNumberOfAgent;
            end;




implementation

uses
    System.SysUtils,System.Math,System.DateUtils,
    Vcl.Dialogs,
    winapi.Windows,
    F8GLUtils,
    CellID,
    MengeConditionDefinitions,
    MengeGeneratorDefinitions;

{ MengeAgentClass }
//==============================================================================
constructor MengeAgentClass.Create(mf: MovingFeatureClass; ClassIndex : integer);
    begin
    p_MovingFeature := mf;
    p_ClassIndex := ClassIndex;
    p_StartTime := 0;
    end;

//==============================================================================
procedure MengeAgentClass.AfterConstruction;
    begin
    inherited;
    p_States := TList<TState>.Create;
    p_Transitions := TList<TTransition>.Create;

    p_IsExport := True;
    end;

//==============================================================================
procedure MengeAgentClass.BeforeDestruction;
    begin
    p_MovingFeature := nil;

    FreeAndNil(p_States);
    FreeAndNil(p_Transitions);

    if Assigned(p_profile) then
        FreeAndNil(p_profile);

    if Assigned(p_Group) then
        FreeAndNil(p_Group);

    if Assigned(p_Goals) then
        FreeAndNil(p_Goals);
    inherited;
    end;

//==============================================================================
procedure MengeAgentClass.ExportGoals(parentNode: IXMLNode);
    begin
    if not p_IsExport then
        Exit;
    p_Goals.ExportNode(parentNode);
    end;

//==============================================================================
procedure MengeAgentClass.ExportGroup(parentNode: IXMLNode);
    begin
    if not p_IsExport then
        Exit;
    p_Group.ExportNode(parentNode);
    end;

//==============================================================================
procedure MengeAgentClass.ExportProfile(parentNode: IXMLNode);
    begin
    if not p_IsExport then
        Exit;
    p_Profile.ExportNode(parentNode);
    end;

//==============================================================================
procedure MengeAgentClass.ExportStates(parentNode: IXMLNode);
    var
        state : TState;
    begin
    if not p_IsExport then
        Exit;
    for state in p_States do
        begin
        state.ExportNode(parentNode);
        end;
    end;

//==============================================================================
procedure MengeAgentClass.ExportTransitions(parentNode: IXMLNode);
    var
        Trans : TTransition;
    begin
    if not p_IsExport then
        Exit;
    for Trans in p_Transitions do
        begin
        Trans.ExportNode(parentNode);
        end;
    end;

//==============================================================================
procedure MengeAgentClass.Update(Elapsed,SignalTime : Integer;const  currentState : String);
    var
        tpos : GLPointType;
        index,i : integer;
    begin
    p_IsExport := True;
    if p_Goals.goals.Count=0 then
        begin
        p_IsExport := False;
        Exit
        end;

    //check Agent State at End of Previous Simulation
    if pos(STATE_NAME_WALK,currentState)>0 then
        begin
        p_IsExport := True;
        index := p_MovingFeature.numberoffocusedHistories - 1;
        if index>=0 then
            begin
            tpos := p_MovingFeature.FocusedHistory[index].position;
            TExplicitGenerator(p_Group.Generator).at_AgentArray[0] := ASGLPointType2(tpos[_x],
                                                                                     tpos[_z]);
            end;
        if p_Group.StateSelector.at_name <> currentState then
            begin
            for i :=0 to p_States.Count-1 do
                begin
                if currentState = p_States[i].at_name then
                    begin
                    p_Group.StateSelector.at_name := currentState;
                    Exit;
                    end;
                end;
            {$ifdef Debug}
            outputDebugString(PWideChar(Format('Wrong State is Assigned %s to %s',[p_Group.StateSelector.at_name, currentState])));
            {$endif}
            end;
        Exit;
        end
    else if pos(STATE_NAME_GOAL,currentState)>0 then
        begin
        //this Agent is already went outside in Simlation Area
        p_IsExport := False;
        Exit;
        end
    else if pos(STATE_NAME_WAIT,currentState)>0 then
        begin
        if (p_Transitions[0].Condition.at_type=TYPE_CONDITION_TIMER) then
            begin
            if TTimerCondition(p_Transitions[0].Condition).at_value < (Elapsed + SignalTime) then
                begin
                //Adjusting walking start time for this Simulation
                TTimerCondition(p_Transitions[0].Condition).at_value := p_StartTime - Elapsed;
                end
            else
                begin
                p_IsExport := False;
                Exit;
                end;
            end;
        end
    else if pos(STATE_NAME_NONE,currentState)>0 then
        begin//this Agent is not Calculated
        p_IsExport := False;
        end;
    end;

//==============================================================================
procedure MengeAgentClass.UpdateStateOnMenge(name: String);
    begin
    p_MovingFeature.lastStateName := name;
    end;

//==============================================================================
function MengeAgentClass.GetIsExport: Boolean;
    begin
    Result := p_IsExport;
    end;

//==========================================================================
procedure MengeAgentClass.ConstituteGroup;
    var
         exGenerator : TExplicitGenerator;
    begin
    if Assigned(p_Group) then
        begin
        FreeAndNil(p_Group);
        end;
    if p_States.Count<1 then
        begin
        Exit;
        end;

    p_Group := TAgentGroup.Create;
    p_Group.ProfileSelector := TProfileSelector.Create;
    p_Group.ProfileSelector.at_name := p_profile.at_name;
    p_Group.StateSelector := TStateSelector.Create;
    p_Group.StateSelector.at_name := p_States[0].at_name;

    exGenerator := TExplicitGenerator.Create;
    SetLength(exGenerator.at_AgentArray,1);

    exGenerator.at_AgentArray[0] := AsGLPointType2(-1*(p_ClassIndex div 100)-1,-1*(p_ClassIndex mod 100)-1);//ASGLPointType2(cellpos.X,cellpos.Z);
    p_Group.Generator := exGenerator;
    end;

//==========================================================================
procedure MengeAgentClass.constituteTransitions;
    var
        i : integer;
        trans : TTransition;
        timer : TTimerCondition;
    begin
    p_Transitions.Clear;
    Assert(1<p_States.Count,'invalid state id');
    for I := 0 to p_States.Count-2 do
        begin
        trans := TTransition.Create;

        SetLength(trans.at_from,1);
        trans.at_from[0] := p_States[i];
        trans.at_to := p_States[i+1];

        if (I=0) and (p_StartTime >= 1.0) then//遅延
            begin
            timer := TTimerCondition.Create;
            timer.at_value := p_StartTime;
            timer.at_dist := 'c';
            trans.Condition := timer;
            end
        else
            begin
            trans.Condition := TGoalReachedCondition.Create;
            end;
        p_Transitions.Add(trans);
        end;
    end;

//==============================================================================
function MengeAgentClass.SummarizeBehavior(startTime, endTime: TDateTime;
  setting: TAgentSettingsType; map: TPedestrianMap): Boolean;
    //==========================================================================
    procedure CreateAgentProfile;
        begin
        p_profile := TAgentProfile.Create;
        p_profile.at_name := 'Agent'+IntToStr(p_ClassIndex);
        p_profile.Common := TAgentProfileCommon.Create;
        p_profile.Common.at_maxSpeed := setting.maxWalkSpeed;
        p_profile.Common.at_class := p_ClassIndex;
        if setting.IsRain then
            p_profile.Common.at_r := setting.pedestrainRadiusinRainny
        else
            p_profile.Common.at_r := setting.pedestrainRadiusInSunny;

        end;
    //==========================================================================
    procedure Creategoals;
        var
            goal : TpointGoal;
            i : integer;
            checkRange : Integer;
            cellpos : TPoint3D;

            aID : TCellID;
            cell : TPedestrianCell;
            aStatus: PedestrianAreaStatus;

        begin
        p_Goals := TGoalSet.Create;
        checkRange := Trunc(setting.positionTolerance / map.Config.CellSize);
        for i := 0 to p_MovingFeature.numberOfFocusedHistories-1 do
            begin
            goal := TpointGoal.Create;
            cellpos.Assign(p_MovingFeature.focusedHistory[i].position[_x],0,p_MovingFeature.focusedHistory[i].position[_z]);
            if map.RequireCellInfo(cellpos,aID, aStatus) then
                begin
                if (aStatus = PedestrianAreaStatus._pasNoEntry) then
                    begin
                    if not map.RequireCellInfo(aID.ID,cell) then
                        continue;
                    if map.NearbySearch(checkRange,aID,cell) then
                        begin
                        cellpos := cell.CenterLocal;
                        end
                    else
                        begin
                        continue;
                        end;
                    end;
                goal.at_pos[_x] := cellpos.X;
                goal.at_pos[_y] := cellpos.Z;

                goal.at_id := i;
                p_Goals.goals.Add(goal);
                end;
            end;
        p_Goals.at_id := p_ClassIndex;
        end;
    //==========================================================================
    procedure CreateStates(dTime : double);
        var
            state : TState;
            sAction : TSetPropertyAction;
            exGoalSel : TExplicitGoalSelector;
            i : integer;
            telepo : TTeleportAction;
        begin
        if dTime >= 1.0 then//遅延
            begin
            state := TState.Create;
            state.at_name := Format('%s_%d_%d',[STATE_NAME_WAIT,p_ClassIndex,p_States.Count]);

            exGoalSel := TExplicitGoalSelector.Create;
            exGoalSel.at_goalSet := p_Goals.at_id;
            exGoalSel.at_goal :=  p_Goals.goals[0].at_id;;
            State.GoalSelector := exGoalSel;

            sAction := TSetPropertyAction.Create;
            sAction.at_property := 'pref_speed';
            sAction.at_dist := 'c';
            sAction.at_Value := 0.0;
            sAction.at_exit_reset := 1;

            state.Actions.Add(sAction);

            sAction := TSetPropertyAction.Create;
            sAction.at_property := 'neighbor_dist';
            sAction.at_dist := 'c';
            sAction.at_Value := 0;
            sAction.at_exit_reset := 1;

            state.Actions.Add(sAction);

            p_States.Add(state);
            p_MovingFeature.LastStateName := State.at_name;
            end;

        state := TState.Create;
        state.at_name := Format('%s_%d_%d',[STATE_NAME_TELEPORT,p_ClassIndex,p_States.Count]);
        telepo := TTeleportAction.Create;
        telepo.at_dist := 'u';
        telepo.at_min := TpointGoal(p_Goals.goals[0]).at_pos;
        telepo.at_max := TpointGoal(p_Goals.goals[0]).at_pos;
        state.Actions.Add(telepo);
        p_States.Add(state);

        for i := 0 to p_Goals.goals.count-1 do
            begin
            state := TState.Create;

            state.at_final := 0;
            state.at_name := Format('%s_%d_%d',[STATE_NAME_WALK,p_ClassIndex,p_States.Count]);

            exGoalSel := TExplicitGoalSelector.Create;
            exGoalSel.at_goalSet := p_Goals.at_id;
            exGoalSel.at_goal := p_Goals.goals[i].at_id;
            State.GoalSelector := exGoalSel;
            State.VelComponent := TRoadMapVelComponent.Create;//TGoalVelComponent.Create;//

            p_States.Add(state);
            end;

        state := TState.Create;
        state.at_name := Format('%s_%d_%d',[STATE_NAME_TELEPORT,p_ClassIndex,p_States.Count]);
        telepo := TTeleportAction.Create;
        telepo.at_dist := 'u';
        telepo.at_min := AsGLPointType2(-1*(p_ClassIndex div 100)-1,-1*(p_ClassIndex mod 100)-1);
        telepo.at_max := AsGLPointType2(-1*(p_ClassIndex div 100)-1,-1*(p_ClassIndex mod 100)-1);
        state.Actions.Add(telepo);
        p_States.Add(state);

        state := TState.Create;
        state.at_final := 1;
        state.at_name := Format('%s_%d_%d',[STATE_NAME_GOAL,p_ClassIndex,p_States.Count]);
        state.GoalSelector := TIdentityGoalSelector.Create;
        state.VelComponent := TGoalVelComponent.Create;
        p_States.Add(state);
        end;
    //==========================================================================
    var
        dTime : double;
    begin
    dTime := SecondSpan(p_MovingFeature.startTime,startTime);//移動開始時間

    if p_StartTime =0 then
        p_StartTime := Trunc(dTime)
    else
        p_StartTime := p_StartTime + Trunc(dTime);

    CreateGoals;

    if (p_Goals.goals.count=0) then
        begin
        p_IsExport := False;
        p_MovingFeature.lastStateIndex := -1;
        p_MovingFeature.lastStateName := STATE_NAME_NONE;
        Exit(false);
        end;

    CreateStates(p_StartTime);
    ConstituteTransitions;
    CreateAgentProfile;
    ConstituteGroup;

    Result := true;
    end;

//==============================================================================
procedure MengeAgentClass.ChangeClassIndex(index: Integer);
    var
        sn : String;
        i : integer;
    begin
    p_ClassIndex := index;

    for i := 0 to p_States.Count-1 do
        begin
        if pos(STATE_NAME_WAIT,p_States[i].at_name)>0 then
            begin
            sn := STATE_NAME_WAIT;
            end
        else if pos(STATE_NAME_WALK,p_States[i].at_name)>0 then
            begin
            sn := STATE_NAME_WALK;
            end
        else if pos(STATE_NAME_GOAL,p_States[i].at_name)>0 then
            begin
            sn :=STATE_NAME_GOAL;
            end
        else if pos(STATE_NAME_TELEPORT,p_States[i].at_name)>0 then
            begin
            sn := STATE_NAME_TELEPORT;
            end
        else
            begin
            break;
            end;
        p_States[i].at_name := Format('%s_%d_%d',[sn,p_ClassIndex,i]);
        end;
    p_Goals.at_id := p_ClassIndex;
    p_profile.at_name := 'Agent'+IntToStr(p_ClassIndex);
    p_profile.Common.at_class := p_ClassIndex;
    ConstituteTransitions;
    ConstituteGroup;
    end;

//==============================================================================
function MengeAgentClass.Clone(index,dTime: Integer) : MengeAgentClass;
    begin
    Result := MengeAgentClass.Create(MovingFeatureListClass.CreateMagnificateFeature(p_MovingFeature),index);

    Result.p_StartTime := p_StartTime + dTime;

    Result.p_IsExport := p_IsExport;
    end;

{ MengeAgentList }
//==============================================================================
procedure MengeAgentList.AfterConstruction;
    begin
    inherited;
    p_Agents := TList<MengeAgentClass>.Create;
    end;

//==============================================================================
procedure MengeAgentList.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_Agents);
    end;

//==============================================================================
procedure MengeAgentList.Clear;
    begin
    p_Agents.Clear;
    end;

//==============================================================================
procedure MengeAgentList.CreateAgents(startTime, EndTime: TDateTime; simPer : Integer; setting: TAgentSettingsType);
    var
        i : Integer;
        agent : MengeAgentClass;
        mf : MovingFeatureClass;
    begin
    FreeAndNil(p_Agents);
    p_Agents := TList<MengeAgentClass>.Create;
    for i := 0 to MovingFeatureListClass.numberOfMovingFeatures-1 do
        begin
        mf := MovingFeatureListClass.movingFeature[i];

        if not(mf.communicateMenge) then
            begin
            mf.lastStateIndex := -1;
            mf.lastStateName := STATE_NAME_NONE;
            continue;
            end;

        if (mf.numberOfFocusedHistories = -1)then
            begin
            ShowMessage('先に計算領域を決定してください。');
            Exit;
            end;

        mf.CalcTargetTime(startTime,EndTime,simPer);
        if (mf.numberOfFocusedHistories<=1)then
            begin
            mf.lastStateIndex := -1;
            mf.lastStateName := STATE_NAME_NONE;
            continue;
            end;

        agent := MengeAgentClass.Create(mf,i);
        p_Agents.Add(agent);
        end;

    {$ifdef Debug}
    Assert(p_Agents.Count>0,'計算時間、設定エリア内にシミュレーションすべき歩行者がいません');
    {$endif}
    end;
//==============================================================================
procedure MengeAgentList.CreateMagnificateFeature(magnification : integer);
    const
        specified = 600;//10min
    var
        i,j : Integer;
        mfi,count : Integer;
        dTime : Integer;
        clone : MengeAgentClass;
    begin
    MovingFeatureListClass.ClearMagnificateFeature;

    mfi := MovingFeatureListClass.numberOfMovingFeatures-1;
    count := p_Agents.Count-1;
    for j := 0 to count do
        begin
        for I := 1 to magnification do
            begin
            Inc(mfi);
            dTime :=Trunc((specified/magnification)*i-(specified/2));
            clone := p_Agents[j].Clone(mfi,dTime);
            p_Agents.Add(clone);
            end;
        end;
    end;

//==============================================================================
function MengeAgentList.Update(Elapsed,SignalTime: Integer) : Boolean;
    var
        a : MengeAgentClass;
    begin
    Result := False;
    for a in p_agents do
        begin
        a.Update(Elapsed,SignalTime,a.MovingFeature.LastStateName);
        Result := Result or a.IsExport;
        end;
    end;

//==============================================================================
procedure MengeAgentList.UpdateStatesOnMenge;
    var
        a : MengeAgentClass;
        i : Integer;
    begin
    for a in p_agents do
        begin
        if a.isExport then//falseの場合,直前のSimに存在しないためStateは変わっていない
            begin
            i := a.MovingFeature.LastStateindex;
            if i>0 then
                a.UpdateStateOnMenge(MovingFeatureListClass.GetCurrentState(i));
            end;
        end;
    end;

//==============================================================================
function MengeAgentList.GetAgent(const index: Integer): MengeAgentClass;
    begin
    Result := nil;
    if InRange(index,0, p_Agents.Count-1) then
        begin
        Result := p_Agents[index];
        end;
    end;

//==============================================================================
function MengeAgentList.GetNumberOfAgent: Integer;
    begin
    Result := p_Agents.Count;
    end;

end.
