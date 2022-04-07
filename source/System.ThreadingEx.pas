unit System.ThreadingEx;

interface

uses
  System.Classes, System.SysUtils, System.Threading, System.Types;

type
  TConstProc<T> = reference to procedure(const Arg: T);
  TConstFunc<T1,T2> = reference to function (const Arg1: T1): T2;

  TTaskExceptionMethod = TFunc<Exception,Boolean>;

  TVarRecValue<T> = record
  type
    TValueStore = class
    strict private
      FValue: T;
      function GetValue: T;
    public
      constructor Create(aValue: T);
      property Value: T read GetValue;
    end;
  strict private
    FValue: T;
    constructor Create(aValue: T);
  public
    class operator Implicit(Value: T): TVarRecValue<T>;
    class operator Implicit(Value: TVarRecValue<T>): TVarRec;
    class operator Implicit(Value: TVarRec): TVarRecValue<T>;
    class operator Implicit(Value: TVarRecValue<T>): T;
    class operator Explicit(Value: T): TVarRecValue<T>;
    class operator Explicit(Value: TVarRec): TVarRecValue<T>;
  end;

  TFuture<T, TResult> = class sealed(TTask, IFuture<TResult>)
  private
    FEvent: TFunctionEvent<TResult>;
    FFunc: TFunc<T,TResult>;
    FResult: TResult;
    FArg: T;
    procedure RunEvent(Sender: TObject);
  protected
    function Start: IFuture<TResult>;
    function GetValue: TResult;
    constructor Create(Sender: TObject; Event: TFunctionEvent<TResult>; const Func:
        TFunc<T,TResult>; APool: TThreadPool; P: T); overload;
  end;

  TFuture<T1, T2, TResult> = class sealed(TTask, IFuture<TResult>)
  private
    FEvent: TFunctionEvent<TResult>;
    FFunc: TFunc<T1,T2,TResult>;
    FResult: TResult;
    FArg1: T1;
    FArg2: T2;
    procedure RunEvent(Sender: TObject);
  protected
    function Start: IFuture<TResult>;
    function GetValue: TResult;
    constructor Create(Sender: TObject; Event: TFunctionEvent<TResult>; const Func:
        TFunc<T1,T2,TResult>; APool: TThreadPool; P1: T1; P2: T2); overload;
  end;

  TTaskHelper = class helper for TTask
  strict private
    class function DynamicToGenericArray<T>(A: array of T): TArray<T>; static;
    class procedure HandleException(E: Exception);
  private
    class function WaitForAllArrayWithException(Tasks: TArray<ITask>; Silent:
        Boolean): Boolean; overload; static;
  public
    class procedure CheckException(o: ITask);
    class function CheckWaitForAll<T>(Tasks: array of IFuture<T>; Silent: Boolean =
        False): Boolean; overload; static;
    class function Create<T1, T2, T3, T4>(const Proc: TProc<T1,T2,T3,T4>;
        P1: T1; P2: T2; P3: T3; P4: T4): ITask; overload; static;
    class function Create<T1, T2, T3>(const Proc: TProc<T1,T2,T3>; P1: T1; P2: T2;
        P3: T3): ITask; overload; static;
    class function Create<T1, T2>(const Func: TFunc<T1,T2>; P1: T1): ITask;
        overload; static;
    class function Create<T1, T2>(const Proc: TProc<T1,T2>; P1: T1; P2: T2): ITask;
        overload; static;
    class function Create<T>(const Func: TFunc<T>): ITask; overload; static;
    class function Create<T>(const Proc: TProc<T>; P: T): ITask; overload;
        static;
    class function Future<T, TResult>(const Func: TFunc<T,TResult>; P: T):
        IFuture<TResult>; overload; static;
    class function Future<T1, T2, TResult>(const Func: TFunc<T1,T2,TResult>; P1:
        T1; P2: T2): IFuture<TResult>; overload; static;
    class function IsEnded(aTask: ITask): Boolean;
    class function Run<T1, T2, T3, T4>(const Proc: TProc<T1, T2, T3, T4>; P1: T1;
        P2: T2; P3: T3; P4: T4): ITask; overload; static;
    class function Run<T1, T2, T3>(const Proc: TProc<T1, T2, T3>; P1: T1; P2: T2;
        P3: T3): ITask; overload; static;
    class function Run<T1, T2>(const Func: TFunc<T1,T2>; P1: T1): ITask; overload;
        static;
    class function Run<T1, T2>(const Proc: TProc<T1, T2>; P1: T1; P2: T2): ITask;
        overload; static;
    class function Run<T>(const Func: TFunc<T>): ITask; overload; static;
    class function Run<T>(const Proc: TProc<T>; P: T): ITask; overload; static;
    class procedure SetExceptionMethod(H: TTaskExceptionMethod);
    class function WaitForAllArrayWithException(Tasks: TArray<ITask>): Boolean;
        overload; static;
    class function WaitForAllWithException(Tasks: array of ITask; Silent: Boolean =
        False): Boolean; overload; static;
  end;

  TAsyncFuture<T> = class;

  IAsyncFuture<T> = interface
    function AsObject: TAsyncFuture<T>;
    function BeginInvoke(aProc: TProc<T>): ITask; overload;
    function BeginInvoke(Proc: TConstProc<T>): ITask; overload;
    function BeginInvoke(aFunc: TFunc<T>): IFuture<T>; overload;
  end;

  TAsyncFuture<T> = class(TInterfacedObject, IAsyncFuture<T>)
  strict private
    constructor Create(aComponent: TComponent; aValue: IFuture<T>; aDummy:
        Integer); overload;
  private
    FComponent: TComponent;
    FValue: IFuture<T>;
  protected
    function AsObject: TAsyncFuture<T>;
    function BeginInvoke(Proc: TProc<T>): ITask; overload;
    function BeginInvoke(Proc: TConstProc<T>): ITask; overload;
    function BeginInvoke<T2>(Proc: TProc<T,T2>; P2: T2): ITask; overload;
    function BeginInvoke<T2, T3>(Proc: TProc<T,T2,T3>; P2: T2; P3: T3): ITask;
        overload;
    function BeginInvoke<TResult>(aFunc: TFunc<T,TResult>): IFuture<TResult>;
        overload;
    function BeginInvoke(aFunc: TFunc<T>): IFuture<T>; overload;
  public
    class function Create(aComponent: TComponent; aValue: IFuture<T>):
        IAsyncFuture<T>; overload; inline;
  end;

  TAsyncComponentHelper = class helper for TComponent
  public
    procedure Async(Proc: TProc); overload;
    procedure Async<T1, T2, T3>(Proc: TProc<T1,T2,T3>; P1: T1; P2: T2; P3: T3);
        overload;
    procedure Async<T1,T2>(Proc: TProc<T1,T2>; P1: T1; P2: T2); overload;
    procedure Async<T>(Func: TFunc<T>); overload;
    procedure Async<T>(Proc: TProc<T>; P: T); overload;
    function BeginInvoke<T1, T2, T3>(Proc: TProc<T1,T2,T3>; P1: T1; P2: T2; P3:
        T3): IAsyncResult; overload;
    function BeginInvoke<T1, T2>(Func: TConstFunc<T1,T2>; P1: T1): IAsyncResult;
        overload;
    function BeginInvoke<T1, T2>(Func: TFunc<T1, T2>; P1: T1): IAsyncResult;
        overload;
    function BeginInvoke<T1, T2>(Proc: TProc<T1,T2>; P1: T1; P2: T2): IAsyncResult;
        overload;
    function BeginInvoke<T>(Proc: TConstProc<T>; P: T): IAsyncResult; overload;
    function BeginInvoke<T>(Proc: TProc<T>; P: T): IAsyncResult; overload;
    function Future<T, TResult>(aFunc: TFunc<T,TResult>; P: T):
        IAsyncFuture<TResult>; overload;
    function Future<T>: IAsyncFuture<T>; overload;
    function Future<T>(aValue: IFuture<T>): IAsyncFuture<T>; overload;
    function Future<T>(aFunc: TFunc<T>): IAsyncFuture<T>; overload;
  end;

implementation

uses
  System.TypInfo, System.Rtti;

var TaskExceptionMethod: TTaskExceptionMethod = nil;

constructor TVarRecValue<T>.Create(aValue: T);
begin
  FValue := aValue;
end;

class operator TVarRecValue<T>.Implicit(Value: TVarRecValue<T>): TVarRec;
begin
  var o := PTypeInfo(TypeInfo(T));
  if o.Kind in [tkRecord, tkArray, tkDynArray, tkMethod] then
    Result := TValue.From<TValueStore>(TValueStore.Create(Value.FValue)).AsVarRec
  else
    Result := TValue.From<T>(Value.FValue).AsVarRec;
end;

class operator TVarRecValue<T>.Implicit(Value: T): TVarRecValue<T>;
begin
  Result := TVarRecValue<T>.Create(Value);
end;

class operator TVarRecValue<T>.Implicit(Value: TVarRec): TVarRecValue<T>;
begin
  if (Value.VType = vtObject) and (Value.VObject is TValueStore) then begin
    var o := Value.VObject as TValueStore;
    Result := o.Value;
    o.DisposeOf;
  end else if Value.VType = vtInterface then
    Result := TVarRecValue<T>(IInterface(Value.VInterface))
  else
    Result := TValue.FromVarRec(Value).AsType<T>;
end;

class operator TVarRecValue<T>.Explicit(Value: TVarRec): TVarRecValue<T>;
begin
  Result := Value;
end;

class operator TVarRecValue<T>.Explicit(Value: T): TVarRecValue<T>;
begin
  Result := Value;
end;

class operator TVarRecValue<T>.Implicit(Value: TVarRecValue<T>): T;
begin
  Result := Value.FValue;
end;

constructor TVarRecValue<T>.TValueStore.Create(aValue: T);
begin
  inherited Create;
  FValue := aValue;
end;

function TVarRecValue<T>.TValueStore.GetValue: T;
begin
  Result := FValue;
end;

constructor TFuture<T, TResult>.Create(Sender: TObject; Event:
    TFunctionEvent<TResult>; const Func: TFunc<T,TResult>; APool: TThreadPool;
    P: T);
begin
  FEvent := Event;
  FFunc := Func;
  FArg := P;
  inherited Create(Sender, RunEvent, nil, APool, nil, []);
end;

function TFuture<T, TResult>.GetValue: TResult;
begin
  Wait;
  Result := FResult;
end;

procedure TFuture<T, TResult>.RunEvent(Sender: TObject);
begin
  if Assigned(FEvent) then
    FResult := FEvent(Sender)
  else if Assigned(FFunc) then
    FResult := FFunc(FArg)
  else
    FResult := Default(TResult);
end;

function TFuture<T, TResult>.Start: IFuture<TResult>;
begin
  inherited Start;
  Result := Self;
end;

constructor TFuture<T1, T2, TResult>.Create(Sender: TObject; Event:
    TFunctionEvent<TResult>; const Func: TFunc<T1,T2,TResult>; APool:
    TThreadPool; P1: T1; P2: T2);
begin
  FEvent := Event;
  FFunc := Func;
  FArg1 := P1;
  FArg2 := P2;
  inherited Create(Sender, RunEvent, nil, APool, nil, []);
end;

function TFuture<T1, T2, TResult>.GetValue: TResult;
begin
  Wait;
  Result := FResult;
end;

procedure TFuture<T1, T2, TResult>.RunEvent(Sender: TObject);
begin
  if Assigned(FEvent) then
    FResult := FEvent(Sender)
  else if Assigned(FFunc) then
    FResult := FFunc(FArg1, FArg2)
  else
    FResult := Default(TResult);
end;

function TFuture<T1, T2, TResult>.Start: IFuture<TResult>;
begin
  inherited Start;
  Result := Self;
end;

class procedure TTaskHelper.CheckException(o: ITask);
begin
  Run<TArray<ITask>,Boolean>(TTask.WaitForAllArrayWithException, [o]);
end;

class function TTaskHelper.CheckWaitForAll<T>(Tasks: array of IFuture<T>;
    Silent: Boolean = False): Boolean;
begin
  var o: TArray<ITask> := [];
  for var a in Tasks do o := o + [a];
  Result := WaitForAllArrayWithException(o, Silent);
end;

class function TTaskHelper.Create<T1, T2, T3, T4>(
  const Proc: TProc<T1, T2, T3, T4>; P1: T1; P2: T2; P3: T3; P4: T4): ITask;
begin
  Result := TTask.Create(procedure begin Proc(P1, P2, P3, P4); end);
end;

class function TTaskHelper.Create<T1, T2, T3>(const Proc: TProc<T1,T2,T3>; P1:
    T1; P2: T2; P3: T3): ITask;
begin
  Result := TTask.Create(procedure begin Proc(P1, P2, P3); end);
end;

class function TTaskHelper.Create<T1, T2>(const Func: TFunc<T1,T2>; P1: T1):
    ITask;
begin
  Result := TTask.Create(procedure begin Func(P1); end);
end;

class function TTaskHelper.Create<T1, T2>(const Proc: TProc<T1,T2>; P1: T1; P2:
    T2): ITask;
begin
  Result := TTask.Create(procedure begin Proc(P1, P2); end);
end;

class function TTaskHelper.Create<T>(const Func: TFunc<T>): ITask;
begin
  Result := TTask.Create(procedure begin Func; end);
end;

class function TTaskHelper.Create<T>(const Proc: TProc<T>; P: T): ITask;
begin
  Result := TTask.Create(procedure begin Proc(P); end);
end;

class function TTaskHelper.DynamicToGenericArray<T>(A: array of T): TArray<T>;
var o: T;
begin
  Result := nil;
  for o in A do Result := Result + [o];
end;

class function TTaskHelper.Future<T, TResult>(const Func: TFunc<T,TResult>; P:
    T): IFuture<TResult>;
begin
  Result := TFuture<T,TResult>.Create(TObject(nil), TFunctionEvent<TResult>(nil), Func, TThreadPool.Default, P).Start;
end;

class function TTaskHelper.Future<T1, T2, TResult>(const Func: TFunc<T1,T2,
    TResult>; P1: T1; P2: T2): IFuture<TResult>;
begin
  Result := TFuture<T1,T2,TResult>.Create(TObject(nil), TFunctionEvent<TResult>(nil), Func, TThreadPool.Default, P1, P2).Start;
end;

class procedure TTaskHelper.HandleException(E: Exception);
begin
  if not Assigned(TaskExceptionMethod) or not TaskExceptionMethod(E) or (TTask.CurrentTask = nil) then
    raise E
  else begin
    E.Free;
    Abort;
  end;
end;

class function TTaskHelper.IsEnded(aTask: ITask): Boolean;
begin
  Result := aTask.Status in [TTaskStatus.Completed, TTaskStatus.Canceled, TTaskStatus.Exception];
end;

class function TTaskHelper.Run<T1, T2, T3, T4>(const Proc: TProc<T1, T2, T3,
    T4>; P1: T1; P2: T2; P3: T3; P4: T4): ITask;
begin
  Result := TTask.Create<T1, T2, T3, T4>(Proc, P1, P2, P3, P4).Start;
end;

class function TTaskHelper.Run<T1, T2, T3>(const Proc: TProc<T1, T2, T3>; P1:
    T1; P2: T2; P3: T3): ITask;
begin
  Result := TTask.Create<T1, T2, T3>(Proc, P1, P2, P3).Start;
end;

class function TTaskHelper.Run<T1, T2>(const Func: TFunc<T1,T2>; P1: T1): ITask;
begin
  Result := TTask.Create<T1, T2>(Func, P1).Start;
end;

class function TTaskHelper.Run<T1, T2>(const Proc: TProc<T1, T2>; P1: T1; P2:
    T2): ITask;
begin
  Result := TTask.Create<T1, T2>(Proc, P1, P2).Start;
end;

class function TTaskHelper.Run<T>(const Func: TFunc<T>): ITask;
begin
  Result := TTask.Create<T>(Func).Start;
end;

class function TTaskHelper.Run<T>(const Proc: TProc<T>; P: T): ITask;
begin
  Result := TTask.Create<T>(Proc, P).Start;
end;

class procedure TTaskHelper.SetExceptionMethod(H: TTaskExceptionMethod);
begin
  TaskExceptionMethod := H;
end;

class function TTaskHelper.WaitForAllArrayWithException(Tasks: TArray<ITask>):
    Boolean;
begin
  Result := WaitForAllArrayWithException(Tasks, False);
end;

class function TTaskHelper.WaitForAllArrayWithException(Tasks: TArray<ITask>;
    Silent: Boolean): Boolean;
var T: ITask;
begin
  Result := True;
  try
    for T in Tasks do
      if T.Status = TTaskStatus.Created then
        T.Start;
    Result := WaitForAll(Tasks);
  except
    if Silent then
      Abort
    else
      HandleException(Exception(AcquireExceptionObject));
  end;
end;

class function TTaskHelper.WaitForAllWithException(Tasks: array of ITask;
    Silent: Boolean = False): Boolean;
begin
  Result := WaitForAllArrayWithException(DynamicToGenericArray<ITask>(Tasks), Silent);
end;

procedure TAsyncComponentHelper.Async(Proc: TProc);
begin
  TTask.Run<TProc>(
    procedure (aProc: TProc) begin
      EndInvoke(BeginInvoke(aProc));
    end
  , Proc
  );
end;

procedure TAsyncComponentHelper.Async<T1, T2, T3>(Proc: TProc<T1,T2,T3>; P1:
    T1; P2: T2; P3: T3);
begin
  var o: ITask := TTask.Create<TProc<T1,T2,T3>, T1, T2, T3>(
    procedure (aProc: TProc<T1,T2,T3>; aP1: T1; aP2: T2; aP3: T3) begin
      EndInvoke(BeginInvoke<T1,T2,T3>(aProc, aP1, aP2, aP3));
    end
  , Proc, P1, P2, P3
  );
  TTask.CheckException(o);
end;

procedure TAsyncComponentHelper.Async<T1, T2>(Proc: TProc<T1, T2>; P1: T1;
  P2: T2);
begin
  var o: ITask := TTask.Create<TProc<T1,T2>, T1, T2>(
    procedure (aProc: TProc<T1,T2>; aP1: T1; aP2: T2) begin
      EndInvoke(BeginInvoke<T1,T2>(aProc, aP1, aP2));
    end
    , Proc, P1, P2
  );
  TTask.CheckException(o);
end;

procedure TAsyncComponentHelper.Async<T>(Func: TFunc<T>);
begin
  var o: ITask := TTask.Create<TFunc<T>>(
    procedure (aFunc: TFunc<T>)
    begin
      EndInvoke(BeginInvoke<T>(aFunc));
    end
    , Func
  );
  TTask.CheckException(o);
end;

procedure TAsyncComponentHelper.Async<T>(Proc: TProc<T>; P: T);
begin
  var o: ITask := TTask.Create<TProc<T>, T>(
    procedure (aProc: TProc<T>; aP: T)
    begin
      EndInvoke(BeginInvoke<T>(aProc, aP));
    end
    , Proc, P
  );
  TTask.CheckException(o);
end;

function TAsyncComponentHelper.BeginInvoke<T1, T2, T3>(Proc: TProc<T1,T2,T3>;
    P1: T1; P2: T2; P3: T3): IAsyncResult;
begin
  var M: TAsyncConstArrayProc := procedure (const P: array of const)
  begin
    Proc(TVarRecValue<T1>(P[0]), TVarRecValue<T2>(P[1]), TVarRecValue<T3>(P[2]));
  end;

  Result := BeginInvoke(M, TArray<TVarRec>.Create(TVarRecValue<T1>(P1), TVarRecValue<T2>(P2), TVarRecValue<T3>(P3)));
end;

function TAsyncComponentHelper.BeginInvoke<T1, T2>(Func: TConstFunc<T1,T2>; P1:
    T1): IAsyncResult;
begin
  var M: TAsyncConstArrayFunc<T2> := function (const P: array of const): T2
  begin
    Result := Func(TVarRecValue<T1>(P[0]));
  end;

  Result := BeginInvoke<T2>(M, TArray<TVarRec>.Create(TVarRecValue<T1>(P1)));
end;

function TAsyncComponentHelper.BeginInvoke<T1, T2>(Func: TFunc<T1,T2>;
  P1: T1): IAsyncResult;
begin
  var M: TAsyncConstArrayFunc<T2> := function (const P: array of const): T2
  begin
    Result := Func(TVarRecValue<T1>(P[0]));
  end;

  Result := BeginInvoke<T2>(M, TArray<TVarRec>.Create(TVarRecValue<T1>(P1)));
end;

function TAsyncComponentHelper.BeginInvoke<T1, T2>(Proc: TProc<T1,T2>; P1: T1;
    P2: T2): IAsyncResult;
begin
  var M: TAsyncConstArrayProc := procedure (const P: array of const)
  begin
    Proc(TVarRecValue<T1>(P[0]), TVarRecValue<T2>(P[1]));
  end;

  Result := BeginInvoke(M, TArray<TVarRec>.Create(TVarRecValue<T1>(P1), TVarRecValue<T2>(P2)));
end;

function TAsyncComponentHelper.BeginInvoke<T>(Proc: TConstProc<T>; P: T):
    IAsyncResult;
begin
  var M: TAsyncConstArrayProc := procedure (const P: array of const)
  begin
    Proc(TVarRecValue<T>(P[0]));
  end;

  Result := BeginInvoke(M, TArray<TVarRec>.Create(TVarRecValue<T>(P)));
end;

function TAsyncComponentHelper.BeginInvoke<T>(Proc: TProc<T>; P: T):
    IAsyncResult;
begin
  var M: TAsyncConstArrayProc := procedure (const P: array of const)
  begin
    Proc(TVarRecValue<T>(P[0]));
  end;

  Result := BeginInvoke(M, TArray<TVarRec>.Create(TVarRecValue<T>(P)));
end;

function TAsyncComponentHelper.Future<T, TResult>(aFunc: TFunc<T,TResult>; P:
    T): IAsyncFuture<TResult>;
begin
  Result := TAsyncFuture<TResult>.Create(Self, TTask.Future<T, TResult>(aFunc, P));
end;

function TAsyncComponentHelper.Future<T>: IAsyncFuture<T>;
begin
  Result := TAsyncFuture<T>.Create(Self, nil);
end;

function TAsyncComponentHelper.Future<T>(aValue: IFuture<T>): IAsyncFuture<T>;
begin
  Result := TAsyncFuture<T>.Create(Self, aValue);
end;

function TAsyncComponentHelper.Future<T>(aFunc: TFunc<T>): IAsyncFuture<T>;
begin
  Result := TAsyncFuture<T>.Create(Self, TTask.Future<T>(aFunc));
end;

class function TAsyncFuture<T>.Create(aComponent: TComponent; aValue:
    IFuture<T>): IAsyncFuture<T>;
begin
  Result := TAsyncFuture<T>.Create(aComponent, aValue, 0);
end;

constructor TAsyncFuture<T>.Create(aComponent: TComponent; aValue: IFuture<T>;
    aDummy: Integer);
begin
  inherited Create;
  FComponent := aComponent;
  FValue := aValue;
end;

function TAsyncFuture<T>.AsObject: TAsyncFuture<T>;
begin
  Result := Self;
end;

function TAsyncFuture<T>.BeginInvoke(Proc: TProc<T>): ITask;
begin
  Result := TTask.Run<TComponent,IFuture<T>>(
    procedure (a: TComponent; b: IFuture<T>)
    begin
      a.EndInvoke(a.BeginInvoke<T>(Proc, b.Value));
    end
  , FComponent, FValue
  );
end;

function TAsyncFuture<T>.BeginInvoke(aFunc: TFunc<T>): IFuture<T>;
begin
  Result := TTask.Future<TComponent, T>(
    function (a: TComponent): T
    begin
      Result := a.EndInvoke<T>(a.BeginInvoke<T>(aFunc));
    end
  , FComponent
  );
end;

function TAsyncFuture<T>.BeginInvoke(Proc: TConstProc<T>): ITask;
begin
  Result := TTask.Run<TComponent,IFuture<T>>(
    procedure (a: TComponent; b: IFuture<T>)
    begin
      a.EndInvoke(a.BeginInvoke<T>(Proc, b.Value));
    end
  , FComponent, FValue
  );
end;

function TAsyncFuture<T>.BeginInvoke<T2, T3>(Proc: TProc<T,T2,T3>; P2: T2; P3:
    T3): ITask;
begin
  Result := TTask.Run<TComponent,IFuture<T>,T2,T3>(
    procedure (a: TComponent; b: IFuture<T>; c: T2; d: T3)
    begin
      a.EndInvoke(a.BeginInvoke<T, T2, T3>(Proc, b.Value, c, d));
    end
  , FComponent, FValue, P2, P3
  );
end;

function TAsyncFuture<T>.BeginInvoke<T2>(Proc: TProc<T,T2>; P2: T2): ITask;
begin
  Result := TTask.Run<TComponent,IFuture<T>,T2>(
    procedure (a: TComponent; b: IFuture<T>; c: T2)
    begin
      a.EndInvoke(a.BeginInvoke<T, T2>(Proc, b.Value, c));
    end
  , FComponent, FValue, P2
  );
end;

function TAsyncFuture<T>.BeginInvoke<TResult>(aFunc: TFunc<T,TResult>):
    IFuture<TResult>;
begin
  Result := TTask.Future<TComponent,IFuture<T>,TResult>(
    function(a: TComponent; b: IFuture<T>): TResult
    begin
      Result := a.EndInvoke<TResult>(a.BeginInvoke<T,TResult>(aFunc, b.Value));
    end
  , FComponent, FValue
  );
end;

end.
