# Introduction

Delphi include [threads](http://docwiki.embarcadero.com/Libraries/Rio/en/System.Classes.TThread)
support since very early stage.  However, writing a thread safe application isn't an easy task.
When application grows to certain complexity, growing a stable thread safe codes
is a big challenge to maintain its quality.  This situation is more noticeable
for [thread unsafe](https://stackoverflow.com/questions/28655758/delphi-why-vcl-is-not-thread-safe-how-can-be)
 VCL GUI application.

To ease thread safe coding for thread unsafe VCL application, Delphi
introduces [Parallel Programming LIbrary](http://docwiki.embarcadero.com/RADStudio/Rio/en/Using_the_Parallel_Programming_Library)
(a.k.a delphi PPL) at later stage.  Using the library further reduce coding
complexities for VCL application.

The purpose of this [delphi-ppl](https://github.com/eStreamSoftware/delphi-ppl) library
attempt to extend Delphi parallel programming library with clean and neat coding experience.

`delphi-ppl` trying to re-use existing PPL library's methods and structure
as much as possible and avoid inventing new terminology when possible.
The library encourage using [PODO](https://edn.embarcadero.com/article/41338) practice.

# Compile and Build

This library use [inline variable](http://docwiki.embarcadero.com/RADStudio/Rio/en/Inline_Variable_Declaration) extensively.
It may only compile and build with [RAD Studio Rio 10.3](http://docwiki.embarcadero.com/RADStudio/Rio/en/Main_Page) and above.

# Example

A simple example showing the coding construct using `delphi-ppl`:

```delphi
uses System.ThreadingEx;

function TForm1.GetValue: string;
begin
  Sleep(1000);
  Result := 'Hello World';
end;

procedure TForm1.ShowValue(a: string);
begin
  ShowMessage(a);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Future<string>(GetValue).BeginInvoke(ShowValue);
end;
```

The `sleep` call in `GetValue` method mimic the blocking behavior.
However, user will not experience any blocking when execute above code.

More example is available in test case project.

# Known issue with Delphi PPL

Delphi PPL has a memory leak issue in class `TLightWeightEvent`.  This issue has reported
to [RSP-25999](https://quality.embarcadero.com/browse/RSP-25999).  A patch `System.SyncObjs.RSP25999.pas`
is included in the repository.  The patch requires a library from [DDetours](https://github.com/MahdiSafsafi/DDetours).
Include the patch and DDetours library to build a memory leak free codes.

# Background story growing `delphi-ppl`

The following examples describe how to use standard Delphi PPL and propose possible enhancement
using the library.  `delphi-ppl` is carved along the way.

## UI design strategy for long running task

A long running task may cause application runtime become unresponsive and lead to perception of application halt or hang during the operation.

For example:
1. A database backup operation spend 10 minutes to finish.
2. Perform length report calculation

It is a common practice to use some UI gadget to alert progress or percentage done every few seconds.  These design give an idea to user about the progress and always stay alert.

Some common UI gadget to display progress are:

1. Progress log in scrolling text
2. Progress bar showing job done quantified by percentage
3. A percentage value showing job done quantified by percentage

Here is a simple task using a loop:

```delphi
procedure TForm1.Work;
var i: Integer;
begin
  for i := 1 to 10 do begin
    Sleep(i*300);
  end;
end;
```

## Synchronous design

The most simple yet easy to understand approach is using single threaded UI blocking design:

```delphi
procedure TForm1.Work;
var i: Integer;
begin
  for i := 1 to 10 do begin
    Memo1.Lines.Add(Format('Progress - %d', [i]));
    Sleep(i * 300);
  end;
end;
```
The code works pretty well and alert the progress in timely fashion to some extent.  However, it has some weaknesses when execute the method **procedure Work**.

### Weakness 1 - Form become `Not Responding`

>The form become unresponsive after few loops.  This is due to the Windows operating system does not receive any message from the application for some time and cause the application as **`Not Responding`**

### Weakness 2 - Form become unresponsive

>An active form may freely move on desktop when it doesn't do the work.  However, the form will freeze till the work finish.  This is a blocking behavior happen commonly in application UI.  Subsequent task has to wait till the current task finish.

## Asynchronous design

With wide-spread of multi-core system today, engineer should design application utilizing in multi threading approach when possible.  A proper application UI design in asynchronous strategy should overcome weaknesses found in synchronous design.

Improper implementation for Asynchronous Application will cause unstable runtime, unpredictable behavior, unexpected result and memory leak at runtime.  It is difficult to debug a multi threaded application compare to single threaded application.

### UI controls work in synchronous behavior

UI controls always work synchronously due to the complexities of UI redraw and painting strategy.  It will cause unpredictable behavior or indeterminate errors when run asynchronously.  In most situation, the UI controls update latency is very low.  User may not feel the lagging of any UI updates.

The synchronous behavior also apply to Windows and VCL UI controls.

### Application Design Strategy

Asynchronous design may apply freely for non UI application if race condition or resource sharing and locking is manage in predictable way.  With these restrictions, the UI controls may design to work in synchronous manner only.

In summary, design an application with this strategy: **Asynchronous Business Logic** and **Synchronous UI Controls**.

| Elements | Non UI Application | UI Application|
|-|:-:|:-:|
|Business Logic| Synchronous / Asynchronous | Synchronous / Asynchronous |
|UI controls|-|Synchronous |

### Refactor: Separate business logic and UI controls

From previous example, refactor the code to split the business logic the UI controls:

```delphi
procedure TForm1.Log(S: string);
begin
  Memo1.Lines.Add(S);
end;

procedure TForm1.Work(Log: TProc<string>);
var i: Integer;
begin
  for i := 1 to 10 do begin
    Log('Progress - ' + i.ToString);
    Sleep(i * 300);
  end;
end;
```
A new method **Log** has defined to handle the UI controls separately.  The UI related code has removed from **Work** method and replaced by an anonymous method that may invoke like this:

```delphi
procedure TForm1.Button1Click(Sender: TObject);
begin
  Work(Log);
end;
```
So far so good, the code still behave in synchronous way like before.

### Evolution 1: Transform to Asynchronous strategy

Next, use Delphi [Parallel Programming Library](http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Using_the_Parallel_Programming_Library) to make it work asynchronous:

```delphi
procedure TForm1.Button1Click(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      Work(Log);
    end
  );
end;
```

The asynchronous effects can notice immediately at runtime.  The application no longer freeze when do the lengthy work.

Using **TTask.Run** method cause **Work(Log)** works in asynchronous way, so does the **Log** method that deal with UI controls.  In most situation, it works without much trouble but it is not a valid move.  The synchronous design rule should always apply for UI related coding to avoid unpredictable errors.  This errors become obvious when the code grows to certain complexities.

### Evolution 2: Transform UI related code to work synchronously

The UI related method **Log** should implement in synchronous way:

```delphi
procedure TForm1.Log(S: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(S);
    end
  );
end;
```
Using **[TThread.Synchronize](http://docwiki.embarcadero.com/Libraries/Tokyo/en/System.Classes.TThread.Synchronize)** in a classic approach make the UI code works synchronously.

Now, both UI controls and business logic works in synchronous and asynchronous manner respectively.

### Evolution 3: Using TThread.Queue

**TThread.Synchronize** will block and wait until the underlying anonymous method invocation is fully completed.  If subsequent operations don't depend on the synchronize operation, using **[TThread.Queue](http://docwiki.embarcadero.com/Libraries/Tokyo/en/System.Classes.TThread.Queue)** that works is "queue and forget" way is a better move here:

```delphi
procedure TForm1.Log(S: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      Memo1.Lines.Add(S);
    end
  );
end;
```

**TThread.Queue** asynchronously execute the anonymous method in main thread (imply synchronous).  Unlike TThread.Synchronize, it will not block and continue the execution immediately.  It should do better than **TThread.Synchronous** if there are multiple call of operations that need to execute in main thread.  For simple example like `TForm1.Log`, it will not show any noticeable performance gain.

### Evolution 4: Using Delphi Asynchronous Programming Library in queue and forget strategy

Since Delphi 2009, two new methods add to **TComponent** class: [BeginInvoke](http://docwiki.embarcadero.com/Libraries/Tokyo/en/System.Classes.TComponent.BeginInvoke) and [EndInvoke](http://docwiki.embarcadero.com/Libraries/Tokyo/en/System.Classes.TComponent.EndInvoke).  Using these methods allows us to code the asynchronous operation in under `TComponent` context instead of `TThread` context:

```delphi
procedure TForm1.Log(S: string);
begin
  BeginInvoke(
    procedure
    begin
      Memo1.Lines.Add(S);
    end
  );
end;
```
`BeginInvoke` works exactly like `TThread.Queue` by default:

```delphi
function TComponent.BeginInvoke(const AProc: TProc; const AContext: TObject): IAsyncResult;
begin
  Result := TAsyncProcedureResult.Create(AProc, AContext, Self).Invoke;
end;

function TBaseAsyncResult.Invoke: IAsyncResult;
begin
  SetFlagsAtomic([TAsyncFlag.Invoked], [TAsyncFlag.Invoked]);
  FInvokingThread := TThread.CurrentThread.ThreadID;
  _AddRef;
  Result := Self;
  Schedule;
end;

procedure TBaseAsyncResult.Schedule;
begin
  TThread.Queue(nil, DoAsyncDispatch);
end;
```

### Evolution 5: Using Delphi Asynchronous Programming Library in block and wait strategy
To simulate `TThread.Synchronize` operate in block and wait way, use `EndInvoke` as follow:

```delphi
procedure TForm1.Log(S: string);
var R: IAsyncResult;
begin
  R := BeginInvoke(
    procedure
    begin
      Memo1.Lines.Add(S);
    end
  );
  EndInvoke(R);
end;
```

or direct invocation without using an intermediate variable:

```delphi
procedure TForm1.Log(S: string);
begin
  EndInvoke(
    BeginInvoke(
      procedure
      begin
        Memo1.Lines.Add(S);
      end
    );
  );
end;
```

`EndInvoke` also allow capture value returned by BeginInvoke operation with generic method:

```delphi
var R: IAsyncResult;
    i: Integer;
begin
  R := BeginInvoke<Integer>(
    function: Integer
    begin
      Result := Memo1.Lines.Add(S);
    end
  );
  i := EndInvoke<Integer>(R);
  OutputDebugString(PChar(i.ToString));
end;
```

If exception happen in `BeginInvoke` operation, using `EndInvoke` will raise exception and block any further operation:

```delphi
procedure TForm1.Log(S: string);
var R: IAsyncResult;
begin
  R := BeginInvoke(
    procedure
    begin
      Memo1.Lines.Add(S);
      raise Exception.Create('Error Message');
    end
  );
  EndInvoke(R);
end;
```

Remove the `EndInvoke(R)` and the lengthy task will continue execute regardless of exception raised n other thread.

Exception happen in thread other than main UI thread will happen at runtime but it won't alert to user at runtime.  Catch the exception and use `Application.ShowException` to show it explicitly:

```delphi
procedure TForm1.Log(S: string);
var R: IAsyncResult;
begin
  R := BeginInvoke(
    procedure
    begin
      Memo1.Lines.Add(S);
      raise Exception.Create('Error Message');
    end
  );
  try
    EndInvoke(R);
  except
    on E: Exception do Application.ShowException(E);
  end;
end;
```

In summary, using `EndInvoke(R)` allow these to happen:
1. Serialized the operation like `TThread.Synchronize`.
2. Return value in `BeginInvoke` operation.
3. Handle exception raised in BeginInvoke operation.

### Evolution 5: Avoid race condition for value consumed in BeginInvoke

Continue from the `Log` example.

If the log message variable `S` is declared in outer scope:

```delphi
var S: string;

procedure TForm1.Log;
var R: IAsyncResult;
begin
  R := BeginInvoke(
    procedure
    begin
      Memo1.Lines.Add(S);
    end
  );
  EndInvoke(R);
end;
```

Multi-threading invocation to  `Log` method may access get access to variable `S` in unpredictable way.

To overcome the problem, Delphi Asynchronous Programming Library declared these:

```delphi
TAsyncConstArrayProc = reference to procedure (const Params: array of const);

function TComponent.BeginInvoke(const AProc: TAsyncConstArrayProc; const Params: array of const; const AContext: TObject = nil): IAsyncResult; overload;
```

Utilizing the method allow us to get access to value in predictable manner:

```delphi
procedure TForm1.Log(S: string);
var ConstArrayProc: TAsyncConstArrayProc;
    A: TArray<TVarRec>;
begin
  ConstArrayProc := procedure (const P: array of const)
  begin
    Memo1.Lines.Add(string(TVarRec(P[0]).VUnicodeString));
  end;

  BeginInvoke(ConstArrayProc, [S]);
end;
```

Next topic will further enhance the code for better reusability.

### Evolution 6: Enhance Delphi Asynchronous Programming Library

Continue from previous example, writing code for multi threading operation that work for all aspects isn't straight forward.  More constructs introduce for simple `Log` complicate the codes.  This topic use [class helper](http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Class_and_Record_Helpers_(Delphi)) to enhance `BeginInvoke` with additional generic type:

```delphi
type
  TAsyncComponentHelper = class helper for TComponent
  public
    function BeginInvoke<T>(AsyncProc: TProc<T>; P: T): IAsyncResult; overload;
  end;

function TAsyncComponentHelper.BeginInvoke<T>(AsyncProc: TProc<T>; P: T):
    IAsyncResult;
var ConstArrayProc: TAsyncConstArrayProc;
    A: TArray<TVarRec>;
begin
  ConstArrayProc := procedure (const P: array of const)
  begin
    AsyncProc(TValue.FromVarRec(P[0]).AsType<T>);
  end;

  SetLength(A, 1);
  A[0] := TValue.From<T>(P).AsVarRec;

  Result := BeginInvoke(ConstArrayProc, A);
end;
```

And `Log` may construct as:

```delphi
procedure TForm1.Log(S: string);
begin
  BeginInvoke<string>(
    procedure(o: string)
    begin
      Memo1.Lines.Add(o);
    end
  , S
  );
end;
```

### Evolution 7: One liner coding with Delphi Asynchronous Programming Library

Using anonymous method with `BeginInvoke` introduces method `procedure begin end` construct in code.  Further enhance Delphi Asynchronous Programming Library:

```delphi
type
  TConstFunc<T1,T2> = reference to function(const Arg1: T1): T2;

  TAsyncComponentHelper = class helper for TComponent
  public
    function BeginInvoke<T1, T2>(Func: TConstFunc<T1,T2>; P1: T1): IAsyncResult;
        overload;
  end;

function TAsyncComponentHelper.BeginInvoke<T1, T2>(Func: TConstFunc<T1, T2>;
  P1: T1): IAsyncResult;
var ConstArrayProc: TAsyncConstArrayFunc<T2>;
    A: TArray<TVarRec>;
begin
  ConstArrayProc := function (const P: array of const): T2
  begin
    Result := Func(TValue.FromVarRec(P[0]).AsType<T1>);
  end;

  SetLength(A, 1);
  A[0] := TValue.From<T1>(P1).AsVarRec;

  Result := BeginInvoke<T2>(ConstArrayProc, A);
end;
```

Now, the `Log` method may construct with much simple code:

```delphi
procedure TForm1.Log(S: string);
begin
  BeginInvoke<string,Integer>(Memo1.Lines.Add, S);
end;
```

### Evolution 8: One liner for TTask
Now back to first example using `TTask`:

```delphi
procedure TForm1.Button1Click(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      Work(Log);
    end
  );
end;
```

TTask may enhance to support one liner coding too:

```delphi
type
  TTaskHelper = class helper for TTask
    class function Run<T>(const Proc: TProc<T>; P: T): ITask; overload; static;
  end;

class function TTaskHelper.Run<T>(const Proc: TProc<T>; P: T): ITask;
begin
  Result := TTask.Create<T>(Proc, P).Start;
end;
```

and utilize it:

```delphi
procedure TForm1.Button1Click(Sender: TObject);
begin
  TTask.Run<TProc<string>>(Work, Log);
end;
```

### Evolution 9: Handle exception raised in a Task

Continue from previous example, the code evolve to:

```delphi
begin
  TTask.Run<TProc<string>>(Work, Log);
end;
```

What if exception raised in the middle of the task execution?  No alert shown when executing the code at runtime, it does show at when debugging.

A quick solution is using `TTask.WaitForAll`:

```delphi
var T: ITask;
begin
  T := TTask.Run<TProc<string>>(Work, Log);
  TTask.WaitForAll(T);
end;
```

At runtime, the exception message will show as

>One or more errors occurred

This is rather generic and doesn't give much information about the error.  By adding some class method to TTask, the error may handle in better way:

```delphi
class function TTaskHelper.WaitForAllWithException(Tasks: array of ITask):
    Boolean;
begin
  Result := True;
  try
    Result := WaitForAll(Tasks);
  except
    HandleException(Exception(AcquireExceptionObject));
  end;
end;

class procedure TTaskHelper.HandleException(E: Exception);
var X: Exception;
    A: TArray<string>;
    s: string;
begin
  if E is EAggregateException then begin
    A := nil;
    for X in EAggregateException(E) do A := A + [X.Message];
    s := string.Join(#13#10, A);
  end else
    s := E.Message;

  E.Free;

  Application.BeginInvoke<string>(
    procedure (S: string)
    begin
      Application.MessageBox(PChar(S), PChar(Application.Title), MB_OK + MB_ICONSTOP);
    end
  , s
  );
end;
```

And the code may written as:
```delphi
var T: ITask;
begin
  T := TTask.Run<TProc<string>>(Work, Log);
  TTask.WaitForAllWithException(T);
end;
```

## Use case: Log Multi-thread operation in ShowModal application

A classical approach of using modal form in Delphi may code as:

```delphi
F := TLogForm.CreateNew(nil);
try
  F.ShowModal;
finally
  F.Release;
end;
```
Assuming the `TLogForm` has the following implementation:

```delphi
type
  TLogForm = class(TForm)
  private
    FMemo: TMemo;
    FOK: TButton;
    FCancel: TButton;
  public
    procedure AfterConstruction; override;
    procedure Log(S: string);
  end;

procedure TLogForm.AfterConstruction;
begin
  Width := 500;
  Height := 500;
  FMemo := TMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Align := alClient;

  FOK := TButton.Create(Self);
  FOK.Parent := Self;
  FOK.Caption := 'OK';
  FOK.ModalResult := mrOk;
  FOK.Align := alBottom;

  FCancel := TButton.Create(Self);
  FCancel.Parent := Self;
  FCancel.Caption := 'Cancel';
  FCancel.ModalResult := mrCancel;
  FCancel.Align := alBottom;
end;

procedure TLogForm.Log(S: string);
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FMemo.Lines.Add(S)
  else
    Application.BeginInvoke<string>(Log, S);
end;
```

Then, to execute an asynchronous task and show progress log in the log form showing in modal mode:

```delphi
procedure TForm1.Button1Click(Sender: TObject);
begin
  TTask.Run<TProc<string>>(Work, Log);
end;
```

First, we need to find a spot to run the `Work` method.  The best spot is before ShowModal and after TLogForm instantiation:

```delphi
F := TLogForm.CreateNew(nil);
try
  TTask.Run<TProc<string>>(Work, F.Log);
  F.ShowModal;
finally
  F.Release;
end;
```
>**Note**
>
>There is an inherent problem with above code, if ModalResult was send to the modal form without waiting for the task to complete followed by terminated the application, the application will memory leak.  The issue will further discuss after exploring the cancel operation.

Next, to support cancel operation in the middle of execution:

```delphi
procedure TForm1.Work(Log: TProc<string>);
var i: Integer;
begin
  for i := 1 to 5 do begin
    Log('Progress - ' + i.ToString);
    Sleep(i*300);
    if Assigned(TTask.CurrentTask()) and (TTask.CurrentTask.Status = TTaskStatus.Canceled) then
      TTask.CurrentTask.Cancel;
  end;
end;

var T: ITask;
    F: TLogForm;
begin
  F := TLogForm.CreateNew(nil);
  try
    T := TTask.Run<TProc<string>>(Work, F.Log);
    if F.ShowModal = mrCancel then
      T.Cancel;
  finally
    F.Release;
  end;
end;
```

Above code works without any problem if the `work` task completed before terminating application.  Memory leak shall occur if close the application immediately after ShowModal.  To overcome the immature cancel of threaded task, use TTask.WaitForAll like:

```delphi
var T: ITask;
    F: TLogForm;
begin
  F := TLogForm.CreateNew(nil);
  try
    T := TTask.Run<TProc<string>>(Work, F.Log);
    if F.ShowModal = mrCancel then
      T.Cancel;
    TTaks.WaitForAll(T);
  finally
    F.Release;
  end;
end;
```

If the application does not allow cancelling the task in the middle, the code may implement as follows:

```delphi
var T: ITask;
    F: TLogForm;
begin
  F := TLogForm.CreateNew(nil);
  try
    T := TTask.Run<TProc<string>>(Work, F.Log);
    while not (T.Status in [TTaskStatus.Completed, TTaskStatus.Canceled, TTaskStatus.Exception]) do
      F.ShowModal;
    TTaks.WaitForAll(T);
  finally
    F.Release;
  end;
end;
```

An added advantage using `TTask.WaitForall` is exception will show during runtime.

# Reference

1. [Retrofitting a classic](https://community.embarcadero.com/blogs/entry/retrofitting-a-classic-38868)
2. [A Sink Programming](https://blog.therealoracleatdelphi.com/2008/09/a-sink-programming_26.html)
3. [More A Sink Kronos programming](https://blog.therealoracleatdelphi.com/2008/09/more-sink-kronos-programming_29.html)
4. [Value Capture vs. Variable Capture](https://blog.therealoracleatdelphi.com/2008/10/value-capture-vs-variable-capture_15.html)
5. [Using the Asynchronous Programming Library](http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Using_the_Asynchronous_Programming_Library)
