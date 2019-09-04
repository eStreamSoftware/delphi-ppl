unit System.ThreadingEx.TestCase;

interface

uses
  System.Classes, System.SysUtils, System.Threading, System.Types, Vcl.Forms,
  TestFramework;

type
  TFixedArray = array[0..2] of Integer;
  TDynArray = TArray<Integer>;

  TTestCase_PPL = class(TTestCase)
  private
    FCheckProc: TProc;
    FHost: TComponent;
    FInteger: IFuture<Integer>;
    FLastDynArray: TDynArray;
    FLastFixedArray: TFixedArray;
    FLastInteger: Integer;
    FLastPoint: TPoint;
    procedure CheckTestProc;
    function NewArray: TFixedArray;
    function NewDynArray: TDynArray;
    function NewInteger: Integer;
    function NewPoint: TPoint;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    class constructor Create;
    // Test name is coded as A_FuncN_[Func,Proc]N
    // A: Data type
    // FuncN: Future function with N parameters
    // [Func,Proc]N: BeginInvoke with N parameters
  published
    procedure Ordinal_Async_0;
    procedure Ordinal_Async_1;
    procedure Ordinal_Async_2;
    procedure Ordinal_Async_3;
    procedure DynArray_Async_1;
    procedure DynArray_Async_2;
    procedure DynArray_Async_3;
    procedure DynArray_Func0_ConstProc;
    procedure DynArray_Func0_Func1;
    procedure DynArray_Func0_Proc1;
    procedure DynArray_Func0_Proc2;
    procedure FixedArray_Func0_Func1;
    procedure FixedArray_Func0_Proc1;
    procedure Ordinal_Func0_Func0;
    procedure Ordinal_Func0_Func1;
    procedure Ordinal_Func0_Proc1;
    procedure Ordinal_Func1_Proc1;
    procedure Record_Func0_Func1;
    procedure Record_Func0_Proc1;
  end;

implementation

uses
  System.ThreadingEx;

class constructor TTestCase_PPL.Create;
begin
  RegisterTest(Suite);
end;

procedure TTestCase_PPL.Ordinal_Async_0;
begin
  Application.Async(procedure begin
    FCheckProc := procedure begin
      Check(True);
    end
  end
  );
end;

procedure TTestCase_PPL.Ordinal_Async_1;
begin
  Application.Async<Integer>(procedure (a: Integer) begin
    FCheckProc := procedure begin
      CheckEquals(12345, a);
    end
  end
  , 12345
  );
end;

procedure TTestCase_PPL.Ordinal_Async_2;
begin
  Application.Async<Integer,Integer>(procedure (a, b: Integer) begin
    FCheckProc := procedure begin
      CheckEquals(3, a + b);
    end
  end
  , 1, 2
  );
end;

procedure TTestCase_PPL.Ordinal_Async_3;
begin
  Application.Async<Integer,Integer,Integer>(procedure (a, b, c: Integer) begin
    FCheckProc := procedure begin
      CheckEquals(6, a + b + c);
    end
  end
  , 1, 2, 3
  );
end;

procedure TTestCase_PPL.CheckTestProc;
begin
  if Assigned(FCheckProc) then
    FCheckProc
  else
    Check(False, 'no test procedure assigned');
end;

procedure TTestCase_PPL.DynArray_Async_1;
begin
  Application.Async<TDynArray>(procedure (a: TDynArray) begin
    FCheckProc := procedure begin
      CheckEquals(6, a[0] + a[1] + a[2]);
    end
  end
  , TDynArray.Create(1, 2, 3)
  );
end;

procedure TTestCase_PPL.DynArray_Async_2;
begin
  Application.Async<TDynArray, Integer>(procedure (a: TDynArray; b: Integer) begin
    FCheckProc := procedure begin
      CheckEquals(10, a[0] + a[1] + a[2] + b);
    end
  end
  , TDynArray.Create(1, 2, 3), 4
  );
end;

procedure TTestCase_PPL.DynArray_Async_3;
begin
  Application.Async<TDynArray, Integer, Integer>(procedure (a: TDynArray; b, c: Integer) begin
    FCheckProc := procedure begin
      CheckEquals(15, a[0] + a[1] + a[2] + b + c);
    end
  end
  , TDynArray.Create(1, 2, 3), 4, 5
  );
end;

procedure TTestCase_PPL.DynArray_Func0_ConstProc;
begin
  FHost
  .Future<TArray<Integer>>(NewDynArray)
  .BeginInvoke(
     procedure (const a: TDynArray)
     begin
       FCheckProc := procedure begin
         CheckEquals(Length(FLastDynArray), Length(a));
         Status('Actual: ' + Length(FLastDynArray).ToString);
         for var i := Low(a) to High(a) do begin
           CheckEquals(FLastDynArray[i], a[i]);
           Status('Actual: ' + FLastDynArray[i].ToString);
         end;
       end;
     end
  );

  CheckEquals(0, Length(FLastDynArray), 'Expect a clean value');
end;

procedure TTestCase_PPL.DynArray_Func0_Func1;
begin
  FHost
  .Future<TArray<Integer>>(NewDynArray)
  .AsObject
  .BeginInvoke<Integer>(
    function (a: TDynArray): Integer
    begin
      Result := Length(a);
      FCheckProc := procedure begin
        CheckEquals(Length(FLastDynArray), Length(a));
        Status('Actual: ' + Length(FLastDynArray).ToString);
        for var i := Low(a) to High(a) do begin
          CheckEquals(FLastDynArray[i], a[i]);
          Status('Actual: ' + FLastDynArray[i].ToString);
        end;
      end;
    end
  );

  CheckEquals(0, Length(FLastDynArray), 'Expect a clean value');
end;

procedure TTestCase_PPL.DynArray_Func0_Proc1;
begin
  FHost
  .Future<TArray<Integer>>(NewDynArray)
  .BeginInvoke(
    procedure (a: TDynArray)
    begin
      FCheckProc := procedure begin
        CheckEquals(Length(FLastDynArray), Length(a));
        Status('Actual: ' + Length(FLastDynArray).ToString);
        for var i := Low(a) to High(a) do begin
          CheckEquals(FLastDynArray[i], a[i]);
          Status('Actual: ' + FLastDynArray[i].ToString);
        end;
      end;
    end
  );

  CheckEquals(0, Length(FLastDynArray), 'Expect a clean value');
end;

procedure TTestCase_PPL.DynArray_Func0_Proc2;
begin
  FHost
  .Future<TArray<Integer>>(NewDynArray)
  .AsObject
  .BeginInvoke<Integer>(
    procedure (a: TDynArray; b: Integer)
    begin
      FCheckProc := procedure begin
        CheckEquals(-1, b);
        CheckEquals(Length(FLastDynArray), Length(a));
        Status('Actual: ' + Length(FLastDynArray).ToString);
        for var i := Low(a) to High(a) do begin
          CheckEquals(FLastDynArray[i], a[i]);
          Status('Actual: ' + FLastDynArray[i].ToString);
        end;
      end;
    end
  , -1
  );

  CheckEquals(0, Length(FLastDynArray), 'Expect a clean value');
end;

procedure TTestCase_PPL.FixedArray_Func0_Func1;
begin
  FHost
  .Future<TFixedArray>(NewArray)
  .AsObject
  .BeginInvoke<Integer>(
    function (a: TFixedArray): Integer
    begin
      Result := Length(a);
      FCheckProc := procedure begin
        CheckEquals(Length(FLastFixedArray), Length(a));
        Status('Actual: ' + Length(FLastFixedArray).ToString);
        for var i := Low(a) to High(a) do begin
          CheckEquals(FLastFixedArray[i], a[i]);
          Status('Actual: ' + FLastFixedArray[i].ToString);
        end;
      end;
    end
  );

  for var a in FLastFixedArray do
    CheckEquals(-1, a, 'Expect a clean value');
end;

procedure TTestCase_PPL.FixedArray_Func0_Proc1;
begin
  FHost
  .Future<TFixedArray>(NewArray)
  .BeginInvoke(
    procedure (a: TFixedArray)
    begin
      FCheckProc := procedure begin
        CheckEquals(Length(FLastFixedArray), Length(a));
        Status('Actual: ' + Length(FLastFixedArray).ToString);
        for var i := Low(a) to High(a) do begin
          CheckEquals(FLastFixedArray[i], a[i]);
          Status('Actual: ' + FLastFixedArray[i].ToString);
        end;
      end;
    end
  );

  for var a in FLastFixedArray do
    CheckEquals(-1, a, 'Expect a clean value');
end;

function TTestCase_PPL.NewArray: TFixedArray;
begin
  for var i := Low(Result) to High(Result) do
    Result[i] := Random(MaxInt);
  FLastFixedArray := Result;
end;

function TTestCase_PPL.NewDynArray: TDynArray;
begin
  FLastDynArray := TDynArray.Create(Random(MaxInt), Random(MaxInt), Random(MaxInt));
  Exit(FLastDynArray);
end;

function TTestCase_PPL.NewInteger: Integer;
begin
  FLastInteger := Random(MaxInt);
  Result := FLastInteger;
end;

function TTestCase_PPL.NewPoint: TPoint;
begin
  Result := TPoint.Create(Random(MaxInt), Random(MaxInt));
  FLastPoint := Result;
end;

procedure TTestCase_PPL.Ordinal_Func0_Func0;
begin
  FInteger := FHost
  .Future<Integer>
  .BeginInvoke(
    function: Integer
    begin
      Exit(Random(MaxInt));
    end
  );

  FCheckProc := procedure begin
    CheckEquals(FInteger.Value, FInteger.Value);
    Status('Actual: ' + FInteger.Value.ToString);
  end;

  CheckFalse(TTask.IsEnded(FInteger), 'Expect incomplete seed value');
end;

procedure TTestCase_PPL.Ordinal_Func0_Func1;
begin
  FHost
  .Future<Integer>(NewInteger)
  .AsObject
  .BeginInvoke<Integer>(
    function (a: Integer): Integer
    begin
      Result := a;
      FCheckProc := procedure
      begin
        CheckEquals(FLastInteger, a);
        Status('Actual: ' + FLastInteger.ToString);
      end;
    end
  );

  CheckEquals(-1, FLastInteger, 'Expect a clean value');
end;

procedure TTestCase_PPL.Ordinal_Func0_Proc1;
begin
  FHost
  .Future<Integer>(NewInteger)
  .BeginInvoke(
    procedure (a: Integer)
    begin
      FCheckProc := procedure begin
        CheckEquals(FLastInteger, a);
        Status('Actual: ' + FLastInteger.ToString);
      end;
    end
  );

  CheckEquals(-1, FLastInteger, 'Expect a clean value');
end;

procedure TTestCase_PPL.Ordinal_Func1_Proc1;
begin
  var iSeed := Random(MaxInt div 3);

  FHost
  .Future<Integer,Integer>(
    function(a: Integer): Integer
    begin
      FHost.BeginInvoke<string>(Status, 'Expected: ' + iSeed.ToString);
      Exit(iSeed * 2);
    end
  , iSeed
  )
  .BeginInvoke(
    procedure(a: Integer)
    begin
      FCheckProc := procedure
      begin
        CheckEquals(iSeed * 2, a);
        Status('Actual: ' + (iSeed * 2).ToString);
      end;
    end
  );
end;

procedure TTestCase_PPL.Record_Func0_Func1;
begin
  FHost
  .Future<TPoint>(NewPoint)
  .AsObject
  .BeginInvoke<Integer>(
    function (a: TPoint): Integer
    begin
      Result := SizeOf(a);
      FCheckProc := procedure begin
        CheckEquals(FLastPoint.X, a.X);
        CheckEquals(FLastPoint.Y, a.Y);
      end;
    end
  );

  CheckEquals(-1, FLastPoint.X, 'Expect a clean value');
  CheckEquals(-1, FLastPoint.Y, 'Expect a clean value');
end;

procedure TTestCase_PPL.Record_Func0_Proc1;
begin
  FHost
  .Future<TPoint>(NewPoint)
  .BeginInvoke(
    procedure (a: TPoint)
    begin
      FCheckProc := procedure begin
        CheckEquals(FLastPoint.X, a.X);
        CheckEquals(FLastPoint.Y, a.Y);
      end;
    end
  );

  CheckEquals(-1, FLastPoint.X, 'Expect a clean value');
  CheckEquals(-1, FLastPoint.Y, 'Expect a clean value');
end;

procedure TTestCase_PPL.SetUp;
begin
  inherited;
  {$IFNDEF MSWINDOWS}Assert(False, 'Test is only available in Windows GUI application');{$ENDIF}
  FHost := TComponent.Create(nil);
  FCheckProc := nil;

  Randomize;
  FInteger := nil;
  FLastInteger := -1;
  FLastDynArray := nil;
  FLastPoint := TPoint.Create(-1, -1);

  FillChar(FLastFixedArray, SizeOf(FLastFixedArray), $FF);
end;

procedure TTestCase_PPL.TearDown;
begin
  Sleep(1);
  Application.ProcessMessages;
  try
    CheckTestProc;
  finally
    inherited;
    FInteger := nil;
    FHost.Free;
  end;
end;

end.
