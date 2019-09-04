unit System.SyncObjs.RSP25999;

interface

implementation

uses
  System.Classes, System.SyncObjs,
  DDetours;

type
  TMultiWaitEventAccess = class
  private
    FEvent: TLightweightEvent;
  end;

  TBaseAsyncResultAccess = class(TInterfacedObject)
  private type
    TAsyncFlag = (Completed, Synchronous, Invoked, Cancelled, ForceSize = SizeOf(Integer) * 8 - 1); // make the size the same as Integer.
    TAsyncFlags = set of TAsyncFlag;
  private
    {$hints off}
    FContext: TObject;
    FAsyncFlags: TAsyncFlags;
    FInvokingThread: TThreadID;
    FAsyncHandle: TMultiWaitEventAccess;
    {$hints on}
  end;

var TBaseAsyncResult_Destroy: procedure (Self: TBaseAsyncResult) = nil;

procedure TBaseAsyncResult_Destroy_Patch(Self: TBaseAsyncResult);
var o: TObject;
begin
  // Capture the FEvent object
  o := nil;
  if Assigned(TBaseAsyncResultAccess(Self).FAsyncHandle) then
    o := TBaseAsyncResultAccess(Self).FAsyncHandle.FEvent;

  // Invoke original destructor
  TBaseAsyncResult_Destroy(Self);

  // and free FEvent object here
  if Assigned(o) then
    o.Free;
end;

initialization
  TBaseAsyncResult_Destroy := InterceptCreate(@TBaseAsyncResult.Destroy, @TBaseAsyncResult_Destroy_Patch);
finalization
  InterceptRemove(@TBaseAsyncResult_Destroy);
end.

