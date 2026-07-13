with GNAT.Source_Info;
with AUnit.Assertions; use AUnit.Assertions;
with ZMQ.Pollsets; use ZMQ.Pollsets;

package body ZMQ.Tests.TestCases.Test_Polling is
   use AUnit;
   use type ZMQ.Sockets.Any_Socket;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File);
   end Name;

   Test_Port : constant String := "inproc://polling";


   -----------------
   -- Set_Up_Case --
   -----------------

   overriding
   procedure Set_Up_Case (Test : in out Test_Case) is
      T : Test_Case renames Test;
   begin
      T.Pub.Initialize (T.Ctx, Sockets.PUB);
      T.Sub.Initialize (T.Ctx, Sockets.SUB);
      T.Pub_Access := T.Pub'Unchecked_Access;
      T.Sub_Access := T.Sub'Unchecked_Access;

      T.Pub.Bind    (Test_Port);
      T.Sub.Connect (Test_Port);
      T.Sub.Establish_Message_Filter ("");
      delay 0.1;
   end Set_Up_Case;

   ------------
   -- Append --
   ------------

   procedure Append (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T    : Test_Case renames Test_Case (Test);
      PS   : Poll_Set (10);
      Item : constant Poll_Item := (T.Pub_Access, Poll_In or Poll_Out);
   begin
      --  Verify poll item existence after appending it to the poll set.
      Assert (not PS.Contains (Item),
              "Poll item must not exist before appending it to poll set");
      PS.Append (Item);
      Assert (PS.Contains (Item),
              "Poll item not found after appending it to poll set");
   end Append;

   ------------
   -- Remove --
   ------------

   procedure Remove (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T    : Test_Case renames Test_Case (Test);
      PS   : Poll_Set (10);
      Item : constant Poll_Item := (T.Pub_Access, Poll_In or Poll_Out);
   begin
      PS.Append (Item);
      Assert (PS.Contains (Item),
              "Poll item not found after appending it to poll set");

      --  Verify poll item non-existence after removing it from the poll set.
      PS.Remove (Item);
      Assert (not PS.Contains (Item),
              "Poll item found after removing it from poll set");

      --  Verify idempotency.
      PS.Remove (Item);
      Assert (not PS.Contains (Item),
              "Poll item found after removing it twice from poll set");
   end Remove;

   ----------
   -- Poll --
   ----------

   procedure Poll (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T      : Test_Case renames Test_Case (Test);
      PS     : Poll_Set (10);
      Item   : constant Poll_Item := (T.Sub_Access, Poll_In or Poll_Out);
      Events : Natural := 0;
   begin
      --  Register for Pub socket polling of any events, poll and verify there
      --  is not event yet available on the Sub socket.
      PS.Append (Item);
      PS.Poll (Timeout => 1, Signaled_Events => Events);
      Assert (Events = 0, "Expected 0 events to be available");

      --  Send some data on the Pub socket, poll and verify there is 1 event
      --  available on the Sub socket.
      T.Pub.Send ("Data");

      PS.Poll (Timeout => 1, Signaled_Events => Events);
      Assert (Events = 1, "Expected 1 event to be available");

      --  Retrieve the signaled events and verify.
      declare
         Signaled    : constant Poll_Items := PS.Signaled_Events;
         First_Event : constant Natural := Signaled'First;
      begin
         Assert (Signaled'Length = 1, "Expected 1 signaled event");
         Assert (Signaled (First_Event).Socket = T.Sub_Access,
                 "Expected Sub socket is invalid");
         Assert (Signaled (First_Event).Events = (Poll_In or Poll_Out),
                 "Expected to have Poll_In + Poll_Out signaled events");
      end;
   end Poll;

   ---------------
   -- Poll_Many --
   ---------------

   procedure Poll_Many (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T       : Test_Case renames Test_Case (Test);
      PS      : Poll_Set (10);
      Item1   : constant Poll_Item := (T.Pub_Access, Poll_Out);
      Item2   : constant Poll_Item := (T.Sub_Access, Poll_In);
      Events  : Natural := 0;
   begin
      --  Register for Pub and Sub socket polling of any events and send some
      --  data on the Sub socket.
      PS.Append (Item1);
      PS.Append (Item2);
      T.Pub.Send ("Data");

      --  Poll and verify there are 2 events available.
      PS.Poll (Timeout => 1, Signaled_Events => Events);
      Assert (Events = 2, "Expected 2 events to be available");

      --  Retrieve the signaled events and verify.
      declare
         Signaled    : constant Poll_Items := PS.Signaled_Events;
         First_Event : constant Natural := Signaled'First;
      begin
         Assert (Signaled'Length = 2, "Expected 2 signaled events");
         Assert (Signaled (First_Event).Socket = T.Pub_Access,
                 "Expected Pub socket is invalid");
         Assert (Signaled (First_Event).Events = (Poll_Out),
                 "Expected to have Poll_Out signaled events");
         Assert (Signaled (First_Event + 1).Socket = T.Sub_Access,
                 "Expected Sub socket is invalid");
         Assert (Signaled (First_Event + 1).Events = (Poll_In),
                 "Expected to have Poll_In signaled events");
      end;
   end Poll_Many;

   ------------------------------
   -- Append_Resets_Poll_State --
   ------------------------------

   procedure Append_Resets_Poll_State
     (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T       : Test_Case renames Test_Case (Test);
      PS      : Poll_Set (10);
      Item1   : constant Poll_Item := (T.Pub_Access, Poll_Out);
      Item2   : constant Poll_Item := (T.Sub_Access, Poll_In);
      Events  : Natural := 0;
   begin
      --  Append one item, poll and then append another item. Calling
      --  Signaled_Events must return empty events.
      PS.Append (Item1);
      PS.Poll (Timeout => 1, Signaled_Events => Events);
      PS.Append (Item2);

      Assert (PS.Signaled_Events'Length = 0, "Expected no signaled events");
   end Append_Resets_Poll_State;

   ------------------------------
   -- Remove_Resets_Poll_State --
   ------------------------------

   procedure Remove_Resets_Poll_State
     (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T       : Test_Case renames Test_Case (Test);
      PS      : Poll_Set (10);
      Item1   : constant Poll_Item := (T.Pub_Access, Poll_Out);
      Item2   : constant Poll_Item := (T.Sub_Access, Poll_In);
      Events  : Natural := 0;
   begin
      --  Append two items, poll and then remove one item. Calling
      --  Signaled_Events must return empty events.
      PS.Append (Item1);
      PS.Append (Item2);
      PS.Poll (Timeout => 1, Signaled_Events => Events);
      PS.Remove (Item1);

      Assert (PS.Signaled_Events'Length = 0, "Expected no signaled events");
   end Remove_Resets_Poll_State;


   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding
   procedure Tear_Down_Case (Test : in out Test_Case) is
      T : Test_Case renames Test;
   begin
      T.Sub.Finalize;
      T.Pub.Finalize;
   end Tear_Down_Case;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;

   begin
      Register_Routine (T, Append'Access, "Append");
      Register_Routine (T, Remove'Access, "Remove");
      Register_Routine (T, Poll'Access, "Poll");
      Register_Routine (T, Poll_Many'Access, "Poll_Many");
      Register_Routine (T, Append_Resets_Poll_State'Access,
                        "Append_Resets_Poll_State");
      Register_Routine (T, Remove_Resets_Poll_State'Access,
                        "Remove_Resets_Poll_State");
   end Register_Tests;

end ZMQ.Tests.TestCases.Test_Polling;
