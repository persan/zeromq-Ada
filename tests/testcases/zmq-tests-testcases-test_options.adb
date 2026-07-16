with GNAT.Source_Info;
with AUnit.Assertions; use AUnit.Assertions;

package body ZMQ.Tests.TestCases.Test_Options is

   use AUnit;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File);
   end Name;

   Test_Port : constant String := "inproc://options";


   ------------
   -- Set_Up --
   ------------

   overriding
   procedure Set_Up (Test : in out Test_Case) is
      T : Test_Case renames Test;
   begin
      T.Rep.Initialize (T.Ctx, Sockets.REP);

      T.Rep.Bind    (Test_Port);
   end Set_Up;


   -------------------
   -- Check_Options --
   -------------------

   procedure Check_Options (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T              : Test_Case renames Test_Case (Test);
      B_Value_Ignore : Boolean;
      I_Value_Ignore : Integer;
      D_Value_Ignore : Duration;
      L_Value_Ignore : Long_Long_Integer;
      T_Value_Ignore : ZMQ.Sockets.Thread_Bitmap;
   begin
      --  With checks enabled, any of the following functions could raise if
      --  there is a mismatch with 0MQ's expected data types/sizes.
      --  Values are simply ignored.
      I_Value_Ignore := T.Rep.Get_High_Water_Mark_For_Inbound_Messages;
      I_Value_Ignore := T.Rep.Get_High_Water_Mark_For_Outbound_Messages;
      T_Value_Ignore := T.Rep.Get_IO_Thread_Affinity;
      I_Value_Ignore := T.Rep.Get_Kernel_Receive_Buffer_Size;
      I_Value_Ignore := T.Rep.Get_Kernel_Transmit_Buffer_Size;
      D_Value_Ignore := T.Rep.Get_Linger_Period_For_Socket_Shutdown;
      L_Value_Ignore := T.Rep.Get_Maximum_Acceptable_Inbound_Message_Size;
      I_Value_Ignore :=
        T.Rep.Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections;
      I_Value_Ignore := T.Rep.Get_Maximum_Network_Hops_For_Multicast_Packets;
      D_Value_Ignore := T.Rep.Get_Maximum_Reconnection_Interval;
      I_Value_Ignore := T.Rep.Get_Multicast_Data_Rate;
      D_Value_Ignore := T.Rep.Get_Multicast_Recovery_Interval;
      D_Value_Ignore := T.Rep.Get_Recieve_Timeout;
      D_Value_Ignore := T.Rep.Get_Reconnection_Interval;
      D_Value_Ignore := T.Rep.Get_Send_Timeout;
      B_Value_Ignore := T.Rep.Get_Use_IPv4_Only;
      B_Value_Ignore := T.Rep.More_Message_Parts_To_Follow;
   end Check_Options;

   ---------------
   -- Durations --
   ---------------

   procedure Durations (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T      : Test_Case renames Test_Case (Test);
      Value  : Duration;
   begin
      --  Set to 1 second. Internally, 0MQ takes milliseconds.
      T.Rep.Set_Linger_Period_For_Socket_Shutdown (1.0);
      Value := T.Rep.Get_Linger_Period_For_Socket_Shutdown;
      Assert (Value = 1.0,
              "Expected a linger period of 1 second; got " & Value'Image);

      T.Rep.Set_Send_Timeout (1.0);
      Value := T.Rep.Get_Send_Timeout;
      Assert (Value = 1.0,
              "Expected a send timeout of 1 second; got " & Value'Image);
   end Durations;

   ---------------
   -- Tear_Down --
   ---------------

   overriding
   procedure Tear_Down (Test : in out Test_Case) is
      T : Test_Case renames Test;
   begin
      T.Rep.Finalize;
      delay 0.1;
   end Tear_Down;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;

   begin
      Register_Routine (T, Check_Options'Access, "Check_Options");
      Register_Routine (T, Durations'Access, "Durations");
   end Register_Tests;

end ZMQ.Tests.TestCases.Test_Options;
