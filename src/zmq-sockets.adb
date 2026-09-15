-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                           Z M Q . S O C K E T S                           --
--                                                                           --
--                                  B o d y                                  --
--                                                                           --
--            Copyright (C) 2020-2030, per.s.sandberg@bahnhof.se             --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------

with ZMQ.Errors;
with ZMQ.Low_Level;
with Interfaces.C.Strings;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body ZMQ.Sockets is
   use Interfaces.C.Strings;
   use Interfaces.C;
   use System;

   ----------------
   -- Get_Option --
   ----------------

   function Get_Option (This : Socket; Option : Interfaces.C.int) return T is
      Value : aliased T;
      Size  : Natural := T'Size / System.Storage_Unit;
   begin
      This.Getsockopt (Option, Value'Address, Size);

      if Size /= T'Size / System.Storage_Unit then
         raise Program_Error;
      end if;

      return Value;
   end Get_Option;

   function Get_C_Int is new Get_Option (int);
   function Get_C_Unsigned_Long is new Get_Option (unsigned_long);
   function Get_Long_Long is new Get_Option (Long_Long_Integer);

   -----------------------
   -- Duration_To_Msecs --
   -----------------------

   function Duration_To_Msecs (D : Duration) return Interfaces.C.int is
      MSec : Long_Long_Integer;
   begin
      if D = Duration'Last then
         return Interfaces.C.int (-1);
      elsif D < 0.0 then
         --  Map negative to -1.
         return Interfaces.C.int (-1);
      else
         --  Round to nearest millisecond, then check bounds.
         MSec := Long_Long_Integer (D * 1000.0 + Duration (0.5));
         if MSec > Long_Long_Integer (Interfaces.C.int'Last) then
            raise Constraint_Error with
              "Duration too large to convert to milliseconds";
         end if;
         return Interfaces.C.int (MSec);
      end if;
   end Duration_To_Msecs;

   -----------------------
   -- Msecs_To_Duration --
   -----------------------

   function Msecs_To_Duration (MSec : Interfaces.C.int) return Duration is
   begin
      if MSec < 0 then
         return Duration'Last;
      else
         return Duration (MSec) / 1000.0;
      end if;
   end Msecs_To_Duration;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (This         : in out Socket;
      With_Context : Contexts.Context;
      Kind         : Socket_Type)
   is
   begin
      if With_Context.GetImpl = Null_Address then
         raise ZMQ_Error with "Context Not Initialized";
      end if;
      if This.C /= Null_Address then
         raise ZMQ_Error with "Socket Initialized";
      end if;

      This.C := Low_Level.zmq_socket (With_Context.GetImpl,
                                      Socket_Type'Pos (Kind));
      if This.C = Null_Address then
         ZMQ.Errors.Set_To_Errno;

         raise ZMQ_Error with "Unable to initialize";
      end if;
   end Initialize;

   ----------
   -- Bind --
   ----------

   not overriding procedure Bind
     (This    : in out Socket;
      Address : String)
   is
      Addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      Ret  : int;
   begin
      Ret := Low_Level.zmq_bind (This.C, Addr);

      if Ret /= 0 then
         ZMQ.Errors.Set_To_Errno;

         Free (Addr);

         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;

      Free (Addr);
   end Bind;

   procedure Bind (This    : in out Socket;
                   Address : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Bind (To_String (Address));
   end Bind;

   ------------
   -- Unbind --
   ------------

   not overriding procedure Unbind
     (This    : in out Socket;
      Address : String)
   is
      Addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      Ret  : int;
   begin
      Ret := Low_Level.zmq_unbind (This.C, Addr);

      if Ret /= 0 then
         ZMQ.Errors.Set_To_Errno;

         Free (Addr);

         raise ZMQ_Error with  Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;

      Free (Addr);
   end Unbind;

   procedure Unbind (This    : in out Socket;
                     Address : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Unbind (To_String (Address));
   end Unbind;

   ----------------
   -- Setsockopt --
   ----------------

   procedure Setsockopt (This       : in out Socket;
                         Option     : Interfaces.C.int;
                         Value      : System.Address;
                         Value_Size : Natural) is
      Ret : int;
   begin
      Ret :=
        Low_Level.zmq_setsockopt (This.C, Option, Value, size_t (Value_Size));

      if Ret /= 0 then
         ZMQ.Errors.Set_To_Errno;

         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Option'Img & ")";
      end if;
   end Setsockopt;

   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Interfaces.C.int;
      Value  : String)
   is
   begin
      This.Setsockopt (Option, Value'Address, Value'Length);
   end Setsockopt;

   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Interfaces.C.int;
      Value  : Boolean)
   is
   begin
      This.Setsockopt (Option, Value'Address, 1);
   end Setsockopt;

   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Interfaces.C.int;
      Value  : Integer)
   is
   begin
      This.Setsockopt (Option, Value'Address, 4);
   end Setsockopt;

   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Interfaces.C.int;
      Value  : Long_Long_Integer)
   is
   begin
      This.Setsockopt (Option, Value'Address, 8);
   end Setsockopt;

   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Interfaces.C.int;
      Value  : Ada.Streams.Stream_Element_Array)
   is
   begin
      This.Setsockopt (Option, Value (Value'First)'Address, Value'Length);
   end Setsockopt;

   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Interfaces.C.int;
      Value  : Interfaces.C.unsigned_long)
   is
   begin
      This.Setsockopt (Option, Value'Address, 8);
   end Setsockopt;

   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Interfaces.C.int;
      Value  : Duration)
   is
   begin
      This.Setsockopt (Option, Integer (Duration_To_Msecs (Value)));
   end Setsockopt;

   -------------
   -- Connect --
   -------------

   not overriding procedure Connect
     (This    : in out Socket;
      Address : String)
   is
      Addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      Ret  : int;
   begin
      Ret := Low_Level.zmq_connect (This.C, Addr);

      if Ret /= 0 then
         ZMQ.Errors.Set_To_Errno;

         Free (Addr);

         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;

      Free (Addr);
   end Connect;

   procedure Connect
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      This.Connect (To_String (Address));
   end Connect;

   ----------
   -- Send --
   ----------

   not overriding procedure Send
     (This  : in out Socket;
      Msg   : Messages.Message'Class;
      Flags : Socket_Flags := No_Flags)
   is
      Ret : int;
   begin
      Ret := Low_Level.zmq_msg_send (Msg.GetImpl, This.C, int (Flags));

      if Ret = -1 then
         ZMQ.Errors.Set_To_Errno;

         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Send;

   not overriding procedure Send
     (This  : in out Socket;
      Msg   : String;
      Flags : Socket_Flags := No_Flags)
   is
   begin
      This.Send (Msg'Address, Msg'Length, Flags);
   end Send;

   not overriding procedure Send
     (This  : in out Socket;
      Msg   : Ada.Streams.Stream_Element_Array;
      Flags : Socket_Flags := No_Flags)
   is
   begin
      This.Send (Msg'Address, Msg'Length, Flags);
   end Send;

   not overriding procedure Send
     (This  : in out Socket;
      Msg   : Ada.Strings.Unbounded.Unbounded_String;
      Flags : Socket_Flags := No_Flags)
   is
      M : Messages.Message;
   begin
      M.Initialize (Ada.Strings.Unbounded.To_String (Msg));
      This.Send (M, Flags);
   end Send;

   procedure Send_Generic (This  : in out Socket;
                           Msg   : Element;
                           Flags : Socket_Flags := No_Flags) is
   begin
      This.Send
        (Msg'Address,
         (Msg'Size + Ada.Streams.Stream_Element'Size - 1) /
             Ada.Streams.Stream_Element'Size,
         Flags);
   end Send_Generic;

   not overriding

   procedure Send (This        : in out Socket;
                   Msg_Address : System.Address;
                   Msg_Length  : Natural;
                   Flags       : Socket_Flags := No_Flags)
   is
      Ret : int;
   begin
      Ret :=
        Low_Level.zmq_send
          (This.C, Msg_Address, Interfaces.C.size_t (Msg_Length), int (Flags));

      if Ret = -1 then
         ZMQ.Errors.Set_To_Errno;

         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Send;

   ----------
   -- Recv --
   ----------

   not overriding procedure Recv
     (This  : in Socket;
      Msg   : in out Messages.Message'Class;
      Flags : Socket_Flags := No_Flags)
   is
      Ret : int;
   begin
      Ret := Low_Level.zmq_msg_recv (Msg.GetImpl, This.C, int (Flags));

      if Ret = -1 then
         ZMQ.Errors.Set_To_Errno;

         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Recv;

   procedure Recv (This  : in Socket;
                   Flags : Socket_Flags := No_Flags) is
      Dummy_Msg : Messages.Message;
   begin
      Dummy_Msg.Initialize (0);
      This.Recv (Dummy_Msg, Flags);
   end Recv;

   not overriding
   function Recv (This  : in Socket;
                  Flags : Socket_Flags := No_Flags) return String is
      Msg : Messages.Message;
   begin
      Msg.Initialize (0);
      This.Recv (Msg, Flags);
      return Ret : String (1 .. Msg.GetSize) do
         Ret := Msg.GetData;
      end return;
   end Recv;

   procedure Recv (This  : in Socket;
                   Msg   : out Ada.Strings.Unbounded.Unbounded_String;
                   Flags : Socket_Flags := No_Flags) is
   begin
      Msg := Ada.Strings.Unbounded.To_Unbounded_String (This.Recv (Flags));
   end Recv;

   function Recv
     (This       : in Socket;
      Max_Length : Natural;
      Flags      : Socket_Flags := No_Flags)
      return String is
      Msg : Messages.Message;
   begin
      This.Recv (Msg, Flags);
      if Msg.GetSize > Max_Length then
         raise Constraint_Error with "message size out of bounds" &
           Msg.GetSize'Img & ">" & Max_Length'Img;
      end if;
      return Ret : String (1 .. Msg.GetSize) do
         Ret := Msg.GetData;
      end return;
   end Recv;

   not overriding function Recv
     (This  : in Socket;
      Flags : Socket_Flags := No_Flags)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Ret : Ada.Strings.Unbounded.Unbounded_String do
         This.Recv (Ret, Flags);
      end return;
   end Recv;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (This : in out Socket)
   is
      Ret : int;
   begin
      if This.C /= Null_Address then
         Ret := Low_Level.zmq_close (This.C);

         if Ret /= 0 then
            ZMQ.Errors.Set_To_Errno;

            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
         end if;
         This.C := Null_Address;
      end if;
   end Finalize;

   -----------
   -- Proxy --
   -----------

   procedure Proxy (Frontend : not null access Socket;
                    Backend  : not null access Socket'Class;
                    Capture  : access Socket'Class) is
      Ret : int;
   begin
      Ret := Low_Level.zmq_proxy
        (Frontend.C,
         Backend.C,
         (if Capture /= null then Capture.C else System.Null_Address));

      if Ret /= 0 then
         ZMQ.Errors.Set_To_Errno;

         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
      end if;
   end Proxy;

   -----------------------------------------------
   -- Set_High_Water_Mark_For_Outbound_Messages --
   -----------------------------------------------

   not overriding
   procedure Set_High_Water_Mark_For_Outbound_Messages
     (This     : in out Socket;
      Messages : Natural := 1_000)
   is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDHWM, Messages);
   end Set_High_Water_Mark_For_Outbound_Messages;

   ----------------------------------------------
   -- Set_High_Water_Mark_For_Inbound_Messages --
   ----------------------------------------------

   not overriding
   procedure Set_High_Water_Mark_For_Inbound_Messages
     (This     : in out Socket;
      Messages : Natural := 1_000)
   is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVHWM, Messages);
   end Set_High_Water_Mark_For_Inbound_Messages;

   ---------------------------
   -- Set_Disk_Offload_Size --
   ---------------------------

   not overriding
   procedure Set_Disk_Offload_Size (This  : in out Socket;
                                    Value : Natural) is
   begin
      null; -- This.setsockopt (ZMQ.Low_Level.Defs.SWAP, Value);
   end Set_Disk_Offload_Size;

   ----------------------------
   -- Set_IO_Thread_Affinity --
   ----------------------------

   not overriding
   procedure Set_IO_Thread_Affinity (This    : in out Socket;
                                     Threads : Thread_Bitmap) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_AFFINITY, Threads'Address, 4);
   end Set_IO_Thread_Affinity;

   -------------------------
   -- Set_Socket_Identity --
   -------------------------

   not overriding
   procedure Set_Socket_Identity
     (This  : in out Socket;
      Value : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_IDENTITY, Value);
   end Set_Socket_Identity;

   procedure Set_Socket_Identity
     (This  : in out Socket;
      Value : String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_IDENTITY, Value);
   end Set_Socket_Identity;

   ------------------------------
   -- Establish_Message_Filter --
   ------------------------------

   not overriding
   procedure Establish_Message_Filter (This  : in out Socket;
                                       Value : String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SUBSCRIBE, Value);
   end Establish_Message_Filter;

   ------------------------------
   -- Establish_Message_Filter --
   ------------------------------

   not overriding
   procedure Establish_Message_Filter
     (This  : in out Socket;
      Value : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SUBSCRIBE, Value);
   end Establish_Message_Filter;

   procedure Establish_Message_Filter
     (This  : in out Socket;
      Value : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SUBSCRIBE, To_String (Value));
   end Establish_Message_Filter;

   ---------------------------
   -- Remove_Message_Filter --
   ---------------------------

   not overriding
   procedure Remove_Message_Filter (This  : in out Socket;
                                    Value : String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_UNSUBSCRIBE, Value);
   end Remove_Message_Filter;

   procedure Remove_Message_Filter
     (This  : in out Socket;
      Value : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_UNSUBSCRIBE, To_String (Value));
   end Remove_Message_Filter;

   procedure Remove_Message_Filter
     (This  : in out Socket;
      Value : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_UNSUBSCRIBE, Value);
   end Remove_Message_Filter;

   -----------------------------
   -- Set_Multicast_Data_Rate --
   -----------------------------

   not overriding
   procedure Set_Multicast_Data_Rate
     (This                : in out Socket;
      Kilobits_Per_Second : Natural := 100) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_RATE, Kilobits_Per_Second);
   end Set_Multicast_Data_Rate;

   -------------------------------------
   -- Set_Multicast_Recovery_Interval --
   -------------------------------------

   not overriding
   procedure Set_Multicast_Recovery_Interval (This : in out Socket;
                                              Time : Duration := 10.0) is
   begin
      This.Setsockopt
        (ZMQ.Low_Level.Defs.ZMQ_RECOVERY_IVL, Integer (Time * 1000));
   end Set_Multicast_Recovery_Interval;
   not overriding

     ----------------------------
     -- Set_Multicast_Loopback --
     ----------------------------

   procedure Set_Multicast_Loopback (This   : in out Socket;
                                     Enable : Boolean) is
   begin
      null; -- This.setsockopt (ZMQ.Low_Level.Defs.ZMQ_HWM, Enable);
   end Set_Multicast_Loopback;

   -------------------------------------
   -- Set_Kernel_Transmit_Buffer_Size --
   -------------------------------------

   not overriding
   procedure Set_Kernel_Transmit_Buffer_Size (This  : in out Socket;
                                              Bytes : Natural) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDBUF, Bytes);
   end Set_Kernel_Transmit_Buffer_Size;

   ------------------------------------
   -- Set_Kernel_Receive_Buffer_Size --
   ------------------------------------

   not overriding
   procedure Set_Kernel_Receive_Buffer_Size (This  : in out Socket;
                                             Bytes : Natural) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVBUF, Bytes);
   end Set_Kernel_Receive_Buffer_Size;

   -------------------------------------------
   -- Get_Linger_Period_For_Socket_Shutdown --
   -------------------------------------------

   not overriding
   function Get_Linger_Period_For_Socket_Shutdown
     (This : Socket) return Duration is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_LINGER);
   end Get_Linger_Period_For_Socket_Shutdown;

   -------------------------------------------
   -- Set_Linger_Period_For_Socket_Shutdown --
   -------------------------------------------

   not overriding
   procedure Set_Linger_Period_For_Socket_Shutdown
     (This   : in out Socket;
      Period : Duration := Duration'Last) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_LINGER, Period);
   end Set_Linger_Period_For_Socket_Shutdown;

   -------------------------------
   -- Get_Reconnection_Interval --
   -------------------------------

   not overriding
   function Get_Reconnection_Interval
     (This : Socket) return Duration is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL);
   end Get_Reconnection_Interval;

   -------------------------------
   -- Set_Reconnection_Interval --
   -------------------------------

   not overriding
   procedure Set_Reconnection_Interval
     (This   : in out Socket;
      Period : Duration := 0.100) is
   begin
      This.Setsockopt
        (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL,
         Integer (Duration_To_Msecs (Period)));
   end Set_Reconnection_Interval;

   ---------------------------------------
   -- Get_Maximum_Reconnection_Interval --
   ---------------------------------------

   not overriding
   function Get_Maximum_Reconnection_Interval
     (This : Socket) return Duration
   is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL_MAX);
   end Get_Maximum_Reconnection_Interval;

   ---------------------------------------
   -- Set_Maximum_Reconnection_Interval --
   ---------------------------------------

   not overriding
   procedure Set_Maximum_Reconnection_Interval
     (This   : in out Socket;
      Period : Duration := 0.0) is
   begin
      This.Setsockopt
        (Low_Level.Defs.ZMQ_RECONNECT_IVL_MAX,
         Integer (Duration_To_Msecs (Period)));
   end Set_Maximum_Reconnection_Interval;

   ----------------------------------------------------------------
   -- Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections --
   ----------------------------------------------------------------

   not overriding
   function Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_BACKLOG);
   end Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections;

   ----------------------------------------------------------------
   -- Set_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections --
   ----------------------------------------------------------------

   not overriding
   procedure Set_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections
     (This        : in out Socket;
      Connections : Natural := 100) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_BACKLOG, Connections);
   end Set_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections;

   -------------------------------------------------
   -- Get_Maximum_Acceptable_Inbound_Message_Size --
   -------------------------------------------------

   not overriding
   function Get_Maximum_Acceptable_Inbound_Message_Size
     (This : Socket) return Long_Long_Integer is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_MAXMSGSIZE);
   end Get_Maximum_Acceptable_Inbound_Message_Size;

   -------------------------------------------------
   -- Set_Maximum_Acceptable_Inbound_Message_Size --
   -------------------------------------------------

   not overriding
   procedure Set_Maximum_Acceptable_Inbound_Message_Size
     (This  : in out Socket;
      Bytes : Long_Long_Integer := 0) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_MAXMSGSIZE, Bytes);
   end Set_Maximum_Acceptable_Inbound_Message_Size;

   ----------------------------------------------------
   -- Get_Maximum_Network_Hops_For_Multicast_Packets --
   ----------------------------------------------------

   not overriding
   function Get_Maximum_Network_Hops_For_Multicast_Packets
     (This : Socket) return Positive is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_MULTICAST_HOPS);
   end Get_Maximum_Network_Hops_For_Multicast_Packets;

   ----------------------------------------------------
   -- Set_Maximum_Network_Hops_For_Multicast_Packets --
   ----------------------------------------------------

   not overriding
   procedure Set_Maximum_Network_Hops_For_Multicast_Packets
     (This         : in out Socket;
      Network_Hops : Positive := 1) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_MULTICAST_HOPS, Network_Hops);
   end Set_Maximum_Network_Hops_For_Multicast_Packets;

   -------------------------
   -- Get_Recieve_Timeout --
   -------------------------

   not overriding
   function Get_Recieve_Timeout
     (This : Socket) return Duration is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_RCVTIMEO);
   end Get_Recieve_Timeout;

   -------------------------
   -- Set_Recieve_Timeout --
   -------------------------

   not overriding
   procedure Set_Recieve_Timeout
     (This    : in out Socket;
      Timeout : Duration := Duration'Last) is
   begin
      This.Setsockopt
        (Low_Level.Defs.ZMQ_RCVTIMEO, Integer (Duration_To_Msecs (Timeout)));
   end Set_Recieve_Timeout;

   ----------------------
   -- Get_Send_Timeout --
   ----------------------

   not overriding
   function Get_Send_Timeout
     (This : Socket) return Duration is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_SNDTIMEO);
   end Get_Send_Timeout;

   ----------------------
   -- Set_Send_Timeout --
   ----------------------

   not overriding
   procedure Set_Send_Timeout
     (This    : in out Socket;
      Timeout : Duration := Duration'Last) is
   begin
      This.Setsockopt
        (Low_Level.Defs.ZMQ_SNDTIMEO, Integer (Duration_To_Msecs (Timeout)));
   end Set_Send_Timeout;

   -----------------------
   -- Get_Use_IPv4_Only --
   -----------------------

   not overriding
   function Get_Use_IPv4_Only
     (This : Socket) return Boolean is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_IPV4ONLY);
   end Get_Use_IPv4_Only;

   -----------------------
   -- Set_Use_IPv4_Only --
   -----------------------

   not overriding
   procedure Set_Use_IPv4_Only
     (This : in out Socket; IPv4 : Boolean := True)
   is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_IPV4ONLY, IPv4);
   end Set_Use_IPv4_Only;

   --  ========================================================================
   --  ========================================================================

   --------------
   -- Get_Impl --
   --------------

   function Get_Impl (This : in Socket) return System.Address is
   begin
      return This.C;
   end Get_Impl;

   -------------

   ----------------
   -- Getsockopt --
   ----------------

   not overriding
   procedure Getsockopt (This       : in Socket;
                         Option     : Interfaces.C.int;
                         Value      : System.Address;
                         Value_Size : in out Natural) is
      Ret          : int;
      Value_Size_I : aliased size_t := size_t (Value_Size);
   begin
      Ret :=
        Low_Level.zmq_getsockopt (This.C, Option, Value, Value_Size_I'Access);
      Value_Size := Natural (Value_Size_I);

      if Ret /= 0 then
         ZMQ.Errors.Set_To_Errno;

         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Option'Img & ")";
      end if;
   end Getsockopt;

   not overriding
   function Getsockopt (This   : in Socket;
                        Option : Interfaces.C.int) return unsigned_long is
   begin
      return Get_C_Unsigned_Long (This, Option);
   end Getsockopt;

   function Getsockopt (This   : in Socket;
                        Option : Interfaces.C.int) return String
   is
      Buffer     : aliased String (1 .. MAX_OPTION_SIZE);
      Value_Size : Natural := Buffer'Length;

   begin
      This.Getsockopt (Option, Buffer'Address, Value_Size);
      return Buffer (1 .. Value_Size);
   end Getsockopt;

   not overriding
   function Getsockopt (This   : in Socket;
                        Option : Interfaces.C.int) return Boolean is
   begin
      return Get_C_Int (This, Option) /= 0;
   end Getsockopt;

   not overriding
   function Getsockopt (This   : in Socket;
                        Option : Interfaces.C.int) return Integer is
   begin
      return Integer (Get_C_Int (This, Option));
   end Getsockopt;

   function Getsockopt (This   : in Socket;
                        Option : Interfaces.C.int) return Long_Long_Integer is
   begin
      return Get_Long_Long (This, Option);
   end Getsockopt;

   not overriding
   function Getsockopt (This   : in Socket;
                        Option : Interfaces.C.int)
                        return Ada.Streams.Stream_Element_Array
   is
      Buffer     : aliased Stream_Element_Array (1 .. MAX_OPTION_SIZE);
      Value_Size : Ada.Streams.Stream_Element_Offset := Buffer'Length;

   begin
      This.Getsockopt (Option, Buffer'Address, Natural (Value_Size));
      return Buffer (1 .. Value_Size);
   end Getsockopt;

   not overriding
   function Getsockopt (This   : in Socket;
                        Option : Interfaces.C.int) return Duration is
   begin
      return Msecs_To_Duration (Get_C_Int (This, Option));
   end Getsockopt;

   ----------------------------------
   -- More_Message_Parts_To_Follow --
   ----------------------------------

   function More_Message_Parts_To_Follow (This : Socket) return Boolean is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVMORE);
   end More_Message_Parts_To_Follow;

   -----------------------------------------------
   -- Get_High_Water_Mark_For_Outbound_Messages --
   -----------------------------------------------

   function Get_High_Water_Mark_For_Outbound_Messages
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDHWM);
   end Get_High_Water_Mark_For_Outbound_Messages;

   ----------------------------------------------
   -- Get_High_Water_Mark_For_Inbound_Messages --
   ----------------------------------------------

   function Get_High_Water_Mark_For_Inbound_Messages
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVHWM);
   end Get_High_Water_Mark_For_Inbound_Messages;

   ----------------------------
   -- Get_IO_Thread_Affinity --
   ----------------------------

   function Get_IO_Thread_Affinity (This : Socket) return Thread_Bitmap is
      Value_Size : Natural := Thread_Bitmap'Size / System.Storage_Unit;
   begin
      return Ret : Thread_Bitmap do
         This.Getsockopt
           (ZMQ.Low_Level.Defs.ZMQ_AFFINITY, Ret'Address, Value_Size);
         if Value_Size /= 8 then
            raise Program_Error with "Invalid bitmap size " & Value_Size'Img;
         end if;
      end return;
   end Get_IO_Thread_Affinity;

   -------------------------
   -- Get_Socket_Identity --
   -------------------------

   function Get_Socket_Identity
     (This : Socket) return Ada.Streams.Stream_Element_Array is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_IDENTITY);
   end Get_Socket_Identity;

   -----------------------------
   -- Get_Multicast_Data_Rate --
   -----------------------------

   function Get_Multicast_Data_Rate (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RATE);
   end Get_Multicast_Data_Rate;

   -------------------------------------
   -- Get_Multicast_Recovery_Interval --
   -------------------------------------

   function Get_Multicast_Recovery_Interval (This : Socket) return Duration is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RECOVERY_IVL);
   end Get_Multicast_Recovery_Interval;

   ----------------------------
   -- Get_Multicast_Loopback --
   ----------------------------

   function Get_Multicast_Loopback (This : Socket) return Boolean is
      pragma Unreferenced (This);
   begin
      return False; -- This.getsockopt (ZMQ.Low_Level.Defs.ZMQ_MCAST_LOOP);
   end Get_Multicast_Loopback;

   -------------------------------------
   -- Get_Kernel_Transmit_Buffer_Size --
   -------------------------------------

   function Get_Kernel_Transmit_Buffer_Size (This : Socket) return Integer is
   begin
      return Getsockopt (This, ZMQ.Low_Level.Defs.ZMQ_SNDBUF);
   end Get_Kernel_Transmit_Buffer_Size;

   ------------------------------------
   -- Get_Kernel_Receive_Buffer_Size --
   ------------------------------------

   function Get_Kernel_Receive_Buffer_Size (This : Socket) return Integer is
   begin
      return Getsockopt (This, ZMQ.Low_Level.Defs.ZMQ_RCVBUF);
   end Get_Kernel_Receive_Buffer_Size;

   not overriding
     --------------------------
     -- Retrieve_Socket_Type --
     --------------------------

   function Retrieve_Socket_Type (This : in Socket) return Socket_Type is
   begin
      return Socket_Type'Val (Get_C_Int (This, Low_Level.Defs.ZMQ_TYPE));
   end Retrieve_Socket_Type;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Socket_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      raise Program_Error with "unimplemented function Read";
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Socket_Stream;
      Item   : Ada.Streams.Stream_Element_Array) is
   begin
      raise Program_Error with "unimplemented function Write";
   end Write;

   -----------------
   -- Read_Socket --
   -----------------

   procedure Read_Socket
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      S      : out Socket) is
   begin
      raise Program_Error with "Sockets are not streameble";
   end Read_Socket;

   ------------------
   -- Write_Socket --
   ------------------

   procedure Write_Socket
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      S      : Socket)
   is
   begin
      raise Program_Error with "Sockets are not streameble";
   end Write_Socket;

   ------------
   -- Stream --
   ------------

   function Stream
     (This : Socket) return not null access Ada.Streams.Root_Stream_Type'Class
   is
   begin
      return This.S'Unrestricted_Access;
   end Stream;

end ZMQ.Sockets;
