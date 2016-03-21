-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                           Z M Q . S O C K E T S                           --
--                                                                           --
--                                  B o d y                                  --
--                                                                           --
--            Copyright (C) 2013-2020, per.s.sandberg@bahnhof.se             --
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
   -- Initialize --
   ----------------
   not overriding procedure Initialize
     (This         : in out Socket;
      With_Context : Contexts.Context;
      Kind         : Socket_Type)
   is
   begin
      if With_Context.GetImpl = Null_Address then
         raise ZMQ_Error with "Contecxt Not Initialized";
      end if;
      if This.C /= Null_Address then
         raise ZMQ_Error with "Socket Initialized";
      end if;

      This.C := Low_Level.Zmq_Socket (With_Context.GetImpl,
                                      Socket_Type'Pos (Kind));
      if This.C = Null_Address then
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
      Ret := Low_Level.Zmq_Bind (This.C, Addr);
      Free (Addr);
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;
   end Bind;

   procedure Bind (This    : in out Socket;
                   Address : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Bind (To_String (Address));
   end Bind;


   not overriding procedure Unbind
     (This    : in out Socket;
      Address : String)
   is
      Addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      Ret  : int;
   begin
      Ret := Low_Level.Zmq_Unbind (This.C, Addr);
      Free (Addr);
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;
   end Unbind;

   procedure Unbind (This    : in out Socket;
                     Address : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Unbind (To_String (Address));
   end Unbind;

   procedure  Setsockopt (This       : in out Socket;
                          Option     : Interfaces.C.int;
                          Value      : System.Address;
                          Value_Size : Natural) is
      Ret     : int;
   begin
      Ret := Low_Level.Zmq_Setsockopt
        (This.C,
         Option,
         Value,
         size_t (Value_Size));
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Option'Img & ")";
      end if;
   end Setsockopt;

   ----------------
   -- setsockopt --
   ----------------

   not overriding procedure Setsockopt
     (This         : in out Socket;
      Option       : Interfaces.C.int;
      Value        : String)
   is
   begin
      This.Setsockopt (Option, Value'Address, Value'Length);
   end Setsockopt;

   ----------------
   -- setsockopt --
   ----------------

   not overriding procedure Setsockopt
     (This    : in out Socket;
      Option  : Interfaces.C.int;
      Value   : Boolean)
   is
   begin
      This.Setsockopt (Option, Value'Address, 1);
   end Setsockopt;

   ----------------
   -- setsockopt --
   ----------------

   not overriding procedure Setsockopt
     (This    : in out Socket;
      Option  : Interfaces.C.int;
      Value   : Integer)
   is
   begin
      This.Setsockopt (Option, Value'Address, 4);
   end Setsockopt;

   not overriding procedure Setsockopt
     (This    : in out Socket;
      Option  : Interfaces.C.int;
      Value   : Long_Long_Integer)
   is
   begin
      This.Setsockopt (Option, Value'Address, 8);
   end Setsockopt;

   ----------------
   -- setsockopt --
   ----------------
   not overriding
   procedure Setsockopt
     (This    : in out Socket;
      Option  : Interfaces.C.int;
      Value   : Ada.Streams.Stream_Element_Array)
   is
   begin
      This.Setsockopt (Option, Value (Value'First)'Address, Value'Length);
   end Setsockopt;



   not overriding procedure  Setsockopt
     (This    : in out Socket;
      Option  : Interfaces.C.int;
      Value   : Interfaces.C.unsigned_long) is
   begin
      This.Setsockopt (Option, Value'Address, 8);
   end Setsockopt;

   not overriding procedure  Setsockopt
     (This    : in out Socket;
      Option  : Interfaces.C.int;
      Value   : Duration) is
   begin
      if Value = Duration'Last or Value = -1.0 then
         This.Setsockopt (Option, Integer'(-1));
      else
         This.Setsockopt (Option, Integer (Value * 1000.0));
      end if;
   end Setsockopt;

   not overriding

     -------------
     -- Connect --
     -------------

   procedure Connect
     (This    : in out Socket;
      Address : String)
   is
      Addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      Ret  : int;
   begin
      Ret := Low_Level.Zmq_Connect (This.C, Addr);
      Free (Addr);
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;
   end Connect;


   procedure Connect (This    : in out Socket;
                      Address : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Connect (To_String (Address));
   end Connect;


   ----------
   -- Send --
   ----------

   not overriding procedure Send
     (This    : in out Socket;
      Msg     : Messages.Message'Class;
      Flags   : Socket_Flags := No_Flags)
   is
      Ret  : int;
   begin
      Ret := Low_Level.Zmq_Msg_Send (Msg.GetImpl, This.C, int (Flags));
      if Ret = -1 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Send;

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : String;
                   Flags   : Socket_Flags := No_Flags) is
   begin
      This.Send (Msg'Address, Msg'Length, Flags);
   end Send;

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Ada.Streams.Stream_Element_Array;
                   Flags   : Socket_Flags := No_Flags) is
   begin
      This.Send (Msg'Address, Msg'Length, Flags);
   end Send;

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags) is
      M : Messages.Message;
   begin
      M.Initialize (Ada.Strings.Unbounded.To_String (Msg));
      This.Send (M, Flags);
   end Send;

   procedure Send_Generic (This    : in out Socket;
                           Msg     : Element;
                           Flags   : Socket_Flags := No_Flags) is
   begin
      This.Send
        (Msg'Address,
         (Msg'Size + Ada.Streams.Stream_Element'Size - 1) /
             Ada.Streams.Stream_Element'Size,
         Flags);
   end Send_Generic;

   not overriding


   procedure Send (This         : in out Socket;
                   Msg_Address  : System.Address;
                   Msg_Length   : Natural;
                   Flags        : Socket_Flags := No_Flags) is
      Ret  : int;
   begin
      Ret := Low_Level.Zmq_Send
        (This.C, Msg_Address, Interfaces.C.size_t (Msg_Length), int (Flags));
      if Ret = -1 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Send;


   --     -----------
   --     -- flush --
   --     -----------
   --
   --     not overriding procedure flush
   --       (This    : in out Socket)
   --     is
   --        ret  : int;
   --     begin
   --        ret := Low_Level.zmq_flush (This.c);
   --        if ret /= 0 then
   --           raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in "
   --             & GNAT.Source_Info.Enclosing_Entity;
   --        end if;
   --     end flush;

   ----------
   -- recv --
   ----------

   not overriding procedure Recv
     (This    : in Socket;
      Msg     : in out Messages.Message'Class;
      Flags   : Socket_Flags := No_Flags)
   is
      Ret  : int;
   begin
      Ret := Low_Level.Zmq_Msg_Recv (Msg.GetImpl,
                                     This.C,
                                     int (Flags));

      if Ret = -1 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in "
           & GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Recv;

   procedure Recv (This    : in Socket;
                   Flags   : Socket_Flags := No_Flags) is
      Dummy_Msg : Messages.Message;
   begin
      Dummy_Msg.Initialize (0);
      This.Recv (Dummy_Msg, Flags);
   end Recv;

   not overriding

   function Recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags) return String is
      Msg     : Messages.Message;
   begin
      Msg.Initialize (0);
      This.Recv (Msg, Flags);
      return Ret : String (1 .. Msg.GetSize) do
         Ret := Msg.GetData;
      end return;
   end Recv;

   procedure Recv (This    : in Socket;
                   Msg     : out Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags) is
   begin
      Msg := Ada.Strings.Unbounded.To_Unbounded_String (This.Recv (Flags));
   end Recv;

   function Recv
     (This       : in Socket;
      Max_Length : Natural;
      Flags      : Socket_Flags := No_Flags)
      return  String is
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

   not overriding
   function Recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags)
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
         Ret := Low_Level.Zmq_Close (This.C);
         if Ret /= 0 then
            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
         end if;
         This.C := Null_Address;
      end if;
   end Finalize;

   procedure Proxy (Frontend  : not null access Socket;
                    Backend   : not null access Socket'Class;
                    Capture   : access Socket'Class) is
      Ret : int;
   begin
      Ret := Low_Level.Zmq_Proxy
        (Frontend.C,
         Backend.C,
         (if Capture /= null then Capture.C else System.Null_Address));
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
      end if;
   end Proxy;




   not overriding
   procedure  Set_High_Water_Mark_For_Outbound_Messages
     (This     : in out Socket;
      Messages : Natural := 1_000)
   is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDHWM, Messages);
   end Set_High_Water_Mark_For_Outbound_Messages;


   not overriding
   procedure  Set_High_Water_Mark_For_Inbound_Messages
     (This     : in out Socket;
      Messages : Natural := 1_000)
   is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVHWM, Messages);
   end Set_High_Water_Mark_For_Inbound_Messages;

   not overriding
   procedure  Set_Disk_Offload_Size (This       : in out Socket;
                                     Value      : Natural) is
   begin
      null; -- This.setsockopt (ZMQ.Low_Level.Defs.SWAP, Value);
   end Set_Disk_Offload_Size;

   not overriding
   procedure  Set_IO_Thread_Affinity (This    : in out Socket;
                                      Threads : Thread_Bitmap) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_AFFINITY, Threads'Address, 4);
   end Set_IO_Thread_Affinity;

   not overriding
   procedure  Set_Socket_Identity
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_IDENTITY, Value);
   end Set_Socket_Identity;

   procedure  Set_Socket_Identity
     (This       : in out Socket;
      Value      : String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_IDENTITY, Value);
   end Set_Socket_Identity;


   not overriding
   procedure  Establish_Message_Filter (This       : in out Socket;
                                        Value      : String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SUBSCRIBE, Value);
   end Establish_Message_Filter;

   not overriding

   procedure  Establish_Message_Filter
     (This       : in out Socket;
      Value      :  Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SUBSCRIBE, Value);
   end Establish_Message_Filter;

   procedure  Establish_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SUBSCRIBE, To_String (Value));
   end Establish_Message_Filter;

   not overriding

   procedure  Remove_Message_Filter (This       : in out Socket;
                                     Value      : String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_UNSUBSCRIBE, Value);
   end Remove_Message_Filter;

   procedure  Remove_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_UNSUBSCRIBE, To_String (Value));
   end Remove_Message_Filter;

   procedure  Remove_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_UNSUBSCRIBE, Value);
   end Remove_Message_Filter;

   not overriding
   procedure  Set_Multicast_Data_Rate
     (This                     : in out Socket;
      Kilobits_Per_Second      : Natural := 100) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_RATE, Kilobits_Per_Second);
   end Set_Multicast_Data_Rate;

   not overriding
   procedure  Set_Multicast_Recovery_Interval (This : in out Socket;
                                               Time : Duration := 10.0) is
   begin
      This.Setsockopt
        (ZMQ.Low_Level.Defs.ZMQ_RECOVERY_IVL, Integer (Time * 1000));
   end Set_Multicast_Recovery_Interval;
   not overriding

   procedure  Set_Multicast_Loopback (This        : in out Socket;
                                      Enable      : Boolean) is
   begin
      null; -- This.setsockopt (ZMQ.Low_Level.Defs.ZMQ_HWM, Enable);
   end Set_Multicast_Loopback;
   not overriding
   procedure  Set_Kernel_Transmit_Buffer_Size (This  : in out Socket;
                                               Bytes : Natural) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDBUF, Bytes);
   end Set_Kernel_Transmit_Buffer_Size;
   not overriding

   procedure  Set_Kernel_Receive_Buffer_Size (This  : in out Socket;
                                              Bytes : Natural) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVBUF, Bytes);
   end Set_Kernel_Receive_Buffer_Size;


   not overriding
   function Get_Linger_Period_For_Socket_Shutdown
     (This : Socket) return Duration is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_LINGER);
   end Get_Linger_Period_For_Socket_Shutdown;

   not overriding
   procedure  Set_Linger_Period_For_Socket_Shutdown
     (This   : in out Socket;
      Period : Duration := Duration'Last) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_LINGER, Period);
   end Set_Linger_Period_For_Socket_Shutdown;


   not overriding
   function Get_Reconnection_Interval
     (This : Socket) return Duration is
   begin
      return Duration (Natural'(This.Getsockopt
                       (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL))) / 1000.0;
   end Get_Reconnection_Interval;

   not overriding
   procedure  Set_Reconnection_Interval
     (This   : in out Socket;
      Period : Duration := 0.100) is
   begin
      if Period < 0.0  then
         This.Setsockopt
           (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL, Integer'(-1));
      else
         This.Setsockopt
           (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL, Natural (Period * 1000.0));
      end if;

   end Set_Reconnection_Interval;


   not overriding
   function Get_Maximum_Reconnection_Interval
     (This : Socket) return Duration is
   begin
      return Duration (Natural'(This.Getsockopt
                       (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL))) / 1000.0;
   end Get_Maximum_Reconnection_Interval;

   not overriding
   procedure  Set_Maximum_Reconnection_Interval
     (This   : in out Socket;
      Period : Duration := 0.0) is
   begin
      if Period < 0.0  then
         This.Setsockopt
           (Low_Level.Defs.ZMQ_RECONNECT_IVL, Integer'(-1));
      else
         This.Setsockopt
           (Low_Level.Defs.ZMQ_RECONNECT_IVL, Natural (Period * 1000.0));
      end if;

   end Set_Maximum_Reconnection_Interval;



   not overriding
   function Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_BACKLOG);
   end Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections;

   not overriding
   procedure Set_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections
     (This        : in out Socket;
      Connections : Natural  := 100) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_BACKLOG, Connections);
   end Set_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections;


   not overriding
   function Get_Maximum_Acceptable_Inbound_Message_Size
     (This : Socket) return Long_Long_Integer  is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_MAXMSGSIZE);
   end Get_Maximum_Acceptable_Inbound_Message_Size;

   not overriding
   procedure Set_Maximum_Acceptable_Inbound_Message_Size
     (This   : in out Socket;
      Bytes  : Long_Long_Integer  := 0) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_MAXMSGSIZE, Bytes);
   end Set_Maximum_Acceptable_Inbound_Message_Size;


   not overriding
   function Get_Maximum_Network_Hops_For_Multicast_Packets
     (This : Socket) return Positive is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_MULTICAST_HOPS);
   end Get_Maximum_Network_Hops_For_Multicast_Packets;

   not overriding
   procedure Set_Maximum_Network_Hops_For_Multicast_Packets
     (This         : in out Socket;
      Network_Hops : Positive  := 1) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_MULTICAST_HOPS, Network_Hops);
   end Set_Maximum_Network_Hops_For_Multicast_Packets;


   not overriding
   function Get_Recieve_Timeout
     (This : Socket) return Duration is
   begin
      return Duration (Integer'(This.Getsockopt
                       (Low_Level.Defs.ZMQ_RCVTIMEO)) * 1000.0);
   end Get_Recieve_Timeout;
   not overriding
   procedure Set_Recieve_Timeout
     (This    : in out Socket;
      Timeout : Duration := Duration'Last) is
   begin
      if Timeout = Duration'Last then
         This.Setsockopt (Low_Level.Defs.ZMQ_RCVTIMEO, Integer (-1));
      else
         This.Setsockopt
           (Low_Level.Defs.ZMQ_RCVTIMEO, Integer (Timeout * 1000.0));
      end if;
   end Set_Recieve_Timeout;


   not overriding
   function Get_Send_Timeout
     (This : Socket) return Duration is
   begin
      return Duration (Integer'(This.Getsockopt
                       (Low_Level.Defs.ZMQ_SNDTIMEO)) * 1000.0);
   end Get_Send_Timeout;
   not overriding
   procedure Set_Send_Timeout
     (This    : in out Socket;
      Timeout : Duration := Duration'Last) is
   begin
      if Timeout = Duration'Last then
         This.Setsockopt (Low_Level.Defs.ZMQ_SNDTIMEO, Integer (-1));
      else
         This.Setsockopt
           (Low_Level.Defs.ZMQ_RCVTIMEO, Integer (Timeout * 1000.0));
      end if;
   end Set_Send_Timeout;

   not overriding
   function Get_Use_IPv4_Only
     (This : Socket) return Boolean is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_IPV4ONLY);
   end Get_Use_IPv4_Only;
   not overriding
   procedure Set_Use_IPv4_Only
     (This   : in out Socket;
      IPv4   : Boolean :=  True) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_IPV4ONLY, IPv4);
   end Set_Use_IPv4_Only;

   --  ========================================================================
   --  ========================================================================

   function Get_Impl (This : in Socket) return System.Address is
   begin
      return This.C;
   end Get_Impl;

   -------------

   not overriding
   procedure  Getsockopt (This       : in Socket;
                          Option     : Interfaces.C.int;
                          Value      : System.Address;
                          Value_Size : in out Natural) is
      Ret          : int;
      Value_Size_I : aliased size_t;
   begin
      Ret := Low_Level.Zmq_Getsockopt
        (This.C,
         Option,
         Value,
         Value_Size_I'Access);
      Value_Size := Natural (Value_Size_I);
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Option'Img & ")";
      end if;
   end Getsockopt;

   not overriding
   function  Getsockopt (This    : in Socket;
                         Option  : Interfaces.C.int) return unsigned_long is
      Dummy_Value_Size : Natural := unsigned_long'Size / System.Storage_Unit;
   begin
      return Ret : unsigned_long do
         This.Getsockopt (Option, Ret'Address, Dummy_Value_Size);
         if Dummy_Value_Size /= 8 then
            raise Program_Error with "Invalid getsockopt for this type";
         end if;
      end return;
   end Getsockopt;



   function  Getsockopt (This    : in Socket;
                         Option  : Interfaces.C.int) return String is
      Buffer     : aliased String (1 .. MAX_OPTION_SIZE);
      Value_Size : Natural := Buffer'Length;

   begin
      This.Getsockopt (Option, Buffer'Address, Value_Size);
      return Buffer (1 .. Value_Size);
   end Getsockopt;

   not overriding
   function  Getsockopt (This    : in Socket;
                         Option  : Interfaces.C.int) return Boolean is
   begin
      return Ret : Boolean do
         Ret := unsigned_long'(This.Getsockopt (Option)) /= 0;
      end return;
   end Getsockopt;

   not overriding
   function  Getsockopt (This    : in Socket;
                         Option  : Interfaces.C.int) return Integer is
   begin
      return Ret : Integer do
         Ret := Integer (unsigned_long'(This.Getsockopt (Option)));
      end return;
   end Getsockopt;

   function  Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return Long_Long_Integer is
   begin
      return Ret : Long_Long_Integer do
         Ret := Long_Long_Integer (unsigned_long'(This.Getsockopt (Option)));
      end return;
   end Getsockopt;

   not overriding
   function Getsockopt
     (This    : in  Socket;
      Option  : Interfaces.C.int) return  Ada.Streams.Stream_Element_Array is
      Buffer     : aliased Stream_Element_Array (1 .. MAX_OPTION_SIZE);
      Value_Size : Ada.Streams.Stream_Element_Offset := Buffer'Length;

   begin
      This.Getsockopt (Option, Buffer'Address, Natural (Value_Size));
      return Buffer (1 .. Value_Size);
   end Getsockopt;


   not overriding
   function  Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return Duration is
   begin
      return Duration (Integer'(This.Getsockopt (Option)) * 1000.0);
   end Getsockopt;

   function More_Message_Parts_To_Follow (This : Socket) return Boolean is
   begin
      return Ret : Boolean do
         Ret := This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVMORE);
      end return;
   end More_Message_Parts_To_Follow;

   function Get_High_Water_Mark_For_Outbound_Messages
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDHWM);
   end Get_High_Water_Mark_For_Outbound_Messages;

   function Get_High_Water_Mark_For_Inbound_Messages
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVHWM);
   end Get_High_Water_Mark_For_Inbound_Messages;


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

   function Get_Socket_Identity
     (This : Socket) return Ada.Streams.Stream_Element_Array  is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_IDENTITY);
   end Get_Socket_Identity;

   function Get_Multicast_Data_Rate (This : Socket) return Natural  is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RATE);
   end Get_Multicast_Data_Rate;

   function Get_Multicast_Recovery_Interval (This : Socket) return Duration is
   begin
      return Duration
        (unsigned_long'(
         This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RECOVERY_IVL)));
   end Get_Multicast_Recovery_Interval;

   function Get_Multicast_Loopback (This : Socket) return Boolean is
      pragma Unreferenced (This);
   begin
      return False; -- This.getsockopt (ZMQ.Low_Level.Defs.ZMQ_MCAST_LOOP);
   end Get_Multicast_Loopback;

   function Get_Kernel_Transmit_Buffer_Size (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDBUF);
   end Get_Kernel_Transmit_Buffer_Size;

   function Get_Kernel_Receive_Buffer_Size (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVBUF);
   end Get_Kernel_Receive_Buffer_Size;

   not overriding function Retrieve_Socket_Type
     (This : in Socket)
      return Socket_Type is
   begin
      return Socket_Type'Val
        (Natural'(This.Getsockopt (Low_Level.Defs.ZMQ_TYPE)));
   end Retrieve_Socket_Type;


   procedure Read
     (Stream : in out Socket_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      raise Program_Error with "unimplemented function Read";
   end Read;

   procedure Write
     (Stream : in out Socket_Stream;
      Item   : Ada.Streams.Stream_Element_Array) is
   begin
      raise Program_Error with "unimplemented function Write";
   end Write;

   procedure Read_Socket
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      S      : out Socket) is
   begin
      raise Program_Error with "Sockets are not streameble";
   end Read_Socket;
   procedure Write_Socket
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      S      : Socket) is
   begin
      raise Program_Error with "Sockets are not streameble";
   end Write_Socket;

   function Stream
     (This : Socket)
      return not null access Ada.Streams.Root_Stream_Type'Class is
   begin
      return This.S'Unrestricted_Access;
   end Stream;


end ZMQ.Sockets;
