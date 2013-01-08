-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                           Z M Q . S O C K E T S                           --
--                                                                           --
--                                  B o d y                                  --
--                                                                           --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net             --
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
with GNAT.Source_Info;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Address_To_Access_Conversions;
package body ZMQ.Sockets is
   use Interfaces.C.Strings;
   use Interfaces.C;
   use System;
   use Ada.Streams;
   type Map_Array is  array (Socket_Opt) of int;
   Map :  constant Map_Array  :=
           (ZMQ_TYPE                  => Low_Level.ZMQ_TYPE,
            AFFINITY                  => Low_Level.ZMQ_AFFINITY,
            IDENTITY                  => Low_Level.ZMQ_IDENTITY,
            SUBSCRIBE                 => Low_Level.ZMQ_SUBSCRIBE,
            UNSUBSCRIBE               => Low_Level.ZMQ_UNSUBSCRIBE,
            RATE                      => Low_Level.ZMQ_RATE,
            RECOVERY_IVL              => Low_Level.ZMQ_RECOVERY_IVL,
            SNDBUF                    => Low_Level.ZMQ_SNDBUF,
            RCVBUF                    => Low_Level.ZMQ_RCVBUF,
            RCVMORE                   => Low_Level.ZMQ_RCVMORE,
            FD                        => Low_Level.ZMQ_FD,
            EVENTS                    => Low_Level.ZMQ_EVENTS,
            GET_TYPE                  => Low_Level.ZMQ_TYPE,
            LINGER                    => Low_Level.ZMQ_LINGER,
            RECONNECT_IVL             => Low_Level.ZMQ_RECONNECT_IVL,
            BACKLOG                   => Low_Level.ZMQ_BACKLOG,
            RECONNECT_IVL_MAX         => Low_Level.ZMQ_RECONNECT_IVL_MAX,
            MAXMSGSIZE                => Low_Level.ZMQ_MAXMSGSIZE,
            SNDHWM                    => Low_Level.ZMQ_SNDHWM,
            RCVHWM                    => Low_Level.ZMQ_RCVHWM,
            MULTICAST_HOPS            => Low_Level.ZMQ_MULTICAST_HOPS,
            RCVTIMEO                  => Low_Level.ZMQ_RCVTIMEO,
            SNDTIMEO                  => Low_Level.ZMQ_SNDTIMEO,
            IPV4ONLY                  => Low_Level.ZMQ_IPV4ONLY,
            LAST_ENDPOINT             => Low_Level.ZMQ_LAST_ENDPOINT,
            ROUTER_MANDATORY          => Low_Level.ZMQ_ROUTER_MANDATORY,
            TCP_KEEPALIVE             => Low_Level.ZMQ_TCP_KEEPALIVE,
            TCP_KEEPALIVE_CNT         => Low_Level.ZMQ_TCP_KEEPALIVE_CNT,
            TCP_KEEPALIVE_IDLE        => Low_Level.ZMQ_TCP_KEEPALIVE_IDLE,
            TCP_KEEPALIVE_INTVL       => Low_Level.ZMQ_TCP_KEEPALIVE_INTVL,
            TCP_ACCEPT_FILTER         => Low_Level.ZMQ_TCP_ACCEPT_FILTER,
            DELAY_ATTACH_ON_CONNECT   => Low_Level.ZMQ_DELAY_ATTACH_ON_CONNECT,
            XPUB_VERBOSE              => Low_Level.ZMQ_XPUB_VERBOSE
           );



   ----------------
   -- Initialize --
   ----------------
   not overriding procedure Initialize
     (This         : in out Socket;
      With_Context : Contexts.Context'Class;
      Kind         : Socket_Type)
   is
   begin

      This.C := Low_Level.zmq_socket (With_Context.GetImpl,
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
      Ret := Low_Level.zmq_bind (This.C, Addr);
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


   not overriding
   procedure UnBind
     (This    : in out Socket;
      Address : String) is
      Addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      Ret  : int;
   begin
      Ret := Low_Level.zmq_unbind (This.C, Addr);
      Free (Addr);
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;
   end UnBind;

   not overriding
   procedure UnBind
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.UnBind (To_String (Address));
   end UnBind;

   procedure  Setsockopt (This       : in out Socket;
                          Option     : Socket_Opt;
                          Value      : System.Address;
                          Value_Size : Natural) is
      Ret     : int;
   begin
      Ret := Low_Level.zmq_setsockopt
        (This.C,
         Map (Option),
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
      Option       : Socket_Opt;
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
      Option  : Socket_Opt;
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
      Option  : Socket_Opt;
      Value   : Natural)
   is
   begin
      This.Setsockopt (Option, Value'Address, 4);
   end Setsockopt;

   ----------------
   -- setsockopt --
   ----------------
   not overriding procedure Setsockopt
     (This    : in out Socket;
      Option  : Socket_Opt;
      Value   : Ada.Streams.Stream_Element_Array)
   is
   begin
      This.Setsockopt (Option, Value (Value'First)'Address, Value'Length);
   end Setsockopt;

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
      Ret := Low_Level.zmq_connect (This.C, Addr);
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

   not overriding
   procedure DisConnect
     (This    : in out Socket;
      Address : String) is
      Addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      Ret  : int;
   begin
      Ret := Low_Level.zmq_disconnect (This.C, Addr);
      Free (Addr);
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Address & ")";
      end if;
   end DisConnect;


   not overriding
   procedure DisConnect
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.DisConnect (To_String (Address));
   end DisConnect;

   ----------
   -- Send --
   ----------


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
   begin
      This.Send (Ada.Strings.Unbounded.To_String (Msg), Flags);
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


   procedure Send (This            : in out Socket;
                   Msg_Address     : System.Address;
                   Msg_Length      : Natural;
                   Flags           : Socket_Flags := No_Flags) is
      Ret  : int;
   begin
      Ret := Low_Level.zmq_send (This.C, Msg_Address,
                                 size_t (Msg_Length),
                                 int (Flags));
      if Ret = -1 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Send;

   not overriding procedure Send
     (This       : in out Socket;
      Msg        : ZMQ.Messages.Message'Class;
      Flags      : Socket_Flags := No_Flags) is
      Ret  : int;
   begin
      Ret := Low_Level.zmq_msg_send (Msg.GetImpl, This.C,
                                     int (Flags));
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
      Ret := Low_Level.zmq_msg_recv (Msg.GetImpl,
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
      This.Recv (Dummy_Msg, Flags);
   end Recv;


   not overriding
   function Recv (This       : in Socket;
                  Max_Length : Natural := 1024;
                  Flags      : Socket_Flags := No_Flags) return String is
   --  Buffer : String (1 .. Max_Length);
   begin
      --  This.Recv (Buffer'Address, Buffer'Length, Flags);
      raise Program_Error with "function Recv not implemented";
      return "dummy";
   end Recv;

   procedure Recv (This    : in Socket;
                   Msg     : out Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags) is
      Temp_Msg : Messages.Message;
   begin
      This.Recv (Temp_Msg, Flags);
      declare
         type Msg_Str is new String (1 .. Temp_Msg.GetSize);
         package Conv is new System.Address_To_Access_Conversions (Msg_Str);
      begin
         Set_Unbounded_String
           (Msg, String (Conv.To_Pointer (Temp_Msg.GetData).all));
      end;
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
         Ret := Low_Level.zmq_close (This.C);
         This.C := Null_Address;
         if Ret /= 0 then
            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
         end if;
      end if;
   end Finalize;


   not overriding
   procedure Set_High_Water_Mark_For_Outbound_Messages
     (This     : in out Socket;
      Messages : Positive := 1000) is
   begin
      This.Setsockopt (Option => SNDHWM,
                       Value  => Messages);

   end Set_High_Water_Mark_For_Outbound_Messages;

   not overriding
   procedure Set_High_Water_Mark_For_Inbound_Messages
     (This     : in out Socket;
      Messages : Positive := 1000) is
   begin
      This.Setsockopt (Option => RCVHWM,
                       Value  => Messages);
   end Set_High_Water_Mark_For_Inbound_Messages;


   not overriding
   procedure  Set_IO_Thread_Affinity (This  : in out Socket;
                                      Value : Thread_Bitmap) is
   begin
      This.Setsockopt (AFFINITY, Value'Address, (Thread_Bitmap'Size + 1) / 8);
   end Set_IO_Thread_Affinity;

   not overriding
   procedure  Set_Socket_Identity
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (IDENTITY, Value);
   end Set_Socket_Identity;

   procedure  Set_Socket_Identity
     (This       : in out Socket;
      Value      : String) is
   begin
      This.Setsockopt (IDENTITY, Value);
   end Set_Socket_Identity;

   not overriding
   procedure Set_Socket_Identity
     (This  : in out Socket;
      Value : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Set_Socket_Identity (Ada.Strings.Unbounded.To_String (Value));
   end Set_Socket_Identity;

   not overriding
   procedure  Set_Message_Filter (This       : in out Socket;
                                        Value      : String) is
   begin
      This.Setsockopt (SUBSCRIBE, Value);
   end Set_Message_Filter;

   not overriding

   procedure  Set_Message_Filter
     (This       : in out Socket;
      Value      :  Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (SUBSCRIBE, Value);
   end Set_Message_Filter;

   procedure  Set_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (SUBSCRIBE, To_String (Value));
   end Set_Message_Filter;

   not overriding

   procedure  Remove_Message_Filter (This       : in out Socket;
                                     Value      : String) is
   begin
      This.Setsockopt (UNSUBSCRIBE, Value);
   end Remove_Message_Filter;

   procedure  Remove_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (UNSUBSCRIBE, To_String (Value));
   end Remove_Message_Filter;

   procedure  Remove_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (UNSUBSCRIBE, Value);
   end Remove_Message_Filter;

   not overriding
   procedure  Set_Multicast_Data_Rate
     (This                : in out Socket;
      Kilobits_Per_Second : Natural := 100) is
   begin
      This.Setsockopt (RATE, Kilobits_Per_Second);
   end Set_Multicast_Data_Rate;

   not overriding
   procedure  Set_Multicast_Recovery_Interval (This     : in out Socket;
                                               Interval : Duration) is
   begin
      This.Setsockopt (RECOVERY_IVL, Integer (Interval));
   end Set_Multicast_Recovery_Interval;
   not overriding

   procedure  Set_Kernel_Transmit_Buffer_Size (This  : in out Socket;
                                               Size  : Natural) is
   begin
      This.Setsockopt (SNDBUF, Size);
   end Set_Kernel_Transmit_Buffer_Size;
   not overriding

   procedure  Set_Kernel_Receive_Buffer_Size (This  : in out Socket;
                                              Size  : Natural) is
   begin
      This.Setsockopt (RCVBUF, Size);
   end Set_Kernel_Receive_Buffer_Size;
   --<<
   not overriding
   procedure Set_Linger_Period_For_Socket_Shutdown
     (This   : in out Socket;
      Period : Duration) is
   begin
      This.Set_Linger_Period_For_Socket_Shutdown (Integer (Period * 1000.0));
   end Set_Linger_Period_For_Socket_Shutdown;
   not overriding
   procedure Set_Linger_Period_For_Socket_Shutdown
     (This   : in out Socket;
      Period : Ada.Real_Time.Time_Span) is
   begin
      This.Set_Linger_Period_For_Socket_Shutdown
        (Ada.Real_Time.To_Duration (Period));
   end Set_Linger_Period_For_Socket_Shutdown;
   not overriding

   procedure Set_Linger_Period_For_Socket_Shutdown
     (This        : in out Socket;
      Miliseconds : Integer) is
   begin
      This.Setsockopt (Option => LINGER,
                       Value  => Integer'Max (-1, Miliseconds));
   end Set_Linger_Period_For_Socket_Shutdown;


   procedure Set_Reconnection_Interval
     (This     : in out Socket;
      Interval : Duration) is
   begin
      This.Set_Reconnection_Interval (Integer (Interval * 1000.0));
   end Set_Reconnection_Interval;

   not overriding
   procedure Set_Reconnection_Interval
     (This     : in out Socket;
      Interval : Ada.Real_Time.Time_Span) is
   begin
      This.Set_Reconnection_Interval
        (Ada.Real_Time.To_Duration (Interval));
   end Set_Reconnection_Interval;
   not overriding

   procedure Set_Reconnection_Interval
     (This        : in out Socket;
      Miliseconds : Integer) is
   begin
      This.Setsockopt (Option => RECONNECT_IVL,
                       Value  => Integer'Max (-1, Miliseconds));
   end Set_Reconnection_Interval;

   procedure Set_Maximum_Reconnection_Interval
     (This     : in out Socket;
      Interval : Duration := 0.0) is
   begin
      This.Set_Maximum_Reconnection_Interval (Integer (Interval * 1000.0));
   end Set_Maximum_Reconnection_Interval;

   not overriding
   procedure Set_Maximum_Reconnection_Interval
     (This     : in out Socket;
      Interval : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero) is
   begin
      This.Set_Maximum_Reconnection_Interval
        (Ada.Real_Time.To_Duration (Interval));
   end Set_Maximum_Reconnection_Interval;

   not overriding
   procedure Set_Maximum_Reconnection_Interval
     (This        : in out Socket;
      Miliseconds : Integer := 0) is
   begin
      This.Setsockopt (Option => RECONNECT_IVL_MAX,
                       Value  => Integer'Max (-1, Miliseconds));
   end Set_Maximum_Reconnection_Interval;

   procedure Set_Maximum_Queue_Length_Of_Outstanding_Connections
     (This        : in out Socket;
      Connections : Positive) is
   begin
      This.Setsockopt (Option => BACKLOG,
                       Value  => Connections);
   end Set_Maximum_Queue_Length_Of_Outstanding_Connections;

   procedure Set_Maximum_Acceptable_Inbound_Message_Size
     (This  : in out Socket;
      Size  : Integer) is
   begin
      This.Setsockopt (Option => MAXMSGSIZE,
                       Value  => Integer'Max (-1, Size));
   end Set_Maximum_Acceptable_Inbound_Message_Size;

   procedure Set_Maximum_Network_Hops_For_Multicast_Packets
     (This      : in out Socket;
      Max_Hops  : Positive := 1)
   is
   begin
      This.Setsockopt (Option => MULTICAST_HOPS,
                       Value  => Max_Hops);
   end Set_Maximum_Network_Hops_For_Multicast_Packets;


   procedure Set_Recieve_Time_Out
     (This      : in out Socket;
      Time      : Duration) is
   begin
      This.Set_Recieve_Time_Out (Integer (Time * 1000.0));
   end Set_Recieve_Time_Out;
   procedure Set_Recieve_Time_Out
     (This      : in out Socket;
      Time      : Ada.Real_Time.Time_Span) is
   begin
      This.Set_Recieve_Time_Out (Ada.Real_Time.To_Duration (Time));
   end Set_Recieve_Time_Out;
   procedure Set_Recieve_Time_Out
     (This         : in out Socket;
      Milliseconds : Integer) is
   begin
      This.Setsockopt (Option => RCVTIMEO,
                       Value  => Integer'Max (-1, Milliseconds));
   end Set_Recieve_Time_Out;

   procedure Set_Send_Time_Out
     (This      : in out Socket;
      Time      : Duration) is
   begin
      This.Set_Send_Time_Out (Integer (Time * 1000.0));
   end Set_Send_Time_Out;
   procedure Set_Send_Time_Out
     (This      : in out Socket;
      Time      : Ada.Real_Time.Time_Span) is
   begin
      This.Set_Send_Time_Out (Ada.Real_Time.To_Duration (Time));
   end Set_Send_Time_Out;
   procedure Set_Send_Time_Out
     (This         : in out Socket;
      Milliseconds : Integer) is
   begin
      This.Setsockopt (Option => SNDTIMEO,
                       Value  => Integer'Max (-1, Milliseconds));
   end Set_Send_Time_Out;

   not overriding
   procedure Use_IPv4_Sockets_Only
     (This         : in out Socket;
      Value        : Boolean) is
   begin
      This.Setsockopt (Option => IPV4ONLY,
                       Value  => Value);
   end Use_IPv4_Sockets_Only;

   not overriding
   procedure Accept_Messages_Only_When_Connections_Are_Made
     (This         : in out Socket;
      Value        : Boolean) is
   begin
      This.Setsockopt (Option => DELAY_ATTACH_ON_CONNECT,
                       Value  => Value);
   end Accept_Messages_Only_When_Connections_Are_Made;

   procedure Set_Accept_Only_Routable_Messages_On_ROUTER_Sockets
     (This         : in out Socket;
      Value        : Boolean) is
   begin
      This.Setsockopt (Option => ROUTER_MANDATORY,
                       Value  => Value);
   end Set_Accept_Only_Routable_Messages_On_ROUTER_Sockets;

   not overriding
   procedure Provide_All_Subscription_Messages_On_XPUB_Sockets
     (This         : in out Socket;
      Value        : Boolean) is
   begin
      This.Setsockopt (Option => XPUB_VERBOSE,
                       Value  => Value);
   end Provide_All_Subscription_Messages_On_XPUB_Sockets;

   not overriding
   procedure Override_SO_KEEPALIVE_Socket_Option
     (This  : in out Socket;
      Value : SO_KEEPALIVE_Type) is
      Map : constant array (SO_KEEPALIVE_Type) of Integer := (-1, 0, 1);
   begin
      This.Setsockopt (Option => TCP_KEEPALIVE,
                       Value  => Map (Value));

   end Override_SO_KEEPALIVE_Socket_Option;

   not overriding
   procedure Override_TCP_KEEPCNT_IDLE_Socket_Option
     (This  : in out Socket;
      Value : Integer := -1) is
   begin
      if Value < -1 or Value = 0 then
         raise Constraint_Error with "Value out of bounds:" & Value'Img;
      end if;
      This.Setsockopt (Option => TCP_KEEPALIVE_IDLE,
                       Value  => Value);
   end Override_TCP_KEEPCNT_IDLE_Socket_Option;
   not overriding

   procedure Override_TCP_KEEPCNT_CNT_Socket_Option
     (This  : in out Socket;
      Value : Integer := -1) is
   begin
      if Value < -1 or Value = 0 then
         raise Constraint_Error with "Value out of bounds:" & Value'Img;
      end if;
      This.Setsockopt (Option => TCP_KEEPALIVE_CNT,
                       Value  => Value);
   end Override_TCP_KEEPCNT_CNT_Socket_Option;


   not overriding
   procedure Override_TCP_KEEPINTVL_socket_option
     (This  : in out Socket;
      Value : Integer := -1) is
   begin
      if Value < -1 or Value = 0 then
         raise Constraint_Error with "Value out of bounds:" & Value'Img;
      end if;
      This.Setsockopt (Option => TCP_KEEPALIVE_INTVL,
                       Value  => Value);

   end Override_TCP_KEEPINTVL_socket_option;
   not overriding
   procedure  Assign_Filters_To_Allow_New_TCP_Connections
     (This   : in out Socket;
      Filter : String) is
   begin
      This.Setsockopt (Option => TCP_ACCEPT_FILTER,
                       Value  => Filter);
   end Assign_Filters_To_Allow_New_TCP_Connections;
   not overriding
   procedure  Assign_Filters_To_Allow_New_TCP_Connections
     (This   : in out Socket;
      Filter : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (Option => TCP_ACCEPT_FILTER,
                       Value  => To_String (Filter));
   end Assign_Filters_To_Allow_New_TCP_Connections;
   not overriding
   procedure  Assign_Filters_To_Allow_New_TCP_Connections
     (This   : in out Socket;
      Filter : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (Option => TCP_ACCEPT_FILTER,
                       Value  => Filter);
   end Assign_Filters_To_Allow_New_TCP_Connections;


   --=======================================================================
   --=======================================================================


   function Get_Impl (This : in Socket) return System.Address is
   begin
      return This.C;
   end Get_Impl;

   --=======================================================================
   --=======================================================================

   not overriding
   procedure  Getsockopt (This       : in Socket;
                          Option     : Socket_Opt;
                          Value      : System.Address;
                          Value_Size : out Natural) is
      Ret          : int;
      Value_Size_I : aliased size_t;
   begin
      Ret := Low_Level.zmq_getsockopt
        (This.C,
         Map (Option),
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
                         Option  : Socket_Opt) return unsigned_long is
      Dummy_Value_Size : Natural;
   begin
      return Ret : unsigned_long do
         This.Getsockopt (Option, Ret'Address, Dummy_Value_Size);
         if Dummy_Value_Size /= 8 then
            raise Program_Error with "Invalid getsockopt for this type";
         end if;
      end return;
   end Getsockopt;



   function  Getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return String is
      Buffer     : aliased String (1 .. MAX_OPTION_SIZE);
      Value_Size : Natural;

   begin
      This.Getsockopt (Option, Buffer'Address, Value_Size);
      return Buffer (1 .. Value_Size);
   end Getsockopt;

   not overriding
   function  Getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return Boolean is
   begin
      return Ret : Boolean do
         Ret := unsigned_long'(This.Getsockopt (Option)) /= 0;
      end return;
   end Getsockopt;

   not overriding
   function  Getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return Natural is
   begin
      return Ret : Natural do
         Ret := Natural (unsigned_long'(This.Getsockopt (Option)));
      end return;
   end Getsockopt;
   not overriding
   function Getsockopt
     (This    : in  Socket;
      Option  : Socket_Opt) return  Ada.Streams.Stream_Element_Array is
      Buffer     : aliased Stream_Element_Array (1 .. MAX_OPTION_SIZE);
      Value_Size : Ada.Streams.Stream_Element_Offset;

   begin
      This.Getsockopt (Option, Buffer'Address, Natural (Value_Size));
      return Buffer (1 .. Value_Size);
   end Getsockopt;


   not overriding
   function Retrieve_Socket_Type (This : Socket) return Socket_Type is
   begin
      return Socket_Type'Val (Natural'(This.Getsockopt (ZMQ_TYPE)));
   end Retrieve_Socket_Type;

   not overriding
   function More_Message_Parts_To_Follow (This : Socket) return Boolean is
   begin
      return Ret : Boolean do
         Ret := This.Getsockopt (RCVMORE);
      end return;
   end More_Message_Parts_To_Follow;


   not overriding
   function Get_High_Water_Mark_For_Outbound_Messages
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (SNDHWM);
   end Get_High_Water_Mark_For_Outbound_Messages;

   not overriding
   function Get_High_Water_Mark_For_Inbound_Messages
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (RCVHWM);
   end Get_High_Water_Mark_For_Inbound_Messages;

   function Get_IO_Thread_Affinity (This : Socket) return Thread_Bitmap is
      Value_Size : Natural;
   begin
      return Ret :  Thread_Bitmap do
         This.Getsockopt (AFFINITY, Ret'Address, Value_Size);
         if Value_Size /= 8 then
            raise Program_Error with "Invalid bitmap size " & Value_Size'Img;
         end if;
      end return;
   end Get_IO_Thread_Affinity;

   function Get_Socket_Identity
     (This : Socket) return Ada.Streams.Stream_Element_Array  is
   begin
      return This.Getsockopt (IDENTITY);
   end Get_Socket_Identity;
   not overriding
   function Get_Socket_Identity
     (This : Socket)
      return String is
   begin
      return This.Getsockopt (IDENTITY);
   end Get_Socket_Identity;
   not overriding
   function Get_Socket_Identity
     (This : Socket)
      return Ada.Strings.Unbounded.Unbounded_String  is
   begin
      return To_Unbounded_String (String'(This.Getsockopt (IDENTITY)));
   end Get_Socket_Identity;


   function Get_Multicast_Data_Rate (This : Socket) return Natural  is
   begin
      return This.Getsockopt (RATE);
   end Get_Multicast_Data_Rate;

   function Get_Multicast_Recovery_Interval (This : Socket) return Duration is
   begin
      return Duration (unsigned_long'(This.Getsockopt (RECOVERY_IVL)));
   end Get_Multicast_Recovery_Interval;

   function Get_Kernel_Transmit_Buffer_Size (This : Socket) return Natural is
   begin
      return This.Getsockopt (SNDBUF);
   end Get_Kernel_Transmit_Buffer_Size;

   function Get_Kernel_Receive_Buffer_Size (This : Socket) return Natural is
   begin
      return This.Getsockopt (RCVBUF);
   end Get_Kernel_Receive_Buffer_Size;


   not overriding
   function Get_Linger_Period_For_Socket_Shutdown
     (This : Socket) return Duration is
   begin
      return Natural'(This.Get_Linger_Period_For_Socket_Shutdown) * 1000.0;
   end Get_Linger_Period_For_Socket_Shutdown;

   not overriding
   function Get_Linger_Period_For_Socket_Shutdown
     (This : Socket) return Ada.Real_Time.Time_Span is
   begin
      return Ada.Real_Time.To_Time_Span
        (This.Get_Linger_Period_For_Socket_Shutdown);
   end Get_Linger_Period_For_Socket_Shutdown;

   not overriding
   function Get_Linger_Period_For_Socket_Shutdown
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (LINGER);
   end Get_Linger_Period_For_Socket_Shutdown;


   not overriding
   function Get_Reconnection_Interval
     (This : Socket) return Duration is
   begin
      return Natural'(This.Get_Reconnection_Interval) * 1000.0;
   end Get_Reconnection_Interval;

   not overriding
   function Get_Reconnection_Interval
     (This : Socket) return Ada.Real_Time.Time_Span is
   begin
      return Ada.Real_Time.To_Time_Span
        (This.Get_Reconnection_Interval);
   end Get_Reconnection_Interval;

   not overriding
   function Get_Reconnection_Interval
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (RECONNECT_IVL);
   end Get_Reconnection_Interval;




   not overriding
   function Get_Maximum_Reconnection_Interval
     (This : Socket) return Duration is
   begin
      return Natural'(This.Get_Maximum_Reconnection_Interval) * 1000.0;
   end Get_Maximum_Reconnection_Interval;

   not overriding
   function Get_Maximum_Reconnection_Interval
     (This : Socket) return Ada.Real_Time.Time_Span is
   begin
      return Ada.Real_Time.To_Time_Span
        (This.Get_Maximum_Reconnection_Interval);
   end Get_Maximum_Reconnection_Interval;

   not overriding
   function Get_Maximum_Reconnection_Interval
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (RECONNECT_IVL_MAX);
   end Get_Maximum_Reconnection_Interval;

   not overriding
   function Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (BACKLOG);
   end Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections;

   not overriding
   function Get_Maximum_Acceptable_Inbound_Message_Size
     (This : Socket) return Integer  is
   begin
      return This.Getsockopt (MAXMSGSIZE);
   end Get_Maximum_Acceptable_Inbound_Message_Size;

   not overriding
   function Get_Maximum_Network_Hops_For_Multicast_Packets
     (This : Socket) return Positive  is
   begin
      return This.Getsockopt (MULTICAST_HOPS);
   end Get_Maximum_Network_Hops_For_Multicast_Packets;

   not overriding
   function Get_Recieve_Timeout
     (This : Socket) return Duration is
      Temp : constant Integer := This.Get_Recieve_Timeout;
   begin
      if Temp < 0 then
         return  Duration'Last;
      else
         return 1000.0 * Temp;
      end if;
   end Get_Recieve_Timeout;

   not overriding
   function Get_Recieve_Timeout
     (This : Socket) return Ada.Real_Time.Time_Span is
   begin
      return Ada.Real_Time.To_Time_Span (This.Get_Recieve_Timeout);
   end Get_Recieve_Timeout;

   not overriding
   function Get_Recieve_Timeout  -- Millisecond
     (This : Socket) return Integer is
   begin
      return This.Getsockopt (RCVTIMEO);
   end Get_Recieve_Timeout;


   not overriding
   function Get_Send_Timeout
     (This : Socket) return Duration is
      Temp : constant Integer := This.Get_Send_Timeout;
   begin
      if Temp < 0 then
         return  Duration'Last;
      else
         return 1000.0 * Temp;
      end if;
   end Get_Send_Timeout;

   not overriding
   function Get_Send_Timeout
     (This : Socket) return Ada.Real_Time.Time_Span is
   begin
      return Ada.Real_Time.To_Time_Span (This.Get_Send_Timeout);
   end Get_Send_Timeout;

   not overriding
   function Get_Send_Timeout  -- Millisecond
     (This : Socket) return Integer is
   begin
      return This.Getsockopt (SNDTIMEO);
   end Get_Send_Timeout;

   not overriding
   function Get_IPv4_only_socket_override
     (This : Socket) return Boolean is
   begin
      return This.Getsockopt (IPV4ONLY);
   end Get_IPv4_only_socket_override;

   not overriding
   function Get_Attach_On_Connect
     (This : Socket) return Boolean is
   begin
      return This.Getsockopt (DELAY_ATTACH_ON_CONNECT);
   end Get_Attach_On_Connect;

   not overriding
   function Get_File_Descriptor
     (This : Socket) return GNAT.OS_Lib.File_Descriptor is
   begin
      return GNAT.OS_Lib.File_Descriptor
        (Interfaces.C.unsigned_long'(This.Getsockopt (FD)));
   end Get_File_Descriptor;

   not overriding
   function Get_Last_Endpoint_Set
     (This : Socket) return String is
   begin
      return This.Getsockopt (LAST_ENDPOINT);
   end Get_Last_Endpoint_Set;

   not overriding
   function Get_Last_Endpoint_Set
     (This : Socket) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Ret : Ada.Strings.Unbounded.Unbounded_String do
         Set_Unbounded_String (Ret, This.Get_Last_Endpoint_Set);
      end return;
   end Get_Last_Endpoint_Set;

   not overriding
   function Get_SO_KEEPALIVE_Socket_Option
     (This : in Socket) return SO_KEEPALIVE_Type is
      Map : constant array (-1 .. 1) of SO_KEEPALIVE_Type :=
              (OS_Default, Disable, Enable);
   begin
      return Map (This.Getsockopt (LAST_ENDPOINT));
   end Get_SO_KEEPALIVE_Socket_Option;

   not overriding
   function Get_TCP_KEEPCNT_IDLE_socket_option
     (This : in Socket) return Integer is
   begin
      return This.Getsockopt (TCP_KEEPALIVE_IDLE);
   end Get_TCP_KEEPCNT_IDLE_socket_option;

   not overriding
   function Get_TCP_KEEPCNT_CNT_socket_option
     (This : in Socket) return Integer is
   begin
      return This.Getsockopt (TCP_KEEPALIVE_CNT);
   end Get_TCP_KEEPCNT_CNT_socket_option;

   not overriding
   function Get_TCP_KEEPINTVL_Socket_Option
     (This : in Socket) return Integer is
   begin
      return This.Getsockopt (TCP_KEEPALIVE_INTVL);
   end Get_TCP_KEEPINTVL_Socket_Option;


   procedure Set_Monitor
     (This    : Socket;
      Address : String;
      Mask    : Mask_Type) is
   begin
      null;
   end Set_Monitor;
end ZMQ.Sockets;
