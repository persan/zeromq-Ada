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
      if This.c /= Null_Address then
         raise ZMQ_Error with "Socket Initialized";
      end if;

      This.c := Low_Level.zmq_socket (With_Context.GetImpl,
                                      Socket_Type'Pos (Kind));
      if This.c = Null_Address then
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
      addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      ret  : int;
   begin
      ret := Low_Level.zmq_bind (This.c, addr);
      Free (addr);
      if ret /= 0 then
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
      addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      ret  : int;
   begin
      ret := Low_Level.zmq_unbind (This.c, addr);
      Free (addr);
      if ret /= 0 then
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
      ret     : int;
   begin
      ret := Low_Level.zmq_setsockopt
        (This.c,
         Option,
         Value,
         size_t (Value_Size));
      if ret /= 0 then
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
      addr : chars_ptr := Interfaces.C.Strings.New_String (Address);
      ret  : int;
   begin
      ret := Low_Level.zmq_connect (This.c, addr);
      Free (addr);
      if ret /= 0 then
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
      ret  : int;
   begin
      ret := Low_Level.zmq_msg_send (Msg.GetImpl, This.c, int (Flags));
      if ret = -1 then
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
      ret  : int;
   begin
      ret := Low_Level.zmq_send
        (This.c, Msg_Address, Interfaces.C.size_t (Msg_Length), int (Flags));
      if ret = -1 then
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
      ret  : int;
   begin
      ret := Low_Level.zmq_msg_recv (Msg.GetImpl,
                                     This.c,
                                     int (Flags));

      if ret = -1 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in "
           & GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Recv;

   procedure recv (This    : in Socket;
                   Flags   : Socket_Flags := No_Flags) is
      dummy_Msg : Messages.Message;
   begin
      dummy_Msg.Initialize;
      This.Recv (dummy_Msg, Flags);
      dummy_Msg.Finalize;
   end recv;

   not overriding

   function Recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags) return String is
      Msg     : Messages.Message;
   begin
      Msg.Initialize;
      This.Recv (Msg, Flags);
      return ret : String (1 .. Msg.GetSize) do
         ret := Msg.GetData;
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
      Max_Length : Natural := 1024;
      Flags      : Socket_Flags := No_Flags)
      return  String is
      msg : Messages.Message;
   begin
      This.Recv (msg, Flags);
      if msg.GetSize > Max_Length then
         raise Constraint_Error with "message size out of bounds" &
           msg.GetSize'Img & ">" & Max_Length'Img;
      end if;
      return ret : String (1 .. msg.GetSize) do
         ret := msg.GetData;
      end return;
   end Recv;

   not overriding
   function Recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags)
                  return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return ret : Ada.Strings.Unbounded.Unbounded_String do
         This.Recv (ret, Flags);
      end return;
   end Recv;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (this : in out Socket)
   is
      ret : int;
   begin
      if this.c /= Null_Address then
         ret := Low_Level.zmq_close (this.c);
         if ret /= 0 then
            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
         end if;
         this.c := Null_Address;
      end if;
   end Finalize;

   procedure Proxy (frontend  : not null access Socket;
                    backend   : not null access Socket'Class;
                    capture   : access Socket'Class) is
      ret : int;
   begin
      ret := Low_Level.zmq_proxy
        (frontend.c,
         backend.c,
         (if capture /= null then capture.c else System.Null_Address));
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
      end if;
   end Proxy;




   not overriding
   procedure  Set_high_water_mark_for_outbound_messages
     (This     : in out Socket;
      messages : Natural := 1_000)
   is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDHWM, messages);
   end Set_high_water_mark_for_outbound_messages;


   not overriding
   procedure  Set_high_water_mark_for_inbound_messages
     (This     : in out Socket;
      messages : Natural := 1_000)
   is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVHWM, messages);
   end Set_high_water_mark_for_inbound_messages;

   not overriding
   procedure  Set_disk_offload_size (This       : in out Socket;
                                     Value      : Natural) is
   begin
      null; -- This.setsockopt (ZMQ.Low_Level.Defs.SWAP, Value);
   end Set_disk_offload_size;

   not overriding
   procedure  Set_IO_thread_affinity (This    : in out Socket;
                                      Threads : Thread_Bitmap) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_AFFINITY, Threads'Address, 4);
   end Set_IO_thread_affinity;

   not overriding
   procedure  Set_socket_identity
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_IDENTITY, Value);
   end Set_socket_identity;

   procedure  Set_socket_identity
     (This       : in out Socket;
      Value      : String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_IDENTITY, Value);
   end Set_socket_identity;


   not overriding
   procedure  Establish_message_filter (This       : in out Socket;
                                        Value      : String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SUBSCRIBE, Value);
   end Establish_message_filter;

   not overriding

   procedure  Establish_message_filter
     (This       : in out Socket;
      Value      :  Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SUBSCRIBE, Value);
   end Establish_message_filter;

   procedure  Establish_message_filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SUBSCRIBE, To_String (Value));
   end Establish_message_filter;

   not overriding

   procedure  Remove_message_filter (This       : in out Socket;
                                     Value      : String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_UNSUBSCRIBE, Value);
   end Remove_message_filter;

   procedure  Remove_message_filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_UNSUBSCRIBE, To_String (Value));
   end Remove_message_filter;

   procedure  Remove_message_filter
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_UNSUBSCRIBE, Value);
   end Remove_message_filter;

   not overriding
   procedure  Set_multicast_data_rate
     (This                     : in out Socket;
      kilobits_per_second      : Natural := 100) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_RATE, kilobits_per_second);
   end Set_multicast_data_rate;

   not overriding
   procedure  set_multicast_recovery_interval (This : in out Socket;
                                               Time : Duration := 10.0) is
   begin
      This.Setsockopt
        (ZMQ.Low_Level.Defs.ZMQ_RECOVERY_IVL, Integer (Time * 1000));
   end set_multicast_recovery_interval;
   not overriding

   procedure  Set_multicast_loopback (This        : in out Socket;
                                      Enable      : Boolean) is
   begin
      null; -- This.setsockopt (ZMQ.Low_Level.Defs.ZMQ_HWM, Enable);
   end Set_multicast_loopback;
   not overriding
   procedure  Set_kernel_transmit_buffer_size (This  : in out Socket;
                                               bytes : Natural) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDBUF, bytes);
   end Set_kernel_transmit_buffer_size;
   not overriding

   procedure  Set_kernel_receive_buffer_size (This  : in out Socket;
                                              bytes : Natural) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVBUF, bytes);
   end Set_kernel_receive_buffer_size;


   not overriding
   function Get_linger_period_for_socket_shutdown
     (This : Socket) return Duration is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_LINGER);
   end Get_linger_period_for_socket_shutdown;

   not overriding
   procedure  Set_linger_period_for_socket_shutdown
     (This   : in out Socket;
      period : Duration := Duration'Last) is
   begin
      This.Setsockopt (ZMQ.Low_Level.Defs.ZMQ_LINGER, period);
   end Set_linger_period_for_socket_shutdown;


   not overriding
   function Get_reconnection_interval
     (This : Socket) return Duration is
   begin
      return Duration (Natural'(This.Getsockopt
                       (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL))) / 1000.0;
   end Get_reconnection_interval;

   not overriding
   procedure  Set_reconnection_interval
     (This   : in out Socket;
      period : Duration := 0.100) is
   begin
      if period < 0.0  then
         This.Setsockopt
           (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL, Integer'(-1));
      else
         This.Setsockopt
           (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL, Natural (period * 1000.0));
      end if;

   end Set_reconnection_interval;


   not overriding
   function Get_Maximum_reconnection_interval
     (This : Socket) return Duration is
   begin
      return Duration (Natural'(This.Getsockopt
                       (ZMQ.Low_Level.Defs.ZMQ_RECONNECT_IVL))) / 1000.0;
   end Get_Maximum_reconnection_interval;

   not overriding
   procedure  Set_Maximum_reconnection_interval
     (This   : in out Socket;
      period : Duration := 0.0) is
   begin
      if period < 0.0  then
         This.Setsockopt
           (Low_Level.Defs.ZMQ_RECONNECT_IVL, Integer'(-1));
      else
         This.Setsockopt
           (Low_Level.Defs.ZMQ_RECONNECT_IVL, Natural (period * 1000.0));
      end if;

   end Set_Maximum_reconnection_interval;



   not overriding
   function Get_maximum_length_of_the_queue_of_outstanding_connections
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_BACKLOG);
   end Get_maximum_length_of_the_queue_of_outstanding_connections;

   not overriding
   procedure Set_maximum_length_of_the_queue_of_outstanding_connections
     (This        : in out Socket;
      connections : Natural  := 100) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_BACKLOG, connections);
   end Set_maximum_length_of_the_queue_of_outstanding_connections;


   not overriding
   function Get_Maximum_acceptable_inbound_message_size
     (This : Socket) return Long_Long_Integer  is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_MAXMSGSIZE);
   end Get_Maximum_acceptable_inbound_message_size;

   not overriding
   procedure Set_Maximum_acceptable_inbound_message_size
     (This   : in out Socket;
      Bytes  : Long_Long_Integer  := 0) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_MAXMSGSIZE, Bytes);
   end Set_Maximum_acceptable_inbound_message_size;


   not overriding
   function Get_Maximum_network_hops_for_multicast_packets
     (This : Socket) return Positive is
   begin
      return This.Getsockopt (Low_Level.Defs.ZMQ_MULTICAST_HOPS);
   end Get_Maximum_network_hops_for_multicast_packets;

   not overriding
   procedure Set_Maximum_network_hops_for_multicast_packets
     (This         : in out Socket;
      network_hops : Positive  := 1) is
   begin
      This.Setsockopt (Low_Level.Defs.ZMQ_MULTICAST_HOPS, network_hops);
   end Set_Maximum_network_hops_for_multicast_packets;


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
      return This.c;
   end Get_Impl;

   -------------

   not overriding
   procedure  Getsockopt (This       : in Socket;
                          Option     : Interfaces.C.int;
                          Value      : System.Address;
                          Value_Size : in out Natural) is
      ret          : int;
      Value_Size_i : aliased size_t;
   begin
      ret := Low_Level.zmq_getsockopt
        (This.c,
         Option,
         Value,
         Value_Size_i'Access);
      Value_Size := Natural (Value_Size_i);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity & "(" & Option'Img & ")";
      end if;
   end Getsockopt;

   not overriding
   function  Getsockopt (This    : in Socket;
                         Option  : Interfaces.C.int) return unsigned_long is
      Dummy_Value_Size : Natural := unsigned_long'Size / System.Storage_Unit;
   begin
      return ret : unsigned_long do
         This.Getsockopt (Option, ret'Address, Dummy_Value_Size);
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
      return ret : Boolean do
         ret := unsigned_long'(This.Getsockopt (Option)) /= 0;
      end return;
   end Getsockopt;

   not overriding
   function  Getsockopt (This    : in Socket;
                         Option  : Interfaces.C.int) return Integer is
   begin
      return ret : Integer do
         ret := Integer (unsigned_long'(This.Getsockopt (Option)));
      end return;
   end Getsockopt;

   function  Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return Long_Long_Integer is
   begin
      return ret : Long_Long_Integer do
         ret := Long_Long_Integer (unsigned_long'(This.Getsockopt (Option)));
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

   function More_message_parts_to_follow (This : Socket) return Boolean is
   begin
      return ret : Boolean do
         ret := This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVMORE);
      end return;
   end More_message_parts_to_follow;

   function Get_high_water_mark_for_outbound_messages
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDHWM);
   end Get_high_water_mark_for_outbound_messages;

   function Get_high_water_mark_for_inbound_messages
     (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVHWM);
   end Get_high_water_mark_for_inbound_messages;


   function Get_IO_thread_affinity (This : Socket) return Thread_Bitmap is
      Value_Size : Natural := Thread_Bitmap'Size / System.Storage_Unit;
   begin
      return ret : Thread_Bitmap do
         This.Getsockopt
           (ZMQ.Low_Level.Defs.ZMQ_AFFINITY, ret'Address, Value_Size);
         if Value_Size /= 8 then
            raise Program_Error with "Invalid bitmap size " & Value_Size'Img;
         end if;
      end return;
   end Get_IO_thread_affinity;

   function Get_socket_identity
     (This : Socket) return Ada.Streams.Stream_Element_Array  is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_IDENTITY);
   end Get_socket_identity;

   function Get_multicast_data_rate (This : Socket) return Natural  is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RATE);
   end Get_multicast_data_rate;

   function Get_multicast_recovery_interval (This : Socket) return Duration is
   begin
      return Duration
        (unsigned_long'(
         This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RECOVERY_IVL)));
   end Get_multicast_recovery_interval;

   function Get_multicast_loopback (This : Socket) return Boolean is
      pragma Unreferenced (This);
   begin
      return False; -- This.getsockopt (ZMQ.Low_Level.Defs.ZMQ_MCAST_LOOP);
   end Get_multicast_loopback;

   function Get_kernel_transmit_buffer_size (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_SNDBUF);
   end Get_kernel_transmit_buffer_size;

   function Get_kernel_receive_buffer_size (This : Socket) return Natural is
   begin
      return This.Getsockopt (ZMQ.Low_Level.Defs.ZMQ_RCVBUF);
   end Get_kernel_receive_buffer_size;

   not overriding function Retrieve_socket_type
     (This : in Socket)
      return Socket_Type is
   begin
      return Socket_Type'Val
        (Natural'(This.Getsockopt (Low_Level.Defs.ZMQ_TYPE)));
   end Retrieve_socket_type;


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
     (stream : not null access Ada.Streams.Root_Stream_Type'class;
      s      : out Socket) is
   begin
      raise Program_Error with "Sockets are not streameble";
   end Read_Socket;
   procedure Write_Socket
     (stream : not null access Ada.Streams.Root_Stream_Type'class;
      s      : Socket) is
   begin
      raise Program_Error with "Sockets are not streameble";
   end Write_Socket;

   function Stream
     (This : Socket)
      return not null access Ada.Streams.Root_Stream_Type'class is
   begin
      return This.S'Unrestricted_Access;
   end Stream;


end ZMQ.Sockets;
