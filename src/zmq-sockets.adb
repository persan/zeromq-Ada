------------------------------------------------------------------------------
--                                                                          --
--                             0MQ Ada-binding                              --
--                                                                          --
--                           Z M Q . S O C K E T S                          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net            --
--                                                                          --
-- 0MQ Ada-binding is free software;  you can  redistribute it  and/or      --
-- modify it under terms of the  GNU General Public License as published    --
-- by the Free Soft-ware  Foundation;                                       --
-- either version 2,  or (at your option) any later version.                --
-- 0MQ Ada-binding is distributed in the hope that it will be useful, but   --
-- WITH OUT ANY WARRANTY;                                                   --
-- without even the  implied warranty of MERCHANTABILITY or                 --
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License    --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------
with ZMQ.Low_Level;
with Interfaces.C.Strings;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body ZMQ.Sockets is
   use Interfaces.C.Strings;
   use Interfaces.C;
   use System;
   use Ada.Streams;
   type Map_Array is  array (Socket_Opt) of int;
   Map :  constant Map_Array  :=
           (HWM                          => Low_Level.Defs.ZMQ_HWM,
            SWAP                         => Low_Level.Defs.ZMQ_SWAP,
            AFFINITY                     => Low_Level.Defs.ZMQ_AFFINITY,
            IDENTITY                     => Low_Level.Defs.ZMQ_IDENTITY,
            SUBSCRIBE                    => Low_Level.Defs.ZMQ_SUBSCRIBE,
            UNSUBSCRIBE                  => Low_Level.Defs.ZMQ_UNSUBSCRIBE,
            RATE                         => Low_Level.Defs.ZMQ_RATE,
            RECOVERY_IVL                 => Low_Level.Defs.ZMQ_RECOVERY_IVL,
            MCAST_LOOP                   => Low_Level.Defs.ZMQ_MCAST_LOOP,
            SNDBUF                       => Low_Level.Defs.ZMQ_SNDBUF,
            RCVBUF                       => Low_Level.Defs.ZMQ_RCVBUF,
            RCVMORE                      => Low_Level.Defs.ZMQ_RCVMORE,
            FD                           => Low_Level.Defs.ZMQ_FD,
            EVENTS                       => Low_Level.Defs.ZMQ_EVENTS,
            GET_TYPE                     => Low_Level.Defs.ZMQ_TYPE,
            LINGER                       => Low_Level.Defs.ZMQ_LINGER,
            RECONNECT_IVL                => Low_Level.Defs.ZMQ_RECONNECT_IVL,
            BACKLOG                      => Low_Level.Defs.ZMQ_BACKLOG
           );


   function img (item : Ada.Streams.Stream_Element_Array) return String is
      ret    : String (1 .. item'Length * 2);
      cursor : Natural := 1;
      type map_string is array (Stream_Element (0) ..
                                  Stream_Element (15)) of Character;
      hex    : constant map_string := ('0', '1', '2', '3',
                                       '4', '5', '6', '7',
                                       '8', '9', 'A', 'B',
                                      'C', 'D', 'E', 'F');
   begin
      for i in item'Range loop
         ret (cursor) := hex (item (i) / 16);
         cursor := cursor + 1;
         ret (cursor) := hex (item (i) mod 16);
         cursor := cursor + 1;
      end loop;
      return ret;
   end img;

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

   procedure  setsockopt (This       : in out Socket;
                          Option     : Socket_Opt;
                          Value      : System.Address;
                          Value_Size : Natural) is
      ret     : int;
   begin
      ret := Low_Level.zmq_setsockopt
        (This.c,
         Map (Option),
         Value,
         size_t (Value_Size));
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity & "(" & Option'Img & ")";
      end if;
   end setsockopt;

   ----------------
   -- setsockopt --
   ----------------

   not overriding procedure setsockopt
     (This         : in out Socket;
      Option       : Socket_Opt;
      Value        : String)
   is
   begin
      This.setsockopt (Option, Value'Address, Value'Length);
   end setsockopt;

   ----------------
   -- setsockopt --
   ----------------

   not overriding procedure setsockopt
     (This    : in out Socket;
      Option  : Socket_Opt;
      Value   : Boolean)
   is
   begin
      This.setsockopt (Option, Value'Address, 1);
   end setsockopt;

   ----------------
   -- setsockopt --
   ----------------

   not overriding procedure setsockopt
     (This    : in out Socket;
      Option  : Socket_Opt;
      Value   : Natural)
   is
   begin
      This.setsockopt (Option, Value'Address, 4);
   end setsockopt;

   ----------------
   -- setsockopt --
   ----------------
   not overriding procedure setsockopt
     (This    : in out Socket;
      Option  : Socket_Opt;
      Value   : Ada.Streams.Stream_Element_Array)
   is
   begin
      This.setsockopt (Option, Value (Value'First)'Address, Value'Length);
   end setsockopt;

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
      ret := Low_Level.zmq_send (This.c, Msg.getImpl, int (Flags));
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Send;

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : String;
                   Flags   : Socket_Flags := No_Flags) is
      m : Messages.Message;
   begin
      m.Initialize (Msg);
      This.Send (m, Flags);
      m.Finalize;
   end Send;

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Ada.Streams.Stream_Element_Array;
                   Flags   : Socket_Flags := No_Flags) is
      M : Messages.Message;
   begin
      M.Initialize (Msg);
      This.Send (M, Flags);
      M.Finalize;
   end Send;

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags) is
      M : Messages.Message;
   begin
      M.Initialize (Ada.Strings.Unbounded.To_String (Msg));
      This.Send (M, Flags);
      M.Finalize;
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


   procedure Send (This           : in out Socket;
                   Msg_Addres     : System.Address;
                   Msg_Length     : Natural;
                   Flags          : Socket_Flags := No_Flags) is
      M : Messages.Message;
   begin
      M.Initialize (Msg_Addres, Msg_Length);
      This.Send (M, Flags);
      M.Finalize;
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

   not overriding procedure recv
     (This    : in Socket;
      Msg     : Messages.Message'Class;
      Flags   : Socket_Flags := No_Flags)
   is
      ret  : int;
   begin
      ret := Low_Level.zmq_recv (This.c,
                                 Msg.getImpl,
                                 int (Flags));

      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in "
           & GNAT.Source_Info.Enclosing_Entity;
      end if;
   end recv;
   not overriding
   function recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags) return String is
      Msg     : Messages.Message;
   begin
      Msg.Initialize;
      This.recv (Msg, Flags);
      return ret : String (1 .. Msg.getSize) do
         ret := Msg.getData;
      end return;
   end recv;
   procedure recv (This    : in Socket;
                   msg     : out Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags) is
   begin
      msg := Ada.Strings.Unbounded.To_Unbounded_String (This.recv (Flags));
   end recv;


   not overriding
   function recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags)
                  return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return ret : Ada.Strings.Unbounded.Unbounded_String do
         This.recv (ret, Flags);
      end return;
   end recv;

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
         this.c := Null_Address;
         if ret /= 0 then
            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno);
         end if;
      end if;
   end Finalize;


   not overriding

   procedure  Set_high_water_mark (This       : in out Socket;
                                   Value      : Natural) is
   begin
      This.setsockopt (HWM, Value);
   end Set_high_water_mark;


   not overriding
   procedure  Set_disk_offload_size (This       : in out Socket;
                                     Value      : Boolean) is
   begin
      This.setsockopt (SWAP, Boolean'Pos (Value));
   end Set_disk_offload_size;

   not overriding
   procedure  Set_IO_thread_affinity (This       : in out Socket;
                                      Value      : Natural) is
   begin
      This.setsockopt (AFFINITY, Value);
   end Set_IO_thread_affinity;

   not overriding
   procedure  Set_socket_identity
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array) is
   begin
      This.setsockopt (IDENTITY, Value);
   end Set_socket_identity;

   not overriding
   procedure  Establish_message_filter (This       : in out Socket;
                                        Value      : String) is
   begin
      This.setsockopt (SUBSCRIBE, Value);
   end Establish_message_filter;

   not overriding

   procedure  Establish_message_filter
     (This       : in out Socket;
      Value      :  Ada.Streams.Stream_Element_Array) is
   begin
      This.setsockopt (SUBSCRIBE, Value);
   end Establish_message_filter;

   procedure  Establish_message_filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.setsockopt (SUBSCRIBE, To_String (Value));
   end Establish_message_filter;

   not overriding

   procedure  Remove_message_filter (This       : in out Socket;
                                     Value      : String) is
   begin
      This.setsockopt (UNSUBSCRIBE, Value);
   end Remove_message_filter;

   procedure  Remove_message_filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.setsockopt (UNSUBSCRIBE, To_String (Value));
   end Remove_message_filter;

   procedure  Remove_message_filter
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array) is
   begin
      This.setsockopt (UNSUBSCRIBE, Value);
   end Remove_message_filter;

   not overriding
   procedure  Set_multicast_data_rate (This       : in out Socket;
                                       Value      : Natural) is
   begin
      This.setsockopt (RATE, Value);
   end Set_multicast_data_rate;

   not overriding
   procedure  set_multicast_recovery_interval (This       : in out Socket;
                                               Value      : Duration) is
   begin
      This.setsockopt (RECOVERY_IVL, Integer (Value));
   end set_multicast_recovery_interval;
   not overriding

   procedure  Set_multicast_loopback (This        : in out Socket;
                                      Enable      : Boolean) is
   begin
      This.setsockopt (HWM, Enable);
   end Set_multicast_loopback;
   not overriding
   procedure  Set_kernel_transmit_buffer_size (This       : in out Socket;
                                               Value      : Natural) is
   begin
      This.setsockopt (SNDBUF, Value);
   end Set_kernel_transmit_buffer_size;
   not overriding

   procedure  Set_kernel_receive_buffer_size (This       : in out Socket;
                                              Value      : Natural) is
   begin
      This.setsockopt (RCVBUF, Value);
   end Set_kernel_receive_buffer_size;

   function get_impl (This : in Socket) return System.Address is
   begin
      return This.c;
   end get_impl;

   -------------

   not overriding
   procedure  getsockopt (This       : in Socket;
                          Option     : Socket_Opt;
                          Value      : System.Address;
                          Value_Size : out Natural) is
      ret          : int;
      Value_Size_i : aliased size_t;
   begin
      ret := Low_Level.zmq_getsockopt
        (This.c,
         Map (Option),
         Value,
         Value_Size_i'Access);
      Value_Size := Natural (Value_Size_i);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity & "(" & Option'Img & ")";
      end if;
   end getsockopt;

   not overriding
   function  getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return unsigned_long is
      Dummy_Value_Size : Natural;
   begin
      return ret : aliased unsigned_long do
         This.getsockopt (Option, ret'Address, Dummy_Value_Size);
         if Dummy_Value_Size /= 8 then
            raise Program_Error with "Invalid getsockopt for this type";
         end if;
      end return;
   end getsockopt;



   function  getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return String is
      Buffer     : aliased String (1 .. MAX_OPTION_SIZE);
      Value_Size : Natural;

   begin
      This.getsockopt (Option, Buffer'Address, Value_Size);
      return Buffer (1 .. Value_Size);
   end getsockopt;

   not overriding
   function  getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return Boolean is
   begin
      return ret : Boolean do
         ret := unsigned_long'(This.getsockopt (Option)) /= 0;
      end return;
   end getsockopt;

   not overriding
   function  getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return Natural is
   begin
      return ret : Natural do
         ret := Natural (unsigned_long'(This.getsockopt (Option)));
      end return;
   end getsockopt;
   not overriding

   function getsockopt
     (This    : in  Socket;
      Option  : Socket_Opt) return  Ada.Streams.Stream_Element_Array is
      Buffer     : aliased Stream_Element_Array (1 .. MAX_OPTION_SIZE);
      Value_Size : Ada.Streams.Stream_Element_Offset;

   begin
      This.getsockopt (Option, Buffer'Address, Natural (Value_Size));
      return Buffer (1 .. Value_Size);
   end getsockopt;


   function More_message_parts_to_follow (This : Socket) return Boolean is
   begin
      return ret : Boolean do
         ret := This.getsockopt (RCVMORE);
      end return;
   end More_message_parts_to_follow;

   function Get_high_water_mark (This : Socket) return Natural is
   begin
      return ret : Natural do
         ret := This.getsockopt (HWM);
      end return;
   end Get_high_water_mark;

   function Get_disk_offload_size (This : Socket) return Natural is
   begin
      return ret : Natural do
         ret := This.getsockopt (SWAP);
      end return;
   end Get_disk_offload_size;

   function Get_IO_thread_affinity (This : Socket) return Thread_Bitmap is
      Value_Size : Natural;
   begin
      return ret : aliased Thread_Bitmap do
         This.getsockopt (AFFINITY, ret'Address, Value_Size);
         if Value_Size /= 8 then
            raise Program_Error with "Invalid bitmap size " & Value_Size'Img;
         end if;
      end return;
   end Get_IO_thread_affinity;

   function Get_socket_identity
     (This : Socket) return Ada.Streams.Stream_Element_Array  is
   begin
      return This.getsockopt (IDENTITY);
   end Get_socket_identity;

   function Get_multicast_data_rate (This : Socket) return Natural  is
   begin
      return This.getsockopt (RATE);
   end Get_multicast_data_rate;

   function Get_multicast_recovery_interval (This : Socket) return Duration is
   begin
      return Duration (unsigned_long'(This.getsockopt (RECOVERY_IVL)));
   end Get_multicast_recovery_interval;

   function Get_multicast_loopback (This : Socket) return Boolean is
   begin
      return This.getsockopt (MCAST_LOOP);
   end Get_multicast_loopback;

   function Get_kernel_transmit_buffer_size (This : Socket) return Natural is
   begin
      return This.getsockopt (SNDBUF);
   end Get_kernel_transmit_buffer_size;

   function Get_kernel_receive_buffer_size (This : Socket) return Natural is
   begin
      return This.getsockopt (RCVBUF);
   end Get_kernel_receive_buffer_size;

end ZMQ.Sockets;
