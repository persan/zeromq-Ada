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
           (HWM          => Low_Level.defs.ZMQ_HWM,   -- Set high water mark
            LWM          => Low_Level.defs.ZMQ_LWM,   -- Set low water mark
            SWAP         => Low_Level.defs.ZMQ_SWAP,
            AFFINITY     => Low_Level.defs.ZMQ_AFFINITY,
            IDENTITY     => Low_Level.defs.ZMQ_IDENTITY,
            SUBSCRIBE    => Low_Level.defs.ZMQ_SUBSCRIBE,
            UNSUBSCRIBE  => Low_Level.defs.ZMQ_UNSUBSCRIBE,
            RATE         => Low_Level.defs.ZMQ_RATE,
            RECOVERY_IVL => Low_Level.defs.ZMQ_RECOVERY_IVL,
            MCAST_LOOP   => Low_Level.defs.ZMQ_MCAST_LOOP,
            SNDBUF       => Low_Level.defs.ZMQ_SNDBUF,
            RCVBUF       => Low_Level.defs.ZMQ_RCVBUF);


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
         Value_Size);
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

   procedure  setsockopt_HWM (This       : in out Socket;
                              Value      : Natural) is
   begin
      This.setsockopt (HWM, Value);
   end setsockopt_HWM;

   not overriding
   procedure  setsockopt_LWM (This       : in out Socket;
                              Value      : Natural) is
   begin
      This.setsockopt (LWM, Value);
   end setsockopt_LWM;

   not overriding
   procedure  setsockopt_SWAP (This       : in out Socket;
                               Value      : Boolean) is
   begin
      This.setsockopt (SWAP, Boolean'Pos (Value));
   end setsockopt_SWAP;
   not overriding
   procedure  setsockopt_AFFINITY (This       : in out Socket;
                                   Value      : Natural) is
   begin
      This.setsockopt (AFFINITY, Value);
   end setsockopt_AFFINITY;

   not overriding
   procedure  setsockopt_IDENTITY (This       : in out Socket;
                                   Value      : Natural) is
   begin
      This.setsockopt (IDENTITY, Value);
   end setsockopt_IDENTITY;

   not overriding
   procedure  setsockopt_SUBSCRIBE (This       : in out Socket;
                                    Value      : String) is
   begin
      This.setsockopt (SUBSCRIBE, Value);
   end setsockopt_SUBSCRIBE;

   not overriding

   procedure  setsockopt_SUBSCRIBE
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.setsockopt (SUBSCRIBE, To_String (Value));
   end setsockopt_SUBSCRIBE;

   not overriding

   procedure  setsockopt_UNSUBSCRIBE (This       : in out Socket;
                                      Value      : String) is
   begin
      This.setsockopt (UNSUBSCRIBE, Value);
   end setsockopt_UNSUBSCRIBE;

   not overriding
   procedure  setsockopt_UNSUBSCRIBE
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.setsockopt (UNSUBSCRIBE, To_String (Value));
   end setsockopt_UNSUBSCRIBE;
   not overriding

   procedure  setsockopt_RATE (This       : in out Socket;
                               Value      : Natural) is
   begin
      This.setsockopt (RATE, Value);
   end setsockopt_RATE;

   not overriding
   procedure  setsockopt_RECOVERY_IVL (This       : in out Socket;
                                       Value      : Natural) is
   begin
      This.setsockopt (RECOVERY_IVL, Value);
   end setsockopt_RECOVERY_IVL;
   not overriding
   procedure  setsockopt_MCAST_LOOP (This       : in out Socket;
                                     Value      : Natural) is
   begin
      This.setsockopt (HWM, Value);
   end setsockopt_MCAST_LOOP;
   not overriding
   procedure  setsockopt_SNDBUF (This       : in out Socket;
                                 Value      : Natural) is
   begin
      This.setsockopt (SNDBUF, Value);
   end setsockopt_SNDBUF;
   not overriding

   procedure  setsockopt_RCVBUF (This       : in out Socket;
                                 Value      : Natural) is
   begin
      This.setsockopt (RCVBUF, Value);
   end setsockopt_RCVBUF;

   function get_impl (This : in Socket) return System.Address is
   begin
      return This.c;
   end get_impl;

end ZMQ.Sockets;
