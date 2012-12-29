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
package body ZMQ.Sockets is
   use Interfaces.C.Strings;
   use Interfaces.C;
   use System;
   use Ada.Streams;
   type Map_Array is  array (Socket_Opt) of int;
   Map :  constant Map_Array  :=
           (AFFINITY                     => Low_Level.ZMQ_AFFINITY,
            IDENTITY                     => Low_Level.ZMQ_IDENTITY,
            SUBSCRIBE                    => Low_Level.ZMQ_SUBSCRIBE,
            UNSUBSCRIBE                  => Low_Level.ZMQ_UNSUBSCRIBE,
            RATE                         => Low_Level.ZMQ_RATE,
            RECOVERY_IVL                 => Low_Level.ZMQ_RECOVERY_IVL,
            SNDBUF                       => Low_Level.ZMQ_SNDBUF,
            RCVBUF                       => Low_Level.ZMQ_RCVBUF,
            RCVMORE                      => Low_Level.ZMQ_RCVMORE,
            FD                           => Low_Level.ZMQ_FD,
            EVENTS                       => Low_Level.ZMQ_EVENTS,
            GET_TYPE                     => Low_Level.ZMQ_TYPE,
            LINGER                       => Low_Level.ZMQ_LINGER,
            RECONNECT_IVL                => Low_Level.ZMQ_RECONNECT_IVL,
            BACKLOG                      => Low_Level.ZMQ_BACKLOG,
            RECONNECT_IVL_MAX            => Low_Level.ZMQ_RECONNECT_IVL_MAX,
            MAXMSGSIZE                   => Low_Level.ZMQ_MAXMSGSIZE,
            SNDHWM                       => Low_Level.ZMQ_SNDHWM,
            RCVHWM                       => Low_Level.ZMQ_RCVHWM,
            MULTICAST_HOPS               => Low_Level.ZMQ_MULTICAST_HOPS,
            RCVTIMEO                     => Low_Level.ZMQ_RCVTIMEO,
            SNDTIMEO                     => Low_Level.ZMQ_SNDTIMEO,
            IPV4ONLY                     => Low_Level.ZMQ_IPV4ONLY,
            LAST_ENDPOINT                => Low_Level.ZMQ_LAST_ENDPOINT,
            ROUTER_BEHAVIOR              => Low_Level.ZMQ_ROUTER_BEHAVIOR,
            TCP_KEEPALIVE                => Low_Level.ZMQ_TCP_KEEPALIVE,
            TCP_KEEPALIVE_CNT            => Low_Level.ZMQ_TCP_KEEPALIVE_CNT,
            TCP_KEEPALIVE_IDLE           => Low_Level.ZMQ_TCP_KEEPALIVE_IDLE,
            TCP_KEEPALIVE_INTVL          => Low_Level.ZMQ_TCP_KEEPALIVE_INTVL,
            TCP_ACCEPT_FILTER            => Low_Level.ZMQ_TCP_ACCEPT_FILTER,
            DELAY_ATTACH_ON_CONNECT      => Low_Level.ZMQ_DELAY_ATTACH_ON_CONNECT,
            XPUB_VERBOSE                 => Low_Level.ZMQ_XPUB_VERBOSE
           );


   function Img (Item : Ada.Streams.Stream_Element_Array) return String is
      Ret    : String (1 .. Item'Length * 2);
      Cursor : Natural := 1;
      type Map_String is array (Stream_Element (0) ..
                                  Stream_Element (15)) of Character;
      Hex    : constant Map_String := ('0', '1', '2', '3',
                                        '4', '5', '6', '7',
                                        '8', '9', 'A', 'B',
                                        'C', 'D', 'E', 'F');
   begin
      for I in Item'Range loop
         Ret (Cursor) := Hex (Item (I) / 16);
         Cursor := Cursor + 1;
         Ret (Cursor) := Hex (Item (I) mod 16);
         Cursor := Cursor + 1;
      end loop;
      return Ret;
   end Img;

   ----------------
   -- Initialize --
   ----------------
   overriding procedure Initialize
     (This         : in out Socket)
   is
   begin

      This.C := Low_Level.zmq_socket (This.With_Context.GetImpl,
                                      Socket_Type'Pos (This.Kind));
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


   procedure Send (This           : in out Socket;
                   Msg_Addres     : System.Address;
                   Msg_Length     : Natural;
                   Flags          : Socket_Flags := No_Flags) is
      Ret  : int;
   begin
      Ret := Low_Level.zmq_send (This.C, Msg_Addres,
                                 size_t (Msg_Length),
                                 int (Flags));
      if Ret /= 0 then
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
      Msg     : Messages.Message'Class;
      Flags   : Socket_Flags := No_Flags)
   is
      Ret  : int;
   begin
      Ret := Low_Level.zmq_recvmsg (This.C,
                                    Msg.GetImpl,
                                    int (Flags));

      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in "
           & GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Recv;

   procedure Recv (This    : in Socket;
                   Flags   : Socket_Flags := No_Flags) is
      Dummy_Msg : Messages.Message;
   begin
      Dummy_Msg.Initialize;
      This.Recv (Dummy_Msg, Flags);
      Dummy_Msg.Finalize;
   end Recv;


   not overriding
   function Recv (This       : in Socket;
                  Max_Length : Natural := 1024;
                  Flags      : Socket_Flags := No_Flags) return String is
      Buffer : String (1 .. Max_Length);
   begin
      This.Recv (Buffer'Address, Buffer'Length, Flags);

   end Recv;

   procedure Recv (This    : in Socket;
                   Msg     : out Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags) is
   begin
      Msg := Ada.Strings.Unbounded.To_Unbounded_String (This.Recv (Flags));
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
   procedure  Set_IO_Thread_Affinity (This       : in out Socket;
                                      Value      : Natural) is
   begin
      This.Setsockopt (AFFINITY, Value);
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
   procedure  Establish_Message_Filter (This       : in out Socket;
                                        Value      : String) is
   begin
      This.Setsockopt (SUBSCRIBE, Value);
   end Establish_Message_Filter;

   not overriding

   procedure  Establish_Message_Filter
     (This       : in out Socket;
      Value      :  Ada.Streams.Stream_Element_Array) is
   begin
      This.Setsockopt (SUBSCRIBE, Value);
   end Establish_Message_Filter;

   procedure  Establish_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      This.Setsockopt (SUBSCRIBE, To_String (Value));
   end Establish_Message_Filter;

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
   procedure  Set_Multicast_Data_Rate (This       : in out Socket;
                                       Value      : Natural) is
   begin
      This.Setsockopt (RATE, Value);
   end Set_Multicast_Data_Rate;

   not overriding
   procedure  Set_Multicast_Recovery_Interval (This       : in out Socket;
                                               Value      : Duration) is
   begin
      This.Setsockopt (RECOVERY_IVL, Integer (Value));
   end Set_Multicast_Recovery_Interval;
   not overriding

   procedure  Set_Kernel_Transmit_Buffer_Size (This       : in out Socket;
                                               Value      : Natural) is
   begin
      This.Setsockopt (SNDBUF, Value);
   end Set_Kernel_Transmit_Buffer_Size;
   not overriding

   procedure  Set_Kernel_Receive_Buffer_Size (This       : in out Socket;
                                              Value      : Natural) is
   begin
      This.Setsockopt (RCVBUF, Value);
   end Set_Kernel_Receive_Buffer_Size;

   function Get_Impl (This : in Socket) return System.Address is
   begin
      return This.C;
   end Get_Impl;

   -------------

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


   function More_Message_Parts_To_Follow (This : Socket) return Boolean is
   begin
      return Ret : Boolean do
         Ret := This.Getsockopt (RCVMORE);
      end return;
   end More_Message_Parts_To_Follow;

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

end ZMQ.Sockets;
