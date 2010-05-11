------------------------------------------------------------------------------
--                                                                          --
--                             0MQ Ada-binding                              --
--                                                                          --
--                           Z M Q . S O C K E T S                          --
--                                                                          --
--                                  S p e c                                 --
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

with Ada.Streams;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with System;
with ZMQ.Messages;
with ZMQ.Contexts;
package ZMQ.Sockets is

   type Socket_Type is
     (P2P,
      PUB,
      SUB,
      REQ,
      REP,
      XREQ,
      XREP,
      UPSTREAM,
      DOWNSTREAM);

   type Socket is new Ada.Finalization.Limited_Controlled with private;


   type Socket_Flags is mod 2 ** 32;

   function "+" (L, R : Socket_Flags) return Socket_Flags renames "or";
   No_Flags : constant Socket_Flags := 2#0000_0000_0000_0000#;

   not overriding
   procedure Initialize (This         : in out Socket;
                         With_Context : Contexts.Context;
                         Kind         : Socket_Type);

   not overriding
   procedure Bind (This    : in out Socket;
                   Address : String);
   not overriding
   procedure Bind (This    : in out Socket;
                   Address : Ada.Strings.Unbounded.Unbounded_String);


   not overriding
   procedure  setsockopt_HWM (This       : in out Socket;
                              Value      : Natural);
   not overriding
   procedure  setsockopt_LWM (This       : in out Socket;
                              Value      : Natural);
   not overriding
   procedure  setsockopt_SWAP (This       : in out Socket;
                               Value      : Boolean);
   not overriding
   procedure  setsockopt_AFFINITY (This       : in out Socket;
                                   Value      : Natural);
   not overriding
   procedure  setsockopt_IDENTITY (This       : in out Socket;
                                   Value      : Natural);
   not overriding
   procedure  setsockopt_SUBSCRIBE (This       : in out Socket;
                                    Value      : String);
   not overriding
   procedure  setsockopt_SUBSCRIBE
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String);
   not overriding
   procedure  setsockopt_UNSUBSCRIBE (This       : in out Socket;
                                      Value      : String);
   not overriding
   procedure  setsockopt_UNSUBSCRIBE
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String);
   not overriding

   procedure  setsockopt_RATE (This       : in out Socket;
                               Value      : Natural);

   not overriding
   procedure  setsockopt_RECOVERY_IVL (This       : in out Socket;
                                       Value      : Natural);
   not overriding
   procedure  setsockopt_MCAST_LOOP (This       : in out Socket;
                                     Value      : Natural);
   not overriding
   procedure  setsockopt_SNDBUF (This       : in out Socket;
                                 Value      : Natural);
   not overriding
   procedure  setsockopt_RCVBUF (This       : in out Socket;
                                 Value      : Natural);



   not overriding
   procedure Connect (This    : in out Socket;
                      Address : String);

   procedure Connect (This    : in out Socket;
                      Address : Ada.Strings.Unbounded.Unbounded_String);



   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Messages.Message'Class;
                   Flags   : Socket_Flags := No_Flags);

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : String;
                   Flags   : Socket_Flags := No_Flags);

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags);

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Ada.Streams.Stream_Element_Array;
                   Flags   : Socket_Flags := No_Flags);

   not overriding
   procedure Send (This           : in out Socket;
                   Msg_Addres     : System.Address;
                   Msg_Length     : Natural;
                   Flags          : Socket_Flags := No_Flags);


   --  Creates a Message and sends it over the socket.

   generic
      type Element is private;
   procedure Send_Generic (This    : in out Socket;
                           Msg     : Element;
                           Flags   : Socket_Flags := No_Flags);

   --  Send the message over the socet

--     not overriding
--     procedure flush (This    : in out Socket);

   not overriding
   procedure recv (This    : in Socket;
                   Msg     : Messages.Message'Class;
                   Flags   : Socket_Flags := No_Flags);

   procedure recv (This    : in Socket;
                   msg     : out Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags);


   not overriding
   function recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags) return String;

   not overriding
   function recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags)
                  return Ada.Strings.Unbounded.Unbounded_String;

   overriding
   procedure Finalize (this : in out Socket);


   --  function "=" (Left, Right : in Context) return Boolean;

private
   type Socket is new Ada.Finalization.Limited_Controlled with record
      c : System.Address := System.Null_Address;
   end record;
   function img (item : Ada.Streams.Stream_Element_Array) return String;

      type Socket_Opt is
     (HWM,   -- Set high water mark
      LWM,   -- Set low water mark
      SWAP,
      AFFINITY,
      IDENTITY,
      SUBSCRIBE,
      UNSUBSCRIBE,
      RATE,
      RECOVERY_IVL,
      MCAST_LOOP,
      SNDBUF,
      RCVBUF);
   not overriding

   procedure  setsockopt (This    : in out Socket;
                          Option  : Socket_Opt;
                          Value   : String);
   not overriding
   procedure  setsockopt (This    : in out Socket;
                          Option  : Socket_Opt;
                          Value   : Boolean);
   not overriding
   procedure  setsockopt (This    : in out Socket;
                          Option  : Socket_Opt;
                          Value   : Natural);
   not overriding
   procedure setsockopt
     (This    : in out Socket;
      Option  : Socket_Opt;
      Value   : Ada.Streams.Stream_Element_Array);

   not overriding
   procedure  setsockopt (This       : in out Socket;
                          Option     : Socket_Opt;
                          Value      : System.Address;
                          Value_Size : Natural);




end ZMQ.Sockets;
