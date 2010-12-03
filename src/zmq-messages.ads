------------------------------------------------------------------------------
--                                                                          --
--                             0MQ Ada-binding                              --
--                                                                          --
--                          Z M Q . M E S S A G E S                         --
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

with ZMQ.Low_Level;
with Ada.Streams;
with System;
with Ada.Strings.Unbounded;

package ZMQ.Messages is

   --  A ØMQ message is a discrete unit of data passed between applications
   --  or components of the same application.
   --  ØMQ messages have no internal structure and from the point of view
   --  of ØMQ itself they are considered to be opaque binary data.


   type Message is tagged limited private;


   procedure Initialize (Self : in out Message);
   --  Initialize empty 0MQ message
   --  Initialize the message object referenced by msg to represent
   --  an empty message.
   --  This function is most useful when called before receiving a message.

   procedure Initialize (Self : in out Message; Size : Natural);
   procedure Initialize
     (Self : in out Message; Size : Ada.Streams.Stream_Element_Offset);
   pragma Inline (Initialize);
   --  Initialize 0MQ message of a specified size
   --  Allocate resources required to store a message size bytes long
   --  and initialize the message object referenced by msg to represent
   --  the newly allocated message.
   --  The Allocated message area is not not cleared.

   procedure Initialize (Self : in out Message;
                         Data : String);
   procedure Initialize (Self : in out Message;
                         Data : Ada.Streams.Stream_Element_Array);
   pragma Inline (Initialize);
   procedure Initialize (Self : in out Message;
                         Data : System.Address;
                         Size : Natural);
   pragma Inline (Initialize);
   --  Initialize 0MQ message of a specified size
   --  Allocate resources required to store the data
   --  and initialize the message object referenced by msg to represent
   --  the newly allocated message.
   --  The Data is copied into the message.


   type Free_Proc is not null access
     procedure (data : System.Address; Hint : System.Address);
   procedure Null_Free  (data : System.Address; Hint : System.Address) is null;
   procedure Initialize (Self    : in out Message;
                         Message : System.Address;
                         Size    : Natural;
                         Free    : Free_Proc;
                         Hint    : System.Address := System.Null_Address);
   --  initialise 0MQ message from a supplied buffer


   generic
      type Element is private;
      type Element_Access is access Element;
      with procedure free (data : in out Element_Access);
   procedure Initialize_Generic (Self   : in out Message;
                                 Data   : Element_Access);


   generic
      type Element is private;
      type Element_Access is access Element;
      type Hint_Type is private;
      type Hint_Access is access Hint_Type;
      with procedure free
        (data : in out Element_Access; hint : Hint_Access);
   procedure Initialize_Generic_With_Hint (Self  : in out Message;
                                           Data  : Element_Access;
                                           Hint  : Hint_Access);
   --


   procedure Finalize   (Self : in out Message);

   type zmq_msg_t_Access is access all ZMQ.Low_Level.zmq_msg_t;
   function getImpl (Self : Message) return not null zmq_msg_t_Access;


   function  getData  (Self : Message)
                       return String;

   function  getData  (Self : Message)
                       return Ada.Streams.Stream_Element_Array;

   function  getData  (Self : Message)
                       return Ada.Strings.Unbounded.Unbounded_String;

   generic
      type Element is private;
   function  getData_Generic  (Self : Message)
                               return Element;

   function getData (Self : Message) return System.Address;
   function getSize (Self : Message) return Natural;


private
   type Message is tagged limited record
      Msg            : aliased ZMQ.Low_Level.zmq_msg_t;
   end record;
end ZMQ.Messages;
