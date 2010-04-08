with ZMQ.Low_Level;
with Ada.Streams;
with System;
with System.Address_To_Access_Conversions;
with Ada.Strings.Unbounded;

package ZMQ.Messages is
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

   --#TODO


   generic
      type Element (<>) is private;
      with package conv is new System.Address_To_Access_Conversions (Element);
      with procedure free (data : in out conv.Object_Pointer);
   procedure Initialize_Generic (Self   : in out Message;
                                 Data   : conv.Object_Pointer);

   generic
      type Element (<>) is private;
      with package conv is new System.Address_To_Access_Conversions (Element);
      type Hint_Type (<>) is private;
      type Hint_Access is access Hint_Type;
      with procedure free
        (data : in out conv.Object_Pointer; hint : Hint_Access);
   procedure Initialize_Generic_With_Hint (Self  : in out Message;
                                           Data  : conv.Object_Pointer;
                                           Hint  : Hint_Access);



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
