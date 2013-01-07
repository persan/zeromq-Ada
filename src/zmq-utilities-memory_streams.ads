-------------------------------------------------------------------------------
--                                                                           --
--  Copyright 2007 Per Sandberg <per.sandberg@bredband.net>                  --
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


with Ada.Streams;
with System;
with Ada.Finalization;
package ZMQ.Utilities.Memory_Streams is
   use Ada;
   --  Memory_Streams implements stream functionality to be mapped to any
   --  memory location sample of use
   --     declare
   --        Real_Buffer : String (1 .. 1024) := (others => '#');
   --        S           : aliased Memory_Stream;
   --     begin
   --        S.Set_Address (Real_Buffer'Address);
   --        S.Set_Length (Real_Buffer'Length);
   --
   --        String'Write (S'Access, "bulle");
   --        Integer'Write (S'Access, 123);
   --
   --        Memory_Stream'Write(Some_UDP_Stream, S);
   --        -- Write the whole serialized buffer in one transaction.
   --     end;

   type Memory_Stream_Interface is limited interface;
   type Any_Memory_Stream_Interface is access
     all Memory_Stream_Interface'Class;
   --
   procedure Read
     (This : in out Memory_Stream_Interface;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is abstract;

   procedure Write
     (This : in out Memory_Stream_Interface;
      Item   : in Ada.Streams.Stream_Element_Array) is abstract;

   function Get_Address
     (This : in Memory_Stream_Interface) return System.Address is abstract;
   --  Returns the Address to the real buffer

   procedure Set_Address
     (This : in out Memory_Stream_Interface;
      To   :  in System.Address) is abstract;
   --  Sets the address to the real buffer.

   function Get_Length
     (This : in Memory_Stream_Interface)
      return Ada.Streams.Stream_Element_Count is abstract;
   --  Returns the full length of the buffer.

   procedure Set_Length
     (This : in out Memory_Stream_Interface;
      To   : in Ada.Streams.Stream_Element_Count) is abstract;
   --  Sets the full length of the buffer.

   procedure Reset (This : in out Memory_Stream_Interface) is abstract;
   --  moves the stream position to the first element in the stream.

   procedure Seek (This : in out Memory_Stream_Interface;
                   Pos  : in Ada.Streams.Stream_Element_Offset) is abstract;
   --  Moves the stream position  forward or backward in the message stream.
   --  Sample:
   --  move to the 10 element in the message.
   --   Msg.Reset;
   --   Msg.Seek(10);

   function Pos (This : in Memory_Stream_Interface)
                 return  Ada.Streams.Stream_Element_Offset is abstract;
   --  Returns the current cursor in the buffer
   function Eof (This : in Memory_Stream_Interface) return Boolean is abstract;

   procedure Dump
     (This        : in Memory_Stream_Interface;
      Full_Buffer : in Boolean := False) is null;
   --  Dumps the contents of the buffer from the first element
   --  to the cursor.

   type Memory_Stream is limited new Ada.Streams.Root_Stream_Type
     and Memory_Stream_Interface
   with private;

   type Any_Memory_Stream is access all Memory_Stream'Class;

   overriding
   procedure Set_Address
     (This : in out Memory_Stream; To :  in System.Address);
   --  Sets the address to the real buffer.
   overriding
   function Get_Address
     (This : in Memory_Stream) return System.Address;

   overriding
   function Get_Length (This : in Memory_Stream)
                                   return Ada.Streams.Stream_Element_Count;
   --  Returns the full length of the buffer.

   overriding
   procedure Set_Length (This : in out Memory_Stream;
                         To   : in Ada.Streams.Stream_Element_Count);
   --  Sets the full length of the buffer.

   overriding
   procedure Reset (This : in out Memory_Stream);
   --  moves the stream position to the first element in the stream.

   overriding
   procedure Seek (This : in out Memory_Stream;
                   Pos  : in Ada.Streams.Stream_Element_Offset);
   --  Moves the stream position  forward or backward in the message stream.
   --  Sample:
   --  move to the 10 element in the message.
   --   Msg.Reset;
   --   Msg.Seek(10);

   overriding
   function Pos (This : in Memory_Stream)
                 return  Ada.Streams.Stream_Element_Offset;
   --  Returns the current cursor in the buffer

   overriding
   function Eof (This : in Memory_Stream) return Boolean;

   overriding
   procedure Dump
     (This      : in Memory_Stream;
      Full_Buffer : in Boolean := False);
   --  Dumps the contents of the buffer from the first element
   --  to the cursor.

   overriding procedure Read
     (This : in out Memory_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (This : in out Memory_Stream;
      Item   : in Ada.Streams.Stream_Element_Array);


   type Expand_Strategy is (As_Needed,
                            Multiply_By_Two,
                            Add_Initial_Size);
   type Dynamic_Memory_Stream
     (Initial_Size : Ada.Streams.Stream_Element_Offset;
      Strategy     : Expand_Strategy) is new Memory_Stream with private;

   overriding procedure Write
     (This : in out Dynamic_Memory_Stream;
      Item   : in Ada.Streams.Stream_Element_Array);


private
   subtype large_buffer is
     Streams.Stream_Element_Array (1 .. Streams.Stream_Element_Offset'Last);
   type Large_Buffer_Access is access large_buffer;
   for Large_Buffer_Access'Storage_Size use 0;

   type Large_Buffer_Union (ref  : Boolean := False) is record
      case ref is
      when True =>
         As_Address : System.Address;
      when False =>
         As_Pointer : Large_Buffer_Access;
      end case;
   end record;
   pragma Unchecked_Union (Large_Buffer_Union);

   type Memory_Stream is limited new Streams.Root_Stream_Type
     and Memory_Stream_Interface
   with record
      Buffer        : Large_Buffer_Union;
      Buffer_Length : Streams.Stream_Element_Count  := 0;
      Cursor        : Streams.Stream_Element_Offset := 0;
   end record;

   procedure Read
     (This : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Memory_Stream);
   for Memory_Stream'Read use Read;

   procedure Write
     (This : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Memory_Stream);
   for Memory_Stream'Write use Write;

   type controler (controled : not null access Dynamic_Memory_Stream)
     is new Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (This : in out controler);
   procedure Finalize   (This : in out controler);

   type Dynamic_Memory_Stream
     (Initial_Size : Streams.Stream_Element_Offset;
      Strategy     : Expand_Strategy) is new Memory_Stream with record
      C : controler (Dynamic_Memory_Stream'Access);
   end record;

   procedure Initialize (This : in out Dynamic_Memory_Stream);
   procedure Finalize   (This : in out Dynamic_Memory_Stream);

   procedure Read
     (This : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Dynamic_Memory_Stream);
   for Dynamic_Memory_Stream'Read use Read;

   procedure Write
     (This : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Dynamic_Memory_Stream);
   for Dynamic_Memory_Stream'Write use Write;
   procedure Expand
     (This : in out Dynamic_Memory_Stream;
      to_Size : Ada.Streams.Stream_Element_Offset);

end ZMQ.Utilities.Memory_Streams;
