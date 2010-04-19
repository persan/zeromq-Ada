------------------------------------------------------------------------------
--                                                                          --
--  Copyright 2007 Per Sandberg <per.sandberg@bredband.net>                 --
--                                                                          --
--  This package is free software; you can redistribute it and/or           --
--  modify it under terms of the GNU General Public License as              --
--  published by the Free Software Foundation; either version 2, or         --
--  (at your option) any later version. This package is distributed in      --
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without      --
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A           --
--  PARTICULAR PURPOSE. See the GNU General Public License for more         --
--  details. You should have received a copy of the GNU General Public      --
--  License distributed with this package; see file COPYING.  If not,       --
--  write to the Free Software Foundation, 59 Temple Place - Suite          --
--  330, Boston, MA 02111-1307, USA.                                        --
--                                                                          --
--  As a special exception, if other files instantiate generics from        --
--  this unit, or you link this unit with other files to produce an         --
--  executable, this unit does not by itself cause the resulting            --
--  executable to be covered by the GNU General Public License.  This       --
--  exception does not however invalidate any other reasons why the         --
--  executable file might be covered by the GNU Public License.             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with System;
with Ada.Text_IO;
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
   --        S.Reset;
   --        String'Write (S'Access, "bulle");
   --        S.Dump;
   --     end;

   type Memory_Stream_Interface is limited interface;
   type Any_Memory_Stream_Interface is access
     all Memory_Stream_Interface'Class;
   --
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
      Full_Buffer : in Boolean := False;
      Outf        : in Text_IO.File_Access := Text_IO.Standard_Output) is null;
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
     (This   : in Memory_Stream;
      Full_Buffer : in Boolean := False;
      Outf        : in Ada.Text_IO.File_Access := Ada.Text_IO.Standard_Output);
   --  Dumps the contents of the buffer from the first element
   --  to the cursor.

   overriding procedure Read
     (Stream : in out Memory_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Memory_Stream;
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

end ZMQ.Utilities.Memory_Streams;
