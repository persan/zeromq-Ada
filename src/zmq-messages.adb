-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . M E S S A G E S                          --
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


with Interfaces.C;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;
package body ZMQ.Messages is
   use Interfaces.C;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Message) is
      Ret : int;
   begin
      Ret := Low_Level.zmq_msg_init (Self.Msg'Access);
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;


   --  ========================================================================
   --  Initialize with size
   --  ========================================================================

   procedure Initialize (Self : in out Message; Size : Natural) is
      Ret : int;
   begin
      Ret := Low_Level.zmq_msg_init_size (Self.Msg'Access, size_t (Size));
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;

   procedure Initialize
     (Self : in out Message;
      Size : Ada.Streams.Stream_Element_Offset)
   is
   begin
      Self.Initialize (Natural (Size));
   end Initialize;

   --  ========================================================================
   --  Initialize with data
   --  ========================================================================
   procedure Initialize (Self : in out Message;
                         Data : System.Address;
                         Size : Natural) is
      type Data_Type is new String (1 .. Size);
      package Conv is new System.Address_To_Access_Conversions (Data_Type);
   begin
      Self.Initialize (Size);
      Conv.To_Pointer ((Self.GetData)).all := Conv.To_Pointer (Data).all;
   end Initialize;

   procedure Initialize (Self : in out Message; Data : String) is
   begin
      Self.Initialize (Data'Address, Data'Length);
   end Initialize;

   procedure Initialize
     (Self : in out Message;
      Data : Ada.Streams.Stream_Element_Array) is
   begin
      Self.Initialize (Data'Address, Data'Length);
   end Initialize;

   --  ========================================================================
   --  Initialize with data ref and free
   --  ========================================================================
   procedure Initialize
     (Self    : in out Message;
      Message : System.Address;
      Size    : Natural;
      Free    : Free_Proc;
      Hint    : System.Address := System.Null_Address)
   is
      Ret : int;
   begin
      Ret := Low_Level.zmq_msg_init_data (Self.Msg'Access,
                                          Message,
                                          size_t (Size),
                                          Free,
                                          Hint);
      if Ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;


   ------------------------
   -- Initialize_Generic --
   ------------------------

   procedure Initialize_Generic
     (Self   : in out Message;
      Data   : Element_Access)
   is
      function Conv is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Element_Access);
      procedure Internal_Free (Data : System.Address; Hint : System.Address);
      procedure Internal_Free (Data : System.Address; Hint : System.Address) is
         pragma Unreferenced (Hint);
         Temp  : Element_Access := Conv (Data);
      begin
         Free (Temp);
      end Internal_Free;
   begin
      Self.Initialize (Data.all'Address,
                       Data.all'Size / 8,
                       Internal_Free'Unrestricted_Access);
   end Initialize_Generic;

   ----------------------------------
   -- Initialize_Generic_With_Hint --
   ----------------------------------

   procedure Initialize_Generic_With_Hint
     (Self  : in out Message;
      Data  : Element_Access;
      Hint  : Hint_Access)
   is
      function ConvHint is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Hint_Access);
      function Conv is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Element_Access);

      procedure Internal_Free (Data : System.Address; Hint : System.Address);
      procedure Internal_Free (Data : System.Address; Hint : System.Address) is
         Temp_Data  : Element_Access := Conv (Data);
         Temp_Hint  : constant Hint_Access := ConvHint (Hint);
      begin
         Free (Temp_Data, Temp_Hint);
      end Internal_Free;
   begin
      Self.Initialize (Data.all'Address,
                       Data.all'Size / 8,
                       Internal_Free'Unrestricted_Access,
                       Hint.all'Address);
   end Initialize_Generic_With_Hint;


   --  ========================================================================
   --  GetData / GetSize
   --  ========================================================================

   ---------------
   --  getData  --
   ---------------
   function GetData (Self : Message) return System.Address is
   begin
      return Low_Level.zmq_msg_data (Self.Msg'Unrestricted_Access);
   end GetData;


   ---------------
   --  getData  --
   ---------------
   function GetData (Self : Message) return String is
      type Data_Type is new String (1 .. Self.GetSize);
      package Conv is new System.Address_To_Access_Conversions (Data_Type);
   begin
      return String (Conv.To_Pointer (Self.GetData).all);
   end GetData;

   ---------------
   --  getData  --
   ---------------
   function  GetData  (Self : Message)
                       return Ada.Streams.Stream_Element_Array is
      type Data_Type is new Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (Self.GetSize));
      package Conv is new System.Address_To_Access_Conversions (Data_Type);
   begin
      return Ada.Streams.Stream_Element_Array
        (Conv.To_Pointer (Self.GetData).all);
   end GetData;

   ---------------
   --  getData  --
   ---------------
   function  GetData  (Self : Message)
                       return Ada.Strings.Unbounded.Unbounded_String is
      type Data_Type is new String (1 .. Self.GetSize);
      package Conv is new System.Address_To_Access_Conversions (Data_Type);
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String
        (String (Conv.To_Pointer (Self.GetData).all));
   end GetData;

   -----------------------
   --  getData_Generic  --
   -----------------------
   function  GetData_Generic  (Self : Message)
                               return Element is
      package Conv is new System.Address_To_Access_Conversions (Element);
   begin
      return Conv.To_Pointer (Self.GetData).all;
   end GetData_Generic;


   ---------------
   --  getSize  --
   ---------------
   function GetSize (Self : Message) return Natural is
   begin
      return Natural (Low_Level.zmq_msg_size (Self.Msg'Unrestricted_Access));
   end GetSize;


   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Message) is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_close (Self.Msg'Access);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Finalize;

   -------------
   -- getImpl --
   -------------

   function GetImpl (Self : Message) return not null Zmq_Msg_T_Access is
   begin
      return Self.Msg'Unrestricted_Access;
   end GetImpl;

   procedure Process_Data_Generic
     (Self   : Message;
      Handle : access procedure (Item : Element)) is
      package Conv is new System.Address_To_Access_Conversions (Element);
   begin
      Handle (Conv.To_Pointer (Self.GetData).all);
   end Process_Data_Generic;

end ZMQ.Messages;
