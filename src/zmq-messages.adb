------------------------------------------------------------------------------
--                                                                          --
--                             0MQ Ada-binding                              --
--                                                                          --
--                          Z M Q . M E S S A G E S                         --
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
      ret : int;
   begin
      ret := Low_Level.zmq_msg_init (Self.Msg'Access);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;

   --  ========================================================================
   --  Initialize with size
   --  ========================================================================

   procedure Initialize (Self : in out Message; Size : Natural) is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_init_size (Self.Msg'Access, size_t (Size));
      if ret /= 0 then
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
      type data_Type is new String (1 .. Size);
      package conv is new System.Address_To_Access_Conversions (data_Type);
   begin
      Self.Initialize (Size);
      conv.To_Pointer ((Self.getData)).all := conv.To_Pointer (Data).all;
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
      ret : int;
   begin
      ret := Low_Level.zmq_msg_init_data (Self.Msg'Access,
                                          Message,
                                          size_t (Size),
                                          Free,
                                          Hint);
      if ret /= 0 then
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
      function conv is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Element_Access);
      procedure Internal_free (Data : System.Address; Hint : System.Address);
      procedure Internal_free (Data : System.Address; Hint : System.Address) is
         pragma Unreferenced (Hint);
         temp  : Element_Access := conv (Data);
      begin
         free (temp);
      end Internal_free;
   begin
      Self.Initialize (Data.all'Address,
                       Data.all'Size / 8,
                       Internal_free'Unrestricted_Access);
   end Initialize_Generic;

   ----------------------------------
   -- Initialize_Generic_With_Hint --
   ----------------------------------

   procedure Initialize_Generic_With_Hint
     (Self  : in out Message;
      Data  : Element_Access;
      Hint  : Hint_Access)
   is
      function convHint is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Hint_Access);
      function conv is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Element_Access);

      procedure Internal_free (Data : System.Address; Hint : System.Address);
      procedure Internal_free (Data : System.Address; Hint : System.Address) is
         Temp_Data  : Element_Access := conv (Data);
         Temp_Hint  : constant Hint_Access := convHint (Hint);
      begin
         free (Temp_Data, Temp_Hint);
      end Internal_free;
   begin
      Self.Initialize (Data.all'Address,
                       Data.all'Size / 8,
                       Internal_free'Unrestricted_Access,
                       Hint.all'Address);
   end Initialize_Generic_With_Hint;


   --  ========================================================================
   --  GetData / GetSize
   --  ========================================================================

   ---------------
   --  getData  --
   ---------------
   function getData (Self : Message) return System.Address is
   begin
      return Low_Level.zmq_msg_data (Self.Msg'Unrestricted_Access);
   end getData;


   ---------------
   --  getData  --
   ---------------
   function getData (Self : Message) return String is
      type data_type is new String (1 .. Self.getSize);
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      return String (conv.To_Pointer (Self.getData).all);
   end getData;

   ---------------
   --  getData  --
   ---------------
   function  getData  (Self : Message)
                       return Ada.Streams.Stream_Element_Array is
      type data_type is new Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (Self.getSize));
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      return Ada.Streams.Stream_Element_Array
        (conv.To_Pointer (Self.getData).all);
   end getData;

   ---------------
   --  getData  --
   ---------------
   function  getData  (Self : Message)
                       return Ada.Strings.Unbounded.Unbounded_String is
      type data_type is new String (1 .. Self.getSize);
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String
        (String (conv.To_Pointer (Self.getData).all));
   end getData;

   -----------------------
   --  getData_Generic  --
   -----------------------
   function  getData_Generic  (Self : Message)
                               return Element is
      package conv is new System.Address_To_Access_Conversions (Element);
   begin
      return conv.To_Pointer (Self.getData).all;
   end getData_Generic;


   ---------------
   --  getSize  --
   ---------------
   function getSize (Self : Message) return Natural is
   begin
      return Natural (Low_Level.zmq_msg_size (Self.Msg'Unrestricted_Access));
   end getSize;


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

   function getImpl (Self : Message) return not null zmq_msg_t_Access is
   begin
      return Self.Msg'Unrestricted_Access;
   end getImpl;

   procedure process_data_generic
     (Self   : Message;
      Handle : access procedure (item : Element)) is
      package conv is new System.Address_To_Access_Conversions (Element);
   begin
      Handle (conv.To_Pointer (Self.getData).all);
   end process_data_generic;

end ZMQ.Messages;
