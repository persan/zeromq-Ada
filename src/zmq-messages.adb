with Interfaces.C;
with GNAT.OS_Lib;
with GNAT.Source_Info;
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
      ret := Low_Level.zmq_msg_init_size (Self.Msg'Access, Size);
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
                                          Size,
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
      Data   : conv.Object_Pointer)
   is
   begin
      Self.Initialize;
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning
        (True, "Initialize_Generic unimplemented");
      raise Program_Error with "Unimplemented procedure Initialize_Generic";
   end Initialize_Generic;

   ----------------------------------
   -- Initialize_Generic_With_Hint --
   ----------------------------------

   procedure Initialize_Generic_With_Hint
     (Self  : in out Message;
      Data  : conv.Object_Pointer;
      Hint  : Hint_Access)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning
        (True, "Initialize_Generic_With_Hint unimplemented");
      raise Program_Error with
        "Unimplemented procedure Initialize_Generic_With_Hint";
   end Initialize_Generic_With_Hint;


   --  ========================================================================
   --  GetData / GetSize
   --  ========================================================================

   function getData (Self : Message) return System.Address is
   begin
      return Low_Level.zmq_msg_data (Self.Msg'Unrestricted_Access);
   end getData;


   function getData (Self : Message) return String is
      type data_type is new String (1 .. Self.getSize);
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      return String (conv.To_Pointer (Self.getData).all);
   end getData;

   function  getData  (Self : Message)
                       return Ada.Streams.Stream_Element_Array is
      type data_type is new Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (Self.getSize));
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      return Ada.Streams.Stream_Element_Array
        (conv.To_Pointer (Self.getData).all);
   end getData;

   function  getData  (Self : Message)
                       return Ada.Strings.Unbounded.Unbounded_String is
      type data_type is new String (1 .. Self.getSize);
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String
        (String (conv.To_Pointer (Self.getData).all));
   end getData;

   function  getData_Generic  (Self : Message)
                               return Element is
      package conv is new System.Address_To_Access_Conversions (Element);
   begin
      return conv.To_Pointer (Self.getData).all;
   end getData_Generic;


   function getSize (Self : Message) return Natural is
   begin
      return Low_Level.zmq_msg_size (Self.Msg'Unrestricted_Access);
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

end ZMQ.Messages;
