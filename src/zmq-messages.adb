with Interfaces.C;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with System.Address_To_Access_Conversions;
package body ZMQ.Messages is
   use Interfaces.C;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Message) is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_init (Self.msg'Access);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;

   --  =========================================================================
   --  Initialize with size
   --  =========================================================================

   procedure Initialize (Self : in out Message; Size : Natural) is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_init_size (Self.msg'Access, size);
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
      self.Initialize (natural (size));
   end Initialize;

   --  =========================================================================
   --  Initialize with data
   --  =========================================================================
   procedure Initialize (Self : in out Message;
                         Data : System.Address;
                         Size : Natural) is
      type data_Type is new string (1 .. Size);
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      self.Initialize (size);
      conv.to_pointer ((self.getData)).all := conv.to_pointer (Data).all;
   end;

   procedure Initialize (Self : in out Message; Data : String) is
   begin
      self.Initialize (Data'Address, Data'length);
   end Initialize;

   procedure Initialize
     (Self : in out Message;
      Data : Ada.Streams.Stream_Element_Array) is
   begin
      self.Initialize (Data'Address, Data'length);
   end Initialize;

   --  =========================================================================
   --  Initialize with data ref and free
   --  =========================================================================
   procedure Initialize
     (Self    : in out Message;
      Message : System.Address;
      Size    : Natural;
      Free    : Free_Proc;
      Hint    : System.Address := System.Null_Address)
   is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_init_data (Self.msg'Access,
                                          Message,
                                          Size,
                                          Free,
                                          hint);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;
   --#TODO implement
   --     ------------------------
   --     -- Initialize_Generic --
   --     ------------------------
   --
   --     procedure Initialize_Generic
   --       (Self   : in out Message;
   --        Data   : conv.Object_Pointer)
   --     is
   --     begin
   --        self.Initialize;
   --        --  Generated stub: replace with real body!
   --        pragma Compile_Time_Warning (True, "Initialize_Generic unimplemented");
   --        raise Program_Error with "Unimplemented procedure Initialize_Generic";
   --     end Initialize_Generic;
   --
   --     ----------------------------------
   --     -- Initialize_Generic_With_Hint --
   --     ----------------------------------
   --
   --     procedure Initialize_Generic_With_Hint
   --       (Self  : in out Message;
   --                                             Data  : conv.Object_Pointer;
   --        Hint  : Hint_Access)
   --     is
   --     begin
   --        --  Generated stub: replace with real body!
   --        pragma Compile_Time_Warning (True, "Initialize_Generic_With_Hint unimplemented");
   --        raise Program_Error with "Unimplemented procedure Initialize_Generic_With_Hint";
   --     end Initialize_Generic_With_Hint;


   --  =========================================================================
   --  GetData / GetSize
   --  =========================================================================

   function getData (Self : Message) return System.Address is
   begin
      return Low_Level.zmq_msg_data (self.Msg'Unrestricted_Access);
   end getData;


   function getData (Self : Message) return String is
      type data_type is new string (1 .. self.getSize);
      package conv is new System.Address_To_Access_Conversions (data_type);

   begin
      return String (conv.to_pointer (self.getData).all);
   end getData;

   function getSize (Self : Message) return Natural is
   begin
      return Low_Level.zmq_msg_size (self.Msg'Unrestricted_Access);
   end getSize;


   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Message) is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_close (Self.msg'Access);
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
      return self.Msg'Unrestricted_Access;
   end getImpl;

end ZMQ.Messages;
