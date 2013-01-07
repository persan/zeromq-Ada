package body ZMQ.Sockets.Streams is

   ------------
   -- stream --
   ------------

   function stream (this : Stream_Socket) return Stream_Access is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "stream unimplemented");
      raise Program_Error with "Unimplemented function stream";
      return stream (this);
   end stream;

   ----------
   -- Read --
   ----------

   procedure Read
     (File : Stream_Socket;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset;
      From : Positive_Count)
   is
      pragma Unreferenced (File, Item, Last, From);
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (File : Stream_Socket;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      pragma Unreferenced (File, Item, Last);
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : Stream_Socket;
      Item : Stream_Element_Array;
      To   : Positive_Count)
   is
      pragma Unreferenced (File, Item, To);
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : Stream_Socket;
      Item : Stream_Element_Array)
   is
      pragma Unreferenced (File, Item);
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

end ZMQ.Sockets.Streams;
