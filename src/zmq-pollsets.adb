package body ZMQ.Pollsets is

   ------------
   -- append --
   ------------

   procedure append (this : in out Pollset; item : pollitem'Class) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "append unimplemented");
      raise Program_Error with "Unimplemented procedure append";
   end append;

   ------------
   -- remove --
   ------------

   procedure remove (this : in out Pollset; item : pollitem'Class) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "remove unimplemented");
      raise Program_Error with "Unimplemented procedure remove";
   end remove;

   ----------
   -- poll --
   ----------

   procedure poll
     (this    : in out Pollset;
      Timeout : Duration)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "poll unimplemented");
      raise Program_Error with "Unimplemented procedure poll";
   end poll;

end ZMQ.Pollsets;
