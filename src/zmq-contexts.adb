with Interfaces.C;
with ZMQ.Low_Level;
with GNAT.OS_Lib;
with GNAT.Source_Info;
package body ZMQ.Contexts is

   use Interfaces.C;
   use System;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (this        : in out Context;
      app_threads : Positive;
      io_threads  : Positive;
      flags       : Context_Flags := No_Flags)
   is
   begin
      if This.c /= Null_Address then
         raise ZMQ_Error with "Alredy Initialized";
      end if;
      this.c := Low_Level.zmq_init (int (app_threads), int (io_threads), int (flags));
      if this.c = Null_Address then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;

   overriding
   procedure Initialize (this : in out Context) is
   begin
      This.c := Null_Address;
   end;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (this : in out Context)
   is
      rc : int;
   begin
      if This.c /= Null_Address then
         rc := Low_Level.zmq_term (this.c);
         This.c := Null_Address;
         if rc  /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
         end if;
      end if;
   end Finalize;

   function is_Connected (this : Context) return boolean is
   begin
      return This.c /= Null_Address;
   end;

   function getImpl (This : Context) return System.Address is
   begin
      return This.c;
   end getImpl;

end ZMQ.Contexts;
