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
     (This        : in out Context;
      App_Threads : Positive;
      IO_Threads  : Positive;
      Flags       : Context_Flags := No_Flags)
   is
   begin
      if This.c /= Null_Address then
         raise ZMQ_Error with "Alredy Initialized";
      end if;
      This.c := Low_Level.zmq_init
        (int (App_Threads), int (IO_Threads), int (Flags));
      if This.c = Null_Address then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;

   overriding
   procedure Initialize (This : in out Context) is
   begin
      This.c := Null_Address;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (This : in out Context)
   is
      rc : int;
   begin
      if This.c /= Null_Address then
         rc := Low_Level.zmq_term (This.c);
         This.c := Null_Address;
         if rc  /= 0 then
            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
            GNAT.Source_Info.Enclosing_Entity;
         end if;
      end if;
   end Finalize;

   function Is_Connected (This : Context) return Boolean is
   begin
      return This.c /= Null_Address;
   end Is_Connected;

   function GetImpl (This : Context) return System.Address is
   begin
      return This.c;
   end GetImpl;

end ZMQ.Contexts;
