with Ada.Finalization;
with System;
package ZMQ.Contexts is

   type Context is new Ada.Finalization.Limited_Controlled with private;
   type Any_Context is access all Context'Class;

   type Context_Flags is mod 2 ** 32;

   function "+" (L, R : Context_Flags) return Context_Flags renames "or";

   No_Flags : constant Context_Flags := 0;

   not overriding
   procedure Initialize (this : in out Context;
                         app_threads : Positive;
                         io_threads  : Positive;
                         flags       : Context_Flags := No_Flags);
   overriding
   procedure Initialize (this : in out Context);

   overriding
   procedure Finalize (this : in out Context);

   function is_Connected (this : Context) return boolean;

   function getImpl (This : Context) return System.Address;
private
   type Context is new Ada.Finalization.Limited_Controlled with record
      c : System.Address := System.Null_Address;
   end record;
end ZMQ.Contexts;
