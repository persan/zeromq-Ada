with Ada.Finalization;
with System;
package ZMQ.Contexts is

   type Context is new Ada.Finalization.Limited_Controlled with private;
   type Any_Context is access all Context'Class;

   type Context_Flags is mod 2 ** 32;

   function "+" (L, R : Context_Flags) return Context_Flags renames "or";

   No_Flags : constant Context_Flags := 0;

   not overriding
   procedure Initialize (This : in out Context;
                         App_Threads : Positive;
                         IO_Threads  : Positive;
                         Flags       : Context_Flags := No_Flags);
   overriding
   procedure Initialize (This : in out Context);

   overriding
   procedure Finalize (This : in out Context);

   function Is_Connected (This : Context) return Boolean;

   function GetImpl (This : Context) return System.Address;
private
   type Context is new Ada.Finalization.Limited_Controlled with record
      c : System.Address := System.Null_Address;
   end record;
end ZMQ.Contexts;
