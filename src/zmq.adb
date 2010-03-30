with interfaces.C.Strings;
with ZMQ.Low_Level;
package body ZMQ is
   use interfaces.C;
   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (no : integer) return string is
      s : constant String := no'Img;
   begin
      return "[" &  S (S'FIRST + 1 .. S'Last) & "] " & interfaces.C.Strings.value (Low_Level.zmq_strerror (int (no)));
   end;


end ZMQ;
