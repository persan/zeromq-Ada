with Interfaces.C.Strings;
with ZMQ.Low_Level;
package body ZMQ is
   use Interfaces.C;
   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (no : Integer) return String is
      s : constant String := no'Img;
   begin
      return "[" &  s (s'First + 1 .. s'Last) & "] " &
      Interfaces.C.Strings.Value (Low_Level.zmq_strerror (int (no)));
   end Error_Message;

   function Library_Version return Version_Type is
   begin
      return ret : Version_Type do
         ret := (2, 0, 6); --#TODO fetch from library
      end return;
   end Library_Version;

   function image (item : Version_Type) return String is
      s1 : constant String := item.Major'Img;
      s2 : constant String := item.Minor'Img;
      s3 : constant String := item.Patch'Img;
   begin
      return s1 (s1'First + 1 .. s1'Last) & "." &
      s2 (s2'First + 1 .. s2'Last) & "." &
      s3 (s3'First + 1 .. s3'Last);
   end image;
   procedure Validate_Library_Version is
   begin
      null;
   end Validate_Library_Version;

end ZMQ;
