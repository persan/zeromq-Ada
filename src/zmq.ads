package ZMQ is

   ZMQ_Error : exception;
   type Version_Type is record
      Major : Natural;
      Minor : Natural;
      Patch : Natural;
   end record;

   Binding_Version : constant Version_Type := (0, 0, 2);
   function Library_Version return Version_Type;

private
   pragma Linker_Options ("-lzmq");
   function Error_Message (no : integer) return string;
end ZMQ;
