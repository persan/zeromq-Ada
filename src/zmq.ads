package ZMQ is

   ZMQ_Error : exception;
   type Version_Type is record
      Major : Natural;
      Minor : Natural;
      Patch : Natural;
   end record;

   Binding_Version : constant Version_Type := (0, 0, 1);
   function Library_Version return Version_Type;

   function image (item : Version_Type) return string;

private
   pragma Linker_Options ("-lzmq");
   function Error_Message (no : integer) return string;
   procedure Validate_Library_Version;
   --  Raiese ZMQ_Error if the underlaying library isent a valid version
end ZMQ;
