package ZMQ is

   ZMQ_Error : exception;
private
   pragma Linker_Options ("-lzmq");
   function Error_Message (no : integer) return string;
end ZMQ;
