private with Interfaces.C;
package GNATCOLL.uuid is

   type UUID_Variant is (NCS, DCE, MICROSOFT, OTHER);
   type UUID_Type is (DCE_TIME, DCE_RANDOM, DCE_UNDEFINED);

   type UUID is tagged private;

   procedure Clear (this : in out UUID);

   function "<" (l, r : UUID) return Boolean;
   function ">" (l, r : UUID) return Boolean;
   function "=" (l, r : UUID) return Boolean;


   procedure Generate (this : out UUID);
   function Generate return  UUID;

   function Generate_Random return  UUID;
   procedure Generate_Random (this : out UUID);

   function Generate_Time return  UUID;
   procedure Generate_Time (this : out UUID);

   function Is_Null (this : UUID) return Boolean;

   function Parse (data : String) return UUID;

   function Unparse (this : UUID) return String;

   function Unparse_Lower (this : UUID) return String;

   function Unparse_Upper (this : UUID) return String;

   --  function time (arg1 : access uuid;
   --                 arg2 : access bits_time_h.timeval) return time_h.time_t;
   --

   function Get_Type (this : UUID) return UUID_Type;

   function Get_Variant (this : UUID) return UUID_Variant;

   PARSE_ERROR : exception;
private
   type uuid_t is array (0 .. 15) of aliased Interfaces.C.unsigned_char;
   type UUID is tagged record
      data : uuid_t;
   end record;
end gnatcoll.uuid;
