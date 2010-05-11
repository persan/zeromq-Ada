private with Interfaces.C;
package uuid is
   pragma Pure;
   type UUID_Variant is (NCS, DCE, MICROSOFT, OTHER);
   type UUID_Type is (DCE_TIME, DCE_RANDOM);

   type UUID is tagged private;

   procedure Clear (arg1 : in out UUID);

   function "<" (l, r : UUID) return Boolean;
   function ">" (l, r : UUID) return Boolean;
   function "=" (l, r : UUID) return Boolean;


   function Generate return  UUID;

   function Generate_Random return  UUID;

   function Generate_Time return  UUID;

   function Is_Null (arg1 : UUID) return Boolean;

   function Parse (arg2 : String) return UUID;

   function Unparse (arg1 : access UUID) return String;

   function Unparse_Lower (arg1 : access UUID) return String;

   function Unparse_Upper (arg1 : access UUID) return String;

   --  function time (arg1 : access uuid;
   --                 arg2 : access bits_time_h.timeval) return time_h.time_t;
   --

   function Get_Type (arg1 : access UUID) return UUID_Type;

   function Get_Variant (arg1 : UUID) return UUID_Variant;


private
   type uuid_t is array (0 .. 15) of aliased Interfaces.C.unsigned_char;
   type UUID is tagged record
      data : aliased uuid_t;
   end record;
end uuid;
