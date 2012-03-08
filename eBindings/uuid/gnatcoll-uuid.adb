with System;
package body gnatcoll.uuid is
   use Interfaces.C;
   package bits_types_h is

      type uu_timer_t is new System.Address;  -- types.h:161:27
   end bits_types_h;

   package bits_time_h is


      CLOCKS_PER_SEC : constant := 1000000;  --  time.h:34

      CLOCK_REALTIME : constant := 0;  --  time.h:46

      CLOCK_MONOTONIC : constant := 1;  --  time.h:48

      CLOCK_PROCESS_CPUTIME_ID : constant := 2;  --  time.h:50

      CLOCK_THREAD_CPUTIME_ID : constant := 3;  --  time.h:52

      TIMER_ABSTIME : constant := 1;  --  time.h:55

      type timeval is record
         tv_sec  : aliased long;  -- time.h:71:14
         tv_usec : aliased long;  -- time.h:72:19
      end record;
      pragma Convention (C_Pass_By_Copy, timeval);  -- time.h:70:3

   end bits_time_h;

   package time_h is
      subtype time_t is long;  -- time.h:76:18
   end time_h;

   package uuid_uuid_h is


      UUID_VARIANT_NCS : constant := 0;  --  uuid.h:47
      UUID_VARIANT_DCE : constant := 1;  --  uuid.h:48
      UUID_VARIANT_MICROSOFT : constant := 2;  --  uuid.h:49
      UUID_VARIANT_OTHER : constant := 3;  --  uuid.h:50

      UUID_TYPE_DCE_TIME : constant := 1;  --  uuid.h:53
      UUID_TYPE_DCE_RANDOM : constant := 4;  --  uuid.h:54

      type uuid_t is array (0 .. 15) of aliased unsigned_char;  -- uuid.h:44:23
      pragma Unreferenced (uuid_t);

      procedure uuid_clear (arg1 : access unsigned_char);  -- uuid.h:70:6
      pragma Import (C, uuid_clear, "uuid_clear");

      function uuid_compare
        (arg1 : access unsigned_char;
         arg2 : access unsigned_char) return int;  -- uuid.h:73:5
      pragma Import (C, uuid_compare, "uuid_compare");

      procedure uuid_copy (arg1 : access unsigned_char;
                           arg2 : access unsigned_char);  -- uuid.h:76:6
      pragma Import (C, uuid_copy, "uuid_copy");

      procedure uuid_generate
        (arg1 : access unsigned_char);  -- uuid.h:79:6
      pragma Import (C, uuid_generate, "uuid_generate");

      procedure uuid_generate_random
        (arg1 : access unsigned_char);  -- uuid.h:80:6
      pragma Import (C, uuid_generate_random, "uuid_generate_random");

      procedure uuid_generate_time
        (arg1 : access unsigned_char);  -- uuid.h:81:6
      pragma Import (C, uuid_generate_time, "uuid_generate_time");

      function uuid_is_null
        (arg1 : access unsigned_char) return int;  -- uuid.h:84:5
      pragma Import (C, uuid_is_null, "uuid_is_null");

      function uuid_parse
        (arg1 : access Character;
         arg2 : access unsigned_char) return int;  -- uuid.h:87:5
      pragma Import (C, uuid_parse, "uuid_parse");

      procedure uuid_unparse
        (arg1 : access unsigned_char;
         arg2 : access Character);  -- uuid.h:90:6
      pragma Import (C, uuid_unparse, "uuid_unparse");

      procedure uuid_unparse_lower
        (arg1 : access unsigned_char;
         arg2 : access Character);  -- uuid.h:91:6
      pragma Import (C, uuid_unparse_lower, "uuid_unparse_lower");

      procedure uuid_unparse_upper
        (arg1 : access unsigned_char;
         arg2 : access Character);  -- uuid.h:92:6
      pragma Import (C, uuid_unparse_upper, "uuid_unparse_upper");

      function uuid_time
        (arg1 : access unsigned_char;
         arg2 : access bits_time_h.timeval) return time_h.time_t; -- uuid.h:95
      pragma Import (C, uuid_time, "uuid_time");

      function uuid_type
        (arg1 : access unsigned_char) return int;  -- uuid.h:96:5
      pragma Import (C, uuid_type, "uuid_type");

      function uuid_variant
        (arg1 : access unsigned_char) return int;  -- uuid.h:97:5
      pragma Import (C, uuid_variant, "uuid_variant");

   end uuid_uuid_h;

   -----------------------------------------------------

   procedure Clear (this : in out UUID) is
   begin
      uuid_uuid_h.uuid_clear(this.data(this.data'First)'Unrestricted_Access);
   end Clear;

   function "<" (l, r : UUID) return Boolean is
   begin
      return uuid_uuid_h.uuid_compare (l.data (l.data'first)'Unrestricted_Access,
                                       r.data (r.data'first)'Unrestricted_Access) < 0;
   end "<";
   function ">" (l, r : UUID) return Boolean is
   begin
      return uuid_uuid_h.uuid_compare (l.data (l.data'first)'Unrestricted_Access,
                                       r.data(r.data'first)'Unrestricted_Access) > 0;
   end ">";

   function "=" (l, r : UUID) return Boolean is
   begin
      return uuid_uuid_h.uuid_compare (l.data (l.data'first)'Unrestricted_Access,
                                       r.data (r.data'first)'Unrestricted_Access) = 0;
   end "=";


   function Generate return  UUID is
   begin
      return ret : UUID do
         uuid_uuid_h.uuid_generate (ret.data (ret.data'First)'Unrestricted_Access);
      end return;
   end Generate;

   procedure Generate (this : out UUID) is
   begin
      this := Generate;
   end Generate;

   function Generate_Random return  UUID is
   begin
      return ret : UUID do
         uuid_uuid_h.uuid_generate_random (ret.data (ret.data'First)'Unrestricted_Access);
      end return;
   end Generate_Random;
   procedure Generate_Random (this : out UUID) is
   begin
      this := Generate_Random;
   end;

   procedure Generate_Time (this : out UUID) is
   begin
      this := Generate_Time;
   end;

   function Generate_Time return  UUID is
   begin
      return ret : UUID do
         uuid_uuid_h.uuid_generate_time (ret.data (ret.data'First)'Unrestricted_Access);
      end return;
   end Generate_Time;

   function Is_Null (this : UUID) return Boolean is
   begin
      return uuid_uuid_h.uuid_is_null (this.data (this.data'First)'Unrestricted_Access) /= 0;
   end Is_Null;

   function Parse (data : String) return UUID is
      ret : UUID;
      L_Data : constant string := data & ascii.NUL;
      res    : int;
   begin
      res := uuid_uuid_h.uuid_parse (L_Data (L_Data'First)'unrestricted_access,
                                     ret.data (ret.data'First)'unrestricted_access);
      if res /= 0 then
         raise PARSE_ERROR with "Unable to parse: '" & data & "'";
      else
         return ret;
      end if;
   end Parse;

   function Unparse (this : UUID) return String is
      ret  : String (1 .. 37);
   begin
      uuid_uuid_h.uuid_unparse (this.data (this.data'first)'Unrestricted_access, ret (1)'unrestricted_access);
      return ret (1 .. 36);
   end Unparse;

   function Unparse_Lower (this : UUID) return String is
      ret  : String (1 .. 37);
   begin
      uuid_uuid_h.uuid_unparse_lower (this.data (this.data'first)'Unrestricted_access, ret (1)'unrestricted_access);
      return ret(1..36);
   end Unparse_Lower;

   function Unparse_Upper (this : UUID) return String is
      ret  : String (1 .. 37) ;
   begin
      uuid_uuid_h.uuid_unparse_upper (this.data (this.data'first)'Unrestricted_access, ret (1)'unrestricted_access);
      return ret (1 .. 36);
   end Unparse_Upper;


   function Get_Type (this :  UUID) return UUID_Type is
      ret : int := uuid_uuid_h.uuid_type (this.data (this.data'first)'Unrestricted_access);
   begin
      case ret is
         when uuid_uuid_h.UUID_TYPE_DCE_TIME =>
            return DCE_TIME;
         when uuid_uuid_h.UUID_TYPE_DCE_RANDOM =>
            return DCE_RANDOM;
         when others =>
            return DCE_UNDEFINED;
      end case;
   end Get_Type;

   function Get_Variant (this : UUID) return UUID_Variant is
   begin
      return UUID_Variant'Val (uuid_uuid_h.uuid_variant (this.data (this.data'first)'Unrestricted_access));
   end Get_Variant;

end gnatcoll.uuid;
