

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
package ZMQ.Examples.Sparse is
   type Sparse_Data;
   type Sparse_Data_Access is access all Sparse_Data'Class;

   type Sparse_Data (Name : not null access String) is tagged record
      Changed : Boolean := False;
      Name    : GNAT.Strings.String_Access;
      Parent  : Sparse_Data_Access;
   end record;

   package Sparse_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, Sparse_Data_Access);

   type Sparse_Integer is new Sparse_Data with record
      Value : access Integer;
   end record;
   procedure Set (Self : Sparse_Integer; Value : Integer);

   type Sparse_Boolean is new Sparse_Data with record
      Value : access Boolean;
   end record;
   procedure Set (Self : Sparse_Integer; Value : Boolean);
   type Sparse_Float is new Sparse_Data with record
      Value : access Float;
   end record;
   type Sparse_String is new Sparse_Data with record
      Value : access String;
   end record;
   type Sparse_Container is new Sparse_Data with record
      Value : access Sparse_Vectors.Vector;
   end record;
   type Sparse_Object is new Sparse_Data with record
      Value : access Sparse_Vectors.Vector;
   end record;

end ZMQ.Examples.Sparse;
