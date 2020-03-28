-------------------------------------------------------------------------------
--            Copyright (C) 2020-2030, per.s.sandberg@bahnhof.se             --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------



with Ada.Containers.Indefinite_Vectors;
package ZMQ.Examples.Sparse is
   type Sparse_Data;
   type Sparse_Data_Access is access all Sparse_Data'Class;

   type Sparse_Data (Name : not null access String) is abstract tagged record
      Changed : Boolean := False;
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
