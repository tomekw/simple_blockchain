with Ada.Containers.Vectors; use Ada.Containers;

with Simple_Blockchain.Block;

use Simple_Blockchain;

package Simple_Blockchain.Blockchain is
   use type Block.Object;

   package Block_Vectors is new Vectors (Index_Type => Positive, Element_Type => Block.Object);

   type Object is private;

   function Get_Blocks (This : Object) return Block_Vectors.Vector;
   function Get_Difficulty (This : Object) return Natural;
   function Image (This : Object) return String;
   function Is_Valid (This : Object) return Boolean;
   function Make (Difficulty : Natural) return Object;
   procedure Mine_Block (This : in out Object; Data : String);
private

   type Object is
      record
         Blocks : Block_Vectors.Vector;
         Difficulty : Natural;
      end record;

   function Expected_Hash_Prefix (This : Object) return String;
   function Is_Empty (This : Object) return Boolean;
   function Last_Block (This : Object) return Block.Object;
end Simple_Blockchain.Blockchain;
