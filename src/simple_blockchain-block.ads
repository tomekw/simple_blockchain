with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Simple_Blockchain.Block is
   type Object is private;

   function Get_Data (This : Object) return String;
   function Get_Hash (This : Object) return String;
   function Get_Nonce (This : Object) return Long_Long_Integer;
   function Get_Previous_Hash (This : Object) return String;
   function Get_Timestamp (This : Object) return Time;
   function Image (This : Object) return String;
   function Make (Previous_Hash : String; Data : String) return Object;
   procedure Recalculate_Hash (This : in out Object);

   function Calculate_Hash (Previous_Hash : String; Timestamp : Time; Nonce : Long_Long_Integer; Data: String) return String;

private

   type Object is
      record
         Hash : String (1 .. 64);
         Previous_Hash : String (1 .. 64);
         Timestamp : Time;
         Nonce : Long_Long_Integer;
         Data : Unbounded_String;
      end record;
end Simple_Blockchain.Block;
