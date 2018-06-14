with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with GNAT.SHA256;

package body Simple_Blockchain.Block is
   function Get_Data (This : Object) return String is (To_String (This.Data));

   function Get_Hash (This : Object) return String is (This.Hash);

   function Get_Nonce (This : Object) return Natural is (This.Nonce);

   function Get_Previous_Hash (This : Object) return String is (This.Previous_Hash);

   function Get_Timestamp (This : Object) return Time is (This.Timestamp);

   function Image (This : Object) return String is
   begin
      return "Hash: " & Get_Hash (This) &
        ", Previous hash: " & Get_Previous_Hash (This) &
        ", Timestamp: " & Image (Get_Timestamp (This)) &
        ", Nonce: " & Get_Nonce (This)'Image &
        ", Data: " & Get_Data (This);
   end Image;

   function Make (Previous_Hash : String; Data : String) return Object is
      Now : Time := Clock;
      Nonce : Natural := 0;
   begin
      return (
              Hash => Calculate_Hash (Previous_Hash, Now, Nonce, Data),
              Previous_Hash => Previous_Hash,
              Timestamp => Now,
              Nonce => Nonce,
              Data => To_Unbounded_String (Data));
   end Make;

   procedure Recalculate_Hash (This : in out Object) is
   begin
      This.Nonce := Get_Nonce (This) + 1;
      This.Timestamp := Clock;
      This.Hash := Calculate_Hash (Get_Previous_Hash (This), Get_Timestamp (This), Get_Nonce (This), Get_Data (This));
   end Recalculate_Hash;

   function Calculate_Hash (Previous_Hash : String; Timestamp : Time; Nonce : Natural; Data: String) return String is
   begin
      return GNAT.SHA256.Digest (Previous_Hash & Image (Timestamp) & Nonce'Image & Data);
   end Calculate_Hash;
end Simple_Blockchain.Block;
