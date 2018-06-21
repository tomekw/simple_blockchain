with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Simple_Blockchain.Blockchain is
   function Get_Blocks (This : Object) return Block_Vectors.Vector is (This.Blocks);

   function Get_Difficulty (This : Object) return Natural is (This.Difficulty);

   function Image (This : Object) return String is
      Result : Unbounded_String;
      EOL : String(1 .. 2) := (1 => Ada.Characters.Latin_1.CR, 2 => Ada.Characters.Latin_1.LF);
   begin
      Append (Result, "Blockchain - difficulty: " & Get_Difficulty (This)'Image & ", blocks: " & Block_Vectors.Length (Get_Blocks (This))'Image & EOL);

      for I in Get_Blocks (This).First_Index .. Get_Blocks (This).Last_Index loop
         Append (Result, Block.Image (Get_Blocks (This) (I)) & EOL);
      end loop;

      return To_String (Result);
   end Image;

   function Is_Valid (This : Object) return Boolean is
      Current_Block : Block.Object;
      Next_Block : Block.Object;
   begin
      for I in Get_Blocks (This).First_Index .. (Get_Blocks (This).Last_Index - 1) loop
         Current_Block := Block_Vectors.Element (Get_Blocks (This), I);
         Next_Block := Block_Vectors.Element (Get_Blocks (This), I + 1);

         if Block.Get_Hash (Next_Block) /= Block.Calculate_Hash (Block.Get_Previous_Hash (Next_Block), Block.Get_Timestamp (Next_Block), Block.Get_Nonce (Next_Block), Block.Get_Data (Next_Block)) then
            return False;
         end if;

         if Block.Get_Hash (Current_Block) /= Block.Get_Previous_Hash (Next_Block) then
            return False;
         end if;

         if Block.Get_Hash (Next_Block) (1.. Get_Difficulty (This)) /= Expected_Hash_Prefix (This) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid;

   function Make (Difficulty : Natural) return Object is
   begin
      return (
              Blocks => Block_Vectors.Empty_Vector,
              Difficulty => Difficulty);
   end Make;

   procedure Mine_Block (This : in out Object; Data : String) is
      New_Block : Block.Object;
      Previous_Hash : String (1 .. 64);
   begin
      if Is_Empty (This) then
         Previous_Hash := (others => '0');
      else
         Previous_Hash := Block.Get_Hash (Last_Block (This));
      end if;

      New_Block := Block.Make (Previous_Hash => Previous_Hash, Data => Data);

      while Block.Get_Hash (New_Block) (1 .. Get_Difficulty (This)) /= Expected_Hash_Prefix (This) loop
         Block.Recalculate_Hash (New_Block);
      end loop;

      Block_Vectors.Append (This.Blocks, New_Block);
   end Mine_Block;

   function Expected_Hash_Prefix (This : Object) return String is
   begin
      return Prefix : String (1 .. Get_Difficulty (This)) do
         Prefix := (others => '0');
      end return;
   end Expected_Hash_Prefix;

   function Is_Empty (This : Object) return Boolean is (Block_Vectors.Is_Empty (Get_Blocks (This)));

   function Last_Block (This : Object) return Block.Object is (Block_Vectors.Last_Element (Get_Blocks (This)));
end Simple_Blockchain.Blockchain;
