with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.SHA256;

package body Simple_Blockchain is

    -----------
    -- Block --
    -----------

    function Image (This : Block) return String is
    begin
        return "Hash: " & This.Hash & ", " &
            "Previous Hash: " & This.Previous_Hash & ", " &
            "Timestamp: " & Image (This.Timestamp) & ", " &
            "Nonce: " & This.Nonce'Img & ", " &
            "Data: " & To_String (This.Data);
    end Image;

    function Make_Block (Previous_Hash : String; Data : String) return Block is
        Now : Time := Clock;
    begin
        return Block'(
            Hash => Calculate_Hash (Previous_Hash, Now, 0, Data),
            Previous_Hash => Previous_Hash,
            Timestamp => Now,
            Nonce => 0,
            Data => To_Unbounded_String (Data));
    end Make_Block;

    procedure Recalculate_Hash (This : in out Block) is
    begin
        This.Nonce := This.Nonce + 1;
        This.Timestamp := Clock;
        This.Hash := Calculate_Hash (This.Previous_Hash, This.Timestamp, This.Nonce, To_String (This.Data));
    end Recalculate_Hash;

    function Calculate_Hash (Previous_Hash : String; Timestamp : Time; Nonce : Natural; Data : String) return String is
    begin
        return GNAT.SHA256.Digest (Previous_Hash & Image (Timestamp) & Nonce'Img & Data);
    end Calculate_Hash;

    ----------------
    -- Blockchain --
    ----------------

    function Expected_Hash_Prefix return String is
    begin
        return Prefix : String (1 .. Difficulty) do
            Prefix := (others => '0');
        end return;
    end Expected_Hash_Prefix;

    function Is_Valid return Boolean is
        Current_Block : Block;
        Next_Block : Block;
    begin
        for I in Blocks.First_Index .. Blocks.Last_Index - 1 loop
            Current_Block := Element(Blocks, I);
            Next_Block := Element(Blocks, I + 1);
        end loop;

        if Next_Block.Hash /= Calculate_Hash(Next_Block.Previous_Hash, Next_Block.Timestamp, Next_Block.Nonce,  To_String(Next_Block.Data)) then
            return False;
        end if;

        if Current_Block.Hash /= Next_Block.Previous_Hash then
            return False;
        end if;

        if Next_Block.Hash (1 .. Difficulty) /= Expected_Hash_Prefix then
            return False;
        end if;

        return True;
    end Is_Valid;

    function Last_Block return Block is (Last_Element(Blocks));

    procedure Mine_Block (Data : in String) is
        New_Block : Block;
        Previous_Hash : String (1 .. 64);
    begin
        if Blocks.Is_Empty then
            Previous_Hash := (others => '0');
        else
            Previous_Hash := Last_Block.Hash;
        end if;

        New_Block := Make_Block(Previous_Hash, Data);

        while New_Block.Hash (1 .. Difficulty) /= Expected_Hash_Prefix loop
            Recalculate_Hash(New_Block);
        end loop;

        Blocks.Append(New_Block);
    end Mine_Block;

    procedure Print_Blockchain is
    begin
        Put_Line("Blockchain - difficulty: " & Difficulty'Img & ", blocks: " & Blocks.Length'Img);

        for Block of Blocks loop
            Put_Line(Block.Image);
        end loop;
    end Print_Blockchain;

end Simple_Blockchain;
