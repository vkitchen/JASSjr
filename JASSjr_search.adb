-- Copyright (c) 2024 Vaughan Kitchen
-- Minimalistic BM25 search engine.

with Ada.Containers.Vectors;
with Ada.Sequential_IO;
with Ada.Text_IO;
use Ada;

procedure JASSjr_search is
	package Sequential_Integer_IO is new Sequential_IO (Integer);
	package Integer_Vector is new Ada.Containers.Vectors
		( Index_Type => Natural
		, Element_Type => Integer
		);
	package Text_Integer_IO is new Text_IO.Integer_IO (Integer);
	Doc_lengths_FH : Sequential_Integer_IO.File_Type;
	Doc_Lengths : Integer_Vector.Vector;
	Num : Integer;
begin
	-- Read the primary keys
	-- Read the document lengths
	Sequential_Integer_IO.Open (Doc_Lengths_FH, Sequential_Integer_IO.In_File, "lengths.bin");
	while not Sequential_Integer_IO.End_Of_File (Doc_Lengths_FH) loop
		Sequential_Integer_IO.Read (Doc_Lengths_FH, Num);
		Doc_Lengths.Append (Num);
	end loop;
	Sequential_Integer_IO.Close (Doc_Lengths_FH);
	for Num of Doc_Lengths loop
		Text_Integer_IO.Put (Num);
		Text_IO.New_Line;
	end loop;
end JASSjr_search;
