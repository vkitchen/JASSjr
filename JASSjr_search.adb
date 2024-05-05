-- Copyright (c) 2024 Vaughan Kitchen
-- Minimalistic BM25 search engine.

with Ada.Containers.Vectors;
with Ada.Sequential_IO;
with Ada.Text_IO;
use Ada;

procedure JASSjr_search is
	package Integer_IO is
		new Sequential_IO (Integer);
	use Integer_IO;
	package IIO is
		new Text_IO.Integer_IO (Integer);
	package Integer_vectors is
		new Ada.Containers.Vectors
			(Index_Type => Natural,
			Element_Type => Integer);
	F : Integer_IO.File_Type;
	V : Integer_Vectors.Vector;
	Num : Integer;
begin
	Open (F, In_File, "lengths.bin");
	while not End_Of_File (F) loop
		Read (F, Num);
		V.Append (Num);
	end loop;
	Close (F);
	for Num of V loop
		IIO.Put (Num);
		Text_IO.New_Line;
	end loop;
end JASSjr_search;
