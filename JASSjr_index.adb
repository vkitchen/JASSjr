-- Copyright (c) 2024 Vaughan Kitchen
-- Minimalistic BM25 search engine.

with Ada.Command_line;
with Ada.Text_IO;
use Ada;

procedure JASSjr_index is
begin
	-- Make sure we have one parameter, the filename
	if Command_Line.Argument_Count /= 1 then
		Text_IO.Put("Usage: ");
		Text_IO.Put(Command_Line.Command_Name);
		Text_IO.Put_Line(" <infile.xml>");
		return;
	end if;
	for Next in 1 .. Command_Line.Argument_Count loop
		Text_IO.Put_Line(Command_line.Argument(Next));
	end loop;
end JASSjr_index;
