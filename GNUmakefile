all : ada cpp crystal d_dmd d_ldc fortran java rust zig tools

ada : JASSjr_index_ada JASSjr_search_ada

cpp : JASSjr_index JASSjr_search

crystal : JASSjr_index_crystal JASSjr_search_crystal

d_dmd : JASSjr_index_d_dmd JASSjr_search_d_dmd

d_ldc : JASSjr_index_d_ldc JASSjr_search_d_ldc

fortran : JASSjr_index_fortran JASSjr_search_fortran

java : JASSjr_index.class JASSjr_search.class

rust : JASSjr_index_rust JASSjr_search_rust

zig : JASSjr_index_zig JASSjr_search_zig

JASSjr_index_ada : JASSjr_index.adb
	gnatmake -o JASSjr_index_ada JASSjr_index.adb

JASSjr_search_ada : JASSjr_search.adb
	gnatmake -o JASSjr_search_ada JASSjr_search.adb

JASSjr_index : JASSjr_index.cpp
	$(CXX) -std=c++11 -O3 -Wno-unused-result JASSjr_index.cpp -o JASSjr_index

JASSjr_search : JASSjr_search.cpp
	$(CXX) -std=c++11 -O3 -Wno-unused-result JASSjr_search.cpp -o JASSjr_search

JASSjr_index.class : JASSjr_index.java
	javac JASSjr_index.java

JASSjr_search.class : JASSjr_search.java
	javac JASSjr_search.java

JASSjr_index_crystal : JASSjr_index.cr
	crystal build --release -o JASSjr_index_crystal JASSjr_index.cr

JASSjr_search_crystal : JASSjr_search.cr
	crystal build --release -o JASSjr_search_crystal JASSjr_search.cr

JASSjr_index_d_dmd : JASSjr_index.d
	dmd -O -of=JASSjr_index_d_dmd JASSjr_index.d

JASSjr_search_d_dmd : JASSjr_search.d
	dmd -O -of=JASSjr_search_d_dmd JASSjr_search.d

JASSjr_index_d_ldc : JASSjr_index.d
	ldc2 -O3 --of=JASSjr_index_d_ldc JASSjr_index.d

JASSjr_search_d_ldc : JASSjr_search.d
	ldc2 -O3 --of=JASSjr_search_d_ldc JASSjr_search.d

JASSjr_index_fortran : JASSjr_index.f90
	gfortran -std=f2003 -O3 -Wall -Wextra JASSjr_index.f90 -o JASSjr_index_fortran

JASSjr_search_fortran : JASSjr_search.f90
	gfortran -std=f2003 -O3 -Wall -Wextra JASSjr_search.f90 -o JASSjr_search_fortran

JASSjr_index_rust : JASSjr_index.rs
	rustc -O -o JASSjr_index_rust JASSjr_index.rs

JASSjr_search_rust : JASSjr_search.rs
	rustc -O -o JASSjr_search_rust JASSjr_search.rs

JASSjr_index_zig : JASSjr_index.zig
	zig build-exe -O ReleaseFast --name JASSjr_index_zig JASSjr_index.zig

JASSjr_search_zig : JASSjr_search.zig
	zig build-exe -O ReleaseFast --name JASSjr_search_zig JASSjr_search.zig

.PHONY: tools
tools:
	make -C tools

clean:
	- rm JASSjr_index_ada JASSjr_search_ada
	- rm JASSjr_index JASSjr_search
	- rm 'JASSjr_index.class' 'JASSjr_search.class' 'JASSjr_index$$Posting.class' 'JASSjr_index$$PostingsList.class' 'JASSjr_search$$CompareRsv.class' 'JASSjr_search$$VocabEntry.class'
	- rm JASSjr_index_crystal JASSjr_search_crystal
	- rm JASSjr_index_d_dmd JASSjr_search_d_dmd JASSjr_index_d_ldc JASSjr_search_d_ldc
	- rm JASSjr_index_fortran JASSjr_search_fortran dynarray_integer_mod.mod dynarray_string_mod.mod lexer_mod.mod vocab_mod.mod
	- rm JASSjr_index_rust JASSjr_search_rust
	- rm JASSjr_index_zig JASSjr_search_zig
	- make -C tools clean

clean_index:
	- rm docids.bin lengths.bin postings.bin vocab.bin

clean_all : clean clean_index
