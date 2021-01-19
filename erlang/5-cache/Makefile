all:
	ls *.erl | xargs -I%i erlc +debug_info %i

typer:
	ls *.beam | xargs -I%i dialyzer --build_plt %i
	ls *.beam | xargs -I%i dialyzer %i
	ls *.beam | xargs -I%i typer %i
