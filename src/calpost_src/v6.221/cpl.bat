REM Compiling and linking with LF95

del params.pst
copy paramss.pst params.pst
lf95 calpost.for -o0 -trap doi -out calposts.exe >cpl_s

del params.pst
copy paramsm.pst params.pst
lf95 calpost.for -o0 -trap doi -out calpostm.exe >cpl_m

del params.pst
copy paramsl.pst params.pst
lf95 calpost.for -o0 -trap doi -out calpostl.exe >cpl_l
