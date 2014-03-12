open Khs_ast
open Khs_exec

let primes_khs = [|
    SGoto (EStr "proc_main");

    (* integers *)
    SLabel "proc_integers";
    SGoto (
        ETernary(
            EBinary(ELoad(ELocal "n"), GT, ELoad(ELocal "nmax")),
            EStr "_then1", EStr "_else1"));

    SLabel "_then1";
    SSend(EInt (-1), ELocal "qo");
    SGoto(EStr "_end1");

    SLabel "_else1";
    SSend(ELoad(ELocal "n"), ELocal "qo");
    SSet(ELocal "n",
        EBinary(ELoad(ELocal "n"), PLUS, EInt 1));
    SGoto (EStr "proc_integers");

    SLabel "_end1";
    SSet(ELocal "retval", EEmpty);
    SUnset [ELocal "qo"; ELocal "nmax"; ELocal "n"];
    SGoto(ELoad(ELocal "retpos"));

    (* filter *)
    SLabel "proc_filter";
    SRecv(ELocal "val", ELocal "qi");
    SGoto(ETernary(
        EBinary(ELoad(ELocal "val"), NEQUAL, EInt (-1)),
        EStr "_then2", EStr "_else2"));

    SLabel "_then2";
    SGoto(ETernary(
        EBinary(EBinary(ELoad(ELocal "val"), MOD, ELoad(ELocal "n")), NEQUAL, EInt 0),
        EStr "_then3", EStr "_end3"));

    SLabel "_then3";
    SSend(ELoad(ELocal "val"), ELocal "qo");

    SLabel "_end3";
    SGoto(EStr "proc_filter");
    
    SLabel "_else2";
    SSend(EInt (-1), ELocal "qo");

    SSet(ELocal "retval", EEmpty);
    SUnset[ELocal "qo"; ELocal "qi"; ELocal "val"; ELocal "n"];
    SGoto(ELoad(ELocal "retpos"));

    (* primes *)
    SLabel "proc_primes";
    SRecv(ELocal "val", ELocal "qi");
    SGoto(ETernary(
        EBinary(ELoad(ELocal "val"), NEQUAL, EInt(-1)),
        EStr "_then4", EStr "_end4"));

    SLabel "_then4";
    SSend(ELoad(ELocal "val"), EStr "stdout");
    SSet(ELocal "c", ENewChan);
    SPar(EStr "_par_1");
    SSet(ELocal "qi", ELoad(ECat(ELocal "c", EStr "in")));
    SGoto(EStr "proc_primes");

    SLabel "_end4";
    SSet(ELocal "retval", EEmpty);
    SUnset[ELocal "qi"; ELocal "val"; ELocal "c"];
    SGoto(ELoad(ELocal "retpos"));

    SLabel "_par_1";
    SSet(ELocal "n", ELoad(ELocal "val"));
    SSet(ELocal "qo", ELoad(ECat(ELocal "c", EStr "out")));
    SGoto(EStr "proc_filter");


    (* main *)
    SLabel "proc_main";
    SSet(ELocal "c", ENewChan);
    SPar(EStr "_par_2");

    SSet(ELocal "qi", ELoad(ECat(ELocal "c", EStr "in")));
    SSet(ELocal "retpos", EStr "_ret1");
    SGoto(EStr "proc_primes");

    SLabel "_par_2";
    SSet(ELocal "n", EInt 2);
    SSet(ELocal "nmax", EInt 3000);
    SSet(ELocal "qo", ELoad(ECat(ELocal "c", EStr "out")));
    SSet(ELocal "retpos", EStr "_ret1");
    SGoto(EStr "proc_integers");

    SLabel "_ret1";
    SExit;
|]

let () =
    Array.iter Ksh_print.print_stmt primes_khs;
    Khs_exec_seq.exec_program (load_program primes_khs)
