Caml1999N029����            ,src/game.mli����  �  N  �  �����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���-ppxlib_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������/ppx_optcomp.env�@�@@�������#env��&_none_A@ ��A@ �A��A@ ��A@ �A@@���-ocaml_version����'Defined��A@ ��A@ �A�������!4@��A@ ��A@ �A@@����"12@��#A@ ��$A@ �A@@����!0@��+A@ ��,A@ �A@@@��.A@ ��/A@ �A@@��1A@ ��2A@ �A@@@��4A@ ��5A@ �A@@@�@@�������@�@@@�@@�@@@@�@@@�@�ڠ�����$Base��,src/game.mliA@F�A@J@@��A@@�A@J@@��A@@�A@J@���A�    �!t��CLQ�CLR@@@@A@���(deriving��CLV�CL^@��������'sexp_of��$CL_�%CLf@��'CL_�(CLf@@@@��*CL_�+CLf@@��-CLS�.CLg@���)ocaml.doc��@@ ��@@ �A�������	n A [t] represents the whole game state, which is composed of the
    current caravan, cactus, and game state. �� @@ ��!@@ �A@��BDhh�CE l �@@@@��EDhh�FE l �@@��HDhh�IE l �@@��KCLL�LCLg@@��NCLL�OCLg@�����������-ocaml.warning���A@ ���A@ �A�������#-32��I@@ ��J@@ �A@���A@ ���A@ �A@@@���A@ ���A@ �A@���A@ ���A@ �A��tCLL�uCLgA���Р)sexp_of_t��}CLQ�~CLR@��@����v���CLQ��CLR@@���CLQ��CLR@@@������1Ppx_sexp_conv_lib$Sexp!t���CLQ��CLRA@���CLQ��CLRA@@���CLQ��CLRA@@@@���CLL��CLg@���CLL��CLg@@���CLL��CLgA@���CLL��CLgA���)ocaml.doc���A@ ���A@ �A�������'@inline���@@ ���@@ �A@���A@ �� A@ �A@@@��A@ ��A@ �A@��A@ ��A@ �A���+merlin.hide��A@ ��A@ �A�@���@@ ���@@ �A@���CLL��CLgA���Р)in_bounds���G � ���G � �@��@����!t���G � ���G � �@@���G � ���G � �@@@��@�����(Position!t���G � ���G � �@@���G � ���G � �@@@����$bool���G � ���G � �@@�� G � ��G � �@@@��G � ��G � �@@@��G � ��G � �@@@@���ٰ��@@ ���@@ �A�������	� [in_bounds game { Position.row; col }] is whether the position is
    within the valid bounds of the game. It returns [true] if the
    position is valid, false otherwise. ���@@ ���@@ �A@��H � ��JMw@@@@��H � ��JMw@@�� H � ��!JMw@@��#G � ��$G � �@��&G � ��'G � �@���Р&create��/Ly}�0Ly�@���&height����#int��;Ly��<Ly�@@��>Ly��?Ly�@@@���%width����#int��JLy��KLy�@@��MLy��NLy�@@@���6initial_caravan_length����#int��YLy��ZLy�@@��\Ly��]Ly�@@@����!t��dLy��eLy�@@��gLy��hLy�@@@��jLy��kLy�@@@��mLy��nLy�@@@��pLy��qLy�@@@@���C��W@@ ��X@@ �A�������	d [create ~height ~width ~initial_caravan_lengthh] creates a new game
    with specified parameters. ��b@@ ��c@@ �A@���M����N	*@@@@���M����N	*@@���M����N	*@@���Lyy��Ly�@���Lyy��Ly�@���Р%width���P,0��P,5@��@����!t���P,8��P,9@@���P,8��P,9@@@����#int���P,=��P,@@@���P,=��P,@@@@���P,8��P,@@@@@�������@@ ���@@ �A�������	- [get_width game] gets the width of the game ���@@ ���@@ �A@���QAA��QAs@@@@���QAA��QAs@@���QAA��QAs@@���P,,��P,@@���P,,��P,@@���Р&height���Suy��Su@��@����!t���Su���Su�@@���Su���Su�@@@����#int���Su���Su�@@���Su���Su�@@@���Su���Su�@@@@���˰��@@ ���@@ �A�������	/ [get_height game] gets the height of the game ���@@ ���@@ �A@��T���T��@@@@��T���T��@@��T���T��@@��Suu�Su�@��Suu�Su�@���Р'caravan��!V���"V��@��@����!t��+V���,V��@@��.V���/V��@@@�����%Camel'caravan��8V���9V��@@��;V���<V��@@@��>V���?V��@@@@�����%@@ ��&@@ �A�������	P [get_caravan game] is the caravan of camels that is currently in the
    game. ��0@@ ��1@@ �A@��RW���SX+7@@@@��UW���VX+7@@��XW���YX+7@@��[V���\V��@��^V���_V��@���Р&cactus��gZ9=�hZ9C@��@����!t��qZ9F�rZ9G@@��tZ9F�uZ9G@@@�����&Cactus!t��~Z9K�Z9S@@���Z9K��Z9S@@@���Z9F��Z9S@@@@���W��k@@ ��l@@ �A�������	< [get_cactus game] returns the cactus currently in the game ��v@@ ��w@@ �A@���[TT��[T�@@@@���[TT��[T�@@���[TT��[T�@@���Z99��Z9S@���Z99��Z9S@���Р-set_direction���]����]��@��@����!t���]����]��@@���]����]��@@@��@�����%Camel)direction���]����]��@@���]����]��@@@����$unit���]����]��@@���]����]��@@@���]����]��@@@���]����]��@@@@�������@@ ���@@ �A�������	D [set_directon game d] sets a new direction that the caravan faces. ���@@ ���@@ �A@���^����^�@@@@���^����^�@@���^����^�@@���]����]��@���]����]��@���Р,set_dir_game��`�`#@��@����!t��`&�`'@@��`&�`'@@@��@�����%Camel)direction��`+�`:@@��`+� `:@@@����!t��'`>�(`?@@��*`>�+`?@@@��-`+�.`?@@@��0`&�1`?@@@@�����@@ ��@@ �A�������	d [set_dir_game game d] sets a new direction that the caravan faces
    and returns the game itself. ��"@@ ��#@@ �A@��Da@@�Eb��@@@@��Ga@@�Hb��@@��Ja@@�Kb��@@��M`�N`?@��P`�Q`?@���Р-get_direction��Yd���Zd��@��@����!t��cd���dd��@@��fd���gd��@@@�����%Camel)direction��pd���qd��@@��sd���td��@@@��vd���wd��@@@@���I��]@@ ��^@@ �A�������	E [get_direction game] gets the new direction that the caravan faces. ��h@@ ��i@@ �A@���e����e�@@@@���e����e�@@���e����e�@@���d����d��@���d����d��@���Р5get_string_game_state���g $��g 9@��@����!t���g <��g =@@���g <��g =@@@����&string���g A��g G@@���g A��g G@@@���g <��g G@@@@�������@@ ���@@ �A�������	M [get_game_state game] gets the string of the current state of the
    game. ���@@ ���@@ �A@���hHH��i��@@@@���hHH��i��@@���hHH��i��@@���g  ��g G@���g  ��g G@���Р*game_state���k����k��@��@����!t���k����k��@@���k����k��@@@�����*Game_state!t���k����k��@@���k����k��@@@�� k���k��@@@@���Ӱ��@@ ���@@ �A�������	5 [game_state game] is the current state of the game. ���@@ ���@@ �A@��l���l��@@@@��l���l��@@��l���l��@@��k���k��@�� k���!k��@���Р$step��)n���*n�@��@����!t��3n��4n�@@��6n��7n�@@@����$unit��>n��?n�@@��An��Bn�@@@��Dn��En�@@@@�����+@@ ��,@@ �A�������	Q [step game] called within a loop, with the game re-rendering after
    each call��6@@ ��7@@ �A@��Xo�YpWf@@@@��[o�\pWf@@��^o�_pWf@@��an���bn�@��dn���en�@@