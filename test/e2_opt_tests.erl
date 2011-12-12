-module(e2_opt_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Opts = e2_opt:validate([{color, "blue"}], [color]),
    ?assertEqual("blue", e2_opt:value(color, Opts)).

required_test() ->
    ?assertError({required, name}, e2_opt:validate([], [name])).

default_test() ->
    Schema = [{name, [{default, "Sam"}]}],
    ?assertEqual("Sam", e2_opt:value(name, e2_opt:validate([], Schema))),
    ?assertEqual("Bob", e2_opt:value(name, e2_opt:validate(
                                             [{name, "Bob"}], Schema))).

undefined_default_val_test() ->
    Opts = e2_opt:validate([], [{name, [{default, undefined}]}]),
    ?assertEqual(undefined, e2_opt:value(name, Opts)).

int_type_test() ->
    Schema = [{age, [{type, int}]}],
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate([{age, 99}], Schema))),
    ?assertError({value, age}, e2_opt:validate([{age, not_an_int}], Schema)).

float_type_test() ->
    Schema = [{age, [{type, float}]}],
    ?assertEqual(9.9, e2_opt:value(age, e2_opt:validate(
                                          [{age, 9.9}], Schema))),
    ?assertError({value, age}, e2_opt:validate(
                                 [{age, 99}], Schema)),
    ?assertError({value, age}, e2_opt:validate(
                                 [{age, "not a float"}], Schema)).

number_type_test() ->
    Schema = [{age, [{type, number}]}],
    ?assertEqual(9.9, e2_opt:value(age, e2_opt:validate(
                                          [{age, 9.9}], Schema))),
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate(
                                          [{age, 99}], Schema))),
    ?assertError({value, age}, e2_opt:validate(
                                 [{age, <<"not a number">>}], Schema)).

string_type_test() ->
    Schema = [{name, [{type, string}]}],
    ?assertEqual("Stan", e2_opt:value(
                           name, e2_opt:validate(
                                   [{name, "Stan"}], Schema))),
    ?assertEqual(<<"Stan">>, e2_opt:value(
                               name, e2_opt:validate(
                                       [{name, <<"Stan">>}], Schema))),
    ?assertError({value, name}, e2_opt:validate([{name, stan}], Schema)).

bad_type_test() ->
    ?assertError({option, type},
                 e2_opt:validate([], [{foo, [{type, widget}]}])).

values_test() ->
    Schema = [{gender, [{values, [male, female]}]}],
    ?assertEqual(male, e2_opt:value(gender, e2_opt:validate(
                                              [{gender, male}], Schema))),
    ?assertEqual(female, e2_opt:value(gender, e2_opt:validate(
                                              [{gender, female}], Schema))),
    ?assertError({value, gender}, e2_opt:validate([{gender, other}], Schema)).

bad_values_test() ->
    ?assertError({option, values},
                 e2_opt:validate([], [{foo, [{values, bad_list}]}])).

min_test() ->
    Schema = [{age, [{min, 0}]}],
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate(
                                         [{age, 99}], Schema))),
    ?assertEqual(0, e2_opt:value(age, e2_opt:validate(
                                         [{age, 0}], Schema))),
    ?assertError({value, age}, e2_opt:validate([{age, -1}], Schema)).

max_test() ->
    Schema = [{age, [{max, 99}]}],
    ?assertEqual(50, e2_opt:value(age, e2_opt:validate(
                                         [{age, 50}], Schema))),
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate(
                                         [{age, 99}], Schema))),
    ?assertError({value, age}, e2_opt:validate([{age, 100}], Schema)).

min_max_test() ->
    Schema = [{age, [{min, 0}, {max, 99}]}],
    ?assertEqual(0, e2_opt:value(age, e2_opt:validate(
                                         [{age, 0}], Schema))),
    ?assertEqual(50, e2_opt:value(age, e2_opt:validate(
                                         [{age, 50}], Schema))),
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate(
                                         [{age, 99}], Schema))),
    ?assertError({value, age}, e2_opt:validate([{age, -1}], Schema)),
    ?assertError({value, age}, e2_opt:validate([{age, 100}], Schema)).

pattern_test() ->
    Schema = [{email, [{pattern, "[a-z]+@[a-z]+\\.[a-z]+"}]}],
    ?assertEqual("dude@car.com", e2_opt:value(
                                   email, e2_opt:validate(
                                            [{email, "dude@car.com"}],
                                            Schema))),
    ?assertError({value, email}, e2_opt:validate([{email, "dude"}], Schema)).

bad_pattern_test() ->
    ?assertError({option, pattern}, e2_opt:validate(
                                      [], [{email, [{pattern, "[a-z"}]}])).

missing_val_test() ->
    Opts = e2_opt:validate([], []),
    ?assertError(badarg, e2_opt:value(name, Opts)).

usage_test() ->
    Schema = [{name, [{type, string}]},
              {email, [{pattern, "[a-z]+@[a-z]+\\.[a-z]+"},
                       {default, undefined}]},
              {gender, [{values, [male, female]}, {default, unknown}]},
              {age, [{type, int}, {min, 21}, {max, 100}]}],

    Jim = e2_opt:validate([{name, "Jim"},
                           {email, "jim@car.com"},
                           {gender, male},
                           {age, 35}], Schema),
    ?assertEqual("Jim", e2_opt:value(name, Jim)),
    ?assertEqual("jim@car.com", e2_opt:value(email, Jim)),
    ?assertEqual(male, e2_opt:value(gender, Jim)),
    ?assertEqual(35, e2_opt:value(age, Jim)),
    ?assertError(badarg, e2_opt:value(ssn, Jim)),

    Katherin = e2_opt:validate([{name, "Katherin"}, {age, 29}], Schema),
    ?assertEqual("Katherin", e2_opt:value(name, Katherin)),
    ?assertEqual(undefined, e2_opt:value(email, Katherin)),
    ?assertEqual(unknown, e2_opt:value(gender, Katherin)),
    ?assertEqual(29, e2_opt:value(age, Katherin)),

    ok.
